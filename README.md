# CCC - The Claude C Compiler

A C compiler written from scratch in Rust, targeting x86-64, i686, AArch64, and RISC-V 64.
No compiler-specific dependencies -- the frontend, IR, optimizer, code generator, peephole
optimizers, and DWARF debug info generation are all implemented from scratch. Assembly and
linking currently delegate to the GNU toolchain; a native assembler/linker is planned.

## Building

```bash
cargo build --release
```

This produces five binaries in `target/release/`, all compiled from the same source.
The target architecture is selected by the binary name at runtime:

| Binary | Target |
|--------|--------|
| `ccc` | x86-64 (default) |
| `ccc-x86` | x86-64 |
| `ccc-arm` | AArch64 |
| `ccc-riscv` | RISC-V 64 |
| `ccc-i686` | i686 (32-bit x86) |

## Usage

```bash
# Compile and link
ccc -o output input.c                # x86-64
ccc-arm -o output input.c            # AArch64
ccc-riscv -o output input.c          # RISC-V 64
ccc-i686 -o output input.c           # i686

# GCC-compatible flags
ccc -S input.c                       # Emit assembly
ccc -c input.c                       # Compile to object file
ccc -E input.c                       # Preprocess only
ccc -O2 -o output input.c            # Optimize (accepts -O0 through -O3, -Os, -Oz)
ccc -g -o output input.c             # DWARF debug info
ccc -DFOO=1 -Iinclude/ input.c       # Define macros, add include paths
ccc -Werror -Wall input.c            # Warning control
ccc -fPIC -shared -o lib.so lib.c    # Position-independent code
ccc -x c -E -                        # Read from stdin

# Build system integration (reports as GCC 14.2.0 for compatibility)
ccc -dumpmachine     # x86_64-linux-gnu / aarch64-linux-gnu / riscv64-linux-gnu / i686-linux-gnu
ccc -dumpversion     # 14
```

The compiler accepts most GCC flags. Unrecognized flags (e.g., architecture-specific `-m`
flags, unknown `-f` flags) are silently ignored so `ccc` can serve as a drop-in GCC
replacement in build systems.

## Status

The compiler can build and run real-world C projects including the Linux kernel
and PostgreSQL across all four target architectures.

### Projects successfully compiled

| Project | Notes |
|---------|-------|
| Linux kernel | Builds and boots on x86-64, AArch64, and RISC-V |
| PostgreSQL | Full regression suite on x86, ARM, RISC-V |
| Redis | SET/GET roundtrip |
| SQLite | sqllogictest suite |
| tcc | Full test suite including tests2 |
| zlib | Build + self-test + minigzip roundtrip |
| Lua | All interpreter tests |
| libsodium | Crypto tests on all architectures |
| QuickJS | Closure, language, loop, builtin, bytecode tests |
| libpng | pngtest |
| libjpeg-turbo | cjpeg/djpeg roundtrip, jpegtran |
| libuv | Loop, timer, idle, async, tcp, fs tests |
| libffi | Call + closure tests |
| musl libc | hello, malloc, string, math, io, environ |
| mbedTLS | AES, RSA, ECP, SHA, ARIA self-tests |
| jq | All tests on all architectures (x86, ARM, RISC-V, i686) |

### Known limitations

- **External toolchain required**: The compiler produces textual assembly. Assembly
  and linking delegate to `gcc` (or the appropriate cross-compiler). A native
  assembler/linker is planned (see `ideas/native_elf_writer.txt`).
- **Optimization levels**: All levels (`-O0` through `-O3`, `-Os`, `-Oz`) run the
  same optimization pipeline. Separate tiers will be added as the compiler matures.
- **Long double**: x86 80-bit extended precision is supported via x87 FPU inline
  assembly for compile-time constant evaluation. Runtime long double operations work
  through the x87 FPU. On ARM/RISC-V, `long double` is IEEE binary128 via compiler-rt
  soft-float libcalls.
- **Complex numbers**: `_Complex` arithmetic has some edge-case failures.
- **GNU extensions**: Partial `__attribute__` support. NEON intrinsics are partially
  implemented (core 128-bit operations work).
- **Atomics**: `_Atomic` is parsed but treated as the underlying type (the qualifier
  is not tracked).

## Architecture

```
src/
  frontend/                  C source -> typed AST
    preprocessor/            Macro expansion, #include, #ifdef, #pragma once
    lexer/                   Tokenization with source locations
    parser/                  Recursive descent, produces spanned AST
    sema/                    Type checking, symbol table, const evaluation

  ir/                        Target-independent SSA IR
    lowering/                AST -> alloca-based IR
    mem2reg/                 SSA promotion (dominator tree, phi insertion)

  passes/                    SSA optimization passes
    constant_fold            Constant folding and propagation
    copy_prop                Copy propagation
    dce                      Dead code elimination
    gvn                      Global value numbering
    licm                     Loop-invariant code motion
    simplify                 Algebraic simplification
    cfg_simplify             CFG cleanup, branch threading
    inline                   Function inlining (always_inline + small static)
    if_convert               Diamond if-conversion to select (cmov/csel)
    narrow                   Integer narrowing (eliminate promotion overhead)
    div_by_const             Division strength reduction (mul+shift)
    ipcp                     Interprocedural constant propagation
    iv_strength_reduce       Induction variable strength reduction
    loop_analysis            Shared natural loop detection (used by LICM, IVSR)
    dead_statics             Dead static function/global elimination
    resolve_asm              Post-inline asm symbol resolution

  backend/                   IR -> textual assembly
    traits.rs                ArchCodegen trait with shared default implementations
    generation.rs            IR instruction dispatch to trait methods
    liveness.rs              Live interval computation for register allocation
    regalloc.rs              Linear scan register allocator
    state.rs                 Shared codegen state (stack slots, register cache)
    stack_layout.rs          Stack frame layout with liveness-based slot packing
    call_abi.rs              Unified ABI classification (caller + callee)
    cast.rs                  Shared cast and float operation classification
    f128_softfloat.rs        IEEE binary128 soft-float (ARM + RISC-V)
    inline_asm.rs            Shared inline assembly framework
    common.rs                Assembly output, data sections, external tool invocation
    x86_common.rs            Shared x86/i686 register names, condition codes
    x86/codegen/             x86-64 (SysV AMD64 ABI) + peephole optimizer
    arm/codegen/             AArch64 (AAPCS64) + peephole optimizer
    riscv/codegen/           RISC-V 64 (LP64D) + peephole optimizer
    i686/codegen/            i686 (cdecl, ILP32) + peephole optimizer

  common/                    Shared types, symbol table, diagnostics
  driver/                    CLI parsing, pipeline orchestration
```

Each subdirectory has its own `README.md` with design details.

### Compilation pipeline

```
C source
  -> Preprocessor (macro expansion, includes, conditionals)
  -> Lexer (tokens with source locations)
  -> Parser (recursive descent -> AST)
  -> Sema (type checking, symbol resolution, const evaluation)
  -> IR lowering (AST -> alloca-based IR)
  -> mem2reg (SSA promotion via iterated dominance frontier)
  -> Optimization passes (up to 3 iterations with dirty tracking)
  -> Phi elimination (SSA -> register copies)
  -> Code generation (IR -> textual assembly)
  -> [external gcc -c] -> object file
  -> [external gcc] -> linked executable
```

### Key design decisions

- **SSA IR**: The IR uses SSA form with phi nodes, constructed via mem2reg over
  alloca-based lowering. This is the same approach as LLVM.
- **Trait-based backends**: All four backends implement the `ArchCodegen` trait.
  Shared logic (call ABI classification, inline asm framework, f128 soft-float)
  lives in default trait methods and shared modules.
- **Linear scan register allocation**: Loop-aware liveness analysis feeds a linear
  scan allocator (callee-saved + caller-saved) on all four backends. Register-allocated
  values bypass stack slots entirely.
- **Text-to-text preprocessor**: The preprocessor operates on raw text, emitting
  GCC-style `# line "file"` markers for source location tracking. Include guard
  detection avoids re-processing headers.
- **Peephole optimization**: Each backend has a post-codegen peephole optimizer that
  eliminates redundant patterns (store/load forwarding, dead stores, copy propagation)
  from the stack-based code generator. The x86 peephole is the most mature with 8+
  pass types.

## Testing

```bash
# Run the full verification suite (unit tests + project builds, all architectures)
python3 /verify/run_all_verify.py --compiler-path target/release/

# Run unit tests only (10% sample for quick iteration)
python3 /verify/verify_compiler.py --compiler target/release/ccc --arch x86 --ratio 10

# Full unit test suite for a specific architecture
python3 /verify/verify_compiler.py --compiler target/release/ccc --arch x86

# Linux kernel build verification
python3 /verify/verify_kernel_defconfig.py --compiler target/release/ccc-x86
python3 /verify/verify_kernel_defconfig_multiarch.py --compiler target/release/ccc-arm
python3 /verify/verify_kernel_defconfig_multiarch.py --compiler target/release/ccc-riscv
```

## Project organization

- `src/` -- Compiler source code (Rust)
- `include/` -- Bundled C headers (SSE/AVX/NEON intrinsic stubs)
- `tests/` -- Test suite (~900 test directories)
- `ideas/` -- Design docs and future work proposals
- `current_tasks/` -- Active work items (lock files for multi-agent coordination)
- `completed_tasks/` -- Finished work items (for reference)
- `scripts/` -- Helper scripts (i686 cross-compilation setup)
