# CCC -- The Claude C Compiler

A C compiler written entirely from scratch in Rust, targeting x86-64, i686,
AArch64, and RISC-V 64. Zero compiler-specific dependencies -- the frontend,
SSA-based IR, optimizer, code generator, peephole optimizers, assembler,
linker, and DWARF debug info generation are all implemented from scratch.
The compiler can produce ELF executables without any external toolchain.

## Building

```bash
cargo build --release
```

This produces five binaries in `target/release/`, all compiled from the same
source. The target architecture is selected by the binary name at runtime:

| Binary | Target |
|--------|--------|
| `ccc` | x86-64 (default) |
| `ccc-x86` | x86-64 |
| `ccc-arm` | AArch64 |
| `ccc-riscv` | RISC-V 64 |
| `ccc-i686` | i686 (32-bit x86) |

## Usage

```bash
# Compile and link (set MY_ASM=builtin MY_LD=builtin for the builtin toolchain)
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

The compiler accepts most GCC flags. Unrecognized flags (e.g., architecture-
specific `-m` flags, unknown `-f` flags) are silently ignored so `ccc` can
serve as a drop-in GCC replacement in build systems.

### Assembler and Linker Selection

Each architecture has a fully integrated **builtin assembler** and **builtin
linker** that produce ELF object files and executables directly, with no
external toolchain required. The builtin backends are selected via environment
variables:

```bash
# Use builtin assembler and linker (no GCC/binutils dependency)
MY_ASM=builtin MY_LD=builtin ccc -o output input.c

# Use external GCC toolchain (fallback)
ccc -o output input.c

# Use a specific external linker
MY_LD=ld.bfd ccc -o output input.c
MY_LD=/usr/bin/ld ccc -o output input.c

# Auto-detect ld for the target architecture
MY_LD=1 ccc-riscv -o output input.c
```

| Variable | Values | Description |
|----------|--------|-------------|
| `MY_ASM` | `builtin` | Use the builtin assembler (encode + ELF object writer) |
| `MY_ASM` | path or command | Use a custom external assembler |
| `MY_LD` | `builtin` | Use the builtin linker (ELF executable writer) |
| `MY_LD` | `1`, `true`, `yes` | Auto-detect the system `ld` for the target |
| `MY_LD` | path or command | Use a specific external linker |

When neither variable is set, the compiler falls back to the GCC cross-
compiler toolchain for the target architecture. A one-time warning suggests
using `MY_ASM=builtin` / `MY_LD=builtin`.

## Status

The compiler can build real-world C codebases across all four architectures,
including the Linux kernel.

### Known Limitations

- **Optimization levels**: All levels (`-O0` through `-O3`, `-Os`, `-Oz`) run
  the same optimization pipeline. Separate tiers will be added as the compiler
  matures.
- **Long double**: x86 80-bit extended precision is supported via x87 FPU
  instructions. On ARM/RISC-V, `long double` is IEEE binary128 via compiler-rt
  soft-float libcalls.
- **Complex numbers**: `_Complex` arithmetic has some edge-case failures.
- **GNU extensions**: Partial `__attribute__` support. NEON intrinsics are
  partially implemented (core 128-bit operations work).
- **Atomics**: `_Atomic` is parsed but treated as the underlying type (the
  qualifier is not tracked through the type system).

---

## Architecture

### High-Level Pipeline

The compiler is a multi-phase pipeline. Each phase is a separate Rust module
with a well-defined input/output interface. The entire flow -- from C source
to ELF executable -- is handled internally with no external tools.

```
    +---------------------------------------------------------------------+
    |                        C Source Files (.c, .h)                       |
    +----------------------------------+----------------------------------+
                                       |
    +----------------------------------v----------------------------------+
    |                    FRONTEND (src/frontend/)                         |
    |                                                                    |
    |  +--------------+    +-------+    +--------+    +--------------+   |
    |  | Preprocessor |---»| Lexer |---»| Parser |---»|     Sema     |   |
    |  |              |    |       |    |        |    |              |   |
    |  | macro expand,|    |tokens |    |spanned |    | type check,  |   |
    |  | #include,    |    | with  |    |  AST   |    | const eval,  |   |
    |  | #ifdef       |    | spans |    |        |    | symbol table |   |
    |  +--------------+    +-------+    +--------+    +------+-------+   |
    +------------------------------------------------------------+-------+
                                                                 |
                                          AST + SemaResult (TypeContext,
                                          expr types, const values)
                                                                 |
    +------------------------------------------------------------v-------+
    |                    IR SUBSYSTEM (src/ir/)                           |
    |                                                                    |
    |  +------------------+         +------------------------------+     |
    |  |   IR Lowering    |--------»|          mem2reg             |     |
    |  |                  |         |                              |     |
    |  | AST -> alloca-   |         | SSA promotion via dominator  |     |
    |  | based IR (every  |         | frontiers; insert phi nodes, |     |
    |  | local is a stack |         | rename values                |     |
    |  | slot)            |         |                              |     |
    |  +------------------+         +--------------+---------------+     |
    +----------------------------------------------+---------------------+
                                                   |  SSA IR
    +----------------------------------------------v---------------------+
    |               OPTIMIZATION PASSES (src/passes/)                    |
    |                                                                    |
    |  Phase 0: Inlining + cleanup                                      |
    |           |                                                        |
    |  Main Loop (up to 3 iterations, dirty-tracked):                    |
    |    cfg_simplify -> copy_prop -> narrow -> simplify -> constant_fold |
    |    -> gvn -> licm -> iv_strength_reduce -> if_convert -> dce       |
    |    -> ipcp                                                         |
    |           |                                                        |
    |  Phase 11: Dead static elimination                                 |
    |           |                                                        |
    |  Phi Elimination (SSA -> register copies)                          |
    +----------------------------------+---------------------------------+
                                       |  non-SSA IR
    +----------------------------------v---------------------------------+
    |                    BACKEND (src/backend/)                           |
    |                                                                    |
    |  +---------------------------------------------------------+      |
    |  |              Code Generation (ArchCodegen trait)          |      |
    |  |                                                          |      |
    |  |  +----------+  +----------+  +----------+  +----------+ |      |
    |  |  |  x86-64  |  |   i686   |  |  AArch64 |  | RISC-V64 | |      |
    |  |  | SysV ABI |  |  cdecl   |  | AAPCS64  |  |  LP64D   | |      |
    |  |  +----+-----+  +----+-----+  +----+-----+  +----+-----+ |      |
    |  +-------+-------------+------------+---------------+-------+      |
    |          |             |            |               |               |
    |  +-------v-------------v------------v---------------v-------+      |
    |  |              Peephole Optimizer (per-arch)                |      |
    |  |  store/load forwarding, dead stores, copy prop, branches |      |
    |  +-------+-------------+------------+---------------+-------+      |
    |          |             |            |               |               |
    |  +-------v-------------v------------v---------------v-------+      |
    |  |         Builtin Assembler (per-arch, MY_ASM=builtin)     |      |
    |  |  parse asm text -> encode instructions -> write ELF .o   |      |
    |  +-------+-------------+------------+---------------+-------+      |
    |          |             |            |               |               |
    |  +-------v-------------v------------v---------------v-------+      |
    |  |           Builtin Linker (per-arch, MY_LD=builtin)       |      |
    |  |  read .o + CRT + libs -> resolve symbols -> write ELF    |      |
    |  +-------+-------------+------------+---------------+-------+      |
    +----------+-------------+------------+---------------+---------------+
               |             |            |               |
               v             v            v               v
             ELF           ELF          ELF             ELF
```

### Source Tree

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

  backend/                   IR -> assembly -> machine code -> ELF
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
    common.rs                Data sections, external tool fallback invocation
    x86_common.rs            Shared x86/i686 register names, condition codes
    x86/
      codegen/               x86-64 code generation (SysV AMD64 ABI) + peephole
      assembler/             Builtin x86-64 assembler (parser, encoder, ELF writer)
      linker/                Builtin x86-64 linker (dynamic linking, PLT/GOT, TLS)
    i686/
      codegen/               i686 code generation (cdecl, ILP32) + peephole
      assembler/             Builtin i686 assembler (reuses x86 parser, 32-bit encoder)
      linker/                Builtin i686 linker (32-bit ELF, R_386 relocations)
    arm/
      codegen/               AArch64 code generation (AAPCS64) + peephole
      assembler/             Builtin AArch64 assembler (parser, encoder, ELF writer)
      linker/                Builtin AArch64 linker (static linking, IFUNC/TLS)
    riscv/
      codegen/               RISC-V 64 code generation (LP64D) + peephole
      assembler/             Builtin RV64 assembler (parser, encoder, RV64C compress)
      linker/                Builtin RV64 linker (dynamic linking)

  common/                    Shared types, symbol table, diagnostics
  driver/                    CLI parsing, pipeline orchestration
```

Each subdirectory has its own `README.md` with detailed design documentation.

### Compilation Pipeline (Data Flow)

Each phase transforms the program into a progressively lower-level
representation. The concrete Rust types flowing between phases are:

```
  &str  (C source text)
    |
    |  Preprocessor::preprocess()
    v
  String  (expanded text with line markers)
    |
    |  Lexer::tokenize()
    v
  Vec<Token>  (each Token = { kind: TokenKind, span: Span })
    |
    |  Parser::parse()
    v
  TranslationUnit  (AST: Vec<ExternalDecl> with source spans)
    |
    |  SemanticAnalyzer::analyze()
    v
  TranslationUnit + SemaResult
    |   SemaResult bundles:
    |     - functions: FxHashMap<String, FunctionInfo>
    |     - type_context: TypeContext (struct layouts, typedefs, enums)
    |     - expr_types: FxHashMap<ExprId, CType>
    |     - const_values: FxHashMap<ExprId, IrConst>
    |
    |  Lowerer::lower()
    v
  IrModule  (alloca-based IR: every local is a stack slot)
    |
    |  promote_allocas()  (mem2reg)
    v
  IrModule  (SSA form: phi nodes, virtual registers)
    |
    |  run_passes()  (up to 3 iterations with dirty tracking)
    v
  IrModule  (optimized SSA)
    |
    |  eliminate_phis()
    v
  IrModule  (non-SSA: phi nodes lowered to register copies)
    |
    |  generate_assembly()  (ArchCodegen trait dispatch)
    v
  String  (target-specific assembly text)
    |
    |  Builtin assembler  (parse -> encode -> ELF .o)
    |  or external: gcc -c
    v
  ELF object file (.o)
    |
    |  Builtin linker  (resolve symbols -> apply relocs -> write ELF)
    |  or external: gcc / ld
    v
  ELF executable
```

### Key Design Decisions

- **SSA IR**: The IR uses SSA form with phi nodes, constructed via mem2reg over
  alloca-based lowering. This is the same approach as LLVM.
- **Trait-based backends**: All four backends implement the `ArchCodegen` trait.
  Shared logic (call ABI classification, inline asm framework, f128 soft-float)
  lives in default trait methods and shared modules.
- **Linear scan register allocation**: Loop-aware liveness analysis feeds a
  linear scan allocator (callee-saved + caller-saved) on all four backends.
  Register-allocated values bypass stack slots entirely.
- **Text-to-text preprocessor**: The preprocessor operates on raw text, emitting
  GCC-style `# line "file"` markers for source location tracking. Include guard
  detection avoids re-processing headers.
- **Peephole optimization**: Each backend has a post-codegen peephole optimizer
  that eliminates redundant patterns (store/load forwarding, dead stores, copy
  propagation) from the stack-based code generator. The x86 peephole is the most
  mature with 8+ pass types.
- **Builtin assembler and linker**: Each architecture has a native assembler
  (AT&T/ARM/RV syntax parser, instruction encoder, ELF object writer) and a
  native linker (symbol resolution, relocation application, ELF executable
  writer). No external toolchain is required.
- **Dual type system**: CType represents C-level types (preserving `int` vs
  `long` distinctions for type checking), while IrType is a flat machine-level
  enumeration (`I8`..`I128`, `U8`..`U128`, `F32`, `F64`, `F128`, `Ptr`,
  `Void`). The lowering phase bridges between them.

### Design Philosophy

- **Separation of concerns through representations.** Each major phase works on
  its own representation: the frontend on text/tokens/AST, the IR subsystem on
  alloca-based IR, the optimizer on SSA IR, and the backend on non-SSA IR. Phase
  boundaries are explicit ownership transfers, not shared mutable state.

- **Alloca-then-promote for SSA construction.** Rather than constructing SSA
  directly during AST lowering (which interleaves C semantics with SSA
  bookkeeping), the lowerer emits simple alloca/load/store sequences. The
  mem2reg pass then promotes these to SSA independently. This is the same
  strategy LLVM uses and cleanly separates the two concerns.

- **Trait-based backend abstraction.** The `ArchCodegen` trait (~185 methods)
  captures the interface between the shared code generation framework and
  architecture-specific instruction emission. Default implementations express
  algorithms once (e.g., the 8-phase call sequence), while backends supply
  only the architecture-specific primitives.

- **Zero external dependencies for compilation.** The entire compilation
  pipeline -- from C source to ELF executable -- is self-contained. No lexer
  generators, parser generators, register allocator libraries, external
  assemblers, or external linkers are required. Every component is implemented
  from scratch using only general-purpose Rust crates.

---

## Project Organization

- `src/` -- Compiler source code (Rust)
- `include/` -- Bundled C headers (SSE/AVX/NEON intrinsic stubs)
- `tests/` -- Compiler tests (each test is a directory with `main.c` and expected output)
- `ideas/` -- Future work proposals and improvement notes
- `scripts/` -- Helper scripts (assembly comparison, cross-compilation setup)
