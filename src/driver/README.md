# Driver

Orchestrates the compilation pipeline from command-line arguments through to final output.
The driver is the entry point that wires together every other compiler subsystem.

## How It Works

```
                    ┌──────────────┐
                    │  CLI args    │
                    └──────┬───────┘
                           │
                    ┌──────▼───────┐
                    │ parse_cli_   │   cli.rs
                    │ args()       │   Parse GCC-compatible flags
                    └──────┬───────┘
                           │
                    ┌──────▼───────┐
                    │    run()     │   driver.rs
                    │              │   Dispatch by CompileMode
                    └──────┬───────┘
                           │
              ┌────────────┼────────────────┬────────────────┐
              │            │                │                │
       PreprocessOnly  AssemblyOnly     ObjectOnly        Full
       (-E)            (-S)             (-c)              (default)
              │            │                │                │
              │     compile_to_assembly     │                │
              │            │                │                │
              │            ▼                │                │
              │    preprocess → lex →       │                │
              │    parse → sema →           │                │
              │    lower → mem2reg →        │                │
              │    optimize → phi_elim →    │                │
              │    codegen                  │                │
              │            │           + assemble        + assemble
              │            ▼                │            + link
              ▼         .s file             ▼                ▼
           stdout                        .o file        executable
```

## Module Layout

| File | Purpose |
|------|---------|
| `driver.rs` | `Driver` struct, `new()`, `run()`, compilation pipeline (`compile_to_assembly`), run modes |
| `cli.rs` | GCC-compatible CLI argument parsing (`parse_cli_args`) |
| `external_tools.rs` | External tool invocation: assembler, linker, dependency files |
| `file_types.rs` | Input file classification by extension or magic bytes |

## Design Details

### The Driver Struct

`Driver` holds all configuration parsed from the command line: target architecture,
optimization level, output path, include paths, defines, warning settings, linker flags,
and feature flags (PIC, debug info, retpoline thunks, patchable function entries, etc.).

Target architecture is determined from the binary name at startup: any name
containing `arm` or `aarch64` selects AArch64, `riscv` selects RISC-V 64,
`i686` or `i386` selects i686, and everything else defaults to x86-64.

### CLI Parsing

The CLI parser (`cli.rs`) is a simple `while` loop with a flat `match` on each argument.
No external parser library is used. Key behaviors:

- **Query flags** (`-dumpmachine`, `-dumpversion`, `--version`, `-print-search-dirs`)
  are handled as early exits before input files are required. Build systems use these
  to detect the compiler and target.
- **Response files** (`@file`) are expanded inline, supporting the GCC/MSVC convention
  used by Meson when command lines exceed OS limits.
- **Unknown flags** are silently ignored (matching GCC behavior for unrecognized `-f`
  and `-m` flags). This is critical for build system compatibility -- the Linux kernel
  build passes many architecture-specific flags that only newer GCC versions understand.
- **Flag ordering** is preserved for linker items (`-l`, `-Wl,`, object files) because
  flags like `-Wl,--whole-archive` must precede the archive they affect.

### Compilation Pipeline

`compile_to_assembly()` implements the core pipeline:

1. **Read source** -- load the input file (or stdin with `-x c -E -`)
2. **Preprocess** -- macro expansion, includes, conditionals
3. **Lex** -- tokenize the preprocessed text
4. **Parse** -- recursive descent to produce a spanned AST
5. **Sema** -- type checking, symbol resolution, constant evaluation
6. **Lower** -- AST to alloca-based IR
7. **mem2reg** -- promote allocas to SSA form
8. **Optimize** -- run the optimization pass pipeline
9. **Phi eliminate** -- convert SSA phi nodes to register copies
10. **Codegen** -- IR to target-specific assembly text

### External Tool Invocation

`external_tools.rs` centralizes all `std::process::Command` usage for invoking the
GNU toolchain. If the compiler later gains a native assembler/linker, only this file
needs to change.

- **Assembler**: invokes `gcc -c` (or the appropriate cross-compiler) with architecture-specific flags
- **Linker**: invokes `gcc` with library paths, linker items, and flags in the correct order
- **Dependency files**: writes Makefile-format `.d` files for build system integration (`-MD`, `-MMD`)

### GCC Compatibility

The driver reports as GCC 14.2.0 for build system compatibility:
- `-dumpmachine` returns the appropriate target triple (`x86_64-linux-gnu`, etc.)
- `-dumpversion` returns `14`
- `__GNUC__`, `__GNUC_MINOR__`, `__GNUC_PATCHLEVEL__` are predefined accordingly

### Why This Split

The driver was originally a single monolithic file. The split follows natural seams:

- **CLI parsing** is self-contained: it reads `args`, mutates `Driver` fields, and returns.
  No coupling to the compilation pipeline.
- **External tool invocation** centralizes all `std::process::Command` usage. If the
  compiler later gains a native assembler/linker, only this file changes.
- **File type detection** is pure logic with no state dependencies.
