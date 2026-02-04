# x86-64 Backend

The x86-64 backend targets AMD64 with the System V AMD64 ABI. It covers the
full pipeline from IR to ELF executable: code generation (instruction
selection, register allocation, peephole optimization), a builtin assembler
(AT&T syntax parser, instruction encoder, ELF object writer), and a builtin
dynamic linker (PLT/GOT, TLS, copy relocations).

## Directory Structure

```
x86/
  codegen/            Code generation and peephole optimizer
    peephole/         Multi-phase post-codegen assembly optimizer
  assembler/          Builtin x86-64 assembler (parser, encoder, ELF writer)
  linker/             Builtin x86-64 linker (dynamic linking, PLT/GOT, TLS)
```

## Sub-Module Documentation

| Module | README |
|--------|--------|
| Code generation | [`codegen/README.md`](codegen/README.md) |
| Peephole optimizer | [`codegen/peephole/README.md`](codegen/peephole/README.md) |
| Assembler | [`assembler/README.md`](assembler/README.md) |
| Linker | [`linker/README.md`](linker/README.md) |

## Key Characteristics

- **ABI**: System V AMD64 -- 6 GP argument registers, 8 XMM argument
  registers, per-eightbyte struct classification
- **Accumulator model**: Most values flow through `%rax`; a linear-scan
  register allocator assigns hot values to callee-saved registers
  (`rbx`, `r12`-`r15`) and caller-saved registers (`r8`-`r11`, `rdi`, `rsi`)
- **F128 (long double)**: Native x87 80-bit extended precision via
  `fldt`/`fstpt`
- **Peephole optimizer**: 13-pass pipeline (local, global, cleanup, loop
  trampoline, never-read store, callee-save elimination)
- **Assembler**: Full AT&T syntax, REX prefixes, SSE/AES-NI encoding
- **Linker**: Dynamic linking with PLT/GOT, TLS (IE-to-LE relaxation),
  copy relocations, GNU symbol versioning
