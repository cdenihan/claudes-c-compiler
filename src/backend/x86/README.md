# x86-64 Backend

Code generation targeting the x86-64 (AMD64) architecture with System V ABI.

## Structure

The codegen is split into focused modules, all implementing or supporting the `ArchCodegen` trait:

| File | Responsibility |
|------|---------------|
| `codegen.rs` | Main `X86Codegen` struct, trait implementation, register allocation setup |
| `alu.rs` | Integer arithmetic and bitwise operations |
| `atomics.rs` | Atomic operations (LOCK CMPXCHG, LOCK XADD, MFENCE) |
| `calls.rs` | Function call emission (SysV AMD64 ABI: rdi, rsi, rdx, rcx, r8, r9 for integer args; xmm0-xmm7 for FP) |
| `cast_ops.rs` | Type casts (int/float conversions, sign/zero extension, truncation) |
| `comparison.rs` | Compare operations and condition code handling |
| `f128.rs` | F128 (long double) via x87 FPU: load/store, casts, x87-specific precision handling |
| `float_ops.rs` | SSE floating-point operations (addsd, mulsd, etc.) |
| `globals.rs` | Global variable access (RIP-relative, GOT/PLT for PIC) |
| `i128_ops.rs` | 128-bit integer operations (split across rax:rdx pairs) |
| `inline_asm.rs` | x86 inline asm template substitution, AT&T syntax with `%` operand references |
| `intrinsics.rs` | SSE/SSE2/AVX intrinsic emission and hardware builtins |
| `memory.rs` | Load/store operations, stack slot access, memory operand folding |
| `prologue.rs` | Function prologue/epilogue, stack frame setup, callee-saved register save/restore |
| `returns.rs` | Return value handling (integer, float, struct, i128) |
| `variadic.rs` | va_list / va_arg implementation (register save area) |
| `asm_emitter.rs` | `InlineAsmEmitter` trait: constraint classification, scratch register allocation |
| `peephole/` | Post-codegen peephole optimizer (see `peephole/README.md`) |

## Register Allocation

Callee-saved registers: `rbx`, `r12`-`r15` (5 registers). The shared linear scan allocator
assigns these to frequently-used IR values based on loop-depth-weighted use counts.
Register-allocated values bypass stack slots entirely.
