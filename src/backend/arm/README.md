# AArch64 Backend

Code generation targeting the AArch64 (ARM64) architecture with AAPCS64 calling convention.

## Structure

| File | Responsibility |
|------|---------------|
| `codegen.rs` | Main `ArmCodegen` struct implementing `ArchCodegen`. Instruction selection, stack frame management, calling convention (x0-x7 integer, d0-d7 FP), atomics (LDXR/STXR), varargs |
| `alu.rs` | Integer arithmetic and bitwise operations |
| `atomics.rs` | Atomic operations via exclusive load/store pairs (LDXR/STXR) |
| `calls.rs` | Function call emission (AAPCS64: x0-x7, d0-d7) |
| `cast_ops.rs` | Type casts (SCVTF, UCVTF, FCVTZS, etc.) |
| `comparison.rs` | Compare operations and condition flags |
| `f128.rs` | F128 (quad-precision) soft-float via compiler-rt libcalls |
| `float_ops.rs` | Floating-point operations (FADD, FMUL, etc.) |
| `globals.rs` | Global variable access (ADRP+ADD, GOT for PIC) |
| `i128_ops.rs` | 128-bit integer operations |
| `inline_asm.rs` | AArch64 inline asm template substitution, register formatting (w/x/s/d modifiers) |
| `intrinsics.rs` | NEON/SIMD intrinsic emission and hardware builtins |
| `memory.rs` | Load/store operations, addressing modes |
| `peephole.rs` | Post-codegen peephole optimizer (store/load forwarding, redundant branches, move chains) |
| `prologue.rs` | Function prologue/epilogue, stack frame setup |
| `returns.rs` | Return value handling |
| `variadic.rs` | va_list / va_arg implementation |
| `asm_emitter.rs` | `InlineAsmEmitter` trait: constraint classification, scratch register allocation |

## Peephole Optimizer

The ARM peephole operates on assembly text and eliminates:
- Adjacent store/load forwarding
- Redundant branches
- Self-moves (64-bit only; `mov wN, wN` is preserved as it zeros the upper 32 bits)
- Move chain optimization
- Branch-over-branch fusion
- Move-immediate chain optimization
