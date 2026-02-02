# RISC-V 64 Backend

Code generation targeting the RISC-V 64-bit (RV64GC) architecture with LP64D calling convention.

## Structure

| File | Responsibility |
|------|---------------|
| `codegen.rs` | Main `RiscvCodegen` struct implementing `ArchCodegen`. Instruction selection, stack frame management (s0 frame pointer), calling convention (a0-a7 integer, fa0-fa7 FP), varargs |
| `alu.rs` | Integer arithmetic and bitwise operations |
| `atomics.rs` | Sub-word atomic RMW/CAS via LR.W/SC.W loops with bit masking, software CLZ/CTZ/BSWAP/POPCOUNT |
| `calls.rs` | Function call emission (LP64D: a0-a7, fa0-fa7) |
| `cast_ops.rs` | Type casts |
| `comparison.rs` | Compare operations and branch condition handling |
| `f128.rs` | F128 (quad-precision) soft-float via compiler-rt libcalls |
| `float_ops.rs` | Floating-point operations |
| `globals.rs` | Global variable access |
| `i128_ops.rs` | 128-bit integer operations |
| `inline_asm.rs` | RISC-V inline asm constraint classification and template substitution (%0, %[name], %lo/%hi) |
| `intrinsics.rs` | Software emulation of SSE-equivalent 128-bit SIMD (bitwise, byte compare, saturating subtract, pmovmskb) using scalar instructions |
| `memory.rs` | Load/store operations |
| `peephole.rs` | Post-codegen peephole optimizer |
| `prologue.rs` | Function prologue/epilogue, stack frame setup |
| `returns.rs` | Return value handling |
| `variadic.rs` | va_list / va_arg implementation |
| `asm_emitter.rs` | `InlineAsmEmitter` trait: constraint classification, scratch register allocation |

## Register Allocation

**Allocated registers**: s1, s7-s11 (always available, 6 registers) plus s2-s6 (conditionally
available, up to 5 more). s0 is the frame pointer. s2-s6 are used as staging temporaries in
`emit_call_reg_args` when a call has >= 4 GP register arguments; any not needed for staging
are available for allocation, giving up to 11 callee-saved registers total.

**Strategy**: Register-only -- values with a register assignment are stored only to the
register, skipping the stack slot entirely. All load paths check register assignments
before falling back to stack loads.

**Eligible instructions**: BinOp, UnaryOp, Cmp, Cast, Load, GEP, Copy, Call, CallIndirect,
Select, GlobalAddr, LabelAddr, AtomicLoad, AtomicRmw, AtomicCmpxchg. Float, i128, and
long-double results are excluded.
