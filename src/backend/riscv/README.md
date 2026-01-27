# RISC-V 64 Backend

Code generation targeting the RISC-V 64-bit (RV64GC) architecture.

## Structure

- `codegen/` - Code generation implementation
  - `codegen.rs` - Main `RiscvCodegen` struct implementing the `ArchCodegen` trait. Handles instruction selection, stack frame management (s0 frame pointer), calling convention (a0-a7 for integer args, fa0-fa7 for FP), varargs, and register allocation integration.
  - `asm_emitter.rs` - `InlineAsmEmitter` trait implementation: constraint classification, scratch register allocation (GP and FP), operand loading/storing for inline asm.
  - `atomics.rs` - Sub-word atomic RMW/CAS via LR.W/SC.W loops with bit masking, and software implementations of CLZ, CTZ, BSWAP, POPCOUNT builtins.
  - `f128.rs` - F128 (quad-precision) soft-float helpers: load/store operations, binary arithmetic via compiler-rt libcalls, and constant handling for IEEE binary128.
  - `inline_asm.rs` - RISC-V inline assembly constraint classification and template substitution (%0, %[name], %lo/%hi operands).
  - `intrinsics.rs` - Software emulation of SSE-equivalent 128-bit SIMD operations (bitwise, byte compare, saturating subtract, pmovmskb) using scalar RISC-V instructions, plus hardware intrinsics (fences, CRC32, sqrt, fabs).

## Register Allocation

The RISC-V backend includes a linear scan register allocator that assigns callee-saved registers to frequently-used IR values. The x86 backend also has register allocation; ARM remains stack-only.

**Allocated registers**: s1, s7-s11 (6 registers). s0 is the frame pointer; s2-s6 are reserved as temporaries for `emit_call_reg_args`.

**Strategy**: Register-only — values with a callee-saved register assignment are stored only to the register, skipping the stack slot entirely. All load paths (including `operand_to_t0`, pointer loads for Store/Load/GEP/Memcpy) check register assignments before falling back to stack loads.

**Disabled for**: functions with loops (back-edges in CFG), inline assembly, atomic operations, float/i128/long-double types. Only BinOp, UnaryOp, Cmp, Cast, Load, and GEP results are eligible.
