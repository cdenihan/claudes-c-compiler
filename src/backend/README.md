# Backend

Code generation from IR to target-specific assembly, followed by assembling and linking.

## Architecture

```
IR Module → Codegen → Assembly text → Assembler → Object file → Linker → Executable
```

Each target architecture (x86-64, AArch64, RISC-V 64) implements the `ArchCodegen` trait, providing arch-specific instruction emission. The shared codegen framework handles instruction dispatch, function structure, and stack slot assignment.

## Modules

- **codegen_shared.rs** - Shared codegen framework: `ArchCodegen` trait, `CodegenState` (stack slots, alloca tracking), shared instruction dispatch loop, stack space calculation, parameter storage helpers. Also provides `CastKind`+`classify_cast()` for shared cast classification, `FloatOp`+`classify_float_binop()` for float dispatch, and `InlineAsmEmitter` trait + `emit_inline_asm_common()` for shared inline assembly processing. This is where the common codegen logic lives. Adding a new IR instruction only requires changes here (dispatch) and in each arch's trait implementation.
- **common.rs** - Assembly output buffer, data section emission (globals, string literals, constants), assembler/linker config and invocation via external GCC toolchain.
- **x86/** - x86-64 codegen (SysV AMD64 ABI): `X86Codegen` implements `ArchCodegen`
- **arm/** - AArch64 codegen (AAPCS64): `ArmCodegen` implements `ArchCodegen`
- **riscv/** - RISC-V 64 codegen (standard calling convention): `RiscvCodegen` implements `ArchCodegen`
- **mod.rs** - `Target` enum for target dispatch

## Key Design Decisions

- **Trait-based deduplication**: The `ArchCodegen` trait in `codegen_shared.rs` eliminates the structural duplication between backends. The shared `generate_module()` function handles instruction dispatch, calling arch-specific methods for each operation. This prevents cross-backend inconsistency bugs and ensures new IR instructions are handled uniformly.
- **Default implementations via primitives**: Most codegen methods have default implementations in the trait that compose small arch-specific primitives. Backends only implement the primitives. Methods with defaults include:
  - **Control flow**: `emit_branch`, `emit_cond_branch`, `emit_unreachable`, `emit_indirect_branch`, `emit_label_addr` (compose `jump_mnemonic()`, `trap_instruction()`, `emit_branch_nonzero()`, `emit_jump_indirect()`)
  - **Store/Load**: `emit_store`, `emit_load` handle i128 pair and typed alloca/indirect paths (compose `emit_load_acc_pair`, `emit_store_pair_to_slot`, `emit_typed_store_to_slot`, `emit_load_ptr_from_slot`, `store_instr_for_type`, etc.)
  - **GEP**: `emit_gep` loads base to secondary register and adds offset (compose `emit_slot_addr_to_secondary`, `emit_add_secondary_to_acc`)
  - **Dynamic alloca**: `emit_dyn_alloca` rounds up size and subtracts from SP (compose `emit_round_up_acc_to_16`, `emit_sub_sp_by_acc`, `emit_mov_sp_to_acc`, `emit_align_acc`)
  - **Memcpy**: `emit_memcpy` loads addresses then calls arch-specific copy (compose `emit_memcpy_load_dest_addr`, `emit_memcpy_load_src_addr`, `emit_memcpy_impl`)
  - **Unary ops**: `emit_unaryop` dispatches i128/float/int to per-operation primitives (`emit_float_neg`, `emit_int_neg`, `emit_int_clz`, `emit_int_ctz`, `emit_int_bswap`, `emit_int_popcount`)
  - **Casts**: `emit_cast` handles i128 widening/narrowing/copy via accumulator pair primitives (`emit_sign_extend_acc_high`, `emit_zero_acc_high`), and delegates non-i128 casts to `emit_cast_instrs`. No backend overrides needed.
  - **Returns**: `emit_return` dispatches i128/f128/f32/f64/int return values to arch-specific primitives (`emit_return_i128_to_regs`, `emit_return_f128_to_reg`, `emit_return_f32_to_reg`, `emit_return_f64_to_reg`, `emit_return_int_to_reg`, `emit_epilogue_and_ret`). No backend overrides needed.
  - **Float binops**: `emit_float_binop` default loads lhs to secondary, rhs to acc, then calls `emit_float_binop_impl(mnemonic, ty)` which each arch implements to move acc/secondary to FP regs, perform the op, and move result back.
  - **i128 binops**: `emit_i128_binop` default dispatches to per-op arch primitives (`emit_i128_add`, `emit_i128_sub`, `emit_i128_mul`, `emit_i128_and`, `emit_i128_or`, `emit_i128_xor`, `emit_i128_shl`, `emit_i128_lshr`, `emit_i128_ashr`, `emit_i128_divrem_call`). The dispatch logic (func name selection for divrem, match structure) is shared.
  - **i128 comparisons**: `emit_i128_cmp` default dispatches Eq/Ne to `emit_i128_cmp_eq(is_ne)` and ordered ops to `emit_i128_cmp_ordered(op)`. The algorithmic structure (XOR+OR for eq/ne, high-then-low for ordered) is shared.
  - **Binop dispatch**: `emit_binop` dispatches float/int (compose `emit_float_binop`, `emit_int_binop`). x86-64 overrides for 128-bit.
  - **Others**: `emit_va_end`, `emit_copy_i128`, `emit_label_addr`
- **Cast classification**: The `classify_cast()` function captures the shared decision logic for type casts (Ptr normalization, F128 reduction, float<->int, widen/narrow). The default `emit_cast` handles i128 via pair primitives and delegates non-i128 casts to the arch-specific `emit_cast_instrs`.
- **Call argument classification**: The `CallArgClass` enum and `classify_call_args()` function provide shared function call argument classification. `CallAbiConfig` parameterizes arch-specific details (register counts, pair alignment, F128 handling).
- **Binop dispatch**: The default `emit_binop` classifies float operations via `classify_float_binop()` and dispatches to `emit_float_binop` or `emit_int_binop`. Architectures with special needs (e.g., x86-64's 128-bit integer arithmetic) override `emit_binop` to handle those first.
- **Inline asm framework**: The `InlineAsmEmitter` trait + `emit_inline_asm_common()` function extracts the 4-phase inline assembly handling: (1) classify constraints and assign registers, (2) resolve tied/read-write operands and build GCC numbering, (3) load inputs and emit substituted template lines, (4) store outputs. Each backend implements the trait to provide arch-specific constraint classification, scratch register pools, operand loading/storing, and template substitution. The shared code handles the orchestration that was previously duplicated ~270 lines per backend.
- **Stack-based codegen**: All backends use a stack-based strategy (no register allocator yet). Each IR value gets a stack slot. Instructions load operands into a primary accumulator register (rax/x0/t0), perform the operation, and store the result back. This produces correct but slow code.
- **Arch-specific stack conventions**: x86 uses negative offsets from %rbp, ARM uses positive offsets from sp (with fp/lr at offset 0), RISC-V uses negative offsets from s0. The `calculate_stack_space_common` helper takes a closure to handle these differences.
- **Large offset handling**: ARM and RISC-V have limited immediate ranges. Their backends include helpers (`emit_store_to_sp`, `emit_load_from_s0`, etc.) that use scratch registers for large offsets.
- **Data emission is shared**: All backends use identical GAS directives for data sections, parameterized only by the 64-bit pointer directive (`.quad`/`.xword`/`.dword`).
- **Assembler/linker via GCC**: Currently delegates to the system's GCC toolchain. Will eventually be replaced by a native ELF writer.
