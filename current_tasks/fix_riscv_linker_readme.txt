Fix RISC-V linker README: correct factual errors and improve accuracy.

Issues found:
- link.rs is ~4600 lines, not ~3900; link_builtin is ~2465 lines, not ~2400; link_shared is ~1997 lines, not ~1500
- Phase 1 incorrectly describes CRT discovery as happening inside the linker; it's actually done by common.rs
- Relocation table missing R_RISCV_ALIGN (43), R_RISCV_RVC_BRANCH (44), R_RISCV_RVC_JUMP (45), R_RISCV_SET_ULEB128 (60), R_RISCV_SUB_ULEB128 (61)
- README says "30+ relocation types" but should say "35+ relocation types"
- link_shared has 6 phases (including 3b), not 5
- Static linking program headers differ from dynamic (6-7 vs 10-11) but README only shows dynamic case
