//! 32-bit ELF relocatable object file writer for i686.
//!
//! Thin wrapper around `ElfWriterCore` that provides i686-specific
//! instruction encoding and relocation types. Uses ELFCLASS32, EM_386,
//! and REL (not RELA) relocation format. All shared logic lives in
//! `backend::elf_writer_common`.

use crate::backend::x86::assembler::parser::*;
use super::encoder::*;
use crate::backend::elf::{ELFCLASS32, EM_386};
use crate::backend::elf_writer_common::{
    X86Arch, ElfWriterCore, EncodeResult, EncoderReloc, JumpDetection,
};

/// i686 architecture implementation for the shared ELF writer.
pub struct I686Arch;

impl X86Arch for I686Arch {
    fn encode_instruction(
        instr: &Instruction,
        section_data_len: u64,
    ) -> Result<EncodeResult, String> {
        let mut encoder = InstructionEncoder::new();
        encoder.offset = section_data_len;
        encoder.encode(instr)?;

        let instr_len = encoder.bytes.len();

        // Detect jump instructions for relaxation
        let jump = {
            let mnem = &instr.mnemonic;
            let is_jump = mnem == "jmp" || mnem == "loop"
                || (mnem.starts_with('j') && mnem.len() >= 2);
            if is_jump && instr.operands.len() == 1 {
                if let Operand::Label(_) = &instr.operands[0] {
                    let is_short_only = matches!(mnem.as_str(), "jecxz" | "jcxz" | "loop");
                    let is_conditional = mnem != "jmp";
                    if is_short_only && instr_len == 2 {
                        // Short-only jumps have no long form; register as already relaxed
                        Some(JumpDetection {
                            is_conditional: true,
                            already_short: true,
                        })
                    } else {
                        let expected_len = if is_conditional { 6 } else { 5 };
                        if instr_len == expected_len {
                            Some(JumpDetection {
                                is_conditional,
                                already_short: false,
                            })
                        } else {
                            None
                        }
                    }
                } else {
                    None
                }
            } else {
                None
            }
        };

        let relocations = encoder.relocations.into_iter().map(|r| {
            EncoderReloc {
                offset: r.offset,
                symbol: r.symbol,
                reloc_type: r.reloc_type,
                addend: r.addend,
                diff_symbol: r.diff_symbol,
            }
        }).collect();

        Ok(EncodeResult {
            bytes: encoder.bytes,
            relocations,
            jump,
        })
    }

    fn elf_machine() -> u16 { EM_386 }
    fn elf_class() -> u8 { ELFCLASS32 }

    fn reloc_abs(size: usize) -> u32 {
        let _ = size; // i686 always uses R_386_32 for absolute
        R_386_32
    }
    fn reloc_abs64() -> u32 { R_386_32 } // i686 doesn't have 64-bit relocs
    fn reloc_pc32() -> u32 { R_386_PC32 }
    fn reloc_plt32() -> u32 { R_386_PLT32 }

    fn uses_rel_format() -> bool { true }
}

/// Builds a 32-bit ELF relocatable object file from parsed assembly items.
pub type ElfWriter = ElfWriterCore<I686Arch>;
