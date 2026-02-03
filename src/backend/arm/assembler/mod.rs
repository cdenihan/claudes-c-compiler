//! Native AArch64 assembler.
//!
//! Parses `.s` assembly text (as emitted by the AArch64 codegen) and produces
//! ELF `.o` object files, removing the dependency on `aarch64-linux-gnu-gcc`
//! for assembly.
//!
//! Architecture:
//! - `parser.rs`     – Tokenize + parse assembly text into `AsmStatement` items
//! - `encoder.rs`    – Encode AArch64 instructions into 32-bit machine words
//! - `elf_writer.rs` – Write ELF object files with sections, symbols, and relocations

pub mod parser;
pub mod encoder;
pub mod elf_writer;

use parser::parse_asm;
use elf_writer::ElfWriter;

/// Assemble AArch64 assembly text into an ELF object file.
///
/// This is the main entry point, called when MY_ASM is set to use the
/// built-in assembler instead of the external `aarch64-linux-gnu-gcc`.
pub fn assemble(asm_text: &str, output_path: &str) -> Result<(), String> {
    let statements = parse_asm(asm_text)?;
    let mut writer = ElfWriter::new();
    writer.process_statements(&statements)?;
    writer.write_elf(output_path)?;
    Ok(())
}
