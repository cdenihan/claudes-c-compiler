//! Native RISC-V 64-bit ELF linker.
//!
//! Reads ELF .o relocatable files and .a static archives, resolves symbols,
//! applies RISC-V relocations, generates PLT/GOT for dynamic symbols, and
//! emits a dynamically-linked ELF executable.
//!
//! Used when MY_LD=builtin is set for the RISC-V backend.

mod elf_read;
mod link;

pub use link::link_to_executable;
