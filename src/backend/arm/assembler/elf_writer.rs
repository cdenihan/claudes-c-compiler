//! ELF object file writer for AArch64.
//!
//! Takes parsed assembly statements and produces an ELF .o (relocatable) file
//! with proper sections, symbols, and relocations for AArch64/ELF64.

#![allow(dead_code)]

use std::collections::HashMap;
use super::parser::{AsmStatement, Operand};
use super::encoder::{encode_instruction, EncodeResult, RelocType};

const EM_AARCH64: u16 = 183;
const ELFCLASS64: u8 = 2;
const ELFDATA2LSB: u8 = 1;
const EV_CURRENT: u8 = 1;
const ELFOSABI_NONE: u8 = 0;
const ET_REL: u16 = 1;

// Section types
const SHT_NULL: u32 = 0;
const SHT_PROGBITS: u32 = 1;
const SHT_SYMTAB: u32 = 2;
const SHT_STRTAB: u32 = 3;
const SHT_RELA: u32 = 4;
const SHT_NOBITS: u32 = 8;
const SHT_NOTE: u32 = 7;

// Section flags
const SHF_WRITE: u64 = 1;
const SHF_ALLOC: u64 = 2;
const SHF_EXECINSTR: u64 = 4;
const SHF_MERGE: u64 = 0x10;
const SHF_STRINGS: u64 = 0x20;
const SHF_INFO_LINK: u64 = 0x40;
const SHF_TLS: u64 = 0x400;
const SHF_GROUP: u64 = 0x200;

// Symbol bindings
const STB_LOCAL: u8 = 0;
const STB_GLOBAL: u8 = 1;
const STB_WEAK: u8 = 2;

// Symbol types
const STT_NOTYPE: u8 = 0;
const STT_OBJECT: u8 = 1;
const STT_FUNC: u8 = 2;
const STT_SECTION: u8 = 3;
const STT_TLS: u8 = 6;

// Symbol visibility
const STV_DEFAULT: u8 = 0;
const STV_HIDDEN: u8 = 2;
const STV_PROTECTED: u8 = 3;
const STV_INTERNAL: u8 = 1;

/// An ELF section being built.
struct Section {
    name: String,
    sh_type: u32,
    sh_flags: u64,
    data: Vec<u8>,
    sh_addralign: u64,
    sh_entsize: u64,
    sh_link: u32,
    sh_info: u32,
    /// Relocations for this section
    relocs: Vec<ElfReloc>,
}

/// A relocation entry.
struct ElfReloc {
    offset: u64,
    reloc_type: u32,
    symbol_name: String,
    addend: i64,
}

/// A symbol entry.
struct ElfSymbol {
    name: String,
    value: u64,
    size: u64,
    binding: u8,
    sym_type: u8,
    visibility: u8,
    section_name: String,
}

/// The ELF writer state machine.
pub struct ElfWriter {
    /// Current section we're emitting into
    current_section: String,
    /// All sections being built
    sections: HashMap<String, Section>,
    /// Section order (for deterministic output)
    section_order: Vec<String>,
    /// Symbol table
    symbols: Vec<ElfSymbol>,
    /// Local labels -> (section, offset) for branch resolution
    labels: HashMap<String, (String, u64)>,
    /// Pending relocations that reference labels (need fixup)
    pending_branch_relocs: Vec<PendingReloc>,
    /// Current alignment state
    current_align: u64,
    /// Symbols that have been declared .globl
    global_symbols: HashMap<String, bool>,
    /// Symbols declared .weak
    weak_symbols: HashMap<String, bool>,
    /// Symbol types from .type directives
    symbol_types: HashMap<String, u8>,
    /// Symbol sizes from .size directives
    symbol_sizes: HashMap<String, u64>,
    /// Symbol visibility from .hidden/.protected/.internal
    symbol_visibility: HashMap<String, u8>,
}

struct PendingReloc {
    section: String,
    offset: u64,
    reloc_type: u32,
    symbol: String,
    addend: i64,
}

impl ElfWriter {
    pub fn new() -> Self {
        Self {
            current_section: String::new(),
            sections: HashMap::new(),
            section_order: Vec::new(),
            symbols: Vec::new(),
            labels: HashMap::new(),
            pending_branch_relocs: Vec::new(),
            current_align: 4,
            global_symbols: HashMap::new(),
            weak_symbols: HashMap::new(),
            symbol_types: HashMap::new(),
            symbol_sizes: HashMap::new(),
            symbol_visibility: HashMap::new(),
        }
    }

    fn ensure_section(&mut self, name: &str, sh_type: u32, sh_flags: u64, align: u64) {
        if !self.sections.contains_key(name) {
            self.sections.insert(name.to_string(), Section {
                name: name.to_string(),
                sh_type,
                sh_flags,
                data: Vec::new(),
                sh_addralign: align,
                sh_entsize: 0,
                sh_link: 0,
                sh_info: 0,
                relocs: Vec::new(),
            });
            self.section_order.push(name.to_string());
        }
    }

    fn current_offset(&self) -> u64 {
        self.sections.get(&self.current_section)
            .map(|s| s.data.len() as u64)
            .unwrap_or(0)
    }

    fn emit_bytes(&mut self, bytes: &[u8]) {
        if let Some(section) = self.sections.get_mut(&self.current_section) {
            section.data.extend_from_slice(bytes);
        }
    }

    fn emit_u32_le(&mut self, val: u32) {
        self.emit_bytes(&val.to_le_bytes());
    }

    fn add_reloc(&mut self, reloc_type: u32, symbol: String, addend: i64) {
        let offset = self.current_offset();
        let section = self.current_section.clone();
        if let Some(s) = self.sections.get_mut(&section) {
            s.relocs.push(ElfReloc {
                offset,
                reloc_type,
                symbol_name: symbol,
                addend,
            });
        }
    }

    fn align_to(&mut self, align: u64) {
        if align <= 1 {
            return;
        }
        if let Some(section) = self.sections.get_mut(&self.current_section) {
            let current = section.data.len() as u64;
            let aligned = (current + align - 1) & !(align - 1);
            let padding = (aligned - current) as usize;
            section.data.extend(std::iter::repeat(0u8).take(padding));
            if align > section.sh_addralign {
                section.sh_addralign = align;
            }
        }
    }

    /// Process all parsed assembly statements.
    pub fn process_statements(&mut self, statements: &[AsmStatement]) -> Result<(), String> {
        for stmt in statements {
            self.process_statement(stmt)?;
        }
        self.resolve_local_branches()?;
        Ok(())
    }

    fn process_statement(&mut self, stmt: &AsmStatement) -> Result<(), String> {
        match stmt {
            AsmStatement::Empty => Ok(()),

            AsmStatement::Label(name) => {
                // Record label position
                let section = self.current_section.clone();
                let offset = self.current_offset();
                self.labels.insert(name.clone(), (section, offset));
                Ok(())
            }

            AsmStatement::Directive { name, args } => {
                self.process_directive(name, args)
            }

            AsmStatement::Instruction { mnemonic, operands, raw_operands } => {
                self.process_instruction(mnemonic, operands, raw_operands)
            }
        }
    }

    fn process_directive(&mut self, name: &str, args: &str) -> Result<(), String> {
        match name {
            ".section" => {
                let (sec_name, flags, sec_type) = parse_section_directive(args);
                let sh_type = match sec_type.as_str() {
                    "@nobits" => SHT_NOBITS,
                    "@note" => SHT_NOTE,
                    _ => SHT_PROGBITS,
                };
                let mut sh_flags = 0u64;
                if flags.contains('a') { sh_flags |= SHF_ALLOC; }
                if flags.contains('w') { sh_flags |= SHF_WRITE; }
                if flags.contains('x') { sh_flags |= SHF_EXECINSTR; }
                if flags.contains('M') { sh_flags |= SHF_MERGE; }
                if flags.contains('S') { sh_flags |= SHF_STRINGS; }
                if flags.contains('T') { sh_flags |= SHF_TLS; }
                if flags.contains('G') { sh_flags |= SHF_GROUP; }

                // Set default flags based on section name
                if sh_flags == 0 {
                    if sec_name == ".text" {
                        sh_flags = SHF_ALLOC | SHF_EXECINSTR;
                    } else if sec_name == ".data" || sec_name.starts_with(".data.") {
                        sh_flags = SHF_ALLOC | SHF_WRITE;
                    } else if sec_name == ".bss" || sec_name.starts_with(".bss.") {
                        sh_flags = SHF_ALLOC | SHF_WRITE;
                    } else if sec_name == ".rodata" || sec_name.starts_with(".rodata.") {
                        sh_flags = SHF_ALLOC;
                    } else if sec_name.starts_with(".note") {
                        sh_flags = SHF_ALLOC;
                    } else if sec_name.starts_with(".tdata") {
                        sh_flags = SHF_ALLOC | SHF_WRITE | SHF_TLS;
                    } else if sec_name.starts_with(".tbss") {
                        sh_flags = SHF_ALLOC | SHF_WRITE | SHF_TLS;
                    }
                }

                let align = if sec_name == ".text" { 4 } else { 1 };
                self.ensure_section(&sec_name, sh_type, sh_flags, align);
                self.current_section = sec_name;
                Ok(())
            }

            ".text" => {
                self.ensure_section(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR, 4);
                self.current_section = ".text".to_string();
                Ok(())
            }

            ".data" => {
                self.ensure_section(".data", SHT_PROGBITS, SHF_ALLOC | SHF_WRITE, 1);
                self.current_section = ".data".to_string();
                Ok(())
            }

            ".bss" => {
                self.ensure_section(".bss", SHT_NOBITS, SHF_ALLOC | SHF_WRITE, 1);
                self.current_section = ".bss".to_string();
                Ok(())
            }

            ".globl" | ".global" => {
                let sym = args.trim();
                self.global_symbols.insert(sym.to_string(), true);
                Ok(())
            }

            ".weak" => {
                let sym = args.trim();
                self.weak_symbols.insert(sym.to_string(), true);
                Ok(())
            }

            ".hidden" => {
                let sym = args.trim();
                self.symbol_visibility.insert(sym.to_string(), STV_HIDDEN);
                Ok(())
            }

            ".protected" => {
                let sym = args.trim();
                self.symbol_visibility.insert(sym.to_string(), STV_PROTECTED);
                Ok(())
            }

            ".internal" => {
                let sym = args.trim();
                self.symbol_visibility.insert(sym.to_string(), STV_INTERNAL);
                Ok(())
            }

            ".type" => {
                // .type name, %function or .type name, @object
                let parts: Vec<&str> = args.splitn(2, ',').collect();
                if parts.len() == 2 {
                    let sym = parts[0].trim();
                    let ty = parts[1].trim();
                    let st = match ty {
                        "%function" | "@function" => STT_FUNC,
                        "%object" | "@object" => STT_OBJECT,
                        "@tls_object" => STT_TLS,
                        _ => STT_NOTYPE,
                    };
                    self.symbol_types.insert(sym.to_string(), st);
                }
                Ok(())
            }

            ".size" => {
                // .size name, size_expr
                let parts: Vec<&str> = args.splitn(2, ',').collect();
                if parts.len() == 2 {
                    let sym = parts[0].trim();
                    let size_expr = parts[1].trim();
                    // Handle .-name (current offset - label)
                    if size_expr.starts_with(".-") {
                        let label = &size_expr[2..];
                        if let Some((section, label_offset)) = self.labels.get(label) {
                            if *section == self.current_section {
                                let current = self.current_offset();
                                let size = current - label_offset;
                                self.symbol_sizes.insert(sym.to_string(), size);
                            }
                        }
                    } else if let Ok(size) = size_expr.parse::<u64>() {
                        self.symbol_sizes.insert(sym.to_string(), size);
                    }
                }
                Ok(())
            }

            ".align" | ".p2align" => {
                let align_val: u64 = args.trim().split(',').next()
                    .and_then(|s| s.trim().parse().ok())
                    .unwrap_or(0);
                // On ARM, .align N means 2^N bytes
                let bytes = if name == ".p2align" || true {
                    // AArch64 .align is always power-of-2
                    1u64 << align_val
                } else {
                    align_val
                };
                self.align_to(bytes);
                Ok(())
            }

            ".balign" => {
                let align_val: u64 = args.trim().parse().unwrap_or(1);
                self.align_to(align_val);
                Ok(())
            }

            ".byte" => {
                for part in args.split(',') {
                    let val = parse_data_value(part.trim())? as u8;
                    self.emit_bytes(&[val]);
                }
                Ok(())
            }

            ".short" | ".hword" | ".2byte" => {
                for part in args.split(',') {
                    let val = parse_data_value(part.trim())? as u16;
                    self.emit_bytes(&val.to_le_bytes());
                }
                Ok(())
            }

            ".long" | ".4byte" | ".word" => {
                for part in args.split(',') {
                    let trimmed = part.trim();
                    // Handle symbol references and expressions
                    if trimmed.contains('-') && !trimmed.starts_with('-') {
                        // Label difference: lab1-lab2
                        let parts: Vec<&str> = trimmed.splitn(2, '-').collect();
                        if parts.len() == 2 {
                            // Emit as relocation or resolve locally
                            self.add_reloc(RelocType::Abs32.elf_type(), parts[0].trim().to_string(), 0);
                            // TODO: handle label diff properly with subtrahend
                            self.emit_bytes(&0u32.to_le_bytes());
                            continue;
                        }
                    }
                    let val = parse_data_value(trimmed)? as u32;
                    self.emit_bytes(&val.to_le_bytes());
                }
                Ok(())
            }

            ".quad" | ".8byte" | ".xword" => {
                for part in args.split(',') {
                    let trimmed = part.trim();
                    // Check for symbol references
                    if !trimmed.is_empty() && !trimmed.starts_with('-')
                        && !trimmed.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(false)
                    {
                        // This is a symbol reference
                        let (sym, addend) = if let Some(plus_pos) = trimmed.find('+') {
                            let sym = trimmed[..plus_pos].trim();
                            let off: i64 = trimmed[plus_pos + 1..].trim().parse().unwrap_or(0);
                            (sym.to_string(), off)
                        } else if let Some(minus_pos) = trimmed.find('-') {
                            let sym = trimmed[..minus_pos].trim();
                            let off_str = &trimmed[minus_pos..];
                            let off: i64 = off_str.parse().unwrap_or(0);
                            (sym.to_string(), off)
                        } else {
                            (trimmed.to_string(), 0i64)
                        };
                        self.add_reloc(RelocType::Abs64.elf_type(), sym, addend);
                        self.emit_bytes(&0u64.to_le_bytes());
                        continue;
                    }
                    let val = parse_data_value(trimmed)? as u64;
                    self.emit_bytes(&val.to_le_bytes());
                }
                Ok(())
            }

            ".dword" => {
                // .dword is the same as .xword on AArch64 (8 bytes)
                for part in args.split(',') {
                    let trimmed = part.trim();
                    if !trimmed.is_empty() && !trimmed.chars().next().map(|c| c.is_ascii_digit() || c == '-').unwrap_or(false) {
                        let (sym, addend) = if let Some(plus_pos) = trimmed.find('+') {
                            (trimmed[..plus_pos].trim().to_string(), trimmed[plus_pos + 1..].trim().parse::<i64>().unwrap_or(0))
                        } else {
                            (trimmed.to_string(), 0i64)
                        };
                        self.add_reloc(RelocType::Abs64.elf_type(), sym, addend);
                        self.emit_bytes(&0u64.to_le_bytes());
                        continue;
                    }
                    let val = parse_data_value(trimmed)? as u64;
                    self.emit_bytes(&val.to_le_bytes());
                }
                Ok(())
            }

            ".zero" | ".space" => {
                let size: usize = args.trim().parse()
                    .map_err(|_| format!("invalid .zero size: {}", args))?;
                self.emit_bytes(&vec![0u8; size]);
                Ok(())
            }

            ".asciz" => {
                let s = parse_string_literal(args)?;
                self.emit_bytes(s.as_bytes());
                self.emit_bytes(&[0]); // null terminator
                Ok(())
            }

            ".ascii" => {
                let s = parse_string_literal(args)?;
                self.emit_bytes(s.as_bytes());
                Ok(())
            }

            ".string" => {
                let s = parse_string_literal(args)?;
                self.emit_bytes(s.as_bytes());
                self.emit_bytes(&[0]); // null terminator
                Ok(())
            }

            ".comm" => {
                // .comm symbol, size[, align]
                let parts: Vec<&str> = args.split(',').collect();
                if parts.len() >= 2 {
                    let sym = parts[0].trim();
                    let size: u64 = parts[1].trim().parse().unwrap_or(0);
                    let align: u64 = if parts.len() > 2 {
                        parts[2].trim().parse().unwrap_or(1)
                    } else {
                        1
                    };

                    // Create a common symbol
                    self.symbols.push(ElfSymbol {
                        name: sym.to_string(),
                        value: align, // For COMMON, value = alignment
                        size,
                        binding: STB_GLOBAL,
                        sym_type: STT_OBJECT,
                        visibility: STV_DEFAULT,
                        section_name: "*COM*".to_string(),
                    });
                }
                Ok(())
            }

            // CFI directives - skip them (we don't generate DWARF unwind info yet)
            ".cfi_startproc" | ".cfi_endproc" | ".cfi_def_cfa_offset"
            | ".cfi_offset" | ".cfi_def_cfa_register" | ".cfi_restore"
            | ".cfi_remember_state" | ".cfi_restore_state"
            | ".cfi_adjust_cfa_offset" | ".cfi_def_cfa"
            | ".cfi_sections" | ".cfi_personality" | ".cfi_lsda"
            | ".cfi_rel_offset" | ".cfi_register" | ".cfi_return_column"
            | ".cfi_undefined" | ".cfi_same_value" | ".cfi_escape" => Ok(()),

            // Other directives we can safely ignore for now
            ".file" | ".loc" | ".ident" | ".addrsig" | ".addrsig_sym"
            | ".build_attributes" | ".eabi_attribute" => Ok(()),

            _ => {
                // Unknown directive - ignore with a warning
                // TODO: handle more directives
                Ok(())
            }
        }
    }

    fn process_instruction(&mut self, mnemonic: &str, operands: &[Operand], raw_operands: &str) -> Result<(), String> {
        // Make sure we're in a text section
        if self.current_section.is_empty() {
            self.ensure_section(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR, 4);
            self.current_section = ".text".to_string();
        }

        match encode_instruction(mnemonic, operands, raw_operands) {
            Ok(EncodeResult::Word(word)) => {
                self.emit_u32_le(word);
                Ok(())
            }
            Ok(EncodeResult::WordWithReloc { word, reloc }) => {
                // Check if it's a local label reference we can resolve
                let is_local = reloc.symbol.starts_with(".L") || reloc.symbol.starts_with(".l");

                if is_local {
                    // Store pending reloc - will be resolved after all labels are known
                    let offset = self.current_offset();
                    self.pending_branch_relocs.push(PendingReloc {
                        section: self.current_section.clone(),
                        offset,
                        reloc_type: reloc.reloc_type.elf_type(),
                        symbol: reloc.symbol.clone(),
                        addend: reloc.addend,
                    });
                    self.emit_u32_le(word);
                } else {
                    self.add_reloc(reloc.reloc_type.elf_type(), reloc.symbol, reloc.addend);
                    self.emit_u32_le(word);
                }
                Ok(())
            }
            Ok(EncodeResult::Words(words)) => {
                for word in words {
                    self.emit_u32_le(word);
                }
                Ok(())
            }
            Ok(EncodeResult::Skip) => Ok(()),
            Err(e) => {
                // For now, emit a NOP for unsupported instructions and continue
                // TODO: remove this fallback once all instructions are supported
                eprintln!("warning: assembler: {}", e);
                self.emit_u32_le(0xd503201f); // NOP
                Ok(())
            }
        }
    }

    /// Resolve local branch labels to PC-relative offsets.
    fn resolve_local_branches(&mut self) -> Result<(), String> {
        for reloc in &self.pending_branch_relocs {
            let (target_section, target_offset) = self.labels.get(&reloc.symbol)
                .ok_or_else(|| format!("undefined local label: {}", reloc.symbol))?
                .clone();

            if target_section != reloc.section {
                // Cross-section reference - leave as external relocation
                if let Some(section) = self.sections.get_mut(&reloc.section) {
                    section.relocs.push(ElfReloc {
                        offset: reloc.offset,
                        reloc_type: reloc.reloc_type,
                        symbol_name: reloc.symbol.clone(),
                        addend: reloc.addend,
                    });
                }
                continue;
            }

            let pc_offset = (target_offset as i64) - (reloc.offset as i64) + reloc.addend;

            if let Some(section) = self.sections.get_mut(&reloc.section) {
                let instr_offset = reloc.offset as usize;
                if instr_offset + 4 > section.data.len() {
                    continue;
                }

                let mut word = u32::from_le_bytes([
                    section.data[instr_offset],
                    section.data[instr_offset + 1],
                    section.data[instr_offset + 2],
                    section.data[instr_offset + 3],
                ]);

                match reloc.reloc_type {
                    282 | 283 => {
                        // R_AARCH64_JUMP26 / R_AARCH64_CALL26
                        let imm26 = ((pc_offset >> 2) as u32) & 0x3FFFFFF;
                        word |= imm26;
                    }
                    280 => {
                        // R_AARCH64_CONDBR19
                        let imm19 = ((pc_offset >> 2) as u32) & 0x7FFFF;
                        word |= imm19 << 5;
                    }
                    279 => {
                        // R_AARCH64_TSTBR14
                        let imm14 = ((pc_offset >> 2) as u32) & 0x3FFF;
                        word |= imm14 << 5;
                    }
                    _ => {
                        // Unknown reloc type for local branch - leave as external
                        section.relocs.push(ElfReloc {
                            offset: reloc.offset,
                            reloc_type: reloc.reloc_type,
                            symbol_name: reloc.symbol.clone(),
                            addend: reloc.addend,
                        });
                        continue;
                    }
                }

                section.data[instr_offset..instr_offset + 4].copy_from_slice(&word.to_le_bytes());
            }
        }
        Ok(())
    }

    /// Write the final ELF object file.
    pub fn write_elf(&mut self, output_path: &str) -> Result<(), String> {
        // Build the symbol table from labels and directives
        self.build_symbol_table();

        let mut elf = Vec::new();

        // ── Collect section metadata ──
        // Section layout: NULL, then content sections, then rela sections, then .symtab, .strtab, .shstrtab

        let mut shstrtab = StringTable::new();
        let mut strtab = StringTable::new();

        // Add the null string
        shstrtab.add("");
        strtab.add("");

        // Content sections
        let content_sections: Vec<String> = self.section_order.clone();

        // Build symbol table entries
        let mut sym_entries: Vec<SymEntry> = Vec::new();
        // First entry is always NULL
        sym_entries.push(SymEntry {
            st_name: 0,
            st_info: 0,
            st_other: 0,
            st_shndx: 0,
            st_value: 0,
            st_size: 0,
        });

        // Section symbols (one per content section)
        for (i, sec_name) in content_sections.iter().enumerate() {
            strtab.add(sec_name);
            sym_entries.push(SymEntry {
                st_name: strtab.offset_of(sec_name),
                st_info: (STB_LOCAL << 4) | STT_SECTION,
                st_other: 0,
                st_shndx: (i + 1) as u16, // section index (1-based, 0 is NULL)
                st_value: 0,
                st_size: 0,
            });
        }

        // Local symbols (from labels, excluding .L* which are local labels)
        let mut local_syms: Vec<&ElfSymbol> = Vec::new();
        let mut global_syms: Vec<&ElfSymbol> = Vec::new();

        for sym in &self.symbols {
            if sym.binding == STB_LOCAL {
                local_syms.push(sym);
            } else {
                global_syms.push(sym);
            }
        }

        let first_global_idx = sym_entries.len() + local_syms.len();

        for sym in &local_syms {
            let name_offset = strtab.add(&sym.name);
            let shndx = if sym.section_name == "*COM*" {
                0xFFF2u16 // SHN_COMMON
            } else if sym.section_name == "*UND*" || sym.section_name.is_empty() {
                0u16 // SHN_UNDEF
            } else {
                content_sections.iter().position(|s| s == &sym.section_name)
                    .map(|i| (i + 1) as u16)
                    .unwrap_or(0)
            };
            sym_entries.push(SymEntry {
                st_name: name_offset,
                st_info: (sym.binding << 4) | sym.sym_type,
                st_other: sym.visibility,
                st_shndx: shndx,
                st_value: sym.value,
                st_size: sym.size,
            });
        }

        for sym in &global_syms {
            let name_offset = strtab.add(&sym.name);
            let shndx = if sym.section_name == "*COM*" {
                0xFFF2u16 // SHN_COMMON
            } else if sym.section_name == "*UND*" || sym.section_name.is_empty() {
                0u16 // SHN_UNDEF
            } else {
                content_sections.iter().position(|s| s == &sym.section_name)
                    .map(|i| (i + 1) as u16)
                    .unwrap_or(0)
            };
            sym_entries.push(SymEntry {
                st_name: name_offset,
                st_info: (sym.binding << 4) | sym.sym_type,
                st_other: sym.visibility,
                st_shndx: shndx,
                st_value: sym.value,
                st_size: sym.size,
            });
        }

        // Add shstrtab names
        shstrtab.add("");
        for sec_name in &content_sections {
            shstrtab.add(sec_name);
        }
        shstrtab.add(".symtab");
        shstrtab.add(".strtab");
        shstrtab.add(".shstrtab");

        // Build rela section names and add them
        let mut rela_sections: Vec<String> = Vec::new();
        for sec_name in &content_sections {
            if let Some(section) = self.sections.get(sec_name) {
                if !section.relocs.is_empty() {
                    let rela_name = format!(".rela{}", sec_name);
                    shstrtab.add(&rela_name);
                    rela_sections.push(rela_name);
                }
            }
        }

        // ── Calculate layout ──
        let ehdr_size = 64usize; // ELF64 header
        let mut offset = ehdr_size;

        // Content section offsets
        let mut section_offsets: Vec<usize> = Vec::new();
        for sec_name in &content_sections {
            let section = self.sections.get(sec_name).unwrap();
            let align = section.sh_addralign.max(1) as usize;
            offset = (offset + align - 1) & !(align - 1);
            section_offsets.push(offset);
            if section.sh_type != SHT_NOBITS {
                offset += section.data.len();
            }
        }

        // Rela section offsets
        let mut rela_offsets: Vec<usize> = Vec::new();
        for sec_name in &content_sections {
            if let Some(section) = self.sections.get(sec_name) {
                if !section.relocs.is_empty() {
                    offset = (offset + 7) & !7; // 8-byte align
                    rela_offsets.push(offset);
                    offset += section.relocs.len() * 24; // sizeof(Elf64_Rela) = 24
                }
            }
        }

        // Symtab offset
        offset = (offset + 7) & !7;
        let symtab_offset = offset;
        let symtab_size = sym_entries.len() * 24; // sizeof(Elf64_Sym) = 24
        offset += symtab_size;

        // Strtab offset
        let strtab_offset = offset;
        let strtab_data = strtab.data();
        offset += strtab_data.len();

        // Shstrtab offset
        let shstrtab_offset = offset;
        let shstrtab_data = shstrtab.data();
        offset += shstrtab_data.len();

        // Section headers offset (align to 8)
        offset = (offset + 7) & !7;
        let shdr_offset = offset;

        // Section header count:
        // 1 (NULL) + content_sections.len() + rela_sections.len() + 3 (.symtab, .strtab, .shstrtab)
        let num_sections = 1 + content_sections.len() + rela_sections.len() + 3;
        let shstrtab_idx = num_sections - 1;

        // ── Write ELF header ──
        elf.extend_from_slice(&[0x7f, b'E', b'L', b'F']); // e_ident magic
        elf.push(ELFCLASS64);
        elf.push(ELFDATA2LSB);
        elf.push(EV_CURRENT);
        elf.push(ELFOSABI_NONE);
        elf.extend_from_slice(&[0u8; 8]); // padding
        elf.extend_from_slice(&ET_REL.to_le_bytes()); // e_type
        elf.extend_from_slice(&EM_AARCH64.to_le_bytes()); // e_machine
        elf.extend_from_slice(&1u32.to_le_bytes()); // e_version
        elf.extend_from_slice(&0u64.to_le_bytes()); // e_entry
        elf.extend_from_slice(&0u64.to_le_bytes()); // e_phoff
        elf.extend_from_slice(&(shdr_offset as u64).to_le_bytes()); // e_shoff
        elf.extend_from_slice(&0u32.to_le_bytes()); // e_flags
        elf.extend_from_slice(&(ehdr_size as u16).to_le_bytes()); // e_ehsize
        elf.extend_from_slice(&0u16.to_le_bytes()); // e_phentsize
        elf.extend_from_slice(&0u16.to_le_bytes()); // e_phnum
        elf.extend_from_slice(&64u16.to_le_bytes()); // e_shentsize
        elf.extend_from_slice(&(num_sections as u16).to_le_bytes()); // e_shnum
        elf.extend_from_slice(&(shstrtab_idx as u16).to_le_bytes()); // e_shstrndx

        assert_eq!(elf.len(), ehdr_size);

        // ── Write content section data ──
        for (i, sec_name) in content_sections.iter().enumerate() {
            let section = self.sections.get(sec_name).unwrap();
            // Pad to alignment
            while elf.len() < section_offsets[i] {
                elf.push(0);
            }
            if section.sh_type != SHT_NOBITS {
                elf.extend_from_slice(&section.data);
            }
        }

        // ── Write rela section data ──
        let symtab_shndx = 1 + content_sections.len() + rela_sections.len(); // index of .symtab in section header table
        let mut rela_idx = 0;
        for sec_name in &content_sections {
            if let Some(section) = self.sections.get(sec_name) {
                if !section.relocs.is_empty() {
                    while elf.len() < rela_offsets[rela_idx] {
                        elf.push(0);
                    }
                    for reloc in &section.relocs {
                        // Find symbol index
                        let sym_idx = self.find_symbol_index(&reloc.symbol_name, &sym_entries, &strtab, &content_sections);
                        // Write Elf64_Rela
                        elf.extend_from_slice(&reloc.offset.to_le_bytes());
                        let r_info = ((sym_idx as u64) << 32) | (reloc.reloc_type as u64);
                        elf.extend_from_slice(&r_info.to_le_bytes());
                        elf.extend_from_slice(&reloc.addend.to_le_bytes());
                    }
                    rela_idx += 1;
                }
            }
        }

        // ── Write symtab ──
        while elf.len() < symtab_offset {
            elf.push(0);
        }
        for sym in &sym_entries {
            elf.extend_from_slice(&sym.st_name.to_le_bytes());
            elf.push(sym.st_info);
            elf.push(sym.st_other);
            elf.extend_from_slice(&sym.st_shndx.to_le_bytes());
            elf.extend_from_slice(&sym.st_value.to_le_bytes());
            elf.extend_from_slice(&sym.st_size.to_le_bytes());
        }

        // ── Write strtab ──
        assert_eq!(elf.len(), strtab_offset);
        elf.extend_from_slice(&strtab_data);

        // ── Write shstrtab ──
        assert_eq!(elf.len(), shstrtab_offset);
        elf.extend_from_slice(&shstrtab_data);

        // ── Write section headers ──
        while elf.len() < shdr_offset {
            elf.push(0);
        }

        // SHT_NULL entry
        write_shdr(&mut elf, 0, SHT_NULL, 0, 0, 0, 0, 0, 0, 0, 0);

        // Content sections
        for (i, sec_name) in content_sections.iter().enumerate() {
            let section = self.sections.get(sec_name).unwrap();
            let sh_name = shstrtab.offset_of(sec_name);
            let sh_offset = if section.sh_type == SHT_NOBITS { 0 } else { section_offsets[i] as u64 };
            let sh_size = section.data.len() as u64;
            write_shdr(&mut elf, sh_name, section.sh_type, section.sh_flags,
                       0, sh_offset, sh_size, 0, 0, section.sh_addralign, section.sh_entsize);
        }

        // Rela sections
        rela_idx = 0;
        for (i, sec_name) in content_sections.iter().enumerate() {
            if let Some(section) = self.sections.get(sec_name) {
                if !section.relocs.is_empty() {
                    let rela_name = format!(".rela{}", sec_name);
                    let sh_name = shstrtab.offset_of(&rela_name);
                    let sh_offset = rela_offsets[rela_idx] as u64;
                    let sh_size = (section.relocs.len() * 24) as u64;
                    let sh_link = symtab_shndx as u32;
                    let sh_info = (i + 1) as u32; // Index of the section these relocs apply to
                    write_shdr(&mut elf, sh_name, SHT_RELA, SHF_INFO_LINK,
                               0, sh_offset, sh_size, sh_link, sh_info, 8, 24);
                    rela_idx += 1;
                }
            }
        }

        // .symtab
        let symtab_name = shstrtab.offset_of(".symtab");
        let strtab_shndx = symtab_shndx + 1;
        write_shdr(&mut elf, symtab_name, SHT_SYMTAB, 0,
                   0, symtab_offset as u64, symtab_size as u64,
                   strtab_shndx as u32, first_global_idx as u32, 8, 24);

        // .strtab
        let strtab_name = shstrtab.offset_of(".strtab");
        write_shdr(&mut elf, strtab_name, SHT_STRTAB, 0,
                   0, strtab_offset as u64, strtab_data.len() as u64, 0, 0, 1, 0);

        // .shstrtab
        let shstrtab_name = shstrtab.offset_of(".shstrtab");
        write_shdr(&mut elf, shstrtab_name, SHT_STRTAB, 0,
                   0, shstrtab_offset as u64, shstrtab_data.len() as u64, 0, 0, 1, 0);

        // Write to file
        std::fs::write(output_path, &elf)
            .map_err(|e| format!("failed to write ELF file: {}", e))?;

        Ok(())
    }

    /// Build the symbol table from collected labels and directives.
    fn build_symbol_table(&mut self) {
        // Collect all defined labels as symbols
        let labels = self.labels.clone();
        for (name, (section, offset)) in &labels {
            // Skip local labels (.L*)
            if name.starts_with(".L") || name.starts_with(".l") {
                continue;
            }

            let binding = if self.weak_symbols.contains_key(name) {
                STB_WEAK
            } else if self.global_symbols.contains_key(name) {
                STB_GLOBAL
            } else {
                STB_LOCAL
            };

            let sym_type = self.symbol_types.get(name).copied().unwrap_or(STT_NOTYPE);
            let size = self.symbol_sizes.get(name).copied().unwrap_or(0);
            let visibility = self.symbol_visibility.get(name).copied().unwrap_or(STV_DEFAULT);

            self.symbols.push(ElfSymbol {
                name: name.clone(),
                value: *offset,
                size,
                binding,
                sym_type,
                visibility,
                section_name: section.clone(),
            });
        }

        // Add undefined symbols (referenced but not defined)
        let mut referenced: HashMap<String, bool> = HashMap::new();
        for sec in self.sections.values() {
            for reloc in &sec.relocs {
                if !reloc.symbol_name.starts_with(".L") && !reloc.symbol_name.starts_with(".l") {
                    referenced.insert(reloc.symbol_name.clone(), true);
                }
            }
        }

        let defined: HashMap<String, bool> = self.symbols.iter()
            .map(|s| (s.name.clone(), true))
            .collect();

        for (name, _) in &referenced {
            if !defined.contains_key(name) {
                let binding = if self.weak_symbols.contains_key(name) {
                    STB_WEAK
                } else {
                    STB_GLOBAL
                };
                let sym_type = self.symbol_types.get(name).copied().unwrap_or(STT_NOTYPE);
                let visibility = self.symbol_visibility.get(name).copied().unwrap_or(STV_DEFAULT);

                self.symbols.push(ElfSymbol {
                    name: name.clone(),
                    value: 0,
                    size: 0,
                    binding,
                    sym_type,
                    visibility,
                    section_name: "*UND*".to_string(),
                });
            }
        }
    }

    fn find_symbol_index(&self, name: &str, sym_entries: &[SymEntry], strtab: &StringTable, content_sections: &[String]) -> u32 {
        // First check if it's a section symbol
        for (i, sec_name) in content_sections.iter().enumerate() {
            if sec_name == name {
                return (i + 1) as u32; // +1 for NULL entry
            }
        }

        // Then check named symbols
        let name_offset = strtab.offset_of(name);
        for (i, entry) in sym_entries.iter().enumerate() {
            if entry.st_name == name_offset && entry.st_info & 0xF != STT_SECTION {
                return i as u32;
            }
        }

        // Not found - return 0 (undefined)
        0
    }
}

/// Write an Elf64_Shdr entry.
fn write_shdr(
    buf: &mut Vec<u8>,
    sh_name: u32, sh_type: u32, sh_flags: u64,
    sh_addr: u64, sh_offset: u64, sh_size: u64,
    sh_link: u32, sh_info: u32, sh_addralign: u64, sh_entsize: u64,
) {
    buf.extend_from_slice(&sh_name.to_le_bytes());
    buf.extend_from_slice(&sh_type.to_le_bytes());
    buf.extend_from_slice(&sh_flags.to_le_bytes());
    buf.extend_from_slice(&sh_addr.to_le_bytes());
    buf.extend_from_slice(&sh_offset.to_le_bytes());
    buf.extend_from_slice(&sh_size.to_le_bytes());
    buf.extend_from_slice(&sh_link.to_le_bytes());
    buf.extend_from_slice(&sh_info.to_le_bytes());
    buf.extend_from_slice(&sh_addralign.to_le_bytes());
    buf.extend_from_slice(&sh_entsize.to_le_bytes());
}

/// A symbol table entry for writing.
struct SymEntry {
    st_name: u32,
    st_info: u8,
    st_other: u8,
    st_shndx: u16,
    st_value: u64,
    st_size: u64,
}

/// Simple string table builder.
struct StringTable {
    data: Vec<u8>,
    offsets: HashMap<String, u32>,
}

impl StringTable {
    fn new() -> Self {
        Self {
            data: vec![0], // Start with null byte
            offsets: HashMap::new(),
        }
    }

    fn add(&mut self, s: &str) -> u32 {
        if let Some(&offset) = self.offsets.get(s) {
            return offset;
        }
        if s.is_empty() {
            return 0;
        }
        let offset = self.data.len() as u32;
        self.data.extend_from_slice(s.as_bytes());
        self.data.push(0);
        self.offsets.insert(s.to_string(), offset);
        offset
    }

    fn offset_of(&self, s: &str) -> u32 {
        self.offsets.get(s).copied().unwrap_or(0)
    }

    fn data(&self) -> Vec<u8> {
        self.data.clone()
    }
}

/// Parse a .section directive's arguments.
/// Returns (section_name, flags, type).
fn parse_section_directive(args: &str) -> (String, String, String) {
    let parts: Vec<&str> = args.split(',').collect();
    let name = parts[0].trim().to_string();
    let flags = if parts.len() > 1 {
        parts[1].trim().trim_matches('"').to_string()
    } else {
        String::new()
    };
    let sec_type = if parts.len() > 2 {
        parts[2].trim().to_string()
    } else {
        "@progbits".to_string()
    };
    (name, flags, sec_type)
}

/// Parse a data value (integer literal, possibly negative).
fn parse_data_value(s: &str) -> Result<i64, String> {
    let s = s.trim();
    if s.is_empty() {
        return Ok(0);
    }

    let (negative, s) = if s.starts_with('-') {
        (true, &s[1..])
    } else {
        (false, s)
    };

    let val = if s.starts_with("0x") || s.starts_with("0X") {
        u64::from_str_radix(&s[2..], 16)
            .map_err(|e| format!("invalid hex: {}: {}", s, e))?
    } else {
        s.parse::<u64>()
            .map_err(|e| format!("invalid integer: {}: {}", s, e))?
    };

    if negative {
        Ok(-(val as i64))
    } else {
        Ok(val as i64)
    }
}

/// Parse a string literal (strip quotes, handle escapes).
fn parse_string_literal(s: &str) -> Result<String, String> {
    let s = s.trim();
    if !s.starts_with('"') || !s.ends_with('"') {
        return Err(format!("expected string literal: {}", s));
    }
    let inner = &s[1..s.len() - 1];
    let mut result = String::new();
    let mut chars = inner.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('r') => result.push('\r'),
                Some('0') => result.push('\0'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some(c) if c.is_ascii_digit() => {
                    // Octal escape
                    let mut octal = String::new();
                    octal.push(c);
                    // Read up to 2 more octal digits
                    // This is tricky with the chars iterator - simplify
                    result.push(c); // TODO: proper octal parsing
                }
                Some(c) => {
                    result.push('\\');
                    result.push(c);
                }
                None => result.push('\\'),
            }
        } else {
            result.push(c);
        }
    }
    Ok(result)
}
