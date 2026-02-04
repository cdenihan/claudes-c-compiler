/// ELF file parsing for the x86-64 linker.
///
/// Reads ELF relocatable object files (.o) and shared libraries (.so),
/// extracting sections, symbols, and relocations needed for linking.

// ELF constants
pub const ELF_MAGIC: [u8; 4] = [0x7f, b'E', b'L', b'F'];
pub const ELFCLASS64: u8 = 2;
pub const ELFDATA2LSB: u8 = 1;
pub const ET_REL: u16 = 1;
pub const ET_EXEC: u16 = 2;
pub const ET_DYN: u16 = 3;
pub const EM_X86_64: u16 = 62;

// Section header types
pub const SHT_NULL: u32 = 0;
pub const SHT_PROGBITS: u32 = 1;
pub const SHT_SYMTAB: u32 = 2;
pub const SHT_STRTAB: u32 = 3;
pub const SHT_RELA: u32 = 4;
pub const SHT_HASH: u32 = 5;
pub const SHT_DYNAMIC: u32 = 6;
pub const SHT_NOTE: u32 = 7;
pub const SHT_NOBITS: u32 = 8;
pub const SHT_REL: u32 = 9;
pub const SHT_DYNSYM: u32 = 11;
pub const SHT_INIT_ARRAY: u32 = 14;
pub const SHT_FINI_ARRAY: u32 = 15;
pub const SHT_GROUP: u32 = 17;
pub const SHT_GNU_HASH: u32 = 0x6ffffff6;
pub const SHT_GNU_VERSYM: u32 = 0x6fffffff;
pub const SHT_GNU_VERNEED: u32 = 0x6ffffffe;
pub const SHT_GNU_VERDEF: u32 = 0x6ffffffd;

// Section header flags
pub const SHF_WRITE: u64 = 0x1;
pub const SHF_ALLOC: u64 = 0x2;
pub const SHF_EXECINSTR: u64 = 0x4;
pub const SHF_MERGE: u64 = 0x10;
pub const SHF_STRINGS: u64 = 0x20;
pub const SHF_INFO_LINK: u64 = 0x40;
pub const SHF_GROUP: u64 = 0x200;
pub const SHF_TLS: u64 = 0x400;
pub const SHF_EXCLUDE: u64 = 0x80000000;

// Symbol binding
pub const STB_LOCAL: u8 = 0;
pub const STB_GLOBAL: u8 = 1;
pub const STB_WEAK: u8 = 2;

// Symbol type
pub const STT_NOTYPE: u8 = 0;
pub const STT_OBJECT: u8 = 1;
pub const STT_FUNC: u8 = 2;
pub const STT_SECTION: u8 = 3;
pub const STT_FILE: u8 = 4;
pub const STT_COMMON: u8 = 5;
pub const STT_TLS: u8 = 6;

// Symbol visibility
pub const STV_DEFAULT: u8 = 0;
pub const STV_HIDDEN: u8 = 2;
pub const STV_PROTECTED: u8 = 3;

// Special section indices
pub const SHN_UNDEF: u16 = 0;
pub const SHN_ABS: u16 = 0xfff1;
pub const SHN_COMMON: u16 = 0xfff2;

// x86-64 relocation types
pub const R_X86_64_NONE: u32 = 0;
pub const R_X86_64_64: u32 = 1;
pub const R_X86_64_PC32: u32 = 2;
pub const R_X86_64_GOT32: u32 = 3;
pub const R_X86_64_PLT32: u32 = 4;
pub const R_X86_64_GOTPCREL: u32 = 9;
pub const R_X86_64_32: u32 = 10;
pub const R_X86_64_32S: u32 = 11;
pub const R_X86_64_TPOFF32: u32 = 23;
pub const R_X86_64_PC64: u32 = 24;
pub const R_X86_64_GOTPCRELX: u32 = 41;
pub const R_X86_64_REX_GOTPCRELX: u32 = 42;

// Program header types
pub const PT_NULL: u32 = 0;
pub const PT_LOAD: u32 = 1;
pub const PT_DYNAMIC: u32 = 2;
pub const PT_INTERP: u32 = 3;
pub const PT_NOTE: u32 = 4;
pub const PT_PHDR: u32 = 6;
pub const PT_TLS: u32 = 7;
pub const PT_GNU_EH_FRAME: u32 = 0x6474e550;
pub const PT_GNU_STACK: u32 = 0x6474e551;
pub const PT_GNU_RELRO: u32 = 0x6474e552;

// Program header flags
pub const PF_X: u32 = 0x1;
pub const PF_W: u32 = 0x2;
pub const PF_R: u32 = 0x4;

// Dynamic section tags
pub const DT_NULL: i64 = 0;
pub const DT_NEEDED: i64 = 1;
pub const DT_PLTRELSZ: i64 = 2;
pub const DT_PLTGOT: i64 = 3;
pub const DT_HASH: i64 = 4;
pub const DT_STRTAB: i64 = 5;
pub const DT_SYMTAB: i64 = 6;
pub const DT_RELA: i64 = 7;
pub const DT_RELASZ: i64 = 8;
pub const DT_RELAENT: i64 = 9;
pub const DT_STRSZ: i64 = 10;
pub const DT_SYMENT: i64 = 11;
pub const DT_INIT: i64 = 12;
pub const DT_FINI: i64 = 13;
pub const DT_SONAME: i64 = 14;
pub const DT_DEBUG: i64 = 21;
pub const DT_JMPREL: i64 = 23;
pub const DT_INIT_ARRAY: i64 = 25;
pub const DT_FINI_ARRAY: i64 = 26;
pub const DT_INIT_ARRAYSZ: i64 = 27;
pub const DT_FINI_ARRAYSZ: i64 = 28;
pub const DT_FLAGS: i64 = 30;
pub const DT_GNU_HASH: i64 = 0x6ffffef5;
pub const DT_VERSYM: i64 = 0x6ffffff0;
pub const DT_VERNEED: i64 = 0x6ffffffe;
pub const DT_VERNEEDNUM: i64 = 0x6fffffff;
pub const DT_PLTREL: i64 = 20;
pub const DT_RELACOUNT: i64 = 0x6ffffff9;

// Dynamic flags
pub const DF_BIND_NOW: i64 = 0x8;

/// Parsed ELF section header
#[derive(Debug, Clone)]
pub struct SectionHeader {
    pub name_idx: u32,
    pub name: String,
    pub sh_type: u32,
    pub flags: u64,
    pub addr: u64,
    pub offset: u64,
    pub size: u64,
    pub link: u32,
    pub info: u32,
    pub addralign: u64,
    pub entsize: u64,
}

/// Parsed ELF symbol
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name_idx: u32,
    pub name: String,
    pub info: u8,
    pub other: u8,
    pub shndx: u16,
    pub value: u64,
    pub size: u64,
}

impl Symbol {
    pub fn binding(&self) -> u8 {
        self.info >> 4
    }

    pub fn sym_type(&self) -> u8 {
        self.info & 0xf
    }

    pub fn visibility(&self) -> u8 {
        self.other & 0x3
    }

    pub fn is_undefined(&self) -> bool {
        self.shndx == SHN_UNDEF
    }

    pub fn is_global(&self) -> bool {
        self.binding() == STB_GLOBAL
    }

    pub fn is_weak(&self) -> bool {
        self.binding() == STB_WEAK
    }

    pub fn is_local(&self) -> bool {
        self.binding() == STB_LOCAL
    }
}

/// Parsed ELF relocation with addend
#[derive(Debug, Clone)]
pub struct Rela {
    pub offset: u64,
    pub sym_idx: u32,
    pub rela_type: u32,
    pub addend: i64,
}

/// Parsed ELF object file
#[derive(Debug)]
pub struct ElfObject {
    pub sections: Vec<SectionHeader>,
    pub symbols: Vec<Symbol>,
    pub section_data: Vec<Vec<u8>>,
    /// Relocations indexed by the section they apply to
    pub relocations: Vec<Vec<Rela>>,
    pub source_name: String,
}

/// Parsed dynamic symbol from a shared library
#[derive(Debug, Clone)]
pub struct DynSymbol {
    pub name: String,
    pub info: u8,
    pub value: u64,
    pub size: u64,
}

impl DynSymbol {
    pub fn sym_type(&self) -> u8 {
        self.info & 0xf
    }
}

/// Read a little-endian u16 from a byte slice
fn read_u16(data: &[u8], offset: usize) -> u16 {
    u16::from_le_bytes([data[offset], data[offset + 1]])
}

/// Read a little-endian u32 from a byte slice
fn read_u32(data: &[u8], offset: usize) -> u32 {
    u32::from_le_bytes([
        data[offset], data[offset + 1], data[offset + 2], data[offset + 3],
    ])
}

/// Read a little-endian u64 from a byte slice
fn read_u64(data: &[u8], offset: usize) -> u64 {
    u64::from_le_bytes([
        data[offset], data[offset + 1], data[offset + 2], data[offset + 3],
        data[offset + 4], data[offset + 5], data[offset + 6], data[offset + 7],
    ])
}

/// Read a little-endian i64 from a byte slice
fn read_i64(data: &[u8], offset: usize) -> i64 {
    i64::from_le_bytes([
        data[offset], data[offset + 1], data[offset + 2], data[offset + 3],
        data[offset + 4], data[offset + 5], data[offset + 6], data[offset + 7],
    ])
}

/// Read a null-terminated string from a byte slice
fn read_string(data: &[u8], offset: usize) -> String {
    if offset >= data.len() {
        return String::new();
    }
    let end = data[offset..].iter().position(|&b| b == 0).unwrap_or(data.len() - offset);
    String::from_utf8_lossy(&data[offset..offset + end]).to_string()
}

/// Parse an ELF relocatable object file (.o)
pub fn parse_object(data: &[u8], source_name: &str) -> Result<ElfObject, String> {
    if data.len() < 64 {
        return Err(format!("{}: file too small for ELF header", source_name));
    }

    // Validate ELF magic
    if data[0..4] != ELF_MAGIC {
        return Err(format!("{}: not an ELF file", source_name));
    }
    if data[4] != ELFCLASS64 {
        return Err(format!("{}: not 64-bit ELF", source_name));
    }
    if data[5] != ELFDATA2LSB {
        return Err(format!("{}: not little-endian ELF", source_name));
    }

    let e_type = read_u16(data, 16);
    if e_type != ET_REL {
        return Err(format!("{}: not a relocatable object (type={})", source_name, e_type));
    }

    let e_machine = read_u16(data, 18);
    if e_machine != EM_X86_64 {
        return Err(format!("{}: not x86-64 (machine={})", source_name, e_machine));
    }

    let e_shoff = read_u64(data, 40) as usize;
    let e_shentsize = read_u16(data, 58) as usize;
    let e_shnum = read_u16(data, 60) as usize;
    let e_shstrndx = read_u16(data, 62) as usize;

    if e_shoff == 0 || e_shnum == 0 {
        return Err(format!("{}: no section headers", source_name));
    }

    // Parse section headers
    let mut sections = Vec::with_capacity(e_shnum);
    for i in 0..e_shnum {
        let off = e_shoff + i * e_shentsize;
        if off + e_shentsize > data.len() {
            return Err(format!("{}: section header {} out of bounds", source_name, i));
        }
        sections.push(SectionHeader {
            name_idx: read_u32(data, off),
            name: String::new(), // filled in below
            sh_type: read_u32(data, off + 4),
            flags: read_u64(data, off + 8),
            addr: read_u64(data, off + 16),
            offset: read_u64(data, off + 24),
            size: read_u64(data, off + 32),
            link: read_u32(data, off + 40),
            info: read_u32(data, off + 44),
            addralign: read_u64(data, off + 48),
            entsize: read_u64(data, off + 56),
        });
    }

    // Read section name string table
    if e_shstrndx < sections.len() {
        let shstrtab = &sections[e_shstrndx];
        let strtab_off = shstrtab.offset as usize;
        let strtab_size = shstrtab.size as usize;
        if strtab_off + strtab_size <= data.len() {
            let strtab_data = &data[strtab_off..strtab_off + strtab_size];
            for sec in &mut sections {
                sec.name = read_string(strtab_data, sec.name_idx as usize);
            }
        }
    }

    // Read section data
    let mut section_data = Vec::with_capacity(e_shnum);
    for sec in &sections {
        if sec.sh_type == SHT_NOBITS || sec.size == 0 {
            section_data.push(Vec::new());
        } else {
            let start = sec.offset as usize;
            let end = start + sec.size as usize;
            if end > data.len() {
                return Err(format!("{}: section '{}' data out of bounds", source_name, sec.name));
            }
            section_data.push(data[start..end].to_vec());
        }
    }

    // Find symbol table and its string table
    let mut symbols = Vec::new();
    for i in 0..sections.len() {
        if sections[i].sh_type == SHT_SYMTAB {
            let strtab_idx = sections[i].link as usize;
            let strtab_data = if strtab_idx < section_data.len() {
                &section_data[strtab_idx]
            } else {
                continue;
            };
            let sym_data = &section_data[i];
            let sym_count = sym_data.len() / 24; // sizeof(Elf64_Sym) = 24
            for j in 0..sym_count {
                let off = j * 24;
                if off + 24 > sym_data.len() {
                    break;
                }
                let name_idx = read_u32(sym_data, off);
                symbols.push(Symbol {
                    name_idx,
                    name: read_string(strtab_data, name_idx as usize),
                    info: sym_data[off + 4],
                    other: sym_data[off + 5],
                    shndx: read_u16(sym_data, off + 6),
                    value: read_u64(sym_data, off + 8),
                    size: read_u64(sym_data, off + 16),
                });
            }
            break;
        }
    }

    // Parse relocations - index by the section they apply to
    let mut relocations = vec![Vec::new(); e_shnum];
    for i in 0..sections.len() {
        if sections[i].sh_type == SHT_RELA {
            let target_sec = sections[i].info as usize;
            let rela_data = &section_data[i];
            let rela_count = rela_data.len() / 24; // sizeof(Elf64_Rela) = 24
            let mut relas = Vec::with_capacity(rela_count);
            for j in 0..rela_count {
                let off = j * 24;
                if off + 24 > rela_data.len() {
                    break;
                }
                let r_info = read_u64(rela_data, off + 8);
                relas.push(Rela {
                    offset: read_u64(rela_data, off),
                    sym_idx: (r_info >> 32) as u32,
                    rela_type: (r_info & 0xffffffff) as u32,
                    addend: read_i64(rela_data, off + 16),
                });
            }
            if target_sec < relocations.len() {
                relocations[target_sec] = relas;
            }
        }
    }

    Ok(ElfObject {
        sections,
        symbols,
        section_data,
        relocations,
        source_name: source_name.to_string(),
    })
}

/// Parse an archive (.a) file and extract object files on demand.
///
/// Returns a list of (member_name, offset, size) for each archive member.
pub fn parse_archive_members(data: &[u8]) -> Result<Vec<(String, usize, usize)>, String> {
    if data.len() < 8 || &data[0..8] != b"!<arch>\n" {
        return Err("not a valid archive file".to_string());
    }

    let mut members = Vec::new();
    let mut pos = 8;

    // Track extended name table for long names
    let mut extended_names: Option<&[u8]> = None;

    while pos + 60 <= data.len() {
        // Archive member header is 60 bytes
        let name_raw = &data[pos..pos + 16];
        let size_str = std::str::from_utf8(&data[pos + 48..pos + 58])
            .unwrap_or("")
            .trim();
        let magic = &data[pos + 58..pos + 60];
        if magic != b"`\n" {
            break; // Invalid header
        }

        let size: usize = size_str.parse().unwrap_or(0);
        let data_start = pos + 60;

        // Parse member name
        let name_str = std::str::from_utf8(name_raw).unwrap_or("").trim_end();

        if name_str == "/" || name_str == "/SYM64/" {
            // Symbol table - skip
        } else if name_str == "//" {
            // Extended name table
            extended_names = Some(&data[data_start..data_start + size]);
        } else {
            // Regular member
            let member_name = if name_str.starts_with('/') {
                // Extended name: /offset into extended names table
                if let Some(ext) = extended_names {
                    let name_off: usize = name_str[1..].trim_end_matches('/').parse().unwrap_or(0);
                    if name_off < ext.len() {
                        let end = ext[name_off..].iter()
                            .position(|&b| b == b'/' || b == b'\n' || b == 0)
                            .unwrap_or(ext.len() - name_off);
                        String::from_utf8_lossy(&ext[name_off..name_off + end]).to_string()
                    } else {
                        name_str.to_string()
                    }
                } else {
                    name_str.to_string()
                }
            } else {
                // Short name, strip trailing /
                name_str.trim_end_matches('/').to_string()
            };

            if data_start + size <= data.len() {
                members.push((member_name, data_start, size));
            }
        }

        // Align to 2-byte boundary
        pos = data_start + size;
        if pos % 2 != 0 {
            pos += 1;
        }
    }

    Ok(members)
}

/// Extract dynamic symbols from a shared library (.so) file.
///
/// Reads the .dynsym section to find exported symbols.
pub fn parse_shared_library_symbols(data: &[u8], lib_name: &str) -> Result<Vec<DynSymbol>, String> {
    if data.len() < 64 {
        return Err(format!("{}: file too small for ELF header", lib_name));
    }
    if data[0..4] != ELF_MAGIC {
        return Err(format!("{}: not an ELF file", lib_name));
    }
    if data[4] != ELFCLASS64 || data[5] != ELFDATA2LSB {
        return Err(format!("{}: not 64-bit little-endian ELF", lib_name));
    }

    let e_type = read_u16(data, 16);
    if e_type != ET_DYN {
        return Err(format!("{}: not a shared library (type={})", lib_name, e_type));
    }

    let e_shoff = read_u64(data, 40) as usize;
    let e_shentsize = read_u16(data, 58) as usize;
    let e_shnum = read_u16(data, 60) as usize;
    let _e_shstrndx = read_u16(data, 62) as usize;

    if e_shoff == 0 || e_shnum == 0 {
        return Err(format!("{}: no section headers", lib_name));
    }

    // Parse section headers
    let mut sections = Vec::with_capacity(e_shnum);
    for i in 0..e_shnum {
        let off = e_shoff + i * e_shentsize;
        if off + e_shentsize > data.len() {
            break;
        }
        sections.push((
            read_u32(data, off + 4),  // sh_type
            read_u64(data, off + 24), // offset
            read_u64(data, off + 32), // size
            read_u32(data, off + 40), // link
        ));
    }

    // Find .dynsym and its string table
    let mut symbols = Vec::new();
    for i in 0..sections.len() {
        let (sh_type, offset, size, link) = sections[i];
        if sh_type == SHT_DYNSYM {
            // Get the dynamic string table
            let strtab_idx = link as usize;
            if strtab_idx >= sections.len() {
                continue;
            }
            let (_, str_off, str_size, _) = sections[strtab_idx];
            let str_off = str_off as usize;
            let str_size = str_size as usize;
            if str_off + str_size > data.len() {
                continue;
            }
            let strtab = &data[str_off..str_off + str_size];

            let sym_off = offset as usize;
            let sym_size = size as usize;
            if sym_off + sym_size > data.len() {
                continue;
            }
            let sym_data = &data[sym_off..sym_off + sym_size];
            let sym_count = sym_data.len() / 24;

            for j in 1..sym_count { // skip null symbol at index 0
                let off = j * 24;
                if off + 24 > sym_data.len() {
                    break;
                }
                let name_idx = read_u32(sym_data, off) as usize;
                let info = sym_data[off + 4];
                let shndx = read_u16(sym_data, off + 6);
                let value = read_u64(sym_data, off + 8);
                let size = read_u64(sym_data, off + 16);

                // Only include defined symbols (shndx != UND)
                if shndx == SHN_UNDEF {
                    continue;
                }

                let name = read_string(strtab, name_idx);
                if name.is_empty() {
                    continue;
                }

                // Strip version suffixes (e.g., "printf@@GLIBC_2.2.5" -> "printf")
                // Actually, readelf shows versions separately, the name in strtab
                // doesn't have @@ - versions are in .gnu.version section.
                // But some linker scripts may have them.

                symbols.push(DynSymbol { name, info, value, size });
            }
            break;
        }
    }

    Ok(symbols)
}

/// Get the SONAME from a shared library's .dynamic section
pub fn parse_soname(data: &[u8]) -> Option<String> {
    if data.len() < 64 || data[0..4] != ELF_MAGIC {
        return None;
    }

    let e_shoff = read_u64(data, 40) as usize;
    let e_shentsize = read_u16(data, 58) as usize;
    let e_shnum = read_u16(data, 60) as usize;

    // Find .dynamic section
    for i in 0..e_shnum {
        let off = e_shoff + i * e_shentsize;
        if off + 64 > data.len() {
            break;
        }
        let sh_type = read_u32(data, off + 4);
        if sh_type == SHT_DYNAMIC {
            let dyn_off = read_u64(data, off + 24) as usize;
            let dyn_size = read_u64(data, off + 32) as usize;
            let link = read_u32(data, off + 40) as usize;

            // Get the string table for this dynamic section
            let str_sec_off = e_shoff + link * e_shentsize;
            if str_sec_off + 64 > data.len() {
                return None;
            }
            let str_off = read_u64(data, str_sec_off + 24) as usize;
            let str_size = read_u64(data, str_sec_off + 32) as usize;
            if str_off + str_size > data.len() {
                return None;
            }
            let strtab = &data[str_off..str_off + str_size];

            // Find DT_SONAME entry
            let mut pos = dyn_off;
            while pos + 16 <= dyn_off + dyn_size && pos + 16 <= data.len() {
                let tag = read_i64(data, pos);
                let val = read_u64(data, pos + 8);
                if tag == DT_NULL {
                    break;
                }
                if tag == DT_SONAME {
                    return Some(read_string(strtab, val as usize));
                }
                pos += 16;
            }
        }
    }
    None
}

/// Parse a linker script like libc.so to extract GROUP members.
/// Returns the list of library paths referenced by the script.
pub fn parse_linker_script(content: &str) -> Option<Vec<String>> {
    // Look for GROUP ( ... ) pattern
    let group_start = content.find("GROUP")?;
    let paren_start = content[group_start..].find('(')?;
    let paren_end = content[group_start..].find(')')?;
    let inside = &content[group_start + paren_start + 1..group_start + paren_end];

    let mut paths = Vec::new();
    let mut in_as_needed = false;
    for token in inside.split_whitespace() {
        match token {
            "AS_NEEDED" => { in_as_needed = true; continue; }
            "(" => continue,
            ")" => { in_as_needed = false; continue; }
            _ => {}
        }
        if token.starts_with('/') || token.ends_with(".so") || token.ends_with(".a") ||
           token.contains(".so.") {
            if !in_as_needed {
                paths.push(token.to_string());
            }
        }
    }

    if paths.is_empty() { None } else { Some(paths) }
}
