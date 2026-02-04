//! ELF object file and archive reader for the linker.

use std::collections::HashMap;

// ELF constants
pub const ET_REL: u16 = 1;
pub const EM_RISCV: u16 = 243;
pub const SHT_PROGBITS: u32 = 1;
pub const SHT_SYMTAB: u32 = 2;
pub const SHT_STRTAB: u32 = 3;
pub const SHT_RELA: u32 = 4;
pub const SHT_NOBITS: u32 = 8;
pub const SHT_INIT_ARRAY: u32 = 14;
pub const SHT_FINI_ARRAY: u32 = 15;
pub const SHT_PREINIT_ARRAY: u32 = 16;
pub const SHT_GROUP: u32 = 17;
pub const SHT_RISCV_ATTRIBUTES: u32 = 0x70000003;

pub const SHF_WRITE: u64 = 1;
pub const SHF_ALLOC: u64 = 2;
pub const SHF_EXECINSTR: u64 = 4;
pub const SHF_TLS: u64 = 0x400;
pub const SHF_GROUP: u64 = 0x200;

pub const STB_LOCAL: u8 = 0;
pub const STB_GLOBAL: u8 = 1;
pub const STB_WEAK: u8 = 2;

pub const STT_NOTYPE: u8 = 0;
pub const STT_OBJECT: u8 = 1;
pub const STT_FUNC: u8 = 2;
pub const STT_SECTION: u8 = 3;
pub const STT_FILE: u8 = 4;
pub const STT_TLS: u8 = 6;

pub const STV_DEFAULT: u8 = 0;
pub const STV_HIDDEN: u8 = 2;

pub const SHN_UNDEF: u16 = 0;
pub const SHN_ABS: u16 = 0xFFF1;
pub const SHN_COMMON: u16 = 0xFFF2;

/// Parsed ELF section from a .o file.
#[derive(Clone)]
pub struct ObjSection {
    pub name: String,
    pub sh_type: u32,
    pub sh_flags: u64,
    pub data: Vec<u8>,
    pub sh_addralign: u64,
    pub sh_entsize: u64,
    pub sh_link: u32,
    pub sh_info: u32,
}

/// Parsed ELF symbol.
#[derive(Clone, Debug)]
pub struct ObjSymbol {
    pub name: String,
    pub value: u64,
    pub size: u64,
    pub binding: u8,
    pub sym_type: u8,
    pub visibility: u8,
    pub section_idx: u16,
}

/// Parsed relocation entry.
#[derive(Clone, Debug)]
pub struct ObjReloc {
    pub offset: u64,
    pub reloc_type: u32,
    pub symbol_idx: u32,
    pub addend: i64,
}

/// A parsed ELF object file.
#[derive(Clone)]
pub struct ObjFile {
    pub sections: Vec<ObjSection>,
    pub symbols: Vec<ObjSymbol>,
    /// Relocations keyed by section index they apply to.
    pub relocs: HashMap<usize, Vec<ObjReloc>>,
    pub elf_flags: u32,
}

/// Read a u16 LE from a byte slice.
fn r16(data: &[u8], off: usize) -> u16 {
    u16::from_le_bytes([data[off], data[off + 1]])
}
/// Read a u32 LE.
fn r32(data: &[u8], off: usize) -> u32 {
    u32::from_le_bytes(data[off..off + 4].try_into().unwrap())
}
/// Read a u64 LE.
fn r64(data: &[u8], off: usize) -> u64 {
    u64::from_le_bytes(data[off..off + 8].try_into().unwrap())
}

/// Read a null-terminated string from a byte slice.
fn read_cstr(data: &[u8], off: usize) -> String {
    let end = data[off..].iter().position(|&b| b == 0).unwrap_or(data.len() - off);
    String::from_utf8_lossy(&data[off..off + end]).into_owned()
}

/// Parse an ELF .o file from raw bytes.
pub fn parse_obj(data: &[u8]) -> Result<ObjFile, String> {
    if data.len() < 64 || &data[0..4] != b"\x7fELF" {
        return Err("Not an ELF file".into());
    }
    if data[4] != 2 {
        return Err("Not 64-bit ELF".into());
    }
    if data[5] != 1 {
        return Err("Not little-endian ELF".into());
    }

    let e_type = r16(data, 16);
    if e_type != ET_REL {
        return Err(format!("Not a relocatable file (e_type={})", e_type));
    }
    let e_machine = r16(data, 18);
    if e_machine != EM_RISCV {
        return Err(format!("Not RISC-V (e_machine={})", e_machine));
    }

    let elf_flags = r32(data, 48);
    let e_shoff = r64(data, 40) as usize;
    let e_shentsize = r16(data, 58) as usize;
    let e_shnum = r16(data, 60) as usize;
    let e_shstrndx = r16(data, 62) as usize;

    // Read section headers
    let mut sections = Vec::with_capacity(e_shnum);
    let mut raw_shdrs: Vec<(u32, u32, u64, u64, u64, u64, u32, u32)> = Vec::with_capacity(e_shnum);

    for i in 0..e_shnum {
        let off = e_shoff + i * e_shentsize;
        let sh_name = r32(data, off);
        let sh_type = r32(data, off + 4);
        let sh_flags = r64(data, off + 8);
        let sh_offset = r64(data, off + 24);
        let sh_size = r64(data, off + 32);
        let sh_link = r32(data, off + 40);
        let sh_info = r32(data, off + 44);
        let sh_addralign = r64(data, off + 48);
        let sh_entsize = r64(data, off + 56);

        raw_shdrs.push((sh_name, sh_type, sh_flags, sh_offset, sh_size, sh_addralign, sh_link, sh_info));

        let sec_data = if sh_type == SHT_NOBITS {
            vec![0u8; sh_size as usize]
        } else if sh_size > 0 && (sh_offset as usize) < data.len() {
            let start = sh_offset as usize;
            let end = (start + sh_size as usize).min(data.len());
            data[start..end].to_vec()
        } else {
            Vec::new()
        };

        sections.push(ObjSection {
            name: String::new(), // filled in below
            sh_type,
            sh_flags,
            data: sec_data,
            sh_addralign: sh_addralign.max(1),
            sh_entsize,
            sh_link,
            sh_info,
        });
    }

    // Resolve section names from .shstrtab
    if e_shstrndx < sections.len() {
        let shstrtab = &sections[e_shstrndx].data.clone();
        for (i, sec) in sections.iter_mut().enumerate() {
            let name_off = raw_shdrs[i].0 as usize;
            if name_off < shstrtab.len() {
                sec.name = read_cstr(shstrtab, name_off);
            }
        }
    }

    // Parse symbol table
    let mut symbols = Vec::new();
    let mut symtab_idx = None;
    for (i, sec) in sections.iter().enumerate() {
        if sec.sh_type == SHT_SYMTAB {
            symtab_idx = Some(i);
            break;
        }
    }

    if let Some(si) = symtab_idx {
        let strtab_idx = sections[si].sh_link as usize;
        let strtab = if strtab_idx < sections.len() {
            &sections[strtab_idx].data
        } else {
            &sections[0].data
        };
        let symdata = &sections[si].data;
        let entry_size = sections[si].sh_entsize.max(24) as usize;
        let count = if entry_size > 0 { symdata.len() / entry_size } else { 0 };

        for j in 0..count {
            let off = j * entry_size;
            if off + 24 > symdata.len() {
                break;
            }
            let st_name = r32(symdata, off) as usize;
            let st_info = symdata[off + 4];
            let st_other = symdata[off + 5];
            let st_shndx = r16(symdata, off + 6);
            let st_value = r64(symdata, off + 8);
            let st_size = r64(symdata, off + 16);

            let name = if st_name < strtab.len() {
                read_cstr(strtab, st_name)
            } else {
                String::new()
            };

            symbols.push(ObjSymbol {
                name,
                value: st_value,
                size: st_size,
                binding: st_info >> 4,
                sym_type: st_info & 0xf,
                visibility: st_other & 3,
                section_idx: st_shndx,
            });
        }
    }

    // Parse relocations
    let mut relocs: HashMap<usize, Vec<ObjReloc>> = HashMap::new();
    for sec in &sections {
        if sec.sh_type == SHT_RELA {
            let target_sec = sec.sh_info as usize;
            let rela_data = &sec.data;
            let entry_size = sec.sh_entsize.max(24) as usize;
            let count = if entry_size > 0 { rela_data.len() / entry_size } else { 0 };
            let mut rela_list = Vec::with_capacity(count);

            for j in 0..count {
                let off = j * entry_size;
                if off + 24 > rela_data.len() {
                    break;
                }
                let r_offset = r64(rela_data, off);
                let r_info = r64(rela_data, off + 8);
                let r_addend = r64(rela_data, off + 16) as i64;

                rela_list.push(ObjReloc {
                    offset: r_offset,
                    reloc_type: (r_info & 0xFFFFFFFF) as u32,
                    symbol_idx: (r_info >> 32) as u32,
                    addend: r_addend,
                });
            }
            relocs.entry(target_sec).or_default().extend(rela_list);
        }
    }

    Ok(ObjFile {
        sections,
        symbols,
        relocs,
        elf_flags,
    })
}

/// Parse a .a static archive and return all ELF .o members.
pub fn parse_archive(data: &[u8]) -> Result<Vec<(String, ObjFile)>, String> {
    if data.len() < 8 || &data[0..8] != b"!<arch>\n" {
        return Err("Not an ar archive".into());
    }

    let mut results = Vec::new();
    let mut pos = 8;

    // Long name table (for names > 15 chars, GNU-style //)
    let mut long_names: Vec<u8> = Vec::new();

    while pos + 60 <= data.len() {
        let header = &data[pos..pos + 60];
        // ar header: name[16] mtime[12] uid[6] gid[6] mode[8] size[10] magic[2]
        if &header[58..60] != b"`\n" {
            break;
        }

        let raw_name = std::str::from_utf8(&header[0..16])
            .unwrap_or("")
            .trim_end();
        let size_str = std::str::from_utf8(&header[48..58])
            .unwrap_or("0")
            .trim();
        let size: usize = size_str.parse().unwrap_or(0);

        let member_data = if pos + 60 + size <= data.len() {
            &data[pos + 60..pos + 60 + size]
        } else {
            break;
        };

        // Resolve name
        let name = if raw_name == "//" {
            // This is the long name table
            long_names = member_data.to_vec();
            pos += 60 + size;
            if pos % 2 != 0 {
                pos += 1;
            }
            continue;
        } else if raw_name == "/" {
            // Symbol index - skip
            pos += 60 + size;
            if pos % 2 != 0 {
                pos += 1;
            }
            continue;
        } else if raw_name.starts_with('/') {
            // GNU long name: /offset
            let offset: usize = raw_name[1..].trim_end_matches('/').parse().unwrap_or(0);
            if offset < long_names.len() {
                let end = long_names[offset..]
                    .iter()
                    .position(|&b| b == b'/' || b == b'\n')
                    .unwrap_or(long_names.len() - offset);
                String::from_utf8_lossy(&long_names[offset..offset + end]).into_owned()
            } else {
                raw_name.to_string()
            }
        } else {
            // Short name, strip trailing '/'
            raw_name.trim_end_matches('/').to_string()
        };

        // Try to parse as ELF
        if member_data.len() >= 4 && &member_data[0..4] == b"\x7fELF" {
            match parse_obj(member_data) {
                Ok(obj) => results.push((name, obj)),
                Err(_) => {} // Skip non-RISC-V objects
            }
        }

        pos += 60 + size;
        if pos % 2 != 0 {
            pos += 1;
        }
    }

    Ok(results)
}

/// Read symbol names from an ELF shared library's .dynsym section.
/// Returns the set of defined symbol names.
/// Information about a symbol from a shared library.
#[derive(Clone, Debug)]
pub struct SharedSymInfo {
    pub name: String,
    pub sym_type: u8,
    pub size: u64,
}

pub fn read_shared_lib_symbols(path: &str) -> Result<Vec<SharedSymInfo>, String> {
    let data = std::fs::read(path)
        .map_err(|e| format!("Cannot read {}: {}", path, e))?;

    if data.len() < 64 || &data[0..4] != b"\x7fELF" {
        return Err(format!("{}: not an ELF file", path));
    }

    let e_shoff = r64(&data, 40) as usize;
    let e_shentsize = r16(&data, 58) as usize;
    let e_shnum = r16(&data, 60) as usize;

    // Find .dynsym and .dynstr
    let mut dynsym_off = 0usize;
    let mut dynsym_size = 0usize;
    let mut dynsym_entsize = 24usize;
    let mut dynsym_link = 0u32;

    for i in 0..e_shnum {
        let off = e_shoff + i * e_shentsize;
        if off + 64 > data.len() {
            break;
        }
        let sh_type = r32(&data, off + 4);
        // SHT_DYNSYM = 11
        if sh_type == 11 {
            dynsym_off = r64(&data, off + 24) as usize;
            dynsym_size = r64(&data, off + 32) as usize;
            dynsym_link = r32(&data, off + 40);
            let es = r64(&data, off + 56) as usize;
            if es > 0 {
                dynsym_entsize = es;
            }
            break;
        }
    }

    if dynsym_size == 0 {
        return Ok(Vec::new());
    }

    // Read dynstr
    let dynstr_shdr_off = e_shoff + (dynsym_link as usize) * e_shentsize;
    if dynstr_shdr_off + 64 > data.len() {
        return Ok(Vec::new());
    }
    let dynstr_off = r64(&data, dynstr_shdr_off + 24) as usize;
    let dynstr_size = r64(&data, dynstr_shdr_off + 32) as usize;
    let dynstr_end = (dynstr_off + dynstr_size).min(data.len());
    let dynstr = &data[dynstr_off..dynstr_end];

    let mut symbols = Vec::new();
    let count = dynsym_size / dynsym_entsize;
    for j in 1..count {
        // Skip symbol 0 (null)
        let off = dynsym_off + j * dynsym_entsize;
        if off + 24 > data.len() {
            break;
        }
        let st_name = r32(&data, off) as usize;
        let st_info = data[off + 4];
        let st_shndx = r16(&data, off + 6);
        let st_size = r64(&data, off + 8);

        // Only include defined symbols (not UND)
        if st_shndx == 0 {
            continue;
        }

        let binding = st_info >> 4;
        let sym_type = st_info & 0xf;
        // Only global or weak symbols
        if binding != STB_GLOBAL && binding != STB_WEAK {
            continue;
        }

        if st_name < dynstr.len() {
            let name = read_cstr(dynstr, st_name);
            if !name.is_empty() {
                symbols.push(SharedSymInfo { name, sym_type, size: st_size });
            }
        }
    }

    Ok(symbols)
}
