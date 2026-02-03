//! AArch64 instruction encoder.
//!
//! Encodes AArch64 instructions into 32-bit machine code words.
//! This covers the subset of instructions emitted by our codegen.
//!
//! AArch64 instructions are always 4 bytes (32 bits), little-endian.
//! The encoding format varies by instruction class.

#![allow(dead_code)]

use super::parser::Operand;

/// Result of encoding an instruction.
#[derive(Debug, Clone)]
pub enum EncodeResult {
    /// Successfully encoded as a 4-byte instruction word
    Word(u32),
    /// Instruction needs a relocation to be applied later
    WordWithReloc {
        word: u32,
        reloc: Relocation,
    },
    /// Multiple encoded words (e.g., movz+movk sequence)
    Words(Vec<u32>),
    /// Skip this instruction (e.g., pseudo-instruction handled elsewhere)
    Skip,
}

/// Relocation types for AArch64 ELF
#[derive(Debug, Clone)]
pub enum RelocType {
    /// R_AARCH64_CALL26 - for BL instruction (26-bit PC-relative)
    Call26,
    /// R_AARCH64_JUMP26 - for B instruction (26-bit PC-relative)
    Jump26,
    /// R_AARCH64_ADR_PREL_PG_HI21 - for ADRP (page-relative, bits [32:12])
    AdrpPage21,
    /// R_AARCH64_ADD_ABS_LO12_NC - for ADD :lo12: (low 12 bits)
    AddAbsLo12,
    /// R_AARCH64_LDST8_ABS_LO12_NC
    Ldst8AbsLo12,
    /// R_AARCH64_LDST16_ABS_LO12_NC
    Ldst16AbsLo12,
    /// R_AARCH64_LDST32_ABS_LO12_NC
    Ldst32AbsLo12,
    /// R_AARCH64_LDST64_ABS_LO12_NC
    Ldst64AbsLo12,
    /// R_AARCH64_LDST128_ABS_LO12_NC
    Ldst128AbsLo12,
    /// R_AARCH64_ADR_GOT_PAGE21 - GOT-relative ADRP
    AdrGotPage21,
    /// R_AARCH64_LD64_GOT_LO12_NC - GOT entry LDR
    Ld64GotLo12,
    /// R_AARCH64_TLSLE_ADD_TPREL_HI12
    TlsLeAddTprelHi12,
    /// R_AARCH64_TLSLE_ADD_TPREL_LO12_NC
    TlsLeAddTprelLo12,
    /// R_AARCH64_CONDBR19 - conditional branch, 19-bit offset
    CondBr19,
    /// R_AARCH64_TSTBR14 - test-and-branch, 14-bit offset
    TstBr14,
    /// R_AARCH64_ABS64 - 64-bit absolute
    Abs64,
    /// R_AARCH64_ABS32 - 32-bit absolute
    Abs32,
    /// R_AARCH64_PREL32 - 32-bit PC-relative
    Prel32,
}

impl RelocType {
    /// Get the ELF relocation type number
    pub fn elf_type(&self) -> u32 {
        match self {
            RelocType::Abs64 => 257,           // R_AARCH64_ABS64
            RelocType::Abs32 => 258,           // R_AARCH64_ABS32
            RelocType::Prel32 => 261,          // R_AARCH64_PREL32
            RelocType::Call26 => 283,          // R_AARCH64_CALL26
            RelocType::Jump26 => 282,          // R_AARCH64_JUMP26
            RelocType::AdrpPage21 => 275,      // R_AARCH64_ADR_PREL_PG_HI21
            RelocType::AddAbsLo12 => 277,      // R_AARCH64_ADD_ABS_LO12_NC
            RelocType::Ldst8AbsLo12 => 278,    // R_AARCH64_LDST8_ABS_LO12_NC
            RelocType::Ldst16AbsLo12 => 284,   // R_AARCH64_LDST16_ABS_LO12_NC
            RelocType::Ldst32AbsLo12 => 285,   // R_AARCH64_LDST32_ABS_LO12_NC
            RelocType::Ldst64AbsLo12 => 286,   // R_AARCH64_LDST64_ABS_LO12_NC
            RelocType::Ldst128AbsLo12 => 299,  // R_AARCH64_LDST128_ABS_LO12_NC
            RelocType::AdrGotPage21 => 311,    // R_AARCH64_ADR_GOT_PAGE21
            RelocType::Ld64GotLo12 => 312,     // R_AARCH64_LD64_GOT_LO12_NC
            RelocType::TlsLeAddTprelHi12 => 549, // R_AARCH64_TLSLE_ADD_TPREL_HI12
            RelocType::TlsLeAddTprelLo12 => 550, // R_AARCH64_TLSLE_ADD_TPREL_LO12_NC
            RelocType::CondBr19 => 280,        // R_AARCH64_CONDBR19
            RelocType::TstBr14 => 279,         // R_AARCH64_TSTBR14
        }
    }
}

/// A relocation to be applied.
#[derive(Debug, Clone)]
pub struct Relocation {
    pub reloc_type: RelocType,
    pub symbol: String,
    pub addend: i64,
}

/// Parse a register name to its 5-bit encoding number (0-30, 31 for sp/zr).
pub fn parse_reg_num(name: &str) -> Option<u32> {
    let name = name.to_lowercase();
    match name.as_str() {
        "sp" | "wsp" => Some(31),
        "xzr" | "wzr" => Some(31),
        "lr" => Some(30),
        _ => {
            let prefix = name.chars().next()?;
            match prefix {
                'x' | 'w' | 'd' | 's' | 'q' | 'v' | 'h' | 'b' => {
                    let num: u32 = name[1..].parse().ok()?;
                    if num <= 31 { Some(num) } else { None }
                }
                _ => None,
            }
        }
    }
}

/// Check if a register name is a 64-bit (X) register or SP.
fn is_64bit_reg(name: &str) -> bool {
    let name = name.to_lowercase();
    name.starts_with('x') || name == "sp" || name == "xzr" || name == "lr"
}

/// Check if a register name is a 32-bit (W) register.
fn is_32bit_reg(name: &str) -> bool {
    let name = name.to_lowercase();
    name.starts_with('w') || name == "wsp" || name == "wzr"
}

/// Check if a register is a floating-point/SIMD register.
fn is_fp_reg(name: &str) -> bool {
    let c = name.chars().next().unwrap_or(' ').to_ascii_lowercase();
    matches!(c, 'd' | 's' | 'q' | 'v' | 'h' | 'b')
}

/// Encode a condition code string to 4-bit encoding.
fn encode_cond(cond: &str) -> Option<u32> {
    match cond.to_lowercase().as_str() {
        "eq" => Some(0),
        "ne" => Some(1),
        "cs" | "hs" => Some(2),
        "cc" | "lo" => Some(3),
        "mi" => Some(4),
        "pl" => Some(5),
        "vs" => Some(6),
        "vc" => Some(7),
        "hi" => Some(8),
        "ls" => Some(9),
        "ge" => Some(10),
        "lt" => Some(11),
        "gt" => Some(12),
        "le" => Some(13),
        "al" => Some(14),
        "nv" => Some(15),
        _ => None,
    }
}

/// Encode an AArch64 instruction from its mnemonic and parsed operands.
pub fn encode_instruction(mnemonic: &str, operands: &[Operand], raw_operands: &str) -> Result<EncodeResult, String> {
    let mn = mnemonic.to_lowercase();

    // Handle condition-code suffixed branches: b.eq, b.ne, b.lt, etc.
    if mn.starts_with("b.") {
        let cond = &mn[2..];
        return encode_cond_branch(cond, operands);
    }

    match mn.as_str() {
        // Data processing - register
        "mov" => encode_mov(operands),
        "movz" => encode_movz(operands),
        "movk" => encode_movk(operands),
        "movn" => encode_movn(operands),
        "add" => encode_add_sub(operands, false, false),
        "adds" => encode_add_sub(operands, false, true),
        "sub" => encode_add_sub(operands, true, false),
        "subs" => encode_add_sub(operands, true, true),
        "and" => encode_logical(operands, 0b00),
        "orr" => encode_logical(operands, 0b01),
        "eor" => encode_logical(operands, 0b10),
        "ands" => encode_logical(operands, 0b11),
        "mul" => encode_mul(operands),
        "madd" => encode_madd(operands),
        "msub" => encode_msub(operands),
        "udiv" => encode_div(operands, true),
        "sdiv" => encode_div(operands, false),
        "umulh" => encode_umulh(operands),
        "smulh" => encode_smulh(operands),
        "neg" => encode_neg(operands),
        "negs" => encode_negs(operands),
        "mvn" => encode_mvn(operands),
        "adc" => encode_adc(operands, false),
        "adcs" => encode_adc(operands, true),
        "sbc" => encode_sbc(operands, false),
        "sbcs" => encode_sbc(operands, true),

        // Shifts
        "lsl" => encode_shift(operands, 0b00),
        "lsr" => encode_shift(operands, 0b01),
        "asr" => encode_shift(operands, 0b10),
        "ror" => encode_shift(operands, 0b11),

        // Extensions
        "sxtw" => encode_sxtw(operands),
        "sxth" => encode_sxth(operands),
        "sxtb" => encode_sxtb(operands),
        "uxtw" => encode_uxtw(operands),
        "uxth" => encode_uxth(operands),
        "uxtb" => encode_uxtb(operands),

        // Compare
        "cmp" => encode_cmp(operands),
        "cmn" => encode_cmn(operands),
        "tst" => encode_tst(operands),
        "ccmp" => encode_ccmp(operands),

        // Conditional select
        "csel" => encode_csel(operands),
        "csinc" => encode_csinc(operands),
        "csinv" => encode_csinv(operands),
        "csneg" => encode_csneg(operands),
        "cset" => encode_cset(operands),
        "csetm" => encode_csetm(operands),

        // Branches
        "b" => encode_branch(operands),
        "bl" => encode_bl(operands),
        "br" => encode_br(operands),
        "blr" => encode_blr(operands),
        "ret" => encode_ret(operands),
        "cbz" => encode_cbz(operands, false),
        "cbnz" => encode_cbz(operands, true),
        "tbz" => encode_tbz(operands, false),
        "tbnz" => encode_tbz(operands, true),

        // Loads/stores - size determined from register width
        "ldr" => encode_ldr_str_auto(operands, true),
        "str" => encode_ldr_str_auto(operands, false),
        "ldrb" => encode_ldr_str(operands, true, 0b00, false), // byte load
        "strb" => encode_ldr_str(operands, false, 0b00, false),
        "ldrh" => encode_ldr_str(operands, true, 0b01, false), // halfword load
        "strh" => encode_ldr_str(operands, false, 0b01, false),
        "ldrw" | "ldrsw" => encode_ldrsw(operands),
        "ldrsb" => encode_ldrs(operands, 0b00),
        "ldrsh" => encode_ldrs(operands, 0b01),
        "ldp" => encode_ldp_stp(operands, true),
        "stp" => encode_ldp_stp(operands, false),
        "ldxr" => encode_ldxr_stxr(operands, true),
        "stxr" => encode_ldxr_stxr(operands, false),
        "ldaxr" => encode_ldaxr_stlxr(operands, true),
        "stlxr" => encode_ldaxr_stlxr(operands, false),
        "ldar" => encode_ldar_stlr(operands, true),
        "stlr" => encode_ldar_stlr(operands, false),

        // Address computation
        "adrp" => encode_adrp(operands),
        "adr" => encode_adr(operands),

        // Floating point
        "fmov" => encode_fmov(operands),
        "fadd" => encode_fp_arith(operands, 0b0010),
        "fsub" => encode_fp_arith(operands, 0b0011),
        "fmul" => encode_fp_arith(operands, 0b0000),
        "fdiv" => encode_fp_arith(operands, 0b0001),
        "fneg" => encode_fneg(operands),
        "fabs" => encode_fabs(operands),
        "fsqrt" => encode_fsqrt(operands),
        "fcmp" => encode_fcmp(operands),
        "fcvtzs" => encode_fcvt(operands, true),
        "fcvtzu" => encode_fcvt(operands, false),
        "ucvtf" => encode_ucvtf(operands),
        "scvtf" => encode_scvtf(operands),
        "fcvt" => encode_fcvt_precision(operands),

        // NEON/SIMD
        "cnt" => encode_cnt(operands),
        "uaddlv" => encode_uaddlv(operands),

        // System
        "nop" => Ok(EncodeResult::Word(0xd503201f)),
        "dmb" => encode_dmb(operands),
        "dsb" => encode_dsb(operands),
        "isb" => Ok(EncodeResult::Word(0xd5033fdf)),
        "mrs" => encode_mrs(operands),
        "msr" => encode_msr(operands),
        "svc" => encode_svc(operands),
        "brk" => encode_brk(operands),

        // Bit manipulation
        "clz" => encode_clz(operands),
        "cls" => encode_cls(operands),
        "rbit" => encode_rbit(operands),
        "rev" => encode_rev(operands),
        "rev16" => encode_rev16(operands),
        "rev32" => encode_rev32(operands),

        // CRC32
        "crc32b" | "crc32h" | "crc32w" | "crc32x"
        | "crc32cb" | "crc32ch" | "crc32cw" | "crc32cx" => encode_crc32(mnemonic, operands),

        // Prefetch
        "prfm" => Ok(EncodeResult::Word(0xd503201f)), // Encode as NOP for now

        _ => {
            // TODO: handle remaining instructions
            Err(format!("unsupported instruction: {} {}", mnemonic, raw_operands))
        }
    }
}

// ── Encoding helpers ──────────────────────────────────────────────────────

fn get_reg(operands: &[Operand], idx: usize) -> Result<(u32, bool), String> {
    match operands.get(idx) {
        Some(Operand::Reg(name)) => {
            let num = parse_reg_num(name)
                .ok_or_else(|| format!("invalid register: {}", name))?;
            let is_64 = is_64bit_reg(name);
            Ok((num, is_64))
        }
        other => Err(format!("expected register at operand {}, got {:?}", idx, other)),
    }
}

fn get_imm(operands: &[Operand], idx: usize) -> Result<i64, String> {
    match operands.get(idx) {
        Some(Operand::Imm(v)) => Ok(*v),
        other => Err(format!("expected immediate at operand {}, got {:?}", idx, other)),
    }
}

fn get_symbol(operands: &[Operand], idx: usize) -> Result<(String, i64), String> {
    match operands.get(idx) {
        Some(Operand::Symbol(s)) => Ok((s.clone(), 0)),
        Some(Operand::Label(s)) => Ok((s.clone(), 0)),
        Some(Operand::SymbolOffset(s, off)) => Ok((s.clone(), *off)),
        Some(Operand::Modifier { symbol, .. }) => Ok((symbol.clone(), 0)),
        Some(Operand::ModifierOffset { symbol, offset, .. }) => Ok((symbol.clone(), *offset)),
        other => Err(format!("expected symbol at operand {}, got {:?}", idx, other)),
    }
}

fn sf_bit(is_64: bool) -> u32 {
    if is_64 { 1 } else { 0 }
}

// ── MOV ──────────────────────────────────────────────────────────────────

fn encode_mov(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("mov requires 2 operands".to_string());
    }

    // mov Xd, #imm -> movz or movn
    if let Some(Operand::Imm(imm)) = operands.get(1) {
        let (rd, is_64) = get_reg(operands, 0)?;
        let imm = *imm;

        // Check if it can be a simple MOVZ
        if imm >= 0 && imm <= 0xFFFF {
            let sf = sf_bit(is_64);
            let word = (sf << 31) | (0b10100101 << 23) | ((imm as u32 & 0xFFFF) << 5) | rd;
            return Ok(EncodeResult::Word(word));
        }

        // Negative: try MOVN
        if imm < 0 {
            let not_imm = !imm;
            if not_imm >= 0 && not_imm <= 0xFFFF {
                let sf = sf_bit(is_64);
                let word = (sf << 31) | (0b00100101 << 23) | ((not_imm as u32 & 0xFFFF) << 5) | rd;
                return Ok(EncodeResult::Word(word));
            }
        }

        // Need movz + movk sequence for large immediates
        return encode_mov_wide_imm(rd, is_64, imm as u64);
    }

    // mov Xd, Xm -> ORR Xd, XZR, Xm
    if let (Some(Operand::Reg(rd_name)), Some(Operand::Reg(rm_name))) = (operands.get(0), operands.get(1)) {
        let rd = parse_reg_num(rd_name).ok_or("invalid rd")?;
        let rm = parse_reg_num(rm_name).ok_or("invalid rm")?;
        let is_64 = is_64bit_reg(rd_name);

        // Check for MOV to/from SP: uses ADD Xd, Xn, #0
        if rd_name.to_lowercase() == "sp" || rm_name.to_lowercase() == "sp" {
            let sf = sf_bit(is_64);
            // ADD Xd, Xn, #0: sf 0 0 10001 00 imm12=0 Rn Rd
            let word = (sf << 31) | (0b10001 << 24) | (0 << 22) | (0 << 10) | (rm << 5) | rd;
            return Ok(EncodeResult::Word(word));
        }

        let sf = sf_bit(is_64);
        // ORR Rd, XZR, Rm: sf 01 01010 00 0 Rm 000000 11111 Rd
        let word = (sf << 31) | (0b01 << 29) | (0b01010 << 24) | (0 << 21) | (rm << 16) | (0b11111 << 5) | rd;
        return Ok(EncodeResult::Word(word));
    }

    Err(format!("unsupported mov operands: {:?}", operands))
}

fn encode_mov_wide_imm(rd: u32, is_64: bool, imm: u64) -> Result<EncodeResult, String> {
    let sf = sf_bit(is_64);
    let mut words = Vec::new();
    let max_hw = if is_64 { 4 } else { 2 };
    let mut first = true;

    for hw in 0..max_hw {
        let chunk = ((imm >> (hw * 16)) & 0xFFFF) as u32;
        if chunk != 0 || (hw == 0 && imm == 0) {
            if first {
                // MOVZ
                let word = (sf << 31) | (0b10100101 << 23) | (hw << 21) | (chunk << 5) | rd;
                words.push(word);
                first = false;
            } else {
                // MOVK
                let word = (sf << 31) | (0b11100101 << 23) | (hw << 21) | (chunk << 5) | rd;
                words.push(word);
            }
        }
    }

    if words.is_empty() {
        // imm is 0
        let word = (sf << 31) | (0b10100101 << 23) | rd;
        words.push(word);
    }

    if words.len() == 1 {
        Ok(EncodeResult::Word(words[0]))
    } else {
        Ok(EncodeResult::Words(words))
    }
}

fn encode_movz(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let imm = get_imm(operands, 1)?;
    let sf = sf_bit(is_64);

    // Check for lsl #N shift
    let hw = if operands.len() > 2 {
        if let Some(Operand::Shift { kind, amount }) = operands.get(2) {
            if kind == "lsl" {
                *amount / 16
            } else {
                0
            }
        } else {
            0
        }
    } else {
        0
    };

    let word = (sf << 31) | (0b10100101 << 23) | (hw << 21) | (((imm as u32) & 0xFFFF) << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_movk(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let imm = get_imm(operands, 1)?;
    let sf = sf_bit(is_64);

    let hw = if operands.len() > 2 {
        if let Some(Operand::Shift { kind, amount }) = operands.get(2) {
            if kind == "lsl" {
                *amount / 16
            } else {
                0
            }
        } else {
            0
        }
    } else {
        0
    };

    let word = (sf << 31) | (0b11100101 << 23) | (hw << 21) | (((imm as u32) & 0xFFFF) << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_movn(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let imm = get_imm(operands, 1)?;
    let sf = sf_bit(is_64);

    let hw = if operands.len() > 2 {
        if let Some(Operand::Shift { kind, amount }) = operands.get(2) {
            if kind == "lsl" {
                *amount / 16
            } else {
                0
            }
        } else {
            0
        }
    } else {
        0
    };

    let word = (sf << 31) | (0b00100101 << 23) | (hw << 21) | (((imm as u32) & 0xFFFF) << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── ADD/SUB ──────────────────────────────────────────────────────────────

fn encode_add_sub(operands: &[Operand], is_sub: bool, set_flags: bool) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err(format!("add/sub requires 3 operands, got {}", operands.len()));
    }

    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let op = if is_sub { 1u32 } else { 0u32 };
    let s_bit = if set_flags { 1u32 } else { 0u32 };

    // ADD Rd, Rn, #imm
    if let Some(Operand::Imm(imm)) = operands.get(2) {
        let imm12 = (*imm as u32) & 0xFFF;
        // Check for lsl #12 shift
        let sh = if operands.len() > 3 {
            if let Some(Operand::Shift { kind, amount }) = operands.get(3) {
                if kind == "lsl" && *amount == 12 { 1u32 } else { 0u32 }
            } else { 0u32 }
        } else { 0u32 };
        let word = (sf << 31) | (op << 30) | (s_bit << 29) | (0b10001 << 24) | (sh << 22) | (imm12 << 10) | (rn << 5) | rd;
        return Ok(EncodeResult::Word(word));
    }

    // ADD Rd, Rn, :lo12:symbol
    if let Some(Operand::Modifier { kind, symbol }) = operands.get(2) {
        if kind == "lo12" {
            let word = (sf << 31) | (op << 30) | (s_bit << 29) | (0b10001 << 24) | (0 << 22) | (0 << 10) | (rn << 5) | rd;
            return Ok(EncodeResult::WordWithReloc {
                word,
                reloc: Relocation {
                    reloc_type: RelocType::AddAbsLo12,
                    symbol: symbol.clone(),
                    addend: 0,
                },
            });
        }
        if kind == "tprel_lo12_nc" {
            let word = (sf << 31) | (op << 30) | (s_bit << 29) | (0b10001 << 24) | (0 << 22) | (0 << 10) | (rn << 5) | rd;
            return Ok(EncodeResult::WordWithReloc {
                word,
                reloc: Relocation {
                    reloc_type: RelocType::TlsLeAddTprelLo12,
                    symbol: symbol.clone(),
                    addend: 0,
                },
            });
        }
    }
    if let Some(Operand::ModifierOffset { kind, symbol, offset }) = operands.get(2) {
        if kind == "lo12" {
            let word = (sf << 31) | (op << 30) | (s_bit << 29) | (0b10001 << 24) | (0 << 22) | (0 << 10) | (rn << 5) | rd;
            return Ok(EncodeResult::WordWithReloc {
                word,
                reloc: Relocation {
                    reloc_type: RelocType::AddAbsLo12,
                    symbol: symbol.clone(),
                    addend: *offset,
                },
            });
        }
    }

    // ADD Rd, Rn, Rm
    if let Some(Operand::Reg(rm_name)) = operands.get(2) {
        let rm = parse_reg_num(rm_name).ok_or("invalid rm")?;

        // Check for shifted register: add Xd, Xn, Xm, lsl #N
        let (shift_type, shift_amount) = if let Some(Operand::Shift { kind, amount }) = operands.get(3) {
            let st = match kind.as_str() {
                "lsl" => 0b00u32,
                "lsr" => 0b01,
                "asr" => 0b10,
                _ => 0b00,
            };
            (st, *amount)
        } else {
            (0, 0)
        };

        let word = (sf << 31) | (op << 30) | (s_bit << 29) | (0b01011 << 24) | (shift_type << 22)
            | (0 << 21) | (rm << 16) | ((shift_amount as u32 & 0x3F) << 10) | (rn << 5) | rd;
        return Ok(EncodeResult::Word(word));
    }

    Err(format!("unsupported add/sub operands: {:?}", operands))
}

// ── Logical ──────────────────────────────────────────────────────────────

fn encode_logical(operands: &[Operand], opc: u32) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("logical op requires 3 operands".to_string());
    }

    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);

    // AND/ORR/EOR Rd, Rn, #imm (bitmask immediate)
    if let Some(Operand::Imm(imm)) = operands.get(2) {
        if let Some((n, immr, imms)) = encode_bitmask_imm(*imm as u64, is_64) {
            let word = (sf << 31) | (opc << 29) | (0b100100 << 23) | (n << 22) | (immr << 16) | (imms << 10) | (rn << 5) | rd;
            return Ok(EncodeResult::Word(word));
        }
        return Err(format!("cannot encode bitmask immediate: 0x{:x}", imm));
    }

    // AND/ORR/EOR Rd, Rn, Rm [, shift #amount]
    if let Some(Operand::Reg(rm_name)) = operands.get(2) {
        let rm = parse_reg_num(rm_name).ok_or("invalid rm")?;

        let (shift_type, shift_amount) = if let Some(Operand::Shift { kind, amount }) = operands.get(3) {
            let st = match kind.as_str() {
                "lsl" => 0b00u32,
                "lsr" => 0b01,
                "asr" => 0b10,
                "ror" => 0b11,
                _ => 0b00,
            };
            (st, *amount)
        } else {
            (0, 0)
        };

        let word = (sf << 31) | (opc << 29) | (0b01010 << 24) | (shift_type << 22) | (0 << 21)
            | (rm << 16) | ((shift_amount as u32 & 0x3F) << 10) | (rn << 5) | rd;
        return Ok(EncodeResult::Word(word));
    }

    Err("unsupported logical operands".to_string())
}

/// Encode a bitmask immediate for AArch64.
/// Returns (N, immr, imms) if the value is a valid bitmask immediate.
fn encode_bitmask_imm(val: u64, is_64: bool) -> Option<(u32, u32, u32)> {
    if val == 0 || (!is_64 && val == 0xFFFFFFFF) || (is_64 && val == u64::MAX) {
        return None; // Not a valid bitmask immediate
    }

    let width = if is_64 { 64 } else { 32 };
    let val = if !is_64 { val & 0xFFFFFFFF } else { val };

    // Try each possible element size: 2, 4, 8, 16, 32, 64
    for size in [2u32, 4, 8, 16, 32, 64] {
        if size > width {
            continue;
        }

        let mask = if size == 64 { u64::MAX } else { (1u64 << size) - 1 };
        let elem = val & mask;

        // Check that the pattern repeats
        let mut repeats = true;
        let mut pos = size;
        while pos < width {
            if ((val >> pos) & mask) != elem {
                repeats = false;
                break;
            }
            pos += size;
        }
        if !repeats {
            continue;
        }

        // Check that elem is a contiguous run of 1s (possibly rotated)
        let ones = elem.count_ones();
        if ones == 0 || ones == size {
            continue; // All zeros or all ones in element
        }

        // Find rotation: rotate elem right until the least significant bit is 1
        // and the run of 1s starts at bit 0
        let mut rotated = elem;
        let mut rotation = 0u32;
        // Rotate right until we find a 0->1 transition at bit 0
        for r in 0..size {
            let rot = ((elem >> r) | (elem << (size - r))) & mask;
            // Check if this is a contiguous run from bit 0
            let run = rot.trailing_ones();
            if run == ones {
                rotated = rot;
                rotation = r;
                break;
            }
        }

        // Now rotated has `ones` consecutive 1s starting at bit 0
        let _ = rotated;

        // Encode the fields
        let n = if size == 64 { 1u32 } else { 0u32 };
        let immr = rotation;
        let imms = match size {
            2 => 0b111100 | (ones - 1),
            4 => 0b111000 | (ones - 1),
            8 => 0b110000 | (ones - 1),
            16 => 0b100000 | (ones - 1),
            32 => 0b000000 | (ones - 1),
            64 => ones - 1,
            _ => unreachable!(),
        };

        return Some((n, immr, imms));
    }

    None
}

// ── MUL/DIV ──────────────────────────────────────────────────────────────

fn encode_mul(operands: &[Operand]) -> Result<EncodeResult, String> {
    // MUL Rd, Rn, Rm is MADD Rd, Rn, Rm, XZR
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (0b0011011000 << 21) | (rm << 16) | (0b11111 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_madd(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let (ra, _) = get_reg(operands, 3)?;
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (0b0011011000 << 21) | (rm << 16) | (0 << 15) | (ra << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_msub(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let (ra, _) = get_reg(operands, 3)?;
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (0b0011011000 << 21) | (rm << 16) | (1 << 15) | (ra << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_div(operands: &[Operand], unsigned: bool) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let sf = sf_bit(is_64);
    let o1 = if unsigned { 0u32 } else { 1u32 };
    // Data-processing (2 source): sf 0 S=0 11010110 Rm 00001 o1 Rn Rd
    let word = (sf << 31) | (0b0011010110 << 21) | (rm << 16)
        | (0b00001 << 11) | (o1 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_umulh(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    // UMULH: 1 00 11011 1 10 Rm 0 11111 Rn Rd
    let word = (1u32 << 31) | (0b0011011110 << 21) | (rm << 16) | (0b011111 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_smulh(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    // SMULH: 1 00 11011 0 10 Rm 0 11111 Rn Rd
    let word = (1u32 << 31) | (0b0011011010 << 21) | (rm << 16) | (0b011111 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_neg(operands: &[Operand]) -> Result<EncodeResult, String> {
    // NEG Rd, Rm -> SUB Rd, XZR, Rm
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rm, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (1 << 30) | (0b01011 << 24) | (0 << 22) | (0 << 21)
        | (rm << 16) | (0 << 10) | (0b11111 << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_negs(operands: &[Operand]) -> Result<EncodeResult, String> {
    // NEGS Rd, Rm -> SUBS Rd, XZR, Rm
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rm, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (1 << 30) | (1 << 29) | (0b01011 << 24) | (0 << 22) | (0 << 21)
        | (rm << 16) | (0 << 10) | (0b11111 << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_mvn(operands: &[Operand]) -> Result<EncodeResult, String> {
    // MVN Rd, Rm -> ORN Rd, XZR, Rm
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rm, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (0b01 << 29) | (0b01010 << 24) | (0 << 22) | (1 << 21)
        | (rm << 16) | (0 << 10) | (0b11111 << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_adc(operands: &[Operand], set_flags: bool) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let sf = sf_bit(is_64);
    let s = if set_flags { 1u32 } else { 0 };
    let word = (sf << 31) | (0 << 30) | (s << 29) | (0b11010000 << 21) | (rm << 16) | (0 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_sbc(operands: &[Operand], set_flags: bool) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let sf = sf_bit(is_64);
    let s = if set_flags { 1u32 } else { 0 };
    let word = (sf << 31) | (1 << 30) | (s << 29) | (0b11010000 << 21) | (rm << 16) | (0 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── Shifts ───────────────────────────────────────────────────────────────

fn encode_shift(operands: &[Operand], shift_type: u32) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;

    // LSL/LSR/ASR Rd, Rn, #imm (immediate form -> UBFM/SBFM)
    if let Some(Operand::Imm(imm)) = operands.get(2) {
        let sf = sf_bit(is_64);
        let imm = *imm as u32;
        let width = if is_64 { 64 } else { 32 };
        let n = if is_64 { 1u32 } else { 0u32 };

        match shift_type {
            0b00 => {
                // LSL #imm -> UBFM Rd, Rn, #(-imm mod width), #(width-1-imm)
                let immr = (width - imm) % width;
                let imms = width - 1 - imm;
                let word = (sf << 31) | (0b10 << 29) | (0b100110 << 23) | (n << 22) | (immr << 16) | (imms << 10) | (rn << 5) | rd;
                return Ok(EncodeResult::Word(word));
            }
            0b01 => {
                // LSR #imm -> UBFM Rd, Rn, #imm, #(width-1)
                let immr = imm;
                let imms = width - 1;
                let word = (sf << 31) | (0b10 << 29) | (0b100110 << 23) | (n << 22) | (immr << 16) | (imms << 10) | (rn << 5) | rd;
                return Ok(EncodeResult::Word(word));
            }
            0b10 => {
                // ASR #imm -> SBFM Rd, Rn, #imm, #(width-1)
                let immr = imm;
                let imms = width - 1;
                let word = (sf << 31) | (0b00 << 29) | (0b100110 << 23) | (n << 22) | (immr << 16) | (imms << 10) | (rn << 5) | rd;
                return Ok(EncodeResult::Word(word));
            }
            _ => {}
        }
    }

    // LSL/LSR/ASR Rd, Rn, Rm (register form)
    if let Some(Operand::Reg(rm_name)) = operands.get(2) {
        let rm = parse_reg_num(rm_name).ok_or("invalid rm")?;
        let sf = sf_bit(is_64);
        // Data-processing (2 source): sf 0 S=0 11010110 Rm 0010 op2 Rn Rd
        let op2 = shift_type; // 00=LSL, 01=LSR, 10=ASR, 11=ROR
        let word = (sf << 31) | (0b0011010110 << 21) | (rm << 16) | (0b0010 << 12) | (op2 << 10) | (rn << 5) | rd;
        return Ok(EncodeResult::Word(word));
    }

    Err("unsupported shift operands".to_string())
}

// ── Extensions ───────────────────────────────────────────────────────────

fn encode_sxtw(operands: &[Operand]) -> Result<EncodeResult, String> {
    // SXTW Xd, Wn -> SBFM Xd, Xn, #0, #31
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let word = (1u32 << 31) | (0b00 << 29) | (0b100110 << 23) | (1 << 22) | (0 << 16) | (31 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_sxth(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let n = if is_64 { 1u32 } else { 0 };
    let word = (sf << 31) | (0b00 << 29) | (0b100110 << 23) | (n << 22) | (0 << 16) | (15 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_sxtb(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let n = if is_64 { 1u32 } else { 0 };
    let word = (sf << 31) | (0b00 << 29) | (0b100110 << 23) | (n << 22) | (0 << 16) | (7 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_uxtw(operands: &[Operand]) -> Result<EncodeResult, String> {
    // UXTW is MOV Wd, Wn (the upper 32 bits are zeroed)
    // Or: UBFM Xd, Xn, #0, #31
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    // Use 32-bit ORR (MOV alias)
    let word = (0b001010100 << 23) | (rn << 16) | (0b11111 << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_uxth(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let n = if is_64 { 1u32 } else { 0 };
    let word = (sf << 31) | (0b10 << 29) | (0b100110 << 23) | (n << 22) | (0 << 16) | (15 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_uxtb(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let n = if is_64 { 1u32 } else { 0 };
    let word = (sf << 31) | (0b10 << 29) | (0b100110 << 23) | (n << 22) | (0 << 16) | (7 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── Compare ──────────────────────────────────────────────────────────────

fn encode_cmp(operands: &[Operand]) -> Result<EncodeResult, String> {
    // CMP Rn, op -> SUBS XZR, Rn, op
    let mut new_ops = vec![Operand::Reg("xzr".to_string())];
    new_ops.extend(operands.iter().cloned());
    // Determine if 32-bit or 64-bit from the first operand
    let is_32 = if let Some(Operand::Reg(r)) = operands.get(0) {
        is_32bit_reg(r)
    } else {
        false
    };
    if is_32 {
        new_ops[0] = Operand::Reg("wzr".to_string());
    }
    encode_add_sub(&new_ops, true, true)
}

fn encode_cmn(operands: &[Operand]) -> Result<EncodeResult, String> {
    // CMN Rn, op -> ADDS XZR, Rn, op
    let mut new_ops = vec![Operand::Reg("xzr".to_string())];
    new_ops.extend(operands.iter().cloned());
    let is_32 = if let Some(Operand::Reg(r)) = operands.get(0) {
        is_32bit_reg(r)
    } else {
        false
    };
    if is_32 {
        new_ops[0] = Operand::Reg("wzr".to_string());
    }
    encode_add_sub(&new_ops, false, true)
}

fn encode_tst(operands: &[Operand]) -> Result<EncodeResult, String> {
    // TST Rn, op -> ANDS XZR, Rn, op
    let mut new_ops = vec![Operand::Reg("xzr".to_string())];
    new_ops.extend(operands.iter().cloned());
    let is_32 = if let Some(Operand::Reg(r)) = operands.get(0) {
        is_32bit_reg(r)
    } else {
        false
    };
    if is_32 {
        new_ops[0] = Operand::Reg("wzr".to_string());
    }
    encode_logical(&new_ops, 0b11)
}

fn encode_ccmp(operands: &[Operand]) -> Result<EncodeResult, String> {
    // CCMP Rn, #imm5, #nzcv, cond
    let (rn, is_64) = get_reg(operands, 0)?;
    let sf = sf_bit(is_64);

    if let (Some(Operand::Imm(imm5)), Some(Operand::Imm(nzcv)), Some(Operand::Cond(cond))) =
        (operands.get(1), operands.get(2), operands.get(3))
    {
        let cond_val = encode_cond(cond).ok_or("invalid condition")?;
        let word = (sf << 31) | (1 << 30) | (1 << 29) | (0b11010010 << 21)
            | ((*imm5 as u32 & 0x1F) << 16) | (cond_val << 12) | (1 << 11) | (0 << 10) | (rn << 5) | (*nzcv as u32 & 0xF);
        return Ok(EncodeResult::Word(word));
    }

    // CCMP Rn, Rm, #nzcv, cond
    if let (Some(Operand::Reg(rm_name)), Some(Operand::Imm(nzcv)), Some(Operand::Cond(cond))) =
        (operands.get(1), operands.get(2), operands.get(3))
    {
        let rm = parse_reg_num(rm_name).ok_or("invalid rm")?;
        let cond_val = encode_cond(cond).ok_or("invalid condition")?;
        let word = (sf << 31) | (1 << 30) | (1 << 29) | (0b11010010 << 21)
            | (rm << 16) | (cond_val << 12) | (0 << 11) | (0 << 10) | (rn << 5) | (*nzcv as u32 & 0xF);
        return Ok(EncodeResult::Word(word));
    }

    Err("unsupported ccmp operands".to_string())
}

// ── Conditional select ───────────────────────────────────────────────────

fn encode_csel(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let cond = match operands.get(3) {
        Some(Operand::Cond(c)) => encode_cond(c).ok_or("invalid cond")?,
        _ => return Err("csel requires condition".to_string()),
    };
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (0 << 30) | (0 << 29) | (0b11010100 << 21)
        | (rm << 16) | (cond << 12) | (0b00 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_csinc(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let cond = match operands.get(3) {
        Some(Operand::Cond(c)) => encode_cond(c).ok_or("invalid cond")?,
        _ => return Err("csinc requires condition".to_string()),
    };
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (0 << 30) | (0 << 29) | (0b11010100 << 21)
        | (rm << 16) | (cond << 12) | (0b01 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_csinv(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let cond = match operands.get(3) {
        Some(Operand::Cond(c)) => encode_cond(c).ok_or("invalid cond")?,
        _ => return Err("csinv requires condition".to_string()),
    };
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (1 << 30) | (0 << 29) | (0b11010100 << 21)
        | (rm << 16) | (cond << 12) | (0b00 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_csneg(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;
    let cond = match operands.get(3) {
        Some(Operand::Cond(c)) => encode_cond(c).ok_or("invalid cond")?,
        _ => return Err("csneg requires condition".to_string()),
    };
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (1 << 30) | (0 << 29) | (0b11010100 << 21)
        | (rm << 16) | (cond << 12) | (0b01 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_cset(operands: &[Operand]) -> Result<EncodeResult, String> {
    // CSET Rd, cond -> CSINC Rd, XZR, XZR, invert(cond)
    let (rd, is_64) = get_reg(operands, 0)?;
    let cond = match operands.get(1) {
        Some(Operand::Cond(c)) => encode_cond(c).ok_or("invalid cond")?,
        _ => return Err("cset requires condition".to_string()),
    };
    let sf = sf_bit(is_64);
    let inv_cond = cond ^ 1; // invert least significant bit
    let word = (sf << 31) | (0 << 30) | (0 << 29) | (0b11010100 << 21)
        | (0b11111 << 16) | (inv_cond << 12) | (0b01 << 10) | (0b11111 << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_csetm(operands: &[Operand]) -> Result<EncodeResult, String> {
    // CSETM Rd, cond -> CSINV Rd, XZR, XZR, invert(cond)
    let (rd, is_64) = get_reg(operands, 0)?;
    let cond = match operands.get(1) {
        Some(Operand::Cond(c)) => encode_cond(c).ok_or("invalid cond")?,
        _ => return Err("csetm requires condition".to_string()),
    };
    let sf = sf_bit(is_64);
    let inv_cond = cond ^ 1;
    let word = (sf << 31) | (1 << 30) | (0 << 29) | (0b11010100 << 21)
        | (0b11111 << 16) | (inv_cond << 12) | (0b00 << 10) | (0b11111 << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── Branches ─────────────────────────────────────────────────────────────

fn encode_branch(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (sym, addend) = get_symbol(operands, 0)?;
    // B: 000101 imm26 (filled by linker/assembler)
    Ok(EncodeResult::WordWithReloc {
        word: 0b000101 << 26,
        reloc: Relocation {
            reloc_type: RelocType::Jump26,
            symbol: sym,
            addend,
        },
    })
}

fn encode_bl(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (sym, addend) = get_symbol(operands, 0)?;
    // BL: 100101 imm26
    Ok(EncodeResult::WordWithReloc {
        word: 0b100101 << 26,
        reloc: Relocation {
            reloc_type: RelocType::Call26,
            symbol: sym,
            addend,
        },
    })
}

fn encode_cond_branch(cond: &str, operands: &[Operand]) -> Result<EncodeResult, String> {
    let cond_val = encode_cond(cond).ok_or_else(|| format!("unknown condition: {}", cond))?;
    let (sym, addend) = get_symbol(operands, 0)?;
    // B.cond: 01010100 imm19 0 cond
    let word = (0b01010100 << 24) | cond_val;
    Ok(EncodeResult::WordWithReloc {
        word,
        reloc: Relocation {
            reloc_type: RelocType::CondBr19,
            symbol: sym,
            addend,
        },
    })
}

fn encode_br(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rn, _) = get_reg(operands, 0)?;
    // BR: 1101011 0000 11111 000000 Rn 00000
    let word = 0xd61f0000 | (rn << 5);
    Ok(EncodeResult::Word(word))
}

fn encode_blr(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rn, _) = get_reg(operands, 0)?;
    // BLR: 1101011 0001 11111 000000 Rn 00000
    let word = 0xd63f0000 | (rn << 5);
    Ok(EncodeResult::Word(word))
}

fn encode_ret(operands: &[Operand]) -> Result<EncodeResult, String> {
    let rn = if operands.is_empty() {
        30 // default to x30 (LR)
    } else {
        get_reg(operands, 0)?.0
    };
    // RET: 1101011 0010 11111 000000 Rn 00000
    let word = 0xd65f0000 | (rn << 5);
    Ok(EncodeResult::Word(word))
}

fn encode_cbz(operands: &[Operand], is_nz: bool) -> Result<EncodeResult, String> {
    let (rt, is_64) = get_reg(operands, 0)?;
    let (sym, addend) = get_symbol(operands, 1)?;
    let sf = sf_bit(is_64);
    let op = if is_nz { 1u32 } else { 0u32 };
    // CBZ/CBNZ: sf 011010 op imm19 Rt
    let word = (sf << 31) | (0b011010 << 25) | (op << 24) | rt;
    Ok(EncodeResult::WordWithReloc {
        word,
        reloc: Relocation {
            reloc_type: RelocType::CondBr19,
            symbol: sym,
            addend,
        },
    })
}

fn encode_tbz(operands: &[Operand], is_nz: bool) -> Result<EncodeResult, String> {
    let (rt, _) = get_reg(operands, 0)?;
    let bit = get_imm(operands, 1)?;
    let (sym, addend) = get_symbol(operands, 2)?;
    let b5 = ((bit as u32) >> 5) & 1;
    let b40 = (bit as u32) & 0x1F;
    let op = if is_nz { 1u32 } else { 0u32 };
    // TBZ/TBNZ: b5 011011 op b40 imm14 Rt
    let word = (b5 << 31) | (0b011011 << 25) | (op << 24) | (b40 << 19) | rt;
    Ok(EncodeResult::WordWithReloc {
        word,
        reloc: Relocation {
            reloc_type: RelocType::TstBr14,
            symbol: sym,
            addend,
        },
    })
}

// ── Loads/Stores ─────────────────────────────────────────────────────────

/// Auto-detect LDR/STR size from the first register operand.
fn encode_ldr_str_auto(operands: &[Operand], is_load: bool) -> Result<EncodeResult, String> {
    // Determine size from register: Wn -> 32-bit (size=10), Xn -> 64-bit (size=11)
    // FP: Sn -> 32-bit, Dn -> 64-bit, Qn -> 128-bit
    let reg_name = match operands.get(0) {
        Some(Operand::Reg(r)) => r.to_lowercase(),
        _ => return Err("ldr/str needs register operand".to_string()),
    };

    let size = if reg_name.starts_with('w') {
        0b10 // 32-bit
    } else if reg_name.starts_with('x') || reg_name == "sp" || reg_name == "xzr" || reg_name == "lr" {
        0b11 // 64-bit
    } else if reg_name.starts_with('s') {
        0b10 // 32-bit float
    } else if reg_name.starts_with('d') {
        0b11 // 64-bit float
    } else if reg_name.starts_with('q') {
        0b00 // 128-bit (special encoding with V=1)
    } else {
        0b11 // default 64-bit
    };

    encode_ldr_str(operands, is_load, size, false)
}

fn encode_ldr_str(operands: &[Operand], is_load: bool, size: u32, is_signed: bool) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("ldr/str requires at least 2 operands".to_string());
    }

    let (rt, _) = get_reg(operands, 0)?;
    let fp = is_fp_reg(operands.get(0).map(|o| match o { Operand::Reg(r) => r.as_str(), _ => "" }).unwrap_or(""));

    // Use the size parameter as-is (auto-detection happens in encode_ldr_str_auto)
    let actual_size = size;

    let v = if fp { 1u32 } else { 0u32 };

    match operands.get(1) {
        // [base, #offset]
        Some(Operand::Mem { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;

            // Unsigned offset encoding
            // Size determines the shift for offset alignment
            let shift = actual_size as u32;
            let opc = if is_load {
                if is_signed { 0b10 } else { 0b01 }
            } else {
                0b00
            };

            // Check if offset is aligned and fits in 12-bit unsigned field
            let abs_offset = *offset as u64;
            let align = 1u64 << shift;
            if *offset >= 0 && abs_offset % align == 0 {
                let imm12 = (abs_offset / align) as u32;
                if imm12 < 4096 {
                    // Unsigned offset form: size 111 V 01 opc imm12 Rn Rt
                    let word = (actual_size << 30) | (0b111 << 27) | (v << 26) | (0b01 << 24)
                        | (opc << 22) | (imm12 << 10) | (rn << 5) | rt;
                    return Ok(EncodeResult::Word(word));
                }
            }

            // Unscaled offset (LDUR/STUR form)
            let imm9 = (*offset as i32) & 0x1FF;
            let opc = if is_load {
                if is_signed { 0b10 } else { 0b01 }
            } else {
                0b00
            };
            let word = (actual_size << 30) | (0b111 << 27) | (v << 26) | (0b00 << 24) | (opc << 22)
                | ((imm9 as u32 & 0x1FF) << 12) | (0b00 << 10) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }

        // [base, #offset]! (pre-index)
        Some(Operand::MemPreIndex { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let imm9 = (*offset as i32) & 0x1FF;
            let opc = if is_load { 0b01 } else { 0b00 };
            let word = (actual_size << 30) | (0b111 << 27) | (v << 26) | (0b00 << 24) | (opc << 22)
                | ((imm9 as u32 & 0x1FF) << 12) | (0b11 << 10) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }

        // [base], #offset (post-index)
        Some(Operand::MemPostIndex { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let imm9 = (*offset as i32) & 0x1FF;
            let opc = if is_load { 0b01 } else { 0b00 };
            let word = (actual_size << 30) | (0b111 << 27) | (v << 26) | (0b00 << 24) | (opc << 22)
                | ((imm9 as u32 & 0x1FF) << 12) | (0b01 << 10) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }

        // [base, Xm] register offset
        Some(Operand::MemRegOffset { base, index, extend: _, shift: _ }) => {
            // Check if index is a :lo12: modifier
            if index.starts_with(':') {
                // Parse modifier from the index string
                let rn = parse_reg_num(base).ok_or("invalid base reg")?;
                let mod_str = index.trim_start_matches(':');
                let (kind, sym) = if let Some(colon_pos) = mod_str.find(':') {
                    (&mod_str[..colon_pos], &mod_str[colon_pos + 1..])
                } else {
                    return Err(format!("malformed modifier in memory operand: {}", index));
                };

                let (symbol, addend) = if let Some(plus_pos) = sym.find('+') {
                    let s = &sym[..plus_pos];
                    let off: i64 = sym[plus_pos + 1..].parse().unwrap_or(0);
                    (s.to_string(), off)
                } else {
                    (sym.to_string(), 0i64)
                };

                let opc = if is_load { 0b01 } else { 0b00 };

                let reloc_type = match kind {
                    "lo12" => {
                        match actual_size {
                            0b00 => RelocType::Ldst8AbsLo12,
                            0b01 => RelocType::Ldst16AbsLo12,
                            0b10 => RelocType::Ldst32AbsLo12,
                            0b11 => RelocType::Ldst64AbsLo12,
                            _ => RelocType::Ldst64AbsLo12,
                        }
                    }
                    "got_lo12" => RelocType::Ld64GotLo12,
                    _ => return Err(format!("unsupported modifier in load/store: {}", kind)),
                };

                let word = (actual_size << 30) | (0b111 << 27) | (v << 26) | (opc << 22)
                    | (0 << 10) | (rn << 5) | rt;
                return Ok(EncodeResult::WordWithReloc {
                    word,
                    reloc: Relocation {
                        reloc_type,
                        symbol,
                        addend,
                    },
                });
            }

            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let rm = parse_reg_num(index).ok_or("invalid index reg")?;
            let opc = if is_load { 0b01 } else { 0b00 };
            // Register offset: size 111 V opc 1 Rm option S 10 Rn Rt
            let option = 0b011u32; // LSL
            let s = 0u32; // no shift
            let word = (actual_size << 30) | (0b111 << 27) | (v << 26) | (opc << 22)
                | (1 << 21) | (rm << 16) | (option << 13) | (s << 12) | (0b10 << 10) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }

        _ => {}
    }

    Err(format!("unsupported ldr/str operands: {:?}", operands))
}

fn encode_ldrsw(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("ldrsw requires 2 operands".to_string());
    }

    let (rt, _) = get_reg(operands, 0)?;

    match operands.get(1) {
        Some(Operand::Mem { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            // LDRSW: size=10 111 V=0 01 opc=10 -> unsigned offset
            // Actually: 10 111 0 01 10 imm12 Rn Rt
            let abs_offset = *offset as u64;
            if *offset >= 0 && abs_offset % 4 == 0 {
                let imm12 = (abs_offset / 4) as u32;
                if imm12 < 4096 {
                    let word = (0b10 << 30) | (0b111 << 27) | (0 << 26) | (0b01 << 24) | (0b10 << 22)
                        | (imm12 << 10) | (rn << 5) | rt;
                    return Ok(EncodeResult::Word(word));
                }
            }
            // Unscaled: LDURSW
            let imm9 = (*offset as i32) & 0x1FF;
            let word = (0b10 << 30) | (0b111 << 27) | (0 << 26) | (0b00 << 24) | (0b10 << 22)
                | ((imm9 as u32 & 0x1FF) << 12) | (0b00 << 10) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }

        Some(Operand::MemPostIndex { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let imm9 = (*offset as i32) & 0x1FF;
            let word = (0b10 << 30) | (0b111 << 27) | (0 << 26) | (0b00 << 24) | (0b10 << 22)
                | ((imm9 as u32 & 0x1FF) << 12) | (0b01 << 10) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }

        Some(Operand::MemPreIndex { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let imm9 = (*offset as i32) & 0x1FF;
            let word = (0b10 << 30) | (0b111 << 27) | (0 << 26) | (0b00 << 24) | (0b10 << 22)
                | ((imm9 as u32 & 0x1FF) << 12) | (0b11 << 10) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }

        _ => {}
    }

    Err(format!("unsupported ldrsw operands: {:?}", operands))
}

fn encode_ldrs(operands: &[Operand], size: u32) -> Result<EncodeResult, String> {
    // LDRSB/LDRSH: sign-extending byte/halfword loads
    if operands.len() < 2 {
        return Err("ldrsb/ldrsh requires 2 operands".to_string());
    }

    let (rt, is_64) = get_reg(operands, 0)?;
    let opc = if is_64 { 0b10 } else { 0b11 }; // 64-bit target: opc=10, 32-bit: opc=11

    match operands.get(1) {
        Some(Operand::Mem { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let shift = size;
            let abs_offset = *offset as u64;
            let align = 1u64 << shift;
            if *offset >= 0 && abs_offset % align == 0 {
                let imm12 = (abs_offset / align) as u32;
                if imm12 < 4096 {
                    let word = (size << 30) | (0b111 << 27) | (0 << 26) | (0b01 << 24) | (opc << 22)
                        | (imm12 << 10) | (rn << 5) | rt;
                    return Ok(EncodeResult::Word(word));
                }
            }
            // Unscaled
            let imm9 = (*offset as i32) & 0x1FF;
            let word = (size << 30) | (0b111 << 27) | (0 << 26) | (0b00 << 24) | (opc << 22)
                | ((imm9 as u32 & 0x1FF) << 12) | (0b00 << 10) | (rn << 5) | rt;
            return Ok(EncodeResult::Word(word));
        }
        _ => {}
    }

    Err(format!("unsupported ldrsb/ldrsh operands: {:?}", operands))
}

fn encode_ldp_stp(operands: &[Operand], is_load: bool) -> Result<EncodeResult, String> {
    if operands.len() < 3 {
        return Err("ldp/stp requires 3 operands".to_string());
    }

    let (rt1, is_64) = get_reg(operands, 0)?;
    let (rt2, _) = get_reg(operands, 1)?;
    let fp = is_fp_reg(match &operands[0] { Operand::Reg(r) => r.as_str(), _ => "" });

    let opc = if fp {
        let r = match &operands[0] { Operand::Reg(r) => r.to_lowercase(), _ => String::new() };
        if r.starts_with('s') { 0b00 }
        else if r.starts_with('d') { 0b01 }
        else if r.starts_with('q') { 0b10 }
        else { if is_64 { 0b10 } else { 0b00 } }
    } else {
        if is_64 { 0b10 } else { 0b00 }
    };

    let v = if fp { 1u32 } else { 0u32 };
    let l = if is_load { 1u32 } else { 0u32 };

    // Shift depends on register size
    let shift = if fp {
        let r = match &operands[0] { Operand::Reg(r) => r.to_lowercase(), _ => String::new() };
        if r.starts_with('s') { 2 }
        else if r.starts_with('d') { 3 }
        else if r.starts_with('q') { 4 }
        else { if is_64 { 3 } else { 2 } }
    } else {
        if is_64 { 3 } else { 2 }
    };

    match operands.get(2) {
        // STP rt1, rt2, [base, #offset]! (pre-index)
        Some(Operand::MemPreIndex { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let imm7 = ((*offset >> shift) as i32) & 0x7F;
            let word = (opc << 30) | (0b101 << 27) | (v << 26) | (0b011 << 23) | (l << 22)
                | ((imm7 as u32 & 0x7F) << 15) | (rt2 << 10) | (rn << 5) | rt1;
            return Ok(EncodeResult::Word(word));
        }

        // LDP/STP rt1, rt2, [base], #offset (post-index)
        Some(Operand::MemPostIndex { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let imm7 = ((*offset >> shift) as i32) & 0x7F;
            let word = (opc << 30) | (0b101 << 27) | (v << 26) | (0b001 << 23) | (l << 22)
                | ((imm7 as u32 & 0x7F) << 15) | (rt2 << 10) | (rn << 5) | rt1;
            return Ok(EncodeResult::Word(word));
        }

        // LDP/STP rt1, rt2, [base, #offset] (signed offset)
        Some(Operand::Mem { base, offset }) => {
            let rn = parse_reg_num(base).ok_or("invalid base reg")?;
            let imm7 = ((*offset >> shift) as i32) & 0x7F;
            let word = (opc << 30) | (0b101 << 27) | (v << 26) | (0b010 << 23) | (l << 22)
                | ((imm7 as u32 & 0x7F) << 15) | (rt2 << 10) | (rn << 5) | rt1;
            return Ok(EncodeResult::Word(word));
        }

        _ => {}
    }

    Err(format!("unsupported ldp/stp operands: {:?}", operands))
}

// ── Exclusive loads/stores ───────────────────────────────────────────────

fn encode_ldxr_stxr(operands: &[Operand], is_load: bool) -> Result<EncodeResult, String> {
    if is_load {
        // LDXR Rt, [Rn]
        let (rt, is_64) = get_reg(operands, 0)?;
        let rn = match operands.get(1) {
            Some(Operand::Mem { base, .. }) => parse_reg_num(base).ok_or("invalid base")?,
            _ => return Err("ldxr needs memory operand".to_string()),
        };
        let size = if is_64 { 0b11 } else { 0b10 };
        let word = (size << 30) | (0b001000010 << 21) | (0b11111 << 16) | (0 << 15)
            | (0b11111 << 10) | (rn << 5) | rt;
        Ok(EncodeResult::Word(word))
    } else {
        // STXR Ws, Rt, [Rn]
        let (ws, _) = get_reg(operands, 0)?;
        let (rt, is_64) = get_reg(operands, 1)?;
        let rn = match operands.get(2) {
            Some(Operand::Mem { base, .. }) => parse_reg_num(base).ok_or("invalid base")?,
            _ => return Err("stxr needs memory operand".to_string()),
        };
        let size = if is_64 { 0b11 } else { 0b10 };
        let word = (size << 30) | (0b001000000 << 21) | (ws << 16) | (0 << 15)
            | (0b11111 << 10) | (rn << 5) | rt;
        Ok(EncodeResult::Word(word))
    }
}

fn encode_ldaxr_stlxr(operands: &[Operand], is_load: bool) -> Result<EncodeResult, String> {
    if is_load {
        let (rt, is_64) = get_reg(operands, 0)?;
        let rn = match operands.get(1) {
            Some(Operand::Mem { base, .. }) => parse_reg_num(base).ok_or("invalid base")?,
            _ => return Err("ldaxr needs memory operand".to_string()),
        };
        let size = if is_64 { 0b11 } else { 0b10 };
        let word = (size << 30) | (0b001000010 << 21) | (0b11111 << 16) | (1 << 15)
            | (0b11111 << 10) | (rn << 5) | rt;
        Ok(EncodeResult::Word(word))
    } else {
        let (ws, _) = get_reg(operands, 0)?;
        let (rt, is_64) = get_reg(operands, 1)?;
        let rn = match operands.get(2) {
            Some(Operand::Mem { base, .. }) => parse_reg_num(base).ok_or("invalid base")?,
            _ => return Err("stlxr needs memory operand".to_string()),
        };
        let size = if is_64 { 0b11 } else { 0b10 };
        let word = (size << 30) | (0b001000000 << 21) | (ws << 16) | (1 << 15)
            | (0b11111 << 10) | (rn << 5) | rt;
        Ok(EncodeResult::Word(word))
    }
}

fn encode_ldar_stlr(operands: &[Operand], is_load: bool) -> Result<EncodeResult, String> {
    let (rt, is_64) = get_reg(operands, 0)?;
    let rn = match operands.get(1) {
        Some(Operand::Mem { base, .. }) => parse_reg_num(base).ok_or("invalid base")?,
        _ => return Err("ldar/stlr needs memory operand".to_string()),
    };
    let size = if is_64 { 0b11 } else { 0b10 };
    let l = if is_load { 1u32 } else { 0 };
    // LDAR/STLR: size 001000 1 L 0 11111 1 11111 Rn Rt
    let word = (size << 30) | (0b001000 << 24) | (1 << 23) | (l << 22) | (0 << 21)
        | (0b11111 << 16) | (1 << 15) | (0b11111 << 10) | (rn << 5) | rt;
    Ok(EncodeResult::Word(word))
}

// ── Address computation ──────────────────────────────────────────────────

fn encode_adrp(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;

    let (sym, addend) = match operands.get(1) {
        Some(Operand::Symbol(s)) => (s.clone(), 0i64),
        Some(Operand::Modifier { kind, symbol }) if kind == "got" => {
            // adrp x0, :got:symbol
            let word = (1u32 << 31) | (0b10000 << 24) | rd;
            return Ok(EncodeResult::WordWithReloc {
                word,
                reloc: Relocation {
                    reloc_type: RelocType::AdrGotPage21,
                    symbol: symbol.clone(),
                    addend: 0,
                },
            });
        }
        Some(Operand::SymbolOffset(s, off)) => (s.clone(), *off),
        Some(Operand::Label(s)) => (s.clone(), 0i64),
        _ => return Err(format!("adrp needs symbol operand, got {:?}", operands.get(1))),
    };

    // ADRP: 1 immlo[1:0] 10000 immhi[18:0] Rd
    let word = (1u32 << 31) | (0b10000 << 24) | rd;
    Ok(EncodeResult::WordWithReloc {
        word,
        reloc: Relocation {
            reloc_type: RelocType::AdrpPage21,
            symbol: sym,
            addend,
        },
    })
}

fn encode_adr(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (sym, addend) = get_symbol(operands, 1)?;
    // ADR: 0 immlo[1:0] 10000 immhi[18:0] Rd
    let word = (0u32 << 31) | (0b10000 << 24) | rd;
    Ok(EncodeResult::WordWithReloc {
        word,
        reloc: Relocation {
            reloc_type: RelocType::AdrpPage21, // TODO: should use ADR reloc
            symbol: sym,
            addend,
        },
    })
}

// ── Floating point ───────────────────────────────────────────────────────

fn encode_fmov(operands: &[Operand]) -> Result<EncodeResult, String> {
    if operands.len() < 2 {
        return Err("fmov requires 2 operands".to_string());
    }

    let (rd_name, rm_name) = match (&operands[0], &operands[1]) {
        (Operand::Reg(a), Operand::Reg(b)) => (a.clone(), b.clone()),
        (Operand::Reg(_a), Operand::Imm(_)) => {
            // fmov with immediate - encode as immediate float
            // TODO: implement float immediate encoding
            return Ok(EncodeResult::Word(0xd503201f)); // NOP placeholder
        }
        _ => return Err("fmov needs register operands".to_string()),
    };

    let rd = parse_reg_num(&rd_name).ok_or("invalid rd")?;
    let rm = parse_reg_num(&rm_name).ok_or("invalid rm")?;

    let rd_is_fp = is_fp_reg(&rd_name);
    let rm_is_fp = is_fp_reg(&rm_name);
    let rd_lower = rd_name.to_lowercase();
    let rm_lower = rm_name.to_lowercase();

    if rd_is_fp && rm_is_fp {
        // FMOV between FP registers
        let is_double = rd_lower.starts_with('d') || rm_lower.starts_with('d');
        let ftype = if is_double { 0b01 } else { 0b00 };
        // 0 00 11110 ftype 1 0000 00 10000 Rn Rd
        let word = (0b00011110 << 24) | (ftype << 22) | (0b100000 << 16) | (0b10000 << 10) | (rm << 5) | rd;
        return Ok(EncodeResult::Word(word));
    }

    if rd_is_fp && !rm_is_fp {
        // FMOV from GP to FP: FMOV Dn, Xn or FMOV Sn, Wn
        let is_double = rd_lower.starts_with('d');
        if is_double {
            // FMOV Dd, Xn: 1 00 11110 01 1 00 111 000000 Rn Rd
            let word = (0b1001111001 << 22) | (0b100111 << 16) | (0 << 10) | (rm << 5) | rd;
            return Ok(EncodeResult::Word(word));
        } else {
            // FMOV Sd, Wn: 0 00 11110 00 1 00 111 000000 Rn Rd
            let word = (0b0001111000 << 22) | (0b100111 << 16) | (0 << 10) | (rm << 5) | rd;
            return Ok(EncodeResult::Word(word));
        }
    }

    if !rd_is_fp && rm_is_fp {
        // FMOV from FP to GP: FMOV Xn, Dn or FMOV Wn, Sn
        let is_double = rm_lower.starts_with('d');
        if is_double {
            // FMOV Xd, Dn: 1 00 11110 01 1 00 110 000000 Rn Rd
            let word = (0b1001111001 << 22) | (0b100110 << 16) | (0 << 10) | (rm << 5) | rd;
            return Ok(EncodeResult::Word(word));
        } else {
            // FMOV Wd, Sn: 0 00 11110 00 1 00 110 000000 Rn Rd
            let word = (0b0001111000 << 22) | (0b100110 << 16) | (0 << 10) | (rm << 5) | rd;
            return Ok(EncodeResult::Word(word));
        }
    }

    Err(format!("unsupported fmov operands: {} -> {}", rd_name, rm_name))
}

fn encode_fp_arith(operands: &[Operand], opcode: u32) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;

    let rd_name = match &operands[0] { Operand::Reg(r) => r.to_lowercase(), _ => String::new() };
    let is_double = rd_name.starts_with('d');
    let ftype = if is_double { 0b01 } else { 0b00 };

    // 0 00 11110 ftype 1 Rm opcode 10 Rn Rd
    let word = (0b00011110 << 24) | (ftype << 22) | (1 << 21) | (rm << 16) | (opcode << 12) | (0b10 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_fneg(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let rd_name = match &operands[0] { Operand::Reg(r) => r.to_lowercase(), _ => String::new() };
    let is_double = rd_name.starts_with('d');
    let ftype = if is_double { 0b01 } else { 0b00 };
    // FNEG: 0 00 11110 ftype 1 0000 10 10000 Rn Rd
    let word = (0b00011110 << 24) | (ftype << 22) | (0b100001 << 16) | (0b10000 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_fabs(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let rd_name = match &operands[0] { Operand::Reg(r) => r.to_lowercase(), _ => String::new() };
    let is_double = rd_name.starts_with('d');
    let ftype = if is_double { 0b01 } else { 0b00 };
    // FABS: 0 00 11110 ftype 1 0000 01 10000 Rn Rd
    let word = (0b00011110 << 24) | (ftype << 22) | (0b100000 << 16) | (0b110000 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_fsqrt(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let rd_name = match &operands[0] { Operand::Reg(r) => r.to_lowercase(), _ => String::new() };
    let is_double = rd_name.starts_with('d');
    let ftype = if is_double { 0b01 } else { 0b00 };
    // FSQRT: 0 00 11110 ftype 1 0000 11 10000 Rn Rd
    let word = (0b00011110 << 24) | (ftype << 22) | (0b100001 << 16) | (0b110000 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_fcmp(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rn, _) = get_reg(operands, 0)?;
    let rn_name = match &operands[0] { Operand::Reg(r) => r.to_lowercase(), _ => String::new() };
    let is_double = rn_name.starts_with('d');
    let ftype = if is_double { 0b01 } else { 0b00 };

    // FCMP Dn, #0.0
    if operands.len() < 2 || matches!(operands.get(1), Some(Operand::Imm(0))) {
        let word = (0b00011110 << 24) | (ftype << 22) | (1 << 21) | (0 << 16) | (0b001000 << 10) | (rn << 5) | 0b01000;
        return Ok(EncodeResult::Word(word));
    }

    let (rm, _) = get_reg(operands, 1)?;
    // FCMP Dn, Dm: 0 00 11110 ftype 1 Rm 00 1000 Rn 00 000
    let word = (0b00011110 << 24) | (ftype << 22) | (1 << 21) | (rm << 16) | (0b001000 << 10) | (rn << 5) | 0;
    Ok(EncodeResult::Word(word))
}

fn encode_fcvt(_operands: &[Operand], _is_signed: bool) -> Result<EncodeResult, String> {
    // TODO: implement FCVTZS/FCVTZU properly
    Ok(EncodeResult::Word(0xd503201f)) // NOP placeholder
}

fn encode_ucvtf(_operands: &[Operand]) -> Result<EncodeResult, String> {
    // TODO: implement UCVTF properly
    Ok(EncodeResult::Word(0xd503201f)) // NOP placeholder
}

fn encode_scvtf(_operands: &[Operand]) -> Result<EncodeResult, String> {
    // TODO: implement SCVTF properly
    Ok(EncodeResult::Word(0xd503201f)) // NOP placeholder
}

fn encode_fcvt_precision(_operands: &[Operand]) -> Result<EncodeResult, String> {
    // FCVT Sd, Dn or FCVT Dd, Sn (precision conversion)
    // TODO: implement properly
    Ok(EncodeResult::Word(0xd503201f)) // NOP placeholder
}

// ── NEON/SIMD ────────────────────────────────────────────────────────────

fn encode_cnt(_operands: &[Operand]) -> Result<EncodeResult, String> {
    // TODO: implement CNT (popcount) properly
    Ok(EncodeResult::Word(0xd503201f)) // NOP placeholder
}

fn encode_uaddlv(_operands: &[Operand]) -> Result<EncodeResult, String> {
    // TODO: implement UADDLV properly
    Ok(EncodeResult::Word(0xd503201f)) // NOP placeholder
}

// ── System instructions ──────────────────────────────────────────────────

fn encode_dmb(operands: &[Operand]) -> Result<EncodeResult, String> {
    let option = match operands.get(0) {
        Some(Operand::Barrier(b)) => match b.as_str() {
            "sy" => 0b1111,
            "ish" => 0b1011,
            "ishld" => 0b1001,
            "ishst" => 0b1010,
            "osh" => 0b0011,
            "oshld" => 0b0001,
            "oshst" => 0b0010,
            "nsh" => 0b0111,
            "nshld" => 0b0101,
            "nshst" => 0b0110,
            "ld" => 0b1101,
            "st" => 0b1110,
            _ => 0b1111,
        },
        Some(Operand::Symbol(s)) => match s.to_lowercase().as_str() {
            "sy" => 0b1111,
            "ish" => 0b1011,
            "ishld" => 0b1001,
            "ishst" => 0b1010,
            _ => 0b1111,
        },
        _ => 0b1111,
    };
    // DMB: 0xD50330BF | (CRm << 8)
    let word = 0xd50330bf | (option << 8);
    Ok(EncodeResult::Word(word))
}

fn encode_dsb(operands: &[Operand]) -> Result<EncodeResult, String> {
    let option = match operands.get(0) {
        Some(Operand::Barrier(b)) => match b.as_str() {
            "sy" => 0b1111u32,
            "ish" => 0b1011,
            "ishld" => 0b1001,
            "ishst" => 0b1010,
            _ => 0b1111,
        },
        Some(Operand::Symbol(s)) => match s.to_lowercase().as_str() {
            "sy" => 0b1111u32,
            "ish" => 0b1011,
            _ => 0b1111,
        },
        _ => 0b1111,
    };
    // DSB: 0xD503309F | (option << 8)
    let word = 0xd503309f | (option << 8);
    Ok(EncodeResult::Word(word))
}

fn encode_mrs(operands: &[Operand]) -> Result<EncodeResult, String> {
    // MRS Xt, system_reg
    let (rt, _) = get_reg(operands, 0)?;
    let sysreg = match operands.get(1) {
        Some(Operand::Symbol(s)) => s.to_lowercase(),
        _ => return Err("mrs needs system register name".to_string()),
    };

    let encoding = match sysreg.as_str() {
        "tpidr_el0" => 0xde82u32, // 11 011 1101 0000 010
        "cntfrq_el0" => 0xdf00,
        "cntvct_el0" => 0xdf02,
        "fpcr" => 0xda20,
        "fpsr" => 0xda21,
        "nzcv" => 0xda10,
        _ => return Err(format!("unsupported system register: {}", sysreg)),
    };

    let word = 0xd5300000 | (encoding << 5) | rt;
    Ok(EncodeResult::Word(word))
}

fn encode_msr(operands: &[Operand]) -> Result<EncodeResult, String> {
    // MSR system_reg, Xt
    let sysreg = match operands.get(0) {
        Some(Operand::Symbol(s)) => s.to_lowercase(),
        _ => return Err("msr needs system register name".to_string()),
    };
    let (rt, _) = get_reg(operands, 1)?;

    let encoding = match sysreg.as_str() {
        "tpidr_el0" => 0xde82u32,
        "fpcr" => 0xda20,
        "fpsr" => 0xda21,
        "nzcv" => 0xda10,
        _ => return Err(format!("unsupported system register: {}", sysreg)),
    };

    let word = 0xd5100000 | (encoding << 5) | rt;
    Ok(EncodeResult::Word(word))
}

fn encode_svc(operands: &[Operand]) -> Result<EncodeResult, String> {
    let imm = get_imm(operands, 0)?;
    let word = 0xd4000001 | ((imm as u32 & 0xFFFF) << 5);
    Ok(EncodeResult::Word(word))
}

fn encode_brk(operands: &[Operand]) -> Result<EncodeResult, String> {
    let imm = get_imm(operands, 0)?;
    let word = 0xd4200000 | ((imm as u32 & 0xFFFF) << 5);
    Ok(EncodeResult::Word(word))
}

// ── Bit manipulation ─────────────────────────────────────────────────────

fn encode_clz(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    // CLZ: sf 1 0 11010110 00000 00010 0 Rn Rd
    let word = (sf << 31) | (1 << 30) | (0b011010110 << 21) | (0b00000 << 16)
        | (0b000100 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_cls(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (1 << 30) | (0b011010110 << 21) | (0b00000 << 16)
        | (0b000101 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_rbit(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (1 << 30) | (0b011010110 << 21) | (0b00000 << 16)
        | (0b000000 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_rev(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let opc = if is_64 { 0b000011 } else { 0b000010 };
    let word = (sf << 31) | (1 << 30) | (0b011010110 << 21) | (0b00000 << 16)
        | (opc << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_rev16(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, is_64) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let sf = sf_bit(is_64);
    let word = (sf << 31) | (1 << 30) | (0b011010110 << 21) | (0b00000 << 16)
        | (0b000001 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

fn encode_rev32(operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    // REV32 is 64-bit only: 1 1 0 11010110 00000 000010 Rn Rd
    let word = (1u32 << 31) | (1 << 30) | (0b011010110 << 21) | (0b00000 << 16)
        | (0b000010 << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}

// ── CRC32 ────────────────────────────────────────────────────────────────

fn encode_crc32(mnemonic: &str, operands: &[Operand]) -> Result<EncodeResult, String> {
    let (rd, _) = get_reg(operands, 0)?;
    let (rn, _) = get_reg(operands, 1)?;
    let (rm, _) = get_reg(operands, 2)?;

    let is_c = mnemonic.contains("crc32c");
    let c_bit = if is_c { 1u32 } else { 0 };

    let (sf, sz) = match mnemonic {
        "crc32b" | "crc32cb" => (0u32, 0b00u32),
        "crc32h" | "crc32ch" => (0, 0b01),
        "crc32w" | "crc32cw" => (0, 0b10),
        "crc32x" | "crc32cx" => (1, 0b11),
        _ => (0, 0b00),
    };

    // CRC32: sf 0 0 11010110 Rm 010 C sz Rn Rd
    let word = (sf << 31) | (0b0011010110 << 21) | (rm << 16) | (0b010 << 13)
        | (c_bit << 12) | (sz << 10) | (rn << 5) | rd;
    Ok(EncodeResult::Word(word))
}
