//! i686 peephole optimizer for assembly text.
//!
//! Operates on generated assembly text to eliminate redundant patterns from the
//! stack-based codegen. Adapted from the x86-64 peephole optimizer for 32-bit
//! i686 assembly (uses %ebp instead of %rbp, %eax instead of %rax, etc.).
//!
//! ## Pass structure
//!
//! 1. **Local passes** (iterative, up to 8 rounds): adjacent store/load elimination,
//!    self-move elimination, redundant jump elimination, branch inversion, reverse
//!    move elimination.
//!
//! 2. **Global passes** (once): dead register move elimination, dead store elimination,
//!    compare+branch fusion, memory operand folding.
//!
//! 3. **Local cleanup** (up to 4 rounds): re-run local and global passes to clean up
//!    opportunities exposed by the first round.
//!
//! 4. **Never-read store elimination**: global analysis to remove stores to
//!    stack slots that are never read anywhere in the function.

// ── Constants ────────────────────────────────────────────────────────────────

const MAX_LOCAL_PASS_ITERATIONS: usize = 8;
const MAX_POST_GLOBAL_ITERATIONS: usize = 4;

// Register IDs (i686 has fewer registers)
type RegId = u8;
const REG_NONE: RegId = 255;
const REG_EAX: RegId = 0;
const REG_ECX: RegId = 1;
const REG_EDX: RegId = 2;
const REG_EBX: RegId = 3;
const REG_ESP: RegId = 4;
const REG_EBP: RegId = 5;
const REG_ESI: RegId = 6;
const REG_EDI: RegId = 7;
const REG_GP_MAX: RegId = 7;

/// Sentinel value for ebp_offset meaning "no %ebp reference" or "complex reference".
const EBP_OFFSET_NONE: i32 = i32::MIN;

// ── Line classification ──────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LineKind {
    Nop,
    Empty,
    StoreEbp { reg: RegId, offset: i32, size: MoveSize },
    LoadEbp  { reg: RegId, offset: i32, size: MoveSize },
    Move { dst: RegId, src: RegId },
    SelfMove,
    Label,
    Jmp,
    JmpIndirect,
    CondJmp,
    Call,
    Ret,
    Push { reg: RegId },
    Pop { reg: RegId },
    SetCC { reg: RegId },
    Cmp,
    Directive,
    Other { dest_reg: RegId },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MoveSize {
    L,   // movl (32-bit)
    W,   // movw (16-bit)
    B,   // movb (8-bit)
}

impl MoveSize {
    fn mnemonic(self) -> &'static str {
        match self {
            MoveSize::L => "movl",
            MoveSize::W => "movw",
            MoveSize::B => "movb",
        }
    }
}

#[derive(Clone, Copy)]
struct LineInfo {
    kind: LineKind,
    trim_start: u16,
    has_indirect_mem: bool,
    ebp_offset: i32,
}

impl LineInfo {
    #[inline]
    fn is_nop(self) -> bool { self.kind == LineKind::Nop }
    #[inline]
    fn is_barrier(self) -> bool {
        matches!(self.kind,
            LineKind::Label | LineKind::Call | LineKind::Jmp | LineKind::JmpIndirect |
            LineKind::CondJmp | LineKind::Ret | LineKind::Directive)
    }
}

#[inline]
fn line_info(kind: LineKind, ts: u16) -> LineInfo {
    LineInfo { kind, trim_start: ts, has_indirect_mem: false, ebp_offset: EBP_OFFSET_NONE }
}

// ── Register parsing ─────────────────────────────────────────────────────────

/// Map i686 register name to family ID.
fn register_family(name: &str) -> RegId {
    let name = name.trim_start_matches('%');
    match name {
        "eax" | "ax" | "al" | "ah" => REG_EAX,
        "ecx" | "cx" | "cl" | "ch" => REG_ECX,
        "edx" | "dx" | "dl" | "dh" => REG_EDX,
        "ebx" | "bx" | "bl" | "bh" => REG_EBX,
        "esp" | "sp" => REG_ESP,
        "ebp" | "bp" => REG_EBP,
        "esi" | "si" => REG_ESI,
        "edi" | "di" => REG_EDI,
        _ => REG_NONE,
    }
}

/// Get the 32-bit register name for a family ID.
fn reg32_name(id: RegId) -> &'static str {
    match id {
        REG_EAX => "%eax",
        REG_ECX => "%ecx",
        REG_EDX => "%edx",
        REG_EBX => "%ebx",
        REG_ESP => "%esp",
        REG_EBP => "%ebp",
        REG_ESI => "%esi",
        REG_EDI => "%edi",
        _ => "%???",
    }
}

/// Check if a register is caller-saved (clobbered by calls).
fn is_caller_saved(reg: RegId) -> bool {
    matches!(reg, REG_EAX | REG_ECX | REG_EDX)
}

// ── Store/Load parsing ───────────────────────────────────────────────────────

/// Parse `movX %reg, offset(%ebp)` → (reg_name, offset_str, MoveSize)
fn parse_store_to_ebp(s: &str) -> Option<(&str, &str, MoveSize)> {
    let (rest, size) = if let Some(r) = s.strip_prefix("movl ") {
        (r, MoveSize::L)
    } else if let Some(r) = s.strip_prefix("movw ") {
        (r, MoveSize::W)
    } else if let Some(r) = s.strip_prefix("movb ") {
        (r, MoveSize::B)
    } else {
        return None;
    };
    // rest = "%eax, -8(%ebp)"
    let rest = rest.trim();
    if !rest.starts_with('%') { return None; }
    let comma = rest.find(',')?;
    let reg = &rest[..comma];
    let mem = rest[comma + 1..].trim();
    if !mem.ends_with("(%ebp)") { return None; }
    // Reject indirect memory (pointer dereference, not stack slot)
    if mem.contains("(%e") && !mem.ends_with("(%ebp)") { return None; }
    let offset_str = &mem[..mem.len() - 6]; // strip "(%ebp)"
    Some((reg.trim(), offset_str, size))
}

/// Parse `movX offset(%ebp), %reg` → (offset_str, reg_name, MoveSize)
fn parse_load_from_ebp(s: &str) -> Option<(&str, &str, MoveSize)> {
    let (rest, size) = if let Some(r) = s.strip_prefix("movl ") {
        (r, MoveSize::L)
    } else if let Some(r) = s.strip_prefix("movw ") {
        (r, MoveSize::W)
    } else if let Some(r) = s.strip_prefix("movb ") {
        (r, MoveSize::B)
    } else if let Some(r) = s.strip_prefix("movzbl ") {
        (r, MoveSize::L) // movzbl from stack, dest is 32-bit
    } else if let Some(r) = s.strip_prefix("movzwl ") {
        (r, MoveSize::L)
    } else if let Some(r) = s.strip_prefix("movsbl ") {
        (r, MoveSize::L)
    } else if let Some(r) = s.strip_prefix("movswl ") {
        (r, MoveSize::L)
    } else {
        return None;
    };
    let rest = rest.trim();
    // Must start with an offset or directly with (%ebp)
    if !rest.contains("(%ebp)") { return None; }
    let paren_start = rest.find("(%ebp)")?;
    let offset_str = &rest[..paren_start];
    let after = rest[paren_start + 6..].trim();
    if !after.starts_with(',') { return None; }
    let reg = after[1..].trim();
    if !reg.starts_with('%') { return None; }
    Some((offset_str, reg, size))
}

/// Parse `movl %src, %dst` (register-to-register move).
fn parse_reg_to_reg_move(s: &str) -> Option<(RegId, RegId)> {
    let rest = s.strip_prefix("movl ")?.trim();
    if !rest.starts_with('%') { return None; }
    let comma = rest.find(',')?;
    let src_name = rest[..comma].trim();
    let dst_name = rest[comma + 1..].trim();
    if !dst_name.starts_with('%') { return None; }
    // Must not be memory operands
    if src_name.contains('(') || dst_name.contains('(') { return None; }
    let src = register_family(src_name);
    let dst = register_family(dst_name);
    if src <= REG_GP_MAX && dst <= REG_GP_MAX {
        Some((src, dst))
    } else {
        None
    }
}

/// Parse integer offset from string.
fn parse_offset(s: &str) -> i32 {
    if s.is_empty() { return 0; }
    s.parse::<i32>().unwrap_or(EBP_OFFSET_NONE)
}

/// Check if a line has indirect memory access (pointer dereference through a register).
fn has_indirect_memory_access(s: &str) -> bool {
    // Pattern: offset(%eXX) where XX is not bp
    // or (%eXX) where XX is not bp
    // or (%eXX, %eYY, N)
    let bytes = s.as_bytes();
    for i in 0..bytes.len() {
        if bytes[i] == b'(' && i + 4 < bytes.len() && bytes[i + 1] == b'%' {
            // Check if it's (%ebp) - that's a stack access, not indirect
            if i + 5 < bytes.len() && &bytes[i + 1..i + 5] == b"%ebp" {
                // It's ebp-relative, but skip to after the closing paren
                continue;
            }
            return true;
        }
    }
    false
}

/// Parse the %ebp offset from a line, or return EBP_OFFSET_NONE.
fn parse_ebp_offset_in_line(s: &str) -> i32 {
    if let Some(pos) = s.find("(%ebp)") {
        let before = &s[..pos];
        // Find the start of the offset number
        let offset_start = before.rfind(|c: char| !c.is_ascii_digit() && c != '-').map(|p| p + 1).unwrap_or(0);
        let offset_str = &before[offset_start..];
        if offset_str.is_empty() {
            0
        } else {
            offset_str.parse::<i32>().unwrap_or(EBP_OFFSET_NONE)
        }
    } else {
        EBP_OFFSET_NONE
    }
}

/// Parse the destination register of a generic instruction.
/// For two-operand instructions (AT&T syntax), the destination is the last operand.
fn parse_dest_reg(s: &str) -> RegId {
    // Find the last %reg
    if let Some(comma) = s.rfind(',') {
        let after = s[comma + 1..].trim();
        if after.starts_with('%') && !after.contains('(') {
            return register_family(after);
        }
    }
    REG_NONE
}

/// Check if a line references a specific register family.
/// This includes both explicit register operands and implicit register uses
/// by instructions like cltd, idivl, rep movsb, etc.
fn line_references_reg(s: &str, reg: RegId) -> bool {
    // Check explicit register operands
    let names: &[&str] = match reg {
        REG_EAX => &["%eax", "%ax", "%al", "%ah"],
        REG_ECX => &["%ecx", "%cx", "%cl", "%ch"],
        REG_EDX => &["%edx", "%dx", "%dl", "%dh"],
        REG_EBX => &["%ebx", "%bx", "%bl", "%bh"],
        REG_ESP => &["%esp", "%sp"],
        REG_EBP => &["%ebp", "%bp"],
        REG_ESI => &["%esi", "%si"],
        REG_EDI => &["%edi", "%di"],
        _ => return false,
    };
    for name in names {
        if s.contains(name) { return true; }
    }
    // Check implicit register uses by specific instructions
    if implicit_reg_use(s, reg) { return true; }
    false
}

/// Check if an instruction implicitly uses a register (not mentioned in text).
fn implicit_reg_use(s: &str, reg: RegId) -> bool {
    let bytes = s.as_bytes();
    if bytes.is_empty() { return false; }
    match bytes[0] {
        b'c' => {
            // cltd/cdq: reads eax, writes edx
            if s == "cltd" || s == "cdq" {
                return reg == REG_EAX || reg == REG_EDX;
            }
            // cbw/cwde: reads/writes eax
            if s == "cbw" || s == "cwde" || s == "cwtl" {
                return reg == REG_EAX;
            }
        }
        b'i' => {
            // idivl/idivw: implicitly reads edx:eax, writes eax and edx
            if s.starts_with("idivl") || s.starts_with("idivw") || s.starts_with("idivb") {
                return reg == REG_EAX || reg == REG_EDX;
            }
            // imull with 1 operand: reads eax, writes edx:eax
            // imull with 2 or 3 operands has explicit regs
            if s.starts_with("imull ") && !s.contains(',') {
                return reg == REG_EAX || reg == REG_EDX;
            }
        }
        b'd' => {
            // divl/divw: implicitly reads edx:eax, writes eax and edx
            if s.starts_with("divl") || s.starts_with("divw") || s.starts_with("divb") {
                return reg == REG_EAX || reg == REG_EDX;
            }
        }
        b'm' => {
            // mul: reads eax, writes edx:eax
            if s.starts_with("mull ") || s.starts_with("mulw ") || s.starts_with("mulb ") {
                return reg == REG_EAX || reg == REG_EDX;
            }
        }
        b'r' => {
            // rep movsb/movsl: uses esi, edi, ecx
            // rep stosb/stosl: uses edi, ecx, eax
            if s.starts_with("rep") {
                if s.contains("movs") {
                    return reg == REG_ESI || reg == REG_EDI || reg == REG_ECX;
                }
                if s.contains("stos") {
                    return reg == REG_EAX || reg == REG_EDI || reg == REG_ECX;
                }
                if s.contains("scas") || s.contains("cmps") {
                    return reg == REG_ESI || reg == REG_EDI || reg == REG_ECX || reg == REG_EAX;
                }
                // Unknown rep instruction - assume all regs used
                return true;
            }
        }
        b'l' => {
            // loop/loope/loopne: reads ecx
            if s.starts_with("loop") {
                return reg == REG_ECX;
            }
        }
        _ => {}
    }
    false
}

// ── Line classifier ──────────────────────────────────────────────────────────

fn classify_line(raw: &str) -> LineInfo {
    let trim_start = raw.len() - raw.trim_start().len();
    let s = &raw[trim_start..];

    if s.is_empty() {
        return line_info(LineKind::Empty, trim_start as u16);
    }

    let bytes = s.as_bytes();
    let first = bytes[0];
    let last = bytes[bytes.len() - 1];
    let ts = trim_start as u16;

    // Label
    if last == b':' {
        return line_info(LineKind::Label, ts);
    }

    // Directive
    if first == b'.' {
        return line_info(LineKind::Directive, ts);
    }

    // Comment
    if first == b'#' {
        return line_info(LineKind::Directive, ts);
    }

    // mov instructions - check store/load/self-move/reg-reg
    if first == b'm' && bytes.len() >= 4 && bytes[1] == b'o' && bytes[2] == b'v' {
        if let Some((reg_str, offset_str, size)) = parse_store_to_ebp(s) {
            let reg = register_family(reg_str);
            if reg <= REG_GP_MAX {
                let offset = parse_offset(offset_str);
                return line_info(LineKind::StoreEbp { reg, offset, size }, ts);
            }
        }
        if let Some((offset_str, reg_str, size)) = parse_load_from_ebp(s) {
            let reg = register_family(reg_str);
            if reg <= REG_GP_MAX {
                let offset = parse_offset(offset_str);
                return line_info(LineKind::LoadEbp { reg, offset, size }, ts);
            }
        }
        if let Some((src, dst)) = parse_reg_to_reg_move(s) {
            if src == dst {
                return line_info(LineKind::SelfMove, ts);
            }
            return line_info(LineKind::Move { dst, src }, ts);
        }
    }

    // Control flow
    if first == b'j' {
        if bytes.len() >= 4 && bytes[1] == b'm' && bytes[2] == b'p' {
            if bytes.len() > 4 && bytes[4] == b'*' {
                return line_info(LineKind::JmpIndirect, ts);
            }
            if bytes[3] == b' ' {
                if s.contains("indirect_thunk") || s.contains("*%") {
                    return line_info(LineKind::JmpIndirect, ts);
                }
                return line_info(LineKind::Jmp, ts);
            }
        }
        if is_conditional_jump(s) {
            return line_info(LineKind::CondJmp, ts);
        }
    }

    if first == b'c' {
        if bytes.len() >= 4 && bytes[1] == b'a' && bytes[2] == b'l' && bytes[3] == b'l' {
            return line_info(LineKind::Call, ts);
        }
        if bytes.len() >= 4 && bytes[1] == b'm' && bytes[2] == b'p' {
            return line_info(LineKind::Cmp, ts);
        }
    }

    if first == b'r' && s == "ret" {
        return line_info(LineKind::Ret, ts);
    }

    // test instructions
    if first == b't' && bytes.len() >= 5 && bytes[1] == b'e' && bytes[2] == b's' && bytes[3] == b't' {
        return line_info(LineKind::Cmp, ts);
    }

    // push/pop
    if first == b'p' {
        if let Some(rest) = s.strip_prefix("pushl ") {
            let reg = register_family(rest.trim());
            return line_info(LineKind::Push { reg }, ts);
        }
        if let Some(rest) = s.strip_prefix("popl ") {
            let reg = register_family(rest.trim());
            return line_info(LineKind::Pop { reg }, ts);
        }
    }

    // setCC
    if first == b's' && bytes.len() >= 4 && bytes[1] == b'e' && bytes[2] == b't' && parse_setcc(s).is_some() {
        let setcc_reg = if let Some(space_pos) = s.rfind(' ') {
            register_family(s[space_pos + 1..].trim())
        } else {
            REG_EAX
        };
        return line_info(LineKind::SetCC { reg: setcc_reg }, ts);
    }

    // Other instruction
    let dest_reg = parse_dest_reg(s);
    let has_indirect = has_indirect_memory_access(s);
    let ebp_off = if has_indirect { EBP_OFFSET_NONE } else { parse_ebp_offset_in_line(s) };
    LineInfo {
        kind: LineKind::Other { dest_reg },
        trim_start: ts,
        has_indirect_mem: has_indirect,
        ebp_offset: ebp_off,
    }
}

// ── Conditional jump helpers ─────────────────────────────────────────────────

fn is_conditional_jump(s: &str) -> bool {
    let b = s.as_bytes();
    if b.len() < 3 || b[0] != b'j' { return false; }
    // jCC where CC is one of: e, ne, l, le, g, ge, b, be, a, ae, s, ns, o, no, p, np, z, nz
    matches!(&s[1..2], "e" | "a" | "b" | "g" | "l" | "s" | "o" | "p" | "z" | "n")
        && s.contains(' ')
}

/// Invert a condition code.
fn invert_cc(cc: &str) -> Option<&'static str> {
    match cc {
        "e" | "z" => Some("ne"),
        "ne" | "nz" => Some("e"),
        "l" => Some("ge"),
        "ge" => Some("l"),
        "le" => Some("g"),
        "g" => Some("le"),
        "b" => Some("ae"),
        "ae" => Some("b"),
        "be" => Some("a"),
        "a" => Some("be"),
        "s" => Some("ns"),
        "ns" => Some("s"),
        "o" => Some("no"),
        "no" => Some("o"),
        "p" => Some("np"),
        "np" => Some("p"),
        _ => None,
    }
}

/// Extract condition code and target from a conditional jump.
fn parse_condjmp(s: &str) -> Option<(&str, &str)> {
    if !s.starts_with('j') { return None; }
    let space = s.find(' ')?;
    let cc = &s[1..space];
    let target = s[space + 1..].trim();
    Some((cc, target))
}

/// Parse setCC instruction → condition code.
fn parse_setcc(s: &str) -> Option<&str> {
    if !s.starts_with("set") { return None; }
    let rest = &s[3..];
    let space = rest.find(' ')?;
    let cc = &rest[..space];
    // Validate it's a real condition code
    match cc {
        "e" | "ne" | "z" | "nz" | "l" | "le" | "g" | "ge" |
        "b" | "be" | "a" | "ae" | "s" | "ns" | "o" | "no" |
        "p" | "np" => Some(cc),
        _ => None,
    }
}

/// Extract the jump target from a jmp instruction.
fn parse_jmp_target(s: &str) -> Option<&str> {
    s.strip_prefix("jmp ")
}

// ── Line store ───────────────────────────────────────────────────────────────

/// Efficient line storage that avoids reallocating strings.
/// Lines are stored as byte offsets into the original assembly string.
/// Replaced lines are stored in a side buffer.
struct LineStore {
    original: String,
    /// (start, end) byte offsets into `original` for each line.
    /// If start == usize::MAX, the line has been replaced and lives in `replacements`.
    entries: Vec<(usize, usize)>,
    /// Replacement strings, keyed by line index.
    replacements: Vec<(usize, String)>,
}

impl LineStore {
    fn new(asm: String) -> Self {
        let mut entries = Vec::new();
        let mut start = 0;
        for (i, b) in asm.bytes().enumerate() {
            if b == b'\n' {
                entries.push((start, i));
                start = i + 1;
            }
        }
        if start < asm.len() {
            entries.push((start, asm.len()));
        }
        LineStore { original: asm, entries, replacements: Vec::new() }
    }

    fn len(&self) -> usize { self.entries.len() }

    fn get(&self, idx: usize) -> &str {
        let (start, end) = self.entries[idx];
        if start == usize::MAX {
            // Find in replacements
            for (ri, ref s) in &self.replacements {
                if *ri == idx { return s; }
            }
            ""
        } else {
            &self.original[start..end]
        }
    }

    fn replace(&mut self, idx: usize, new_line: String) {
        self.entries[idx] = (usize::MAX, 0);
        // Check if there's already a replacement for this index
        for (ri, ref mut s) in &mut self.replacements {
            if *ri == idx {
                *s = new_line;
                return;
            }
        }
        self.replacements.push((idx, new_line));
    }

    fn build_result(&self, infos: &[LineInfo]) -> String {
        let mut result = String::with_capacity(self.original.len());
        for (i, info) in infos.iter().enumerate() {
            if info.is_nop() { continue; }
            let line = self.get(i);
            result.push_str(line);
            result.push('\n');
        }
        result
    }
}

// ── Trimmed line helper ──────────────────────────────────────────────────────

#[inline]
fn trimmed<'a>(store: &'a LineStore, info: &LineInfo, idx: usize) -> &'a str {
    &store.get(idx)[info.trim_start as usize..]
}

// ── Pass 1: Local patterns ───────────────────────────────────────────────────

/// Combined local pass: scan once, apply multiple patterns.
fn combined_local_pass(store: &mut LineStore, infos: &mut [LineInfo]) -> bool {
    let len = infos.len();
    let mut changed = false;

    let mut i = 0;
    while i < len {
        if infos[i].is_nop() { i += 1; continue; }

        // Pattern 1: Self-move elimination
        if infos[i].kind == LineKind::SelfMove {
            infos[i].kind = LineKind::Nop;
            changed = true;
            i += 1;
            continue;
        }

        // Find next non-nop line
        let mut j = i + 1;
        while j < len && infos[j].is_nop() { j += 1; }
        if j >= len { i += 1; continue; }

        // Pattern 2: Adjacent store/load with same offset
        if let LineKind::StoreEbp { reg: store_reg, offset: store_off, size: store_size } = infos[i].kind {
            if let LineKind::LoadEbp { reg: load_reg, offset: load_off, size: load_size } = infos[j].kind {
                if store_off == load_off && store_size == load_size {
                    if store_reg == load_reg {
                        // movl %eax, -8(%ebp); movl -8(%ebp), %eax → keep store only
                        infos[j].kind = LineKind::Nop;
                        changed = true;
                        i += 1;
                        continue;
                    } else {
                        // movl %eax, -8(%ebp); movl -8(%ebp), %ecx → movl %eax, -8(%ebp); movl %eax, %ecx
                        let new_line = format!("    {} {}, {}", store_size.mnemonic(), reg32_name(store_reg), reg32_name(load_reg));
                        store.replace(j, new_line);
                        infos[j] = LineInfo {
                            kind: LineKind::Move { dst: load_reg, src: store_reg },
                            trim_start: 4,
                            has_indirect_mem: false,
                            ebp_offset: EBP_OFFSET_NONE,
                        };
                        changed = true;
                        i += 1;
                        continue;
                    }
                }
            }
        }

        // Pattern 3: Redundant jump to next label
        if infos[i].kind == LineKind::Jmp {
            if infos[j].kind == LineKind::Label {
                let jmp_s = trimmed(store, &infos[i], i);
                let label_s = trimmed(store, &infos[j], j);
                if let Some(target) = parse_jmp_target(jmp_s) {
                    if let Some(label_name) = label_s.strip_suffix(':') {
                        if target.trim() == label_name {
                            infos[i].kind = LineKind::Nop;
                            changed = true;
                            i += 1;
                            continue;
                        }
                    }
                }
            }
        }

        // Pattern 4: Branch inversion: jCC .L1; jmp .L2; .L1: → j!CC .L2; .L1:
        if infos[i].kind == LineKind::CondJmp {
            let mut k = j + 1;
            while k < len && infos[k].is_nop() { k += 1; }
            if k < len && infos[j].kind == LineKind::Jmp && infos[k].kind == LineKind::Label {
                let cond_s = trimmed(store, &infos[i], i);
                let jmp_s = trimmed(store, &infos[j], j);
                let label_s = trimmed(store, &infos[k], k);
                if let (Some((cc, cond_target)), Some(jmp_target)) =
                    (parse_condjmp(cond_s), parse_jmp_target(jmp_s))
                {
                    if let Some(label_name) = label_s.strip_suffix(':') {
                        if cond_target == label_name {
                            if let Some(inv_cc) = invert_cc(cc) {
                                let new_line = format!("    j{} {}", inv_cc, jmp_target.trim());
                                store.replace(i, new_line);
                                infos[i].kind = LineKind::CondJmp;
                                infos[j].kind = LineKind::Nop;
                                changed = true;
                                i += 1;
                                continue;
                            }
                        }
                    }
                }
            }
        }

        // Pattern 5: Reverse move elimination: movl %A, %B; movl %B, %A → keep first only
        if let LineKind::Move { dst: dst1, src: src1 } = infos[i].kind {
            if let LineKind::Move { dst: dst2, src: src2 } = infos[j].kind {
                if dst1 == src2 && src1 == dst2 {
                    infos[j].kind = LineKind::Nop;
                    changed = true;
                    i += 1;
                    continue;
                }
            }
        }

        i += 1;
    }

    changed
}

// ── Pass 2: Global store forwarding ──────────────────────────────────────────

/// Track which register value is stored at each stack slot.
/// When we see `movl %eax, -8(%ebp)`, record that slot -8 contains eax.
/// When we see `movl -8(%ebp), %ecx`, forward to `movl %eax, %ecx` or eliminate if same reg.
// TODO: Disabled - causes 21 regressions in FP computation tests (matrix/FP operations
// produce wrong numerical results). Needs investigation into FP load/store forwarding patterns.
#[allow(dead_code)]
fn global_store_forwarding(store: &mut LineStore, infos: &mut [LineInfo]) -> bool {
    let len = infos.len();
    let mut changed = false;

    // Mapping: offset → (reg, line_idx)
    // Small flat array for common offsets (-256..0)
    const SLOT_COUNT: usize = 256;
    let mut slots: [(RegId, MoveSize); SLOT_COUNT] = [(REG_NONE, MoveSize::L); SLOT_COUNT];

    // Collect jump targets so we can invalidate at them
    let mut jump_targets = std::collections::HashSet::new();
    for i in 0..len {
        if infos[i].is_nop() { continue; }
        let s = trimmed(store, &infos[i], i);
        match infos[i].kind {
            LineKind::Jmp | LineKind::JmpIndirect => {
                if let Some(target) = parse_jmp_target(s) {
                    jump_targets.insert(target.trim().to_string());
                }
            }
            LineKind::CondJmp => {
                if let Some((_, target)) = parse_condjmp(s) {
                    jump_targets.insert(target.to_string());
                }
            }
            _ => {}
        }
    }

    for i in 0..len {
        if infos[i].is_nop() { continue; }

        match infos[i].kind {
            LineKind::Label => {
                // Check if this label is a jump target (invalidate all)
                let s = trimmed(store, &infos[i], i);
                if let Some(name) = s.strip_suffix(':') {
                    if jump_targets.contains(name) {
                        // This label is a jump target - invalidate all mappings
                        slots = [(REG_NONE, MoveSize::L); SLOT_COUNT];
                    }
                    // If it's just a fallthrough label, keep mappings
                }
            }
            LineKind::StoreEbp { reg, offset, size } => {
                // Record that this slot now contains this register's value
                if offset < 0 && (-offset as usize) <= SLOT_COUNT {
                    slots[(-offset - 1) as usize] = (reg, size);
                }
            }
            LineKind::LoadEbp { reg: load_reg, offset, size: load_size } => {
                // Check if we know what register value is in this slot
                let mut forwarded = false;
                if offset < 0 && (-offset as usize) <= SLOT_COUNT {
                    let (stored_reg, stored_size) = slots[(-offset - 1) as usize];
                    if stored_reg != REG_NONE && stored_size == load_size {
                        if stored_reg == load_reg {
                            // Same register - just eliminate the load
                            infos[i].kind = LineKind::Nop;
                            changed = true;
                            forwarded = true;
                        } else {
                            // Different register - forward as reg-reg move
                            let new_line = format!("    {} {}, {}", load_size.mnemonic(), reg32_name(stored_reg), reg32_name(load_reg));
                            store.replace(i, new_line);
                            infos[i] = LineInfo {
                                kind: LineKind::Move { dst: load_reg, src: stored_reg },
                                trim_start: 4,
                                has_indirect_mem: false,
                                ebp_offset: EBP_OFFSET_NONE,
                            };
                            changed = true;
                            forwarded = true;
                        }
                    }
                }
                // The load writes to load_reg, so invalidate any slot
                // that maps to load_reg (its value has changed).
                // This must happen even if we forwarded, because the
                // destination register now has a new value.
                for slot in slots.iter_mut() {
                    if slot.0 == load_reg {
                        *slot = (REG_NONE, MoveSize::L);
                    }
                }
                if forwarded { continue; }
            }
            LineKind::Call => {
                // Calls clobber caller-saved registers (eax, ecx, edx)
                // Invalidate all mappings involving these registers
                for slot in slots.iter_mut() {
                    if is_caller_saved(slot.0) {
                        *slot = (REG_NONE, MoveSize::L);
                    }
                }
            }
            LineKind::Jmp | LineKind::JmpIndirect | LineKind::Ret => {
                // Control flow change - invalidate all
                slots = [(REG_NONE, MoveSize::L); SLOT_COUNT];
            }
            LineKind::Move { dst, .. } => {
                // Invalidate any slot that was mapped to the overwritten register
                for slot in slots.iter_mut() {
                    if slot.0 == dst {
                        *slot = (REG_NONE, MoveSize::L);
                    }
                }
            }
            LineKind::SetCC { reg } => {
                // setCC modifies a byte register, invalidate its family
                for slot in slots.iter_mut() {
                    if slot.0 == reg {
                        *slot = (REG_NONE, MoveSize::L);
                    }
                }
            }
            LineKind::Other { dest_reg } => {
                // Invalidate any slot mapped to the destination register
                if dest_reg != REG_NONE {
                    for slot in slots.iter_mut() {
                        if slot.0 == dest_reg {
                            *slot = (REG_NONE, MoveSize::L);
                        }
                    }
                }
                // If line has indirect memory access or might clobber stack,
                // invalidate all (conservative)
                let s = trimmed(store, &infos[i], i);
                if infos[i].has_indirect_mem || s.contains("(%ebp)") {
                    // Only invalidate the specific slot if we can parse it
                    let off = infos[i].ebp_offset;
                    if off != EBP_OFFSET_NONE && off < 0 && (-off as usize) <= SLOT_COUNT {
                        slots[(-off - 1) as usize] = (REG_NONE, MoveSize::L);
                    } else if infos[i].has_indirect_mem {
                        // Indirect memory - could write anywhere, invalidate all
                        slots = [(REG_NONE, MoveSize::L); SLOT_COUNT];
                    }
                }
                // Check for inline asm or instructions that clobber multiple regs
                if s.contains(';') || s.starts_with("rdmsr") || s.starts_with("cpuid")
                    || s.starts_with("syscall") || s.starts_with("int ") || s.starts_with("int$")
                    || s.starts_with("rep") || s.starts_with("cld") {
                    slots = [(REG_NONE, MoveSize::L); SLOT_COUNT];
                }
            }
            LineKind::Push { .. } | LineKind::Pop { .. } => {
                // Push/pop modify esp but don't affect ebp-relative slots
                if let LineKind::Pop { reg } = infos[i].kind {
                    // Pop writes to a register, invalidate mappings
                    for slot in slots.iter_mut() {
                        if slot.0 == reg {
                            *slot = (REG_NONE, MoveSize::L);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    changed
}

// ── Pass: Dead store elimination ─────────────────────────────────────────────

/// Remove stores to stack slots that are immediately overwritten.
fn eliminate_dead_stores(store: &LineStore, infos: &mut [LineInfo]) -> bool {
    let len = infos.len();
    let mut changed = false;
    const WINDOW: usize = 16;

    for i in 0..len {
        if infos[i].is_nop() { continue; }
        if let LineKind::StoreEbp { offset: store_off, size: store_size, reg: store_reg } = infos[i].kind {
            // Look ahead for another store to the same slot (meaning this one is dead)
            // or a load from the same slot (meaning this one is alive)
            let mut j = i + 1;
            let mut count = 0;
            while j < len && count < WINDOW {
                if infos[j].is_nop() { j += 1; continue; }

                match infos[j].kind {
                    LineKind::StoreEbp { offset, size, .. } if offset == store_off && size == store_size => {
                        // Another store to the same slot - this store is dead
                        infos[i].kind = LineKind::Nop;
                        changed = true;
                        break;
                    }
                    LineKind::LoadEbp { offset, size, .. } if offset == store_off && size == store_size => {
                        // Load from same slot - this store is alive
                        break;
                    }
                    _ => {}
                }

                // Stop at barriers
                if infos[j].is_barrier() { break; }
                // Stop if the stored register is modified (value may have changed)
                match infos[j].kind {
                    LineKind::Other { dest_reg } if dest_reg == store_reg => break,
                    LineKind::Move { dst, .. } if dst == store_reg => break,
                    LineKind::SetCC { reg } if reg == store_reg => break,
                    _ => {}
                }
                // Stop at indirect memory access (could read the slot)
                let s = trimmed(store, &infos[j], j);
                if infos[j].has_indirect_mem { break; }
                // If line references ebp with same offset, it's alive
                if infos[j].ebp_offset == store_off { break; }
                // leaq N(%ebp) takes address of slot
                if s.contains("(%ebp)") && !matches!(infos[j].kind, LineKind::StoreEbp { .. } | LineKind::LoadEbp { .. }) {
                    break;
                }

                j += 1;
                count += 1;
            }
        }
    }

    changed
}

// ── Pass: Dead register move elimination ─────────────────────────────────────

/// Remove register moves where the destination is overwritten before being read.
fn eliminate_dead_reg_moves(store: &LineStore, infos: &mut [LineInfo]) -> bool {
    let len = infos.len();
    let mut changed = false;
    const WINDOW: usize = 16;

    for i in 0..len {
        if infos[i].is_nop() { continue; }
        let dst_reg = match infos[i].kind {
            LineKind::Move { dst, .. } => dst,
            _ => continue,
        };
        // Don't eliminate moves to esp/ebp
        if dst_reg == REG_ESP || dst_reg == REG_EBP { continue; }

        // Look ahead: if dst is overwritten before being read, this move is dead
        let mut j = i + 1;
        let mut count = 0;
        while j < len && count < WINDOW {
            if infos[j].is_nop() { j += 1; continue; }
            if infos[j].is_barrier() { break; }

            // Check if dst is read by this instruction
            let s = trimmed(store, &infos[j], j);
            match infos[j].kind {
                LineKind::StoreEbp { reg, .. } if reg == dst_reg => {
                    // dst is read (stored to stack) - move is alive
                    break;
                }
                LineKind::Move { src, dst } => {
                    if src == dst_reg {
                        // dst is read - move is alive
                        break;
                    }
                    if dst == dst_reg {
                        // dst is overwritten - this move is dead
                        infos[i].kind = LineKind::Nop;
                        changed = true;
                        break;
                    }
                }
                LineKind::Other { dest_reg } => {
                    if dest_reg == dst_reg && !line_references_reg(s, dst_reg) {
                        // Destination only writes to dst, doesn't read it: dead
                        // But we need to make sure it doesn't ALSO read it
                        // Actually, just check if the line references the reg at all
                        // For "movl $5, %eax", dest is eax and it doesn't read eax
                        // For "addl $5, %eax", dest is eax and it reads eax
                        // parse_dest_reg returns the last operand. If the instruction writes
                        // to dst_reg but the source doesn't reference it, then this move is dead.
                        infos[i].kind = LineKind::Nop;
                        changed = true;
                        break;
                    }
                    if line_references_reg(s, dst_reg) {
                        break; // dst is read
                    }
                }
                _ => {
                    if line_references_reg(s, dst_reg) {
                        break; // dst is read
                    }
                }
            }

            j += 1;
            count += 1;
        }
    }

    changed
}

// ── Pass: Compare and branch fusion ──────────────────────────────────────────

/// Fuse `cmpl/testl + setCC %al + movzbl %al, %eax + testl %eax, %eax + jne/je`
/// into a single `jCC`/`j!CC` directly.
fn fuse_compare_and_branch(store: &mut LineStore, infos: &mut [LineInfo]) -> bool {
    let len = infos.len();
    let mut changed = false;

    let mut i = 0;
    while i < len {
        if infos[i].is_nop() { i += 1; continue; }
        if infos[i].kind != LineKind::Cmp {
            i += 1;
            continue;
        }

        // Look for: cmp/test + setCC %al + movzbl %al, %eax + testl %eax, %eax + jne/je
        let mut positions = [0usize; 5]; // cmp, setCC, movzbl, test, jCC
        positions[0] = i;
        let mut next = i;
        // Find setCC
        next = next_non_nop(infos, next + 1);
        if next >= len { i += 1; continue; }
        let setcc_cc = if let LineKind::SetCC { reg: REG_EAX } = infos[next].kind {
            let s = trimmed(store, &infos[next], next);
            parse_setcc(s)
        } else { None };
        if setcc_cc.is_none() { i += 1; continue; }
        let setcc_cc = setcc_cc.unwrap();
        positions[1] = next;

        // Find movzbl %al, %eax
        next = next_non_nop(infos, next + 1);
        if next >= len { i += 1; continue; }
        let s = trimmed(store, &infos[next], next);
        if s != "movzbl %al, %eax" { i += 1; continue; }
        positions[2] = next;

        // Handle optional store/load pair between movzbl and test
        next = next_non_nop(infos, next + 1);
        if next >= len { i += 1; continue; }

        // Check for store to ebp (cross-block save of the boolean)
        let has_cross_block_store = matches!(infos[next].kind, LineKind::StoreEbp { reg: REG_EAX, .. });
        if has_cross_block_store {
            // Can't fuse if the boolean is saved for use elsewhere
            i += 1;
            continue;
        }

        // Find testl %eax, %eax
        let s = trimmed(store, &infos[next], next);
        if s != "testl %eax, %eax" { i += 1; continue; }
        positions[3] = next;

        // Find jne/je
        next = next_non_nop(infos, next + 1);
        if next >= len { i += 1; continue; }
        if infos[next].kind != LineKind::CondJmp { i += 1; continue; }
        let jmp_s = trimmed(store, &infos[next], next);
        let (jmp_cc, jmp_target) = match parse_condjmp(jmp_s) {
            Some(x) => x,
            None => { i += 1; continue; }
        };
        positions[4] = next;

        // Compute the fused condition
        let fused_cc = match jmp_cc {
            "ne" => setcc_cc,
            "e" => match invert_cc(setcc_cc) {
                Some(inv) => inv,
                None => { i += 1; continue; }
            },
            _ => { i += 1; continue; }
        };

        // Replace: eliminate setCC, movzbl, test; replace jne/je with jCC
        let new_jmp = format!("    j{} {}", fused_cc, jmp_target);
        store.replace(positions[4], new_jmp);
        // Mark intermediate lines as NOP
        infos[positions[1]].kind = LineKind::Nop; // setCC
        infos[positions[2]].kind = LineKind::Nop; // movzbl
        infos[positions[3]].kind = LineKind::Nop; // test
        // Keep the cmp and the new fused jCC
        changed = true;
        i = positions[4] + 1;
    }

    changed
}

// ── Pass: Memory operand folding ─────────────────────────────────────────────

/// Fold `movl -N(%ebp), %ecx; addl %ecx, %eax` into `addl -N(%ebp), %eax`.
fn fold_memory_operands(store: &mut LineStore, infos: &mut [LineInfo]) -> bool {
    let len = infos.len();
    let mut changed = false;

    let mut i = 0;
    while i < len {
        if infos[i].is_nop() { i += 1; continue; }

        // Look for load from stack slot
        if let LineKind::LoadEbp { reg: load_reg, offset, size } = infos[i].kind {
            // Only fold scratch registers (eax, ecx, edx)
            if !is_caller_saved(load_reg) && load_reg != REG_EAX {
                i += 1; continue;
            }

            // Find next non-nop instruction
            let j = next_non_nop(infos, i + 1);
            if j >= len { i += 1; continue; }

            // Check if next instruction uses this register as a source operand
            // Pattern: load into %ecx, then `addl %ecx, %eax` etc.
            let s = trimmed(store, &infos[j], j);
            if let Some(folded) = try_fold_memory_operand(s, load_reg, offset, size) {
                store.replace(j, format!("    {}", folded));
                let dest_reg = parse_dest_reg(&folded);
                infos[j] = LineInfo {
                    kind: LineKind::Other { dest_reg },
                    trim_start: 4,
                    has_indirect_mem: false,
                    ebp_offset: offset,
                };
                infos[i].kind = LineKind::Nop; // Remove the load
                changed = true;
            }
        }
        i += 1;
    }

    changed
}

/// Try to fold a stack slot into an ALU instruction.
/// Returns the folded instruction string if successful.
fn try_fold_memory_operand(s: &str, load_reg: RegId, offset: i32, _size: MoveSize) -> Option<String> {
    let reg_name = reg32_name(load_reg);

    // Try patterns: `OPCODE %load_reg, %other_reg`
    for op in &["addl", "subl", "andl", "orl", "xorl", "cmpl", "testl", "imull"] {
        if let Some(rest) = s.strip_prefix(op) {
            let rest = rest.trim();
            // Pattern: `%load_reg, %dst` → `OPCODE offset(%ebp), %dst`
            if let Some(after) = rest.strip_prefix(reg_name) {
                let after = after.trim();
                if after.starts_with(',') {
                    let dst = after[1..].trim();
                    if dst.starts_with('%') && !dst.contains('(') {
                        // Don't fold if dst is the same as load_reg (would be read after free)
                        if register_family(dst) != load_reg {
                            return Some(format!("{} {}(%ebp), {}", op, offset, dst));
                        }
                    }
                }
            }
        }
    }

    None
}

// ── Pass: Never-read store elimination ───────────────────────────────────────

/// Global pass: find stack slots that are never loaded and remove all stores to them.
fn eliminate_never_read_stores(store: &LineStore, infos: &mut [LineInfo]) {
    let len = infos.len();

    // Collect all loaded offsets
    let mut loaded_offsets = std::collections::HashSet::new();
    let mut addr_taken = false;

    for i in 0..len {
        if infos[i].is_nop() { continue; }
        match infos[i].kind {
            LineKind::LoadEbp { offset, .. } => { loaded_offsets.insert(offset); }
            _ => {
                // Check for address-of-slot patterns (leal N(%ebp), %reg)
                let s = trimmed(store, &infos[i], i);
                if s.starts_with("leal ") && s.contains("(%ebp)") {
                    addr_taken = true;
                }
                // Indirect memory access means we can't know what's read
                if infos[i].has_indirect_mem {
                    addr_taken = true;
                }
            }
        }
    }

    if addr_taken { return; }

    // Remove stores to slots that are never loaded
    for i in 0..len {
        if infos[i].is_nop() { continue; }
        if let LineKind::StoreEbp { offset, .. } = infos[i].kind {
            if !loaded_offsets.contains(&offset) {
                infos[i].kind = LineKind::Nop;
            }
        }
    }
}

// ── Pass: Unused callee-saved register elimination ───────────────────────────

/// Remove pushl/popl of callee-saved registers that are never referenced in the function body.
// TODO: Disabled - buggy leal -N(%ebp),%esp adjustment causes stack misalignment and 97+
// segfault regressions. Needs proper understanding of frame layout before re-enabling.
#[allow(dead_code)]
fn eliminate_unused_callee_saves(store: &LineStore, infos: &mut [LineInfo]) {
    let len = infos.len();

    // Find function boundaries
    let mut func_start = 0;
    for i in 0..len {
        if infos[i].is_nop() { continue; }
        // Look for the prologue pattern: pushl %ebp; movl %esp, %ebp
        if let LineKind::Push { reg: REG_EBP } = infos[i].kind {
            func_start = i;
            break;
        }
        if infos[i].kind == LineKind::Label {
            let s = trimmed(store, &infos[i], i);
            if s.ends_with(':') && !s.starts_with('.') {
                func_start = i;
            }
        }
    }

    // Identify callee-saved registers that are pushed in the prologue
    // and check if they're used in the function body
    for reg in [REG_EBX, REG_ESI, REG_EDI] {
        // Find the push of this register
        let mut push_idx = None;
        let mut pop_idx = None;
        let mut used = false;

        for i in func_start..len {
            if infos[i].is_nop() { continue; }
            match infos[i].kind {
                LineKind::Push { reg: r } if r == reg && push_idx.is_none() => {
                    push_idx = Some(i);
                }
                LineKind::Pop { reg: r } if r == reg => {
                    pop_idx = Some(i);
                }
                _ => {
                    if push_idx.is_some() && pop_idx.is_none() {
                        // Check if the register is referenced in the body
                        let s = trimmed(store, &infos[i], i);
                        if line_references_reg(s, reg) {
                            used = true;
                        }
                    }
                }
            }
        }

        if !used {
            if let Some(pi) = push_idx {
                infos[pi].kind = LineKind::Nop;
                if let Some(qi) = pop_idx {
                    infos[qi].kind = LineKind::Nop;
                }
                // For noreturn functions (no pop), still eliminate the push
            }
        }
    }
}

// ── Pass: Push/pop elimination ───────────────────────────────────────────────

/// Eliminate push/pop pairs where the register is not modified between them.
// TODO: Disabled - removes function-level callee-save push/pops which breaks the
// leal -12(%ebp),%esp epilogue pattern. Needs awareness of function boundaries.
#[allow(dead_code)]
fn eliminate_push_pop_pairs(store: &LineStore, infos: &mut [LineInfo]) -> bool {
    let len = infos.len();
    let mut changed = false;

    for i in 0..len {
        if infos[i].is_nop() { continue; }
        let push_reg = match infos[i].kind {
            LineKind::Push { reg } if reg <= REG_GP_MAX => reg,
            _ => continue,
        };

        // Scan forward for matching pop, checking reg is unmodified
        let mut j = i + 1;
        let mut depth = 0; // Track nested push/pops
        let mut safe = true;
        while j < len {
            if infos[j].is_nop() { j += 1; continue; }

            match infos[j].kind {
                LineKind::Push { .. } => { depth += 1; }
                LineKind::Pop { reg } if depth > 0 => { depth -= 1; }
                LineKind::Pop { reg } if reg == push_reg && depth == 0 => {
                    // Found matching pop
                    if safe {
                        infos[i].kind = LineKind::Nop;
                        infos[j].kind = LineKind::Nop;
                        changed = true;
                    }
                    break;
                }
                LineKind::Pop { .. } if depth == 0 => { break; } // Different register popped
                _ => {}
            }

            // Check if the register is modified
            match infos[j].kind {
                LineKind::Move { dst, .. } if dst == push_reg => { safe = false; }
                LineKind::LoadEbp { reg, .. } if reg == push_reg => { safe = false; }
                LineKind::Other { dest_reg } if dest_reg == push_reg => { safe = false; }
                LineKind::Other { .. } => {
                    // For unknown instructions, check if the raw text references the register
                    // or if it's an instruction that implicitly clobbers registers (rep movsb, etc.)
                    let raw = store.get(j).trim();
                    if raw.starts_with("rep") || raw.starts_with("cld") {
                        // rep movsb/movsl/stosb etc. clobber esi, edi, ecx
                        if push_reg == REG_ESI || push_reg == REG_EDI || push_reg == REG_ECX {
                            safe = false;
                        }
                    }
                    if line_references_reg(raw, push_reg) {
                        safe = false;
                    }
                }
                LineKind::Call => { if is_caller_saved(push_reg) { safe = false; } }
                LineKind::SetCC { reg } if reg == push_reg => { safe = false; }
                _ => {}
            }

            // Stop at barriers that change control flow
            if matches!(infos[j].kind, LineKind::Jmp | LineKind::JmpIndirect | LineKind::Ret | LineKind::Label) {
                break;
            }

            j += 1;
        }
    }

    changed
}

// ── Utility ──────────────────────────────────────────────────────────────────

/// Find the next non-nop line after index `start`.
fn next_non_nop(infos: &[LineInfo], start: usize) -> usize {
    let mut i = start;
    while i < infos.len() && (infos[i].is_nop() || infos[i].kind == LineKind::Empty) {
        i += 1;
    }
    i
}

// ── Main entry point ─────────────────────────────────────────────────────────

/// Run peephole optimization on i686 assembly text.
/// Returns the optimized assembly string.
pub fn peephole_optimize(asm: String) -> String {
    let mut store = LineStore::new(asm);
    let line_count = store.len();
    let mut infos: Vec<LineInfo> = (0..line_count).map(|i| classify_line(store.get(i))).collect();

    // Phase 1: Iterative local passes
    let mut changed = true;
    let mut pass_count = 0;
    while changed && pass_count < MAX_LOCAL_PASS_ITERATIONS {
        changed = false;
        changed |= combined_local_pass(&mut store, &mut infos);
        pass_count += 1;
    }

    // Phase 2: Global passes (run once)
    let global_changed = eliminate_dead_reg_moves(&store, &mut infos);
    let global_changed = global_changed | eliminate_dead_stores(&store, &mut infos);
    let global_changed = global_changed | fuse_compare_and_branch(&mut store, &mut infos);
    let global_changed = global_changed | fold_memory_operands(&mut store, &mut infos);

    // Phase 3: Local cleanup after global passes
    if global_changed {
        let mut changed2 = true;
        let mut pass_count2 = 0;
        while changed2 && pass_count2 < MAX_POST_GLOBAL_ITERATIONS {
            changed2 = false;
            changed2 |= combined_local_pass(&mut store, &mut infos);
            changed2 |= eliminate_dead_reg_moves(&store, &mut infos);
            changed2 |= eliminate_dead_stores(&store, &mut infos);
            changed2 |= fold_memory_operands(&mut store, &mut infos);
            pass_count2 += 1;
        }
    }

    // Phase 4: Never-read store elimination
    eliminate_never_read_stores(&store, &mut infos);

    store.build_result(&infos)
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_redundant_store_load() {
        let asm = "    movl %eax, -8(%ebp)\n    movl -8(%ebp), %eax\n".to_string();
        let result = peephole_optimize(asm);
        assert_eq!(result.trim(), "movl %eax, -8(%ebp)");
    }

    #[test]
    fn test_store_load_different_reg() {
        let asm = "    movl %eax, -8(%ebp)\n    movl -8(%ebp), %ecx\n".to_string();
        let result = peephole_optimize(asm);
        assert!(result.contains("movl %eax, %ecx"), "should forward: {}", result);
        assert!(!result.contains("-8(%ebp), %ecx"), "should eliminate load: {}", result);
    }

    #[test]
    fn test_self_move() {
        let asm = "    movl %eax, %eax\n".to_string();
        let result = peephole_optimize(asm);
        assert_eq!(result.trim(), "");
    }

    #[test]
    fn test_redundant_jump() {
        let asm = "    jmp .Lfoo\n.Lfoo:\n".to_string();
        let result = peephole_optimize(asm);
        assert!(!result.contains("jmp"), "should eliminate redundant jmp: {}", result);
        assert!(result.contains(".Lfoo:"), "should keep label: {}", result);
    }

    #[test]
    fn test_branch_inversion() {
        let asm = [
            "    jl .LBB2",
            "    jmp .LBB4",
            ".LBB2:",
            "    movl %eax, %ecx",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("jge .LBB4"), "should invert to jge: {}", result);
        assert!(!result.contains("jmp .LBB4"), "should remove jmp: {}", result);
    }

    #[test]
    fn test_compare_branch_fusion() {
        let asm = [
            "    cmpl %ecx, %eax",
            "    setl %al",
            "    movzbl %al, %eax",
            "    testl %eax, %eax",
            "    jne .LBB2",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("jl .LBB2"), "should fuse to jl: {}", result);
        assert!(!result.contains("setl"), "should eliminate setl: {}", result);
    }

    #[test]
    fn test_dead_store() {
        let asm = [
            "    movl %eax, -8(%ebp)",
            "    movl %ecx, -8(%ebp)",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(!result.contains("%eax, -8(%ebp)"), "first store dead: {}", result);
        assert!(result.contains("%ecx, -8(%ebp)"), "second store alive: {}", result);
    }

    #[test]
    fn test_memory_fold() {
        let asm = [
            "    movl -48(%ebp), %ecx",
            "    addl %ecx, %eax",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert!(result.contains("addl -48(%ebp), %eax"), "should fold: {}", result);
    }

    // Note: store forwarding tests removed - global_store_forwarding is disabled
    // due to FP computation regressions.

    #[test]
    fn test_reverse_move_elimination() {
        let asm = [
            "    movl %eax, %ecx",
            "    movl %ecx, %eax",
        ].join("\n") + "\n";
        let result = peephole_optimize(asm);
        assert_eq!(result.matches("movl").count(), 1, "should eliminate reverse: {}", result);
    }

    // Note: push/pop elimination test removed - eliminate_push_pop_pairs is disabled
    // due to callee-save/leal epilogue interactions.
}
