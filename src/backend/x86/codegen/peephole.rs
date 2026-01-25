//! x86-64 peephole optimizer for assembly text.
//!
//! This pass operates on the generated assembly text, scanning for and eliminating
//! common redundant instruction patterns that arise from the stack-based codegen:
//!
//! 1. Redundant store/load: `movq %rax, N(%rbp)` followed by `movq N(%rbp), %rax`
//!    -> eliminates the load (value is already in %rax)
//!
//! 2. Store then load to different reg: `movq %rax, N(%rbp)` followed by
//!    `movq N(%rbp), %rcx` -> replaces load with `movq %rax, %rcx`
//!
//! 3. Redundant jump: `jmp .Lfoo` where `.Lfoo:` is the very next label
//!    -> eliminates the jump
//!
//! 4. Push/pop elimination: `pushq %rax` / `movq %rax, %rcx` / `popq %rax`
//!    -> replaces with `movq %rax, %rcx` (saves the push/pop pair when the
//!    pushed value is immediately restored)

/// Run peephole optimization on x86-64 assembly text.
/// Returns the optimized assembly string.
pub fn peephole_optimize(asm: String) -> String {
    let mut lines: Vec<String> = asm.lines().map(|s| s.to_string()).collect();
    let mut changed = true;

    // Run multiple passes since eliminating one pattern may enable another
    let mut pass_count = 0;
    while changed && pass_count < 5 {
        changed = false;
        changed |= eliminate_redundant_store_load(&mut lines);
        changed |= eliminate_store_load_different_reg(&mut lines);
        changed |= eliminate_redundant_jumps(&mut lines);
        changed |= eliminate_push_pop_pairs(&mut lines);
        changed |= eliminate_binop_push_pop_pattern(&mut lines);
        changed |= eliminate_redundant_movq_self(&mut lines);
        pass_count += 1;
    }

    // Remove NOP markers and rebuild
    lines.retain(|l| l != "<<NOP>>");

    let mut result = String::with_capacity(asm.len());
    for line in &lines {
        result.push_str(line);
        result.push('\n');
    }
    result
}

/// Pattern 1: Eliminate redundant store followed by load of the same value.
/// `movq %REG, N(%rbp)` followed by `movq N(%rbp), %REG` -> remove the load.
/// Also handles movl, movb, movw variants.
fn eliminate_redundant_store_load(lines: &mut [String]) -> bool {
    let mut changed = false;
    let len = lines.len();

    for i in 0..len.saturating_sub(1) {
        if lines[i] == "<<NOP>>" || lines[i + 1] == "<<NOP>>" {
            continue;
        }

        let store_line = lines[i].trim();
        let load_line = lines[i + 1].trim();

        // Check: movX %reg, offset(%rbp) followed by movX offset(%rbp), %reg
        if let Some((store_reg, store_offset, store_size)) = parse_store_to_rbp(store_line) {
            if let Some((load_offset, load_reg, load_size)) = parse_load_from_rbp(load_line) {
                if store_offset == load_offset && store_reg == load_reg && store_size == load_size {
                    // The register already has the value; remove the load
                    lines[i + 1] = "<<NOP>>".to_string();
                    changed = true;
                }
            }
        }
    }
    changed
}

/// Pattern 2: Store then load from same offset to different register.
/// `movq %rax, N(%rbp)` followed by `movq N(%rbp), %rcx`
/// -> replace load with `movq %rax, %rcx` (keep the store for other uses)
fn eliminate_store_load_different_reg(lines: &mut [String]) -> bool {
    let mut changed = false;
    let len = lines.len();

    for i in 0..len.saturating_sub(1) {
        if lines[i] == "<<NOP>>" || lines[i + 1] == "<<NOP>>" {
            continue;
        }

        let store_line = lines[i].trim();
        let load_line = lines[i + 1].trim();

        if let Some((store_reg, store_offset, store_size)) = parse_store_to_rbp(store_line) {
            if let Some((load_offset, load_reg, load_size)) = parse_load_from_rbp(load_line) {
                if store_offset == load_offset && store_reg != load_reg && store_size == load_size {
                    // Replace load with reg-to-reg move
                    let mov = match store_size {
                        MoveSize::Q => "movq",
                        MoveSize::L => "movl",
                        MoveSize::W => "movw",
                        MoveSize::B => "movb",
                        MoveSize::SLQ => "movslq",
                    };
                    lines[i + 1] = format!("    {} {}, {}", mov, store_reg, load_reg);
                    changed = true;
                }
            }
        }
    }
    changed
}

/// Pattern 3: Eliminate redundant jumps to the immediately following label.
/// `jmp .Lfoo` followed by `.Lfoo:` -> remove the jmp.
fn eliminate_redundant_jumps(lines: &mut [String]) -> bool {
    let mut changed = false;
    let len = lines.len();

    for i in 0..len.saturating_sub(1) {
        if lines[i] == "<<NOP>>" {
            continue;
        }

        let jmp_line = lines[i].trim();

        // Parse: jmp LABEL
        if let Some(target) = jmp_line.strip_prefix("jmp ") {
            let target = target.trim();
            // Find the next non-NOP, non-empty line
            for j in (i + 1)..len {
                let next = lines[j].trim();
                if next.is_empty() || next == "<<NOP>>" {
                    continue;
                }
                // Check if it's the target label
                if let Some(label) = next.strip_suffix(':') {
                    if label == target {
                        lines[i] = "<<NOP>>".to_string();
                        changed = true;
                    }
                }
                break;
            }
        }
    }
    changed
}

/// Pattern 4: Eliminate push/pop pairs where the pushed value is preserved.
/// `pushq %rax` / ... / `popq %rax` where the intermediate instructions
/// don't use %rax, and the pattern is `pushq %rax` / `movq %rax, %rcx` / `popq %rax`
/// (this is the standard binary op pattern) -> `movq %rax, %rcx`
fn eliminate_push_pop_pairs(lines: &mut [String]) -> bool {
    let mut changed = false;
    let len = lines.len();

    for i in 0..len.saturating_sub(2) {
        if lines[i] == "<<NOP>>" {
            continue;
        }

        let push_line = lines[i].trim();

        // Parse: pushq %REG
        if let Some(push_reg) = push_line.strip_prefix("pushq ") {
            let push_reg = push_reg.trim();
            if !push_reg.starts_with('%') {
                continue;
            }

            // Look for the matching popq within a small window
            for j in (i + 1)..std::cmp::min(i + 4, len) {
                if lines[j] == "<<NOP>>" {
                    continue;
                }
                let line = lines[j].trim();

                // Check for matching pop
                if let Some(pop_reg) = line.strip_prefix("popq ") {
                    let pop_reg = pop_reg.trim();
                    if pop_reg == push_reg {
                        // Check that intermediate instructions don't modify the pushed register
                        let mut safe = true;
                        for k in (i + 1)..j {
                            if lines[k] == "<<NOP>>" {
                                continue;
                            }
                            if instruction_modifies_reg(lines[k].trim(), push_reg) {
                                safe = false;
                                break;
                            }
                        }
                        if safe {
                            // Remove push and pop, keep intermediates
                            lines[i] = "<<NOP>>".to_string();
                            lines[j] = "<<NOP>>".to_string();
                            changed = true;
                        }
                    }
                    break; // Found a pop (matching or not), stop looking
                }
                // If we see a push, stop (nested push/pop)
                if line.starts_with("pushq ") || line.starts_with("push ") {
                    break;
                }
                // If we see a call/jmp/ret, stop
                if line.starts_with("call") || line.starts_with("jmp") || line == "ret" {
                    break;
                }
            }
        }
    }
    changed
}

/// Pattern 5: Eliminate the common binary-op push/pop pattern.
/// The codegen generates:
///   pushq %rax              ; save LHS
///   movq <somewhere>, %rax  ; load RHS into rax
///   movq %rax, %rcx         ; move RHS to rcx
///   popq %rax               ; restore LHS
///
/// This is replaced with:
///   movq <somewhere>, %rcx  ; load RHS directly into rcx
///
/// The key insight is that the push saves %rax, then %rax is loaded with a new value,
/// moved to %rcx, and the original %rax is restored. We can just load directly into %rcx.
fn eliminate_binop_push_pop_pattern(lines: &mut [String]) -> bool {
    let mut changed = false;
    let len = lines.len();

    let mut i = 0;
    while i + 3 < len {
        // Skip NOPs
        if lines[i] == "<<NOP>>" {
            i += 1;
            continue;
        }

        let push_line = lines[i].trim().to_string();

        // Match: pushq %REG
        if let Some(push_reg) = push_line.strip_prefix("pushq ") {
            let push_reg = push_reg.trim();
            if !push_reg.starts_with('%') {
                i += 1;
                continue;
            }

            // Find next 3 non-NOP lines after push
            let mut real_indices = Vec::new();
            let mut j = i + 1;
            while j < len && real_indices.len() < 3 {
                if lines[j] != "<<NOP>>" {
                    real_indices.push(j);
                }
                j += 1;
            }

            if real_indices.len() == 3 {
                let load_idx = real_indices[0];
                let move_idx = real_indices[1];
                let pop_idx = real_indices[2];

                let load_line = lines[load_idx].trim().to_string();
                let move_line = lines[move_idx].trim().to_string();
                let pop_line = lines[pop_idx].trim().to_string();

                // Check: the load instruction writes to push_reg
                // (e.g., `movq -8(%rbp), %rax` or `movq $5, %rax`)
                // Check: movq %push_reg, %other_reg
                // Check: popq %push_reg
                if let Some(pop_reg) = pop_line.strip_prefix("popq ") {
                    let pop_reg = pop_reg.trim();
                    if pop_reg == push_reg {
                        // Check the move: `movq %push_reg, %other`
                        if let Some(move_target) = parse_reg_to_reg_move(&move_line, push_reg) {
                            // Check the load writes to push_reg and can be safely redirected
                            if instruction_writes_to(&load_line, push_reg) && can_redirect_instruction(&load_line) {
                                // Transform: replace load's destination with move_target,
                                // remove the push, move, and pop
                                if let Some(new_load) = replace_dest_register(&load_line, push_reg, move_target) {
                                    lines[i] = "<<NOP>>".to_string(); // push
                                    lines[load_idx] = format!("    {}", new_load); // redirected load
                                    lines[move_idx] = "<<NOP>>".to_string(); // move
                                    lines[pop_idx] = "<<NOP>>".to_string(); // pop
                                    changed = true;
                                    i = pop_idx + 1;
                                    continue;
                                }
                            }
                        }
                    }
                }
            }
        }

        i += 1;
    }
    changed
}

/// Parse `movq %src, %dst` and return %dst if %src matches expected_src.
fn parse_reg_to_reg_move<'a>(line: &'a str, expected_src: &str) -> Option<&'a str> {
    for prefix in &["movq ", "movl "] {
        if let Some(rest) = line.strip_prefix(prefix) {
            if let Some((src, dst)) = rest.split_once(',') {
                let src = src.trim();
                let dst = dst.trim();
                if src == expected_src && dst.starts_with('%') {
                    return Some(dst);
                }
            }
        }
    }
    None
}

/// Check if an instruction writes to a specific register as its destination.
fn instruction_writes_to(inst: &str, reg: &str) -> bool {
    // Two-operand instructions: the destination is the last operand
    if let Some((_op, operands)) = inst.split_once(' ') {
        if let Some((_src, dst)) = operands.rsplit_once(',') {
            let dst = dst.trim();
            if dst == reg || register_overlaps(dst, reg) {
                return true;
            }
        }
    }
    false
}

/// Check if an instruction can have its destination register replaced safely.
/// Some instructions have encoding restrictions (e.g., certain forms of movabs).
fn can_redirect_instruction(inst: &str) -> bool {
    let trimmed = inst.trim();
    // movabsq with immediate can technically take any GPR, but some assembler
    // versions have issues. Also skip any instruction that references memory
    // through %rax (like `movq (%rax), %rax`) since %rax would need to be
    // preserved for the address computation.
    if trimmed.starts_with("movabsq ") {
        return false;
    }
    // Skip inline asm or anything unusual
    if trimmed.starts_with(".") || trimmed.ends_with(":") {
        return false;
    }
    true
}

/// Replace the destination register in an instruction.
/// Only handles the common patterns generated by our codegen:
/// - `movq <mem>, %old` -> `movq <mem>, %new`
/// - `movq $imm, %old` -> `movq $imm, %new`
/// - `leaq <mem>, %old` -> `leaq <mem>, %new`
/// - `xorq %old, %old` -> `xorq %new, %new`
/// - `movslq <mem>, %old` -> `movslq <mem>, %new`
/// Returns None if the instruction cannot be safely redirected.
fn replace_dest_register(inst: &str, old_reg: &str, new_reg: &str) -> Option<String> {
    // Only handle 64-bit registers to avoid partial register complications
    if !old_reg.starts_with("%r") || !new_reg.starts_with("%r") {
        return None;
    }

    // Handle `xorq %reg, %reg` (zero idiom) specially
    if let Some(rest) = inst.strip_prefix("xorq ") {
        if let Some((src, dst)) = rest.split_once(',') {
            let src = src.trim();
            let dst = dst.trim();
            if src == old_reg && dst == old_reg {
                return Some(format!("xorq {}, {}", new_reg, new_reg));
            }
        }
    }

    // Safe instruction prefixes that can have their destination redirected.
    // Only includes instructions where the destination is the last operand
    // and the source doesn't reference the destination register.
    for prefix in &["movq ", "movslq ", "leaq ", "movzbq "] {
        if let Some(rest) = inst.strip_prefix(prefix) {
            if let Some((src, dst)) = rest.rsplit_once(',') {
                let src = src.trim();
                let dst = dst.trim();
                if dst == old_reg {
                    // Safety: check the source doesn't reference old_reg
                    // (e.g., `movq (%rax), %rax` can't be redirected to
                    // `movq (%rax), %rcx` if %rax is the push_reg because
                    // we're eliminating the push that saves it)
                    // Actually wait - the push saves the value, and in the
                    // transformed version %rax is never modified, so accessing
                    // (%rax) is fine. But let me be safe and check anyway.
                    if !src.contains(old_reg) {
                        return Some(format!("{}{}, {}", prefix, src, new_reg));
                    }
                }
            }
        }
    }

    None
}

/// Pattern 6: Eliminate redundant self-moves like `movq %rax, %rax`.
fn eliminate_redundant_movq_self(lines: &mut [String]) -> bool {
    let mut changed = false;
    for line in lines.iter_mut() {
        let trimmed = line.trim();
        if is_self_move(trimmed) {
            *line = "<<NOP>>".to_string();
            changed = true;
        }
    }
    changed
}

/// Check if an instruction is a self-move (e.g., movq %rax, %rax).
fn is_self_move(s: &str) -> bool {
    // Only 64-bit self-moves are true no-ops on x86-64.
    // 32-bit movl %eax, %eax zero-extends to 64 bits (NOT a no-op).
    // 16/8-bit moves have partial register write semantics.
    if let Some(rest) = s.strip_prefix("movq ") {
        let rest = rest.trim();
        if let Some((src, dst)) = rest.split_once(',') {
            let src = src.trim();
            let dst = dst.trim();
            if src == dst && src.starts_with('%') {
                return true;
            }
        }
    }
    false
}

/// Check if an instruction modifies the given register.
fn instruction_modifies_reg(inst: &str, reg: &str) -> bool {
    // Instructions that modify a register: mov to reg, arithmetic with reg as dest, etc.
    // For safety, we check if the instruction writes to the register.
    // Most x86 instructions have the form: op src, dst (AT&T syntax)
    // The destination is the LAST operand.

    // Skip empty/NOP/labels/directives
    if inst.is_empty() || inst == "<<NOP>>" || inst.ends_with(':') || inst.starts_with('.') {
        return false;
    }

    // Special case: `movq %rax, %rcx` -> modifies %rcx, not %rax
    // In AT&T syntax, the last operand is the destination
    if let Some((_op, operands)) = inst.split_once(' ') {
        if let Some((_src, dst)) = operands.rsplit_once(',') {
            let dst = dst.trim();
            // Check if dst is or contains the register
            if dst == reg || register_overlaps(dst, reg) {
                return true;
            }
        } else {
            // Single operand instructions like `popq %rax`, `pushq %rax`
            // `popq %rax` modifies %rax
            let operand = operands.trim();
            if inst.starts_with("pop") && (operand == reg || register_overlaps(operand, reg)) {
                return true;
            }
            // `incq %rax`, `decq %rax`, `notq %rax`, `negq %rax` modify the operand
            if (inst.starts_with("inc") || inst.starts_with("dec") ||
                inst.starts_with("not") || inst.starts_with("neg")) &&
                (operand == reg || register_overlaps(operand, reg)) {
                return true;
            }
        }
    }

    false
}

/// Check if two register names overlap (e.g., %eax overlaps with %rax).
fn register_overlaps(a: &str, b: &str) -> bool {
    if a == b {
        return true;
    }
    let a_family = register_family(a);
    let b_family = register_family(b);
    a_family.is_some() && a_family == b_family
}

/// Get the register family (0-15) for an x86 register name.
fn register_family(reg: &str) -> Option<u8> {
    match reg {
        "%rax" | "%eax" | "%ax" | "%al" | "%ah" => Some(0),
        "%rcx" | "%ecx" | "%cx" | "%cl" | "%ch" => Some(1),
        "%rdx" | "%edx" | "%dx" | "%dl" | "%dh" => Some(2),
        "%rbx" | "%ebx" | "%bx" | "%bl" | "%bh" => Some(3),
        "%rsp" | "%esp" | "%sp" | "%spl" => Some(4),
        "%rbp" | "%ebp" | "%bp" | "%bpl" => Some(5),
        "%rsi" | "%esi" | "%si" | "%sil" => Some(6),
        "%rdi" | "%edi" | "%di" | "%dil" => Some(7),
        "%r8" | "%r8d" | "%r8w" | "%r8b" => Some(8),
        "%r9" | "%r9d" | "%r9w" | "%r9b" => Some(9),
        "%r10" | "%r10d" | "%r10w" | "%r10b" => Some(10),
        "%r11" | "%r11d" | "%r11w" | "%r11b" => Some(11),
        "%r12" | "%r12d" | "%r12w" | "%r12b" => Some(12),
        "%r13" | "%r13d" | "%r13w" | "%r13b" => Some(13),
        "%r14" | "%r14d" | "%r14w" | "%r14b" => Some(14),
        "%r15" | "%r15d" | "%r15w" | "%r15b" => Some(15),
        _ => None,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MoveSize {
    Q,   // movq  (64-bit)
    L,   // movl  (32-bit)
    W,   // movw  (16-bit)
    B,   // movb  (8-bit)
    SLQ, // movslq (sign-extend 32->64)
}

/// Parse `movX %reg, offset(%rbp)` (store to rbp-relative slot).
/// Returns (register, offset, size).
fn parse_store_to_rbp(s: &str) -> Option<(&str, &str, MoveSize)> {
    let (rest, size) = if let Some(r) = s.strip_prefix("movq ") {
        (r, MoveSize::Q)
    } else if let Some(r) = s.strip_prefix("movl ") {
        (r, MoveSize::L)
    } else if let Some(r) = s.strip_prefix("movw ") {
        (r, MoveSize::W)
    } else if let Some(r) = s.strip_prefix("movb ") {
        (r, MoveSize::B)
    } else {
        return None;
    };

    let (src, dst) = rest.split_once(',')?;
    let src = src.trim();
    let dst = dst.trim();

    // src must be a register
    if !src.starts_with('%') {
        return None;
    }

    // dst must be offset(%rbp)
    if !dst.ends_with("(%rbp)") {
        return None;
    }
    let offset = &dst[..dst.len() - 6]; // Strip "(%rbp)"

    Some((src, offset, size))
}

/// Parse `movX offset(%rbp), %reg` or `movslq offset(%rbp), %reg` (load from rbp-relative slot).
/// Returns (offset, register, size).
fn parse_load_from_rbp(s: &str) -> Option<(&str, &str, MoveSize)> {
    let (rest, size) = if let Some(r) = s.strip_prefix("movq ") {
        (r, MoveSize::Q)
    } else if let Some(r) = s.strip_prefix("movl ") {
        (r, MoveSize::L)
    } else if let Some(r) = s.strip_prefix("movw ") {
        (r, MoveSize::W)
    } else if let Some(r) = s.strip_prefix("movb ") {
        (r, MoveSize::B)
    } else if let Some(r) = s.strip_prefix("movslq ") {
        (r, MoveSize::SLQ)
    } else {
        return None;
    };

    let (src, dst) = rest.split_once(',')?;
    let src = src.trim();
    let dst = dst.trim();

    // src must be offset(%rbp)
    if !src.ends_with("(%rbp)") {
        return None;
    }
    let offset = &src[..src.len() - 6];

    // dst must be a register
    if !dst.starts_with('%') {
        return None;
    }

    Some((offset, dst, size))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_redundant_store_load() {
        let asm = "    movq %rax, -8(%rbp)\n    movq -8(%rbp), %rax\n".to_string();
        let result = peephole_optimize(asm);
        assert_eq!(result.trim(), "movq %rax, -8(%rbp)");
    }

    #[test]
    fn test_store_load_different_reg() {
        let asm = "    movq %rax, -8(%rbp)\n    movq -8(%rbp), %rcx\n".to_string();
        let result = peephole_optimize(asm);
        assert!(result.contains("movq %rax, -8(%rbp)"));
        assert!(result.contains("movq %rax, %rcx"));
        assert!(!result.contains("movq -8(%rbp), %rcx"));
    }

    #[test]
    fn test_redundant_jump() {
        let asm = "    jmp .Lfoo\n.Lfoo:\n".to_string();
        let result = peephole_optimize(asm);
        assert!(!result.contains("jmp"));
        assert!(result.contains(".Lfoo:"));
    }

    #[test]
    fn test_push_pop_elimination() {
        let asm = "    pushq %rax\n    movq %rax, %rcx\n    popq %rax\n".to_string();
        let result = peephole_optimize(asm);
        assert!(!result.contains("pushq"));
        assert!(!result.contains("popq"));
        assert!(result.contains("movq %rax, %rcx"));
    }

    #[test]
    fn test_self_move() {
        let asm = "    movq %rax, %rax\n".to_string();
        let result = peephole_optimize(asm);
        assert_eq!(result.trim(), "");
    }

    #[test]
    fn test_parse_store_to_rbp() {
        assert!(parse_store_to_rbp("movq %rax, -8(%rbp)").is_some());
        assert!(parse_store_to_rbp("movl %eax, -16(%rbp)").is_some());
        assert!(parse_store_to_rbp("movq $5, -8(%rbp)").is_none()); // not a register source
    }

    #[test]
    fn test_parse_load_from_rbp() {
        assert!(parse_load_from_rbp("movq -8(%rbp), %rax").is_some());
        assert!(parse_load_from_rbp("movslq -8(%rbp), %rax").is_some());
    }
}
