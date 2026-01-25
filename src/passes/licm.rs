//! Loop-Invariant Code Motion (LICM) pass.
//!
//! Identifies natural loops in the CFG and hoists loop-invariant instructions
//! to preheader blocks that execute before the loop. An instruction is
//! loop-invariant if all of its operands are either:
//! - Constants
//! - Defined outside the loop
//! - Defined by other loop-invariant instructions
//!
//! This is particularly important for:
//! - Array index computations (i * n) in inner loops
//! - Address calculations that depend on outer loop variables
//! - Casts and extensions of loop-invariant values
//!
//! The pass requires loops to have a single-entry preheader block. Loops with
//! multiple outside predecessors are skipped (a future improvement could create
//! dedicated preheader blocks for these cases).
//!
//! Safety: Only pure (side-effect-free) instructions are hoisted. Loads, stores,
//! calls, and other side-effecting instructions are never moved.

use crate::common::fx_hash::{FxHashMap, FxHashSet};
use crate::ir::analysis;
use crate::ir::ir::*;

/// Run LICM on the entire module.
/// Returns the number of instructions hoisted.
pub fn run(module: &mut IrModule) -> usize {
    module.for_each_function(licm_function)
}

/// A natural loop identified by its header block and the set of blocks in the loop body.
struct NaturalLoop {
    /// The header block index - the target of the back edge
    header: usize,
    /// All block indices that form the loop body (includes the header)
    body: FxHashSet<usize>,
}

/// Run LICM on a single function.
fn licm_function(func: &mut IrFunction) -> usize {
    let num_blocks = func.blocks.len();
    if num_blocks < 2 {
        return 0; // Need at least 2 blocks for a loop
    }

    // Build CFG
    let label_to_idx = analysis::build_label_map(func);
    let (preds, succs) = analysis::build_cfg(func, &label_to_idx);

    // Compute dominators
    let idom = analysis::compute_dominators(num_blocks, &preds, &succs);

    // Find natural loops
    let loops = find_natural_loops(num_blocks, &preds, &succs, &idom);
    if loops.is_empty() {
        return 0;
    }

    let mut total_hoisted = 0;

    // Process loops from innermost to outermost (smaller loops first).
    // This ensures inner-loop invariants are hoisted before outer-loop analysis.
    let mut sorted_loops = loops;
    sorted_loops.sort_by_key(|l| l.body.len());

    for natural_loop in &sorted_loops {
        total_hoisted += hoist_loop_invariants(func, natural_loop, &preds, &idom);
    }

    total_hoisted
}

/// Find all natural loops in the CFG.
///
/// A natural loop is defined by a back edge (tail -> header) where the header
/// dominates the tail. The loop body is the set of blocks that can reach the
/// tail without going through the header.
fn find_natural_loops(
    num_blocks: usize,
    preds: &[Vec<usize>],
    succs: &[Vec<usize>],
    idom: &[usize],
) -> Vec<NaturalLoop> {
    let mut loops = Vec::new();

    // Build dominance relation: does block `a` dominate block `b`?
    // We check by walking idom chain from b upward.
    let dominates = |a: usize, mut b: usize| -> bool {
        loop {
            if b == a {
                return true;
            }
            if b == idom[b] || idom[b] == usize::MAX {
                return false;
            }
            b = idom[b];
        }
    };

    // Find back edges: an edge (tail -> header) where header dominates tail
    for tail in 0..num_blocks {
        for &header in &succs[tail] {
            if dominates(header, tail) {
                // Found a back edge: tail -> header
                // Compute the natural loop body
                let body = compute_loop_body(header, tail, preds);
                loops.push(NaturalLoop { header, body });
            }
        }
    }

    loops
}

/// Compute the body of a natural loop given a back edge (tail -> header).
/// Uses a reverse walk from the tail, adding all blocks that can reach the
/// tail without going through the header.
fn compute_loop_body(
    header: usize,
    tail: usize,
    preds: &[Vec<usize>],
) -> FxHashSet<usize> {
    let mut body = FxHashSet::default();
    body.insert(header);

    if header == tail {
        // Self-loop
        return body;
    }

    // Walk backwards from tail, adding predecessors
    let mut worklist = vec![tail];
    body.insert(tail);

    while let Some(block) = worklist.pop() {
        for &pred in &preds[block] {
            if !body.contains(&pred) {
                body.insert(pred);
                worklist.push(pred);
            }
        }
    }

    body
}

/// Check if an instruction is safe to hoist (pure, no side effects, not a phi).
fn is_hoistable(inst: &Instruction) -> bool {
    matches!(
        inst,
        Instruction::BinOp { .. }
            | Instruction::UnaryOp { .. }
            | Instruction::Cmp { .. }
            | Instruction::Cast { .. }
            | Instruction::GetElementPtr { .. }
            | Instruction::Copy { .. }
            | Instruction::GlobalAddr { .. }
    )
}

/// Get all Value IDs referenced as operands by an instruction (not including dest).
fn instruction_operand_values(inst: &Instruction) -> Vec<u32> {
    let mut vals = Vec::new();
    let collect_op = |op: &Operand, vals: &mut Vec<u32>| {
        if let Operand::Value(v) = op {
            vals.push(v.0);
        }
    };

    match inst {
        Instruction::BinOp { lhs, rhs, .. } => {
            collect_op(lhs, &mut vals);
            collect_op(rhs, &mut vals);
        }
        Instruction::UnaryOp { src, .. } => {
            collect_op(src, &mut vals);
        }
        Instruction::Cmp { lhs, rhs, .. } => {
            collect_op(lhs, &mut vals);
            collect_op(rhs, &mut vals);
        }
        Instruction::Cast { src, .. } => {
            collect_op(src, &mut vals);
        }
        Instruction::GetElementPtr { base, offset, .. } => {
            vals.push(base.0);
            collect_op(offset, &mut vals);
        }
        Instruction::Copy { src, .. } => {
            collect_op(src, &mut vals);
        }
        Instruction::GlobalAddr { .. } => {
            // No value operands
        }
        // All other instructions are non-hoistable and should never reach here.
        // If is_hoistable() is extended, this must be updated to match.
        _ => unreachable!("instruction_operand_values called on non-hoistable instruction")
    }

    vals
}

/// Hoist loop-invariant instructions from a natural loop to a preheader.
///
/// Returns the number of instructions hoisted.
fn hoist_loop_invariants(
    func: &mut IrFunction,
    natural_loop: &NaturalLoop,
    preds: &[Vec<usize>],
    idom: &[usize],
) -> usize {
    let header = natural_loop.header;

    // Find the preheader: a predecessor of the header that is NOT in the loop.
    // If multiple predecessors exist outside the loop, we use the one that
    // is the immediate dominator of the header (the natural preheader).
    let preheader = find_preheader(header, &natural_loop.body, preds, idom);
    let preheader = match preheader {
        Some(ph) => ph,
        None => return 0, // No suitable preheader found
    };

    // Build the set of Value IDs defined inside the loop
    let mut loop_defined: FxHashSet<u32> = FxHashSet::default();
    for &block_idx in &natural_loop.body {
        if block_idx < func.blocks.len() {
            for inst in &func.blocks[block_idx].instructions {
                if let Some(dest) = inst.dest() {
                    loop_defined.insert(dest.0);
                }
            }
        }
    }

    // Iteratively identify loop-invariant instructions.
    // An instruction is loop-invariant if:
    // 1. It is hoistable (pure, no side effects)
    // 2. All its Value operands are either:
    //    a. Not defined in the loop (defined outside), OR
    //    b. Already identified as loop-invariant
    let mut invariant: FxHashSet<u32> = FxHashSet::default();
    let mut hoistable_insts: Vec<(usize, usize, Instruction)> = Vec::new(); // (block_idx, inst_idx, inst)

    let mut changed = true;
    while changed {
        changed = false;
        for &block_idx in &natural_loop.body {
            if block_idx >= func.blocks.len() {
                continue;
            }
            let block = &func.blocks[block_idx];
            for (inst_idx, inst) in block.instructions.iter().enumerate() {
                // Skip if not hoistable type
                if !is_hoistable(inst) {
                    continue;
                }

                let dest = match inst.dest() {
                    Some(d) => d,
                    None => continue,
                };

                // Skip if already identified as invariant
                if invariant.contains(&dest.0) {
                    continue;
                }

                // Check all operands
                let operand_vals = instruction_operand_values(inst);
                let all_invariant = operand_vals.iter().all(|&val_id| {
                    // Either defined outside the loop...
                    !loop_defined.contains(&val_id) ||
                    // ...or already proven loop-invariant
                    invariant.contains(&val_id)
                });

                if all_invariant {
                    invariant.insert(dest.0);
                    hoistable_insts.push((block_idx, inst_idx, inst.clone()));
                    changed = true;
                }
            }
        }
    }

    if hoistable_insts.is_empty() {
        return 0;
    }

    // Collect the set of instruction indices to remove from each block
    let mut to_remove: FxHashMap<usize, FxHashSet<usize>> = FxHashMap::default();
    for &(block_idx, inst_idx, _) in &hoistable_insts {
        to_remove.entry(block_idx).or_default().insert(inst_idx);
    }

    // Remove hoisted instructions from their original blocks
    for (&block_idx, indices) in &to_remove {
        if block_idx < func.blocks.len() {
            let block = &mut func.blocks[block_idx];
            let mut new_insts = Vec::with_capacity(block.instructions.len());
            for (i, inst) in block.instructions.drain(..).enumerate() {
                if !indices.contains(&i) {
                    new_insts.push(inst);
                }
            }
            block.instructions = new_insts;
        }
    }

    // Insert hoisted instructions at the end of the preheader block
    // (before its terminator, which is implicit - terminators are separate from instructions).
    // We need to insert in the order they originally appeared to maintain SSA dominance.
    // Sort by (block_idx in RPO order, then inst_idx) to preserve def-before-use.
    hoistable_insts.sort_by_key(|&(block_idx, inst_idx, _)| (block_idx, inst_idx));

    // Deduplicate: if the same instruction was found multiple times (shouldn't happen
    // with our algorithm, but be safe), keep only the first occurrence.
    let mut seen_dests: FxHashSet<u32> = FxHashSet::default();
    let mut unique_insts: Vec<Instruction> = Vec::new();
    for (_, _, inst) in hoistable_insts {
        if let Some(dest) = inst.dest() {
            if seen_dests.insert(dest.0) {
                unique_insts.push(inst);
            }
        } else {
            unique_insts.push(inst);
        }
    }

    let num_hoisted = unique_insts.len();

    // Topologically sort: if instruction A defines a value used by instruction B,
    // A must come before B in the preheader.
    let sorted = topological_sort_instructions(unique_insts);

    // Insert at the end of the preheader (before terminator)
    if preheader < func.blocks.len() {
        let preheader_block = &mut func.blocks[preheader];
        preheader_block.instructions.extend(sorted);
    }

    num_hoisted
}

/// Find a suitable preheader block for a loop.
/// The preheader must:
/// 1. Be a predecessor of the header
/// 2. Not be part of the loop body
/// 3. Preferably be the immediate dominator of the header
fn find_preheader(
    header: usize,
    loop_body: &FxHashSet<usize>,
    preds: &[Vec<usize>],
    idom: &[usize],
) -> Option<usize> {
    // First try: use the immediate dominator if it's outside the loop
    let idom_header = idom[header];
    if idom_header != usize::MAX && idom_header != header && !loop_body.contains(&idom_header) {
        // Verify it's actually a predecessor
        if preds[header].contains(&idom_header) {
            return Some(idom_header);
        }
    }

    // Fallback: find any predecessor of the header that is outside the loop
    // Prefer to find exactly one such predecessor (single-entry)
    let outside_preds: Vec<usize> = preds[header]
        .iter()
        .filter(|&&p| !loop_body.contains(&p))
        .copied()
        .collect();

    if outside_preds.len() == 1 {
        return Some(outside_preds[0]);
    }

    // No outside predecessors or multiple outside predecessors - not safe to
    // hoist without creating a dedicated preheader block. For now, skip this loop.
    // TODO: Create dedicated preheader blocks when multiple outside predecessors exist
    None
}

/// Topologically sort instructions so that definitions come before uses.
/// This ensures SSA correctness in the preheader.
fn topological_sort_instructions(mut insts: Vec<Instruction>) -> Vec<Instruction> {
    if insts.len() <= 1 {
        return insts;
    }

    // Build a map from dest value ID to index in insts
    let mut def_to_idx: FxHashMap<u32, usize> = FxHashMap::default();
    for (i, inst) in insts.iter().enumerate() {
        if let Some(dest) = inst.dest() {
            def_to_idx.insert(dest.0, i);
        }
    }

    // Build dependency edges: inst[i] depends on inst[j] if inst[i] uses a value defined by inst[j]
    let n = insts.len();
    let mut in_degree = vec![0u32; n];
    let mut dependents: Vec<Vec<usize>> = vec![Vec::new(); n];

    for (i, inst) in insts.iter().enumerate() {
        let operand_vals = instruction_operand_values(inst);
        for val_id in operand_vals {
            if let Some(&def_idx) = def_to_idx.get(&val_id) {
                if def_idx != i {
                    dependents[def_idx].push(i);
                    in_degree[i] += 1;
                }
            }
        }
    }

    // Kahn's algorithm
    let mut queue: Vec<usize> = Vec::new();
    for i in 0..n {
        if in_degree[i] == 0 {
            queue.push(i);
        }
    }

    let mut order: Vec<usize> = Vec::with_capacity(n);
    while let Some(idx) = queue.pop() {
        order.push(idx);
        for &dep in &dependents[idx] {
            in_degree[dep] -= 1;
            if in_degree[dep] == 0 {
                queue.push(dep);
            }
        }
    }

    // If there's a cycle (shouldn't happen in SSA), fall back to original order
    if order.len() != n {
        return insts;
    }

    // Reorder instructions according to topological order.
    // Use Option slots to allow taking ownership from arbitrary positions.
    let mut result = Vec::with_capacity(n);
    let mut slots: Vec<Option<Instruction>> = insts.drain(..).map(Some).collect();
    for &idx in &order {
        if let Some(inst) = slots[idx].take() {
            result.push(inst);
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::types::IrType;

    /// Helper to create a simple loop: preheader -> header -> body -> header, header -> exit
    fn make_loop_func() -> IrFunction {
        let mut func = IrFunction::new("test_loop".to_string(), IrType::I32, vec![], false);

        // Block 0 (preheader): i = 0, n = 10
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Copy {
                    dest: Value(0),
                    src: Operand::Const(IrConst::I32(0)),
                },
                Instruction::Copy {
                    dest: Value(1),
                    src: Operand::Const(IrConst::I32(10)),
                },
            ],
            terminator: Terminator::Branch(BlockId(1)),
        });

        // Block 1 (header): phi for i, check i < n
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![
                Instruction::Phi {
                    dest: Value(2),
                    ty: IrType::I32,
                    incoming: vec![
                        (Operand::Value(Value(0)), BlockId(0)),
                        (Operand::Value(Value(5)), BlockId(2)),
                    ],
                },
                Instruction::Cmp {
                    dest: Value(3),
                    op: IrCmpOp::Slt,
                    lhs: Operand::Value(Value(2)),
                    rhs: Operand::Value(Value(1)),
                    ty: IrType::I32,
                },
            ],
            terminator: Terminator::CondBranch {
                cond: Operand::Value(Value(3)),
                true_label: BlockId(2),
                false_label: BlockId(3),
            },
        });

        // Block 2 (body): loop-invariant computation (n * 4), then i++
        func.blocks.push(BasicBlock {
            label: BlockId(2),
            instructions: vec![
                // n * 4 is loop-invariant (both n=Value(1) and 4 are outside loop)
                Instruction::BinOp {
                    dest: Value(4),
                    op: IrBinOp::Mul,
                    lhs: Operand::Value(Value(1)),
                    rhs: Operand::Const(IrConst::I32(4)),
                    ty: IrType::I32,
                },
                // i + 1 is NOT loop-invariant (uses i = Value(2) which is a phi)
                Instruction::BinOp {
                    dest: Value(5),
                    op: IrBinOp::Add,
                    lhs: Operand::Value(Value(2)),
                    rhs: Operand::Const(IrConst::I32(1)),
                    ty: IrType::I32,
                },
            ],
            terminator: Terminator::Branch(BlockId(1)),
        });

        // Block 3 (exit)
        func.blocks.push(BasicBlock {
            label: BlockId(3),
            instructions: vec![],
            terminator: Terminator::Return(Some(Operand::Value(Value(4)))),
        });

        func.next_value_id = 6;
        func
    }

    #[test]
    fn test_find_natural_loops() {
        let func = make_loop_func();
        let label_to_idx = analysis::build_label_map(&func);
        let (preds, succs) = analysis::build_cfg(&func, &label_to_idx);
        let idom = analysis::compute_dominators(func.blocks.len(), &preds, &succs);
        let loops = find_natural_loops(func.blocks.len(), &preds, &succs, &idom);

        assert_eq!(loops.len(), 1);
        assert_eq!(loops[0].header, 1); // header is block 1
        assert!(loops[0].body.contains(&1)); // header in body
        assert!(loops[0].body.contains(&2)); // loop body block in body
        assert!(!loops[0].body.contains(&0)); // preheader not in body
        assert!(!loops[0].body.contains(&3)); // exit not in body
    }

    #[test]
    fn test_licm_hoists_invariant() {
        let mut func = make_loop_func();
        let label_to_idx = analysis::build_label_map(&func);
        let (preds, _succs) = analysis::build_cfg(&func, &label_to_idx);
        let idom = analysis::compute_dominators(func.blocks.len(), &preds, &_succs);
        let loops = find_natural_loops(func.blocks.len(), &preds, &_succs, &idom);

        let hoisted = hoist_loop_invariants(&mut func, &loops[0], &preds, &idom);

        // n * 4 should be hoisted (1 instruction)
        assert_eq!(hoisted, 1);

        // The preheader (block 0) should now have 3 instructions
        assert_eq!(func.blocks[0].instructions.len(), 3);

        // The loop body (block 2) should have only i+1 (the non-invariant)
        assert_eq!(func.blocks[2].instructions.len(), 1);

        // Check that the hoisted instruction is the multiply
        let last_preheader_inst = func.blocks[0].instructions.last().unwrap();
        match last_preheader_inst {
            Instruction::BinOp { op: IrBinOp::Mul, .. } => {} // correct
            other => panic!("Expected BinOp::Mul, got {:?}", other),
        }
    }

    #[test]
    fn test_topological_sort() {
        // a = 1 + 2, b = a + 3 -> b depends on a, so a must come first
        let insts = vec![
            Instruction::BinOp {
                dest: Value(10),
                op: IrBinOp::Add,
                lhs: Operand::Value(Value(5)), // uses value defined by second inst
                rhs: Operand::Const(IrConst::I32(3)),
                ty: IrType::I32,
            },
            Instruction::BinOp {
                dest: Value(5),
                op: IrBinOp::Add,
                lhs: Operand::Const(IrConst::I32(1)),
                rhs: Operand::Const(IrConst::I32(2)),
                ty: IrType::I32,
            },
        ];

        let sorted = topological_sort_instructions(insts);
        // Value(5) should come before Value(10)
        assert_eq!(sorted[0].dest(), Some(Value(5)));
        assert_eq!(sorted[1].dest(), Some(Value(10)));
    }
}
