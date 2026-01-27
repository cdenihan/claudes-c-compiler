//! Phi elimination: lower SSA phi nodes to copies in predecessor blocks.
//!
//! This pass runs after all SSA optimizations and before backend codegen.
//! It converts each Phi instruction into Copy instructions placed at the end
//! of each predecessor block (before the terminator).
//!
//! For correctness with parallel copies (when multiple phis exist in the same
//! block), we use fresh temporary values to avoid lost-copy problems.
//! The pattern is:
//!   pred_block:
//!     ... existing code ...
//!     %tmp1 = copy src1  // for phi1
//!     %tmp2 = copy src2  // for phi2
//!     <terminator>
//!   target_block:
//!     %phi1_dest = copy %tmp1
//!     %phi2_dest = copy %tmp2
//!     ... rest of block ...
//!
//! Critical edge splitting:
//! When a predecessor block has multiple successors (e.g., a CondBranch) and
//! the target block has phis, placing copies at the end of the predecessor
//! would execute them on ALL outgoing paths, not just the edge to the phi's
//! block. This corrupts values used on other paths. To fix this, we split
//! the critical edge by inserting a new trampoline block that contains only
//! the phi copies and branches unconditionally to the target.

use crate::common::fx_hash::FxHashMap;
use crate::ir::ir::*;

/// Eliminate all phi nodes in the module by lowering them to copies.
pub fn eliminate_phis(module: &mut IrModule) {
    // Compute the global max block ID across ALL functions to avoid label collisions
    // when creating trampoline blocks. Labels are module-wide (.L0, .L1, ...).
    let mut next_block_id = 0u32;
    for func in &module.functions {
        for block in &func.blocks {
            if block.label.0 >= next_block_id {
                next_block_id = block.label.0 + 1;
            }
        }
    }

    for func in &mut module.functions {
        if func.is_declaration || func.blocks.is_empty() {
            continue;
        }
        eliminate_phis_in_function(func, &mut next_block_id);
    }
}

/// Returns the number of distinct successor block IDs for a terminator.
fn successor_count(term: &Terminator) -> usize {
    match term {
        Terminator::Return(_) | Terminator::Unreachable => 0,
        Terminator::Branch(_) => 1,
        Terminator::CondBranch { true_label, false_label, .. } => {
            if true_label == false_label { 1 } else { 2 }
        }
        Terminator::IndirectBranch { possible_targets, .. } => possible_targets.len(),
        Terminator::Switch { cases, default, .. } => {
            let mut count = 1; // default
            let mut seen = vec![*default];
            for &(_, label) in cases {
                if !seen.contains(&label) {
                    seen.push(label);
                    count += 1;
                }
            }
            count
        }
    }
}

/// Replace one occurrence of `old_target` with `new_target` in a terminator.
fn retarget_terminator_once(term: &mut Terminator, old_target: BlockId, new_target: BlockId) {
    match term {
        Terminator::Branch(t) => {
            if *t == old_target {
                *t = new_target;
            }
        }
        Terminator::CondBranch { true_label, false_label, .. } => {
            // Only retarget one edge to avoid changing both sides of a diamond
            if *true_label == old_target {
                *true_label = new_target;
            } else if *false_label == old_target {
                *false_label = new_target;
            }
        }
        Terminator::IndirectBranch { possible_targets, .. } => {
            for t in possible_targets.iter_mut() {
                if *t == old_target {
                    *t = new_target;
                    break;
                }
            }
        }
        Terminator::Switch { cases, default, .. } => {
            if *default == old_target {
                *default = new_target;
            } else {
                for (_, t) in cases.iter_mut() {
                    if *t == old_target {
                        *t = new_target;
                        break;
                    }
                }
            }
        }
        _ => {}
    }
}

struct TrampolineBlock {
    label: BlockId,
    copies: Vec<Instruction>,
    branch_target: BlockId,
    pred_idx: usize,
    old_target: BlockId,
}

/// Get or create a trampoline block for a (pred, target) critical edge.
fn get_or_create_trampoline(
    trampoline_map: &mut FxHashMap<(usize, BlockId), usize>,
    trampolines: &mut Vec<TrampolineBlock>,
    pred_idx: usize,
    target_block_id: BlockId,
    next_block_id: &mut u32,
) -> usize {
    *trampoline_map
        .entry((pred_idx, target_block_id))
        .or_insert_with(|| {
            let idx = trampolines.len();
            let label = BlockId(*next_block_id);
            *next_block_id += 1;
            trampolines.push(TrampolineBlock {
                label,
                copies: Vec::new(),
                branch_target: target_block_id,
                pred_idx,
                old_target: target_block_id,
            });
            idx
        })
}

fn eliminate_phis_in_function(func: &mut IrFunction, next_block_id: &mut u32) {
    // Use cached next_value_id if available, otherwise scan
    let mut next_value = if func.next_value_id > 0 {
        func.next_value_id
    } else {
        func.max_value_id() + 1
    };

    // Build label -> block index map
    let label_to_idx: FxHashMap<BlockId, usize> = func.blocks
        .iter()
        .enumerate()
        .map(|(i, b)| (b.label, i))
        .collect();

    // Determine which blocks have multiple successors (for critical edge detection)
    let multi_succ: Vec<bool> = func.blocks.iter()
        .map(|b| successor_count(&b.terminator) > 1)
        .collect();

    // Identify blocks that end with an IndirectBranch (computed goto).
    // These CANNOT use trampoline blocks because the runtime jump target is a
    // computed address that bypasses any CFG retargeting of possible_targets.
    // Phi copies for IndirectBranch predecessors must go before the terminator
    // in the predecessor block itself.
    let is_indirect_branch: Vec<bool> = func.blocks.iter()
        .map(|b| matches!(&b.terminator, Terminator::IndirectBranch { .. }))
        .collect();

    // Collect phi information from all blocks.
    struct PhiInfo {
        dest: Value,
        incoming: Vec<(Operand, BlockId)>,
    }

    let mut block_phis: Vec<Vec<PhiInfo>> = Vec::new();
    for block in &func.blocks {
        let mut phis = Vec::new();
        for inst in &block.instructions {
            if let Instruction::Phi { dest, incoming, .. } = inst {
                phis.push(PhiInfo {
                    dest: *dest,
                    incoming: incoming.clone(),
                });
            }
        }
        block_phis.push(phis);
    }

    // For each block with phis, determine where to place copies.
    // If the predecessor has multiple successors (critical edge), we split the
    // edge by creating a trampoline block for the copies.

    // pred_copies: pred_block_idx -> Vec<Copy instructions>
    // For predecessors with a single successor (safe to append copies directly).
    let mut pred_copies: FxHashMap<usize, Vec<Instruction>> = FxHashMap::default();

    // target_copies: copies to prepend at start of target blocks
    let mut target_copies: Vec<Vec<Instruction>> = vec![Vec::new(); func.blocks.len()];

    // Trampoline blocks for critical edge splitting.
    let mut trampolines: Vec<TrampolineBlock> = Vec::new();

    // Track which (pred_idx, target_block_id) pairs already have trampolines
    let mut trampoline_map: FxHashMap<(usize, BlockId), usize> = FxHashMap::default();

    for (block_idx, phis) in block_phis.iter().enumerate() {
        if phis.is_empty() {
            continue;
        }

        let target_block_id = func.blocks[block_idx].label;
        let use_temporaries = phis.len() > 1;

        for phi in phis {
            if use_temporaries {
                let tmp = Value(next_value);
                next_value += 1;

                for (src, pred_label) in &phi.incoming {
                    if let Some(&pred_idx) = label_to_idx.get(pred_label) {
                        let copy_inst = Instruction::Copy {
                            dest: tmp,
                            src: src.clone(),
                        };

                        if multi_succ[pred_idx] && !is_indirect_branch[pred_idx] {
                            // Critical edge: place copy in a trampoline block.
                            // (Cannot use trampolines for IndirectBranch since
                            // the runtime jump bypasses CFG metadata.)
                            let tramp_idx = get_or_create_trampoline(
                                &mut trampoline_map, &mut trampolines,
                                pred_idx, target_block_id, next_block_id,
                            );
                            trampolines[tramp_idx].copies.push(copy_inst);
                        } else {
                            pred_copies.entry(pred_idx).or_default().push(copy_inst);
                        }
                    }
                }

                target_copies[block_idx].push(Instruction::Copy {
                    dest: phi.dest,
                    src: Operand::Value(tmp),
                });
            } else {
                // Single phi: copy directly
                for (src, pred_label) in &phi.incoming {
                    if let Some(&pred_idx) = label_to_idx.get(pred_label) {
                        let copy_inst = Instruction::Copy {
                            dest: phi.dest,
                            src: src.clone(),
                        };

                        if multi_succ[pred_idx] && !is_indirect_branch[pred_idx] {
                            // Critical edge: place copy in a trampoline block.
                            // (Cannot use trampolines for IndirectBranch since
                            // the runtime jump bypasses CFG metadata.)
                            let tramp_idx = get_or_create_trampoline(
                                &mut trampoline_map, &mut trampolines,
                                pred_idx, target_block_id, next_block_id,
                            );
                            trampolines[tramp_idx].copies.push(copy_inst);
                        } else {
                            pred_copies.entry(pred_idx).or_default().push(copy_inst);
                        }
                    }
                }
            }
        }
    }

    // Apply transformations:
    // 1. Remove phi instructions from all blocks
    // 2. Prepend target copies
    // 3. Insert predecessor copies (for single-successor predecessors only)
    for (block_idx, block) in func.blocks.iter_mut().enumerate() {
        // Remove phi instructions (and their spans)
        if !block.source_spans.is_empty() {
            let mut span_idx = 0;
            block.source_spans.retain(|_| {
                let keep = !matches!(block.instructions.get(span_idx), Some(Instruction::Phi { .. }));
                span_idx += 1;
                keep
            });
        }
        block.instructions.retain(|inst| !matches!(inst, Instruction::Phi { .. }));

        // Prepend target copies (these go at the start, replacing the phis)
        if !target_copies[block_idx].is_empty() {
            let num_copies = target_copies[block_idx].len();
            let mut new_insts = target_copies[block_idx].clone();
            new_insts.append(&mut block.instructions);
            block.instructions = new_insts;
            if !block.source_spans.is_empty() {
                let mut new_spans = vec![crate::common::source::Span::dummy(); num_copies];
                new_spans.append(&mut block.source_spans);
                block.source_spans = new_spans;
            }
        }

        // Insert predecessor copies before terminator (only for single-successor blocks)
        if let Some(copies) = pred_copies.remove(&block_idx) {
            let num_copies = copies.len();
            block.instructions.extend(copies);
            if !block.source_spans.is_empty() {
                block.source_spans.extend(std::iter::repeat(crate::common::source::Span::dummy()).take(num_copies));
            }
        }
    }

    // 4. Retarget predecessors that need trampolines
    for trampoline in &trampolines {
        retarget_terminator_once(
            &mut func.blocks[trampoline.pred_idx].terminator,
            trampoline.old_target,
            trampoline.label,
        );
    }

    // 5. Append trampoline blocks to the function
    for trampoline in trampolines {
        let num_copies = trampoline.copies.len();
        func.blocks.push(BasicBlock {
            label: trampoline.label,
            instructions: trampoline.copies,
            source_spans: vec![crate::common::source::Span::dummy(); num_copies],
            terminator: Terminator::Branch(trampoline.branch_target),
        });
    }

    // Update cached next_value_id for downstream passes
    func.next_value_id = next_value;
}
