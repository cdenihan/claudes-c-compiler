//! CFG Simplification pass.
//!
//! Simplifies the control flow graph by:
//! 1. Converting `CondBranch` where both targets are the same to `Branch`
//! 2. Threading jump chains: if block A branches to empty block B which just
//!    branches to C, redirect A to branch directly to C (only when safe)
//! 3. Removing dead (unreachable) blocks that have no predecessors
//!
//! This pass runs to a fixpoint, since one simplification can enable others.
//! Phi nodes in successor blocks are updated when edges are redirected.

use crate::common::fx_hash::{FxHashMap, FxHashSet};
use crate::ir::ir::*;

/// Maximum depth for resolving transitive jump chains (A→B→C→...),
/// to prevent pathological cases.
const MAX_CHAIN_DEPTH: u32 = 32;

/// Run CFG simplification on the entire module.
/// Returns the number of simplifications made.
pub fn run(module: &mut IrModule) -> usize {
    module.for_each_function(simplify_cfg)
}

/// Simplify the CFG of a single function.
/// Iterates until no more simplifications are possible (fixpoint).
fn simplify_cfg(func: &mut IrFunction) -> usize {
    if func.blocks.is_empty() {
        return 0;
    }

    let mut total = 0;
    loop {
        let mut changed = 0;
        changed += simplify_redundant_cond_branches(func);
        changed += thread_jump_chains(func);
        changed += remove_dead_blocks(func);
        if changed == 0 {
            break;
        }
        total += changed;
    }
    total
}

/// Convert `CondBranch { cond, true_label: X, false_label: X }` to `Branch(X)`.
/// The condition is dead and will be cleaned up by DCE.
fn simplify_redundant_cond_branches(func: &mut IrFunction) -> usize {
    let mut count = 0;
    for block in &mut func.blocks {
        if let Terminator::CondBranch { true_label, false_label, .. } = &block.terminator {
            if true_label == false_label {
                let target = *true_label;
                block.terminator = Terminator::Branch(target);
                count += 1;
            }
        }
    }
    count
}

/// Thread jump chains: if a block branches to an empty forwarding block
/// (no instructions, terminates with unconditional Branch), redirect to
/// skip the intermediate block.
///
/// We only thread through a block if:
/// - The intermediate block has NO instructions (including no phi nodes)
/// - The intermediate block terminates with an unconditional Branch
///
/// After threading, we update phi nodes in the target block to replace
/// references to the intermediate block with references to the redirected
/// predecessor.
fn thread_jump_chains(func: &mut IrFunction) -> usize {
    // Build a map of block_id -> forwarding target for empty blocks.
    // An "empty forwarding block" has no instructions (including no phis)
    // and terminates with Branch(target).
    let forwarding: FxHashMap<BlockId, BlockId> = func.blocks.iter()
        .filter(|block| {
            block.instructions.is_empty()
                && matches!(&block.terminator, Terminator::Branch(_))
        })
        .map(|block| {
            if let Terminator::Branch(target) = &block.terminator {
                (block.label, *target)
            } else {
                unreachable!()
            }
        })
        .collect();

    if forwarding.is_empty() {
        return 0;
    }

    // Resolve transitive chains with cycle detection.
    // If A -> B -> C where both B and C are forwarding blocks, resolve to A -> final.
    // We also track the immediate predecessor of the final target for phi updates:
    // in chain B -> C -> D, the final target is D and the immediate predecessor is C.
    // Phi nodes in D reference C (not B), so we need C to look up phi values.
    let resolved: FxHashMap<BlockId, (BlockId, BlockId)> = {
        let mut resolved = FxHashMap::default();
        for &start in forwarding.keys() {
            let mut prev = start;
            let mut current = start;
            let mut depth = 0;
            while let Some(&next) = forwarding.get(&current) {
                if next == start || depth > MAX_CHAIN_DEPTH {
                    break; // cycle or too deep
                }
                prev = current;
                current = next;
                depth += 1;
            }
            if current != start {
                // resolved maps: start -> (final_target, immediate_predecessor_of_final)
                resolved.insert(start, (current, prev));
            }
        }
        resolved
    };

    if resolved.is_empty() {
        return 0;
    }

    // Collect the redirections we need to make.
    // Each edge change: (old_intermediate, new_target, phi_lookup_block)
    // phi_lookup_block is the immediate predecessor of new_target in the chain,
    // which is the block whose label appears in new_target's phi nodes.
    let mut redirections: Vec<(usize, Vec<(BlockId, BlockId, BlockId)>)> = Vec::new();

    for block_idx in 0..func.blocks.len() {
        let mut edge_changes = Vec::new();

        match &func.blocks[block_idx].terminator {
            Terminator::Branch(target) => {
                if let Some(&(resolved_target, phi_block)) = resolved.get(target) {
                    edge_changes.push((*target, resolved_target, phi_block));
                }
            }
            Terminator::CondBranch { true_label, false_label, .. } => {
                if let Some(&(rt, rt_phi)) = resolved.get(true_label) {
                    edge_changes.push((*true_label, rt, rt_phi));
                }
                if let Some(&(rf, rf_phi)) = resolved.get(false_label) {
                    // Avoid duplicate if both targets resolve to same change
                    if !edge_changes.iter().any(|(old, new, _)| *old == *false_label && *new == rf) {
                        edge_changes.push((*false_label, rf, rf_phi));
                    }
                }
            }
            // TODO: IndirectBranch targets could also be threaded through
            // empty blocks, but computed goto is rare enough to skip for now.
            _ => {}
        }

        if !edge_changes.is_empty() {
            redirections.push((block_idx, edge_changes));
        }
    }

    if redirections.is_empty() {
        return 0;
    }

    // Apply the redirections.
    let mut count = 0;
    for (block_idx, edge_changes) in &redirections {
        let block_label = func.blocks[*block_idx].label;

        // Update the terminator
        match &mut func.blocks[*block_idx].terminator {
            Terminator::Branch(target) => {
                for (old, new, _) in edge_changes {
                    if target == old {
                        *target = *new;
                    }
                }
            }
            Terminator::CondBranch { true_label, false_label, .. } => {
                for (old, new, _) in edge_changes {
                    if true_label == old {
                        *true_label = *new;
                    }
                    if false_label == old {
                        *false_label = *new;
                    }
                }
            }
            _ => {}
        }
        count += 1;

        // Update phi nodes in the new target blocks.
        // For each edge change, we use phi_lookup_block (the immediate predecessor
        // of new_target in the forwarding chain) to find the correct phi value.
        // In a chain A -> B -> C -> D, when redirecting A from B to D:
        //   - old_intermediate = B (the original target)
        //   - new_target = D (the final resolved target)
        //   - phi_lookup_block = C (D's phi entries reference C, not B)
        for (_old_intermediate, new_target, phi_lookup_block) in edge_changes {
            // Find the new_target block and update its phi nodes
            for block in &mut func.blocks {
                if block.label == *new_target {
                    for inst in &mut block.instructions {
                        if let Instruction::Phi { incoming, .. } = inst {
                            // Look up the phi value using the immediate predecessor
                            // in the chain (phi_lookup_block), which is the block
                            // that phi entries in new_target actually reference.
                            let value_from_chain = incoming.iter()
                                .find(|(_, label)| *label == *phi_lookup_block)
                                .map(|(val, _)| *val);
                            if let Some(val) = value_from_chain {
                                // Only add if block_label doesn't already have an entry
                                let already_has_entry = incoming.iter()
                                    .any(|(_, label)| *label == block_label);
                                if !already_has_entry {
                                    incoming.push((val, block_label));
                                }
                            }
                        }
                    }
                    break;
                }
            }
        }
    }

    count
}

/// Remove blocks that have no predecessors (except the entry block, blocks[0]).
/// Returns the number of blocks removed.
fn remove_dead_blocks(func: &mut IrFunction) -> usize {
    if func.blocks.len() <= 1 {
        return 0;
    }

    // Compute the set of blocks reachable from the entry block
    let entry = func.blocks[0].label;
    let mut reachable = FxHashSet::default();
    reachable.insert(entry);

    // Build a map from block ID to index for quick lookup
    let block_map: FxHashMap<BlockId, usize> = func.blocks.iter()
        .enumerate()
        .map(|(i, b)| (b.label, i))
        .collect();

    // BFS from entry block
    let mut worklist = vec![entry];
    while let Some(block_id) = worklist.pop() {
        if let Some(&idx) = block_map.get(&block_id) {
            // Successor blocks from terminator
            let targets = get_terminator_targets(&func.blocks[idx].terminator);
            for target in targets {
                if reachable.insert(target) {
                    worklist.push(target);
                }
            }
            // LabelAddr and InlineAsm goto labels (computed goto targets)
            for inst in &func.blocks[idx].instructions {
                if let Instruction::LabelAddr { label, .. } = inst {
                    if reachable.insert(*label) {
                        worklist.push(*label);
                    }
                }
                if let Instruction::InlineAsm { goto_labels, .. } = inst {
                    for (_, label) in goto_labels {
                        if reachable.insert(*label) {
                            worklist.push(*label);
                        }
                    }
                }
            }
        }
    }

    // Collect dead blocks
    let dead_blocks: FxHashSet<BlockId> = func.blocks.iter()
        .map(|b| b.label)
        .filter(|label| !reachable.contains(label))
        .collect();

    if dead_blocks.is_empty() {
        return 0;
    }

    // Clean up phi nodes in reachable blocks that reference dead blocks
    for block in &mut func.blocks {
        if !reachable.contains(&block.label) {
            continue;
        }
        for inst in &mut block.instructions {
            if let Instruction::Phi { incoming, .. } = inst {
                incoming.retain(|(_, label)| !dead_blocks.contains(label));
            }
        }
    }

    let original_len = func.blocks.len();
    func.blocks.retain(|b| reachable.contains(&b.label));
    original_len - func.blocks.len()
}

/// Get the branch targets of a terminator.
fn get_terminator_targets(term: &Terminator) -> Vec<BlockId> {
    match term {
        Terminator::Branch(target) => vec![*target],
        Terminator::CondBranch { true_label, false_label, .. } => {
            vec![*true_label, *false_label]
        }
        Terminator::IndirectBranch { possible_targets, .. } => possible_targets.clone(),
        Terminator::Return(_) | Terminator::Unreachable => vec![],
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::types::IrType;

    #[test]
    fn test_redundant_cond_branch() {
        let mut func = IrFunction::new("test".to_string(), IrType::Void, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Copy { dest: Value(0), src: Operand::Const(IrConst::I32(1)) },
            ],
            terminator: Terminator::CondBranch {
                cond: Operand::Value(Value(0)),
                true_label: BlockId(1),
                false_label: BlockId(1),
            },
        });
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![],
            terminator: Terminator::Return(None),
        });

        let count = simplify_cfg(&mut func);
        assert!(count > 0);
        assert!(matches!(func.blocks[0].terminator, Terminator::Branch(BlockId(1))));
    }

    #[test]
    fn test_jump_chain_threading() {
        let mut func = IrFunction::new("test".to_string(), IrType::Void, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![],
            terminator: Terminator::Branch(BlockId(1)),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![],
            terminator: Terminator::Branch(BlockId(2)),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(2),
            instructions: vec![],
            terminator: Terminator::Return(None),
        });

        let count = simplify_cfg(&mut func);
        assert!(count > 0);
        assert!(matches!(func.blocks[0].terminator, Terminator::Branch(BlockId(2))));
    }

    #[test]
    fn test_dead_block_elimination() {
        let mut func = IrFunction::new("test".to_string(), IrType::Void, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![],
            terminator: Terminator::Return(None),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![
                Instruction::Copy { dest: Value(0), src: Operand::Const(IrConst::I32(42)) },
            ],
            terminator: Terminator::Return(None),
        });

        let count = simplify_cfg(&mut func);
        assert!(count > 0);
        assert_eq!(func.blocks.len(), 1);
        assert_eq!(func.blocks[0].label, BlockId(0));
    }

    #[test]
    fn test_combined_simplifications() {
        let mut func = IrFunction::new("test".to_string(), IrType::Void, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Copy { dest: Value(0), src: Operand::Const(IrConst::I32(1)) },
            ],
            terminator: Terminator::CondBranch {
                cond: Operand::Value(Value(0)),
                true_label: BlockId(1),
                false_label: BlockId(1),
            },
        });
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![],
            terminator: Terminator::Branch(BlockId(2)),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(2),
            instructions: vec![],
            terminator: Terminator::Return(None),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(3),
            instructions: vec![],
            terminator: Terminator::Return(None),
        });

        let count = simplify_cfg(&mut func);
        assert!(count > 0);
        assert!(func.blocks.len() <= 3);
        match &func.blocks[0].terminator {
            Terminator::Branch(target) => assert_eq!(*target, BlockId(2)),
            _ => panic!("Expected Branch terminator"),
        }
    }

    #[test]
    fn test_phi_update_on_thread() {
        // Block 0 -> Block 1 (empty) -> Block 2 (has phi referencing Block 1)
        let mut func = IrFunction::new("test".to_string(), IrType::I32, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Copy { dest: Value(0), src: Operand::Const(IrConst::I32(42)) },
            ],
            terminator: Terminator::Branch(BlockId(1)),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![],
            terminator: Terminator::Branch(BlockId(2)),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(2),
            instructions: vec![
                Instruction::Phi {
                    dest: Value(1),
                    ty: IrType::I32,
                    incoming: vec![(Operand::Value(Value(0)), BlockId(1))],
                },
            ],
            terminator: Terminator::Return(Some(Operand::Value(Value(1)))),
        });

        let count = simplify_cfg(&mut func);
        assert!(count > 0);
        assert!(matches!(func.blocks[0].terminator, Terminator::Branch(BlockId(2))));

        // The phi in Block 2 should have an entry from Block 0
        let last_block = func.blocks.last().unwrap();
        if let Instruction::Phi { incoming, .. } = &last_block.instructions[0] {
            assert!(incoming.iter().any(|(_, label)| *label == BlockId(0)));
        } else {
            panic!("Expected Phi instruction");
        }
    }

    #[test]
    fn test_no_thread_through_block_with_instructions() {
        // Block 1 has an instruction, so it should NOT be threaded
        let mut func = IrFunction::new("test".to_string(), IrType::I32, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![],
            terminator: Terminator::Branch(BlockId(1)),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![
                Instruction::Copy { dest: Value(0), src: Operand::Const(IrConst::I32(42)) },
            ],
            terminator: Terminator::Branch(BlockId(2)),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(2),
            instructions: vec![],
            terminator: Terminator::Return(Some(Operand::Value(Value(0)))),
        });

        let count = simplify_cfg(&mut func);
        // No jump threading should occur (block 1 has instructions)
        assert!(matches!(func.blocks[0].terminator, Terminator::Branch(BlockId(1))));
        // But the function still has all 3 blocks
        assert_eq!(func.blocks.len(), 3);
    }

    #[test]
    fn test_cond_branch_threading() {
        // Block 0 cond-branches to Block 1 (empty fwd) and Block 2 (empty fwd),
        // both forward to Block 3
        let mut func = IrFunction::new("test".to_string(), IrType::Void, vec![], false);
        func.blocks.push(BasicBlock {
            label: BlockId(0),
            instructions: vec![
                Instruction::Copy { dest: Value(0), src: Operand::Const(IrConst::I32(1)) },
            ],
            terminator: Terminator::CondBranch {
                cond: Operand::Value(Value(0)),
                true_label: BlockId(1),
                false_label: BlockId(2),
            },
        });
        func.blocks.push(BasicBlock {
            label: BlockId(1),
            instructions: vec![],
            terminator: Terminator::Branch(BlockId(3)),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(2),
            instructions: vec![],
            terminator: Terminator::Branch(BlockId(3)),
        });
        func.blocks.push(BasicBlock {
            label: BlockId(3),
            instructions: vec![],
            terminator: Terminator::Return(None),
        });

        let count = simplify_cfg(&mut func);
        assert!(count > 0);
        // After threading, Block 0 should go directly to Block 3 for both targets
        // Then redundant cond branch converts to Branch(3)
        assert!(matches!(func.blocks[0].terminator, Terminator::Branch(BlockId(3))));
    }
}
