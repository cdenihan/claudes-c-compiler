//! Shared CFG and dominator tree analysis utilities.
//!
//! These functions compute control flow graph (CFG) information and dominator
//! trees using the Cooper-Harvey-Kennedy algorithm. They are used by mem2reg
//! for SSA construction and by optimization passes (e.g., GVN) that need
//! dominator information.

use crate::common::fx_hash::{FxHashMap, FxHashSet};
use crate::ir::ir::*;

/// Build a map from block label to block index.
pub fn build_label_map(func: &IrFunction) -> FxHashMap<BlockId, usize> {
    func.blocks
        .iter()
        .enumerate()
        .map(|(i, b)| (b.label, i))
        .collect()
}

/// Build predecessor and successor lists from the function's CFG.
/// Returns (preds, succs) where preds[i] lists predecessors of block i
/// and succs[i] lists successors of block i.
pub fn build_cfg(
    func: &IrFunction,
    label_to_idx: &FxHashMap<BlockId, usize>,
) -> (Vec<Vec<usize>>, Vec<Vec<usize>>) {
    let n = func.blocks.len();
    let mut preds = vec![Vec::new(); n];
    let mut succs = vec![Vec::new(); n];

    for (i, block) in func.blocks.iter().enumerate() {
        match &block.terminator {
            Terminator::Branch(label) => {
                if let Some(&target) = label_to_idx.get(label) {
                    succs[i].push(target);
                    preds[target].push(i);
                }
            }
            Terminator::CondBranch { true_label, false_label, .. } => {
                if let Some(&t) = label_to_idx.get(true_label) {
                    succs[i].push(t);
                    preds[t].push(i);
                }
                if let Some(&f) = label_to_idx.get(false_label) {
                    if !succs[i].contains(&f) {
                        succs[i].push(f);
                    }
                    preds[f].push(i);
                }
            }
            Terminator::IndirectBranch { possible_targets, .. } => {
                for label in possible_targets {
                    if let Some(&t) = label_to_idx.get(label) {
                        if !succs[i].contains(&t) {
                            succs[i].push(t);
                        }
                        preds[t].push(i);
                    }
                }
            }
            Terminator::Return(_) | Terminator::Unreachable => {}
        }
    }

    (preds, succs)
}

/// Compute reverse postorder traversal of the CFG.
pub fn compute_reverse_postorder(num_blocks: usize, succs: &[Vec<usize>]) -> Vec<usize> {
    let mut visited = vec![false; num_blocks];
    let mut postorder = Vec::with_capacity(num_blocks);

    fn dfs(node: usize, succs: &[Vec<usize>], visited: &mut Vec<bool>, postorder: &mut Vec<usize>) {
        visited[node] = true;
        for &succ in &succs[node] {
            if !visited[succ] {
                dfs(succ, succs, visited, postorder);
            }
        }
        postorder.push(node);
    }

    if num_blocks > 0 {
        dfs(0, succs, &mut visited, &mut postorder);
    }

    postorder.reverse();
    postorder
}

/// Intersect two dominators using RPO numbering (Cooper-Harvey-Kennedy).
fn intersect(
    mut finger1: usize,
    mut finger2: usize,
    idom: &[usize],
    rpo_number: &[usize],
) -> usize {
    while finger1 != finger2 {
        while rpo_number[finger1] > rpo_number[finger2] {
            finger1 = idom[finger1];
        }
        while rpo_number[finger2] > rpo_number[finger1] {
            finger2 = idom[finger2];
        }
    }
    finger1
}

/// Compute immediate dominators using the Cooper-Harvey-Kennedy algorithm.
/// Returns idom[i] = immediate dominator of block i (idom[0] = 0 for entry).
/// Uses usize::MAX as sentinel for undefined/unreachable blocks.
pub fn compute_dominators(
    num_blocks: usize,
    preds: &[Vec<usize>],
    succs: &[Vec<usize>],
) -> Vec<usize> {
    const UNDEF: usize = usize::MAX;

    let rpo = compute_reverse_postorder(num_blocks, succs);
    let mut rpo_number = vec![UNDEF; num_blocks];
    for (order, &block) in rpo.iter().enumerate() {
        rpo_number[block] = order;
    }

    let mut idom = vec![UNDEF; num_blocks];
    if rpo.is_empty() {
        return idom;
    }
    idom[rpo[0]] = rpo[0]; // Entry dominates itself

    let mut changed = true;
    while changed {
        changed = false;
        for &b in rpo.iter().skip(1) {
            if rpo_number[b] == UNDEF {
                continue;
            }

            let mut new_idom = UNDEF;
            for &p in &preds[b] {
                if idom[p] != UNDEF {
                    new_idom = p;
                    break;
                }
            }

            if new_idom == UNDEF {
                continue;
            }

            for &p in &preds[b] {
                if p == new_idom {
                    continue;
                }
                if idom[p] != UNDEF {
                    new_idom = intersect(new_idom, p, &idom, &rpo_number);
                }
            }

            if idom[b] != new_idom {
                idom[b] = new_idom;
                changed = true;
            }
        }
    }

    idom
}

/// Compute dominance frontiers for each block.
/// DF(b) = set of blocks where b's dominance ends (join points).
pub fn compute_dominance_frontiers(
    num_blocks: usize,
    preds: &[Vec<usize>],
    idom: &[usize],
) -> Vec<FxHashSet<usize>> {
    let mut df = vec![FxHashSet::default(); num_blocks];

    for b in 0..num_blocks {
        if preds[b].len() < 2 {
            continue;
        }
        for &p in &preds[b] {
            let mut runner = p;
            while runner != idom[b] && runner != usize::MAX {
                df[runner].insert(b);
                if runner == idom[runner] {
                    break;
                }
                runner = idom[runner];
            }
        }
    }

    df
}

/// Build dominator tree children lists from idom array.
/// children[b] lists block indices whose immediate dominator is b.
pub fn build_dom_tree_children(num_blocks: usize, idom: &[usize]) -> Vec<Vec<usize>> {
    let mut children = vec![Vec::new(); num_blocks];
    for b in 1..num_blocks {
        if idom[b] != usize::MAX && idom[b] != b {
            children[idom[b]].push(b);
        }
    }
    children
}
