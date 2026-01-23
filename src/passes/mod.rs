//! Optimization passes for the IR.
//!
//! This module contains various optimization passes that transform the IR
//! to produce better code. Passes are organized by optimization level:
//!
//! - O0: No optimization (skip all passes)
//! - O1: Basic optimizations (constant folding, simplification, DCE)
//! - O2: Standard optimizations (O1 + GVN/CSE, repeated passes)
//! - O3: Aggressive optimizations (O2 + more iterations)

pub mod constant_fold;
pub mod dce;
pub mod gvn;
pub mod simplify;

use crate::ir::ir::IrModule;

/// Run all optimization passes on the module based on the optimization level.
///
/// The pass pipeline is:
/// 1. Algebraic simplification (strength reduction)
/// 2. Constant folding
/// 3. GVN / CSE (local value numbering)
/// 4. Dead code elimination
///
/// Higher optimization levels run more iterations of this pipeline.
pub fn run_passes(module: &mut IrModule, opt_level: u32) {
    if opt_level == 0 {
        return; // No optimization at O0
    }

    // Determine number of pipeline iterations based on opt level
    let iterations = match opt_level {
        1 => 1,
        2 => 2,
        3 => 3,
        _ => 1,
    };

    for _ in 0..iterations {
        // Phase 1: Algebraic simplification (x+0 => x, x*1 => x, etc.)
        simplify::run(module);

        // Phase 2: Constant folding (evaluate const exprs at compile time)
        constant_fold::run(module);

        // Phase 3: GVN / Common Subexpression Elimination
        if opt_level >= 2 {
            gvn::run(module);
        }

        // Phase 4: Dead code elimination (clean up dead instructions)
        dce::run(module);
    }
}
