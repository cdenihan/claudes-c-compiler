//! Optimization passes for the IR.
//!
//! This module contains various optimization passes that transform the IR
//! to produce better code. Passes are organized by optimization level:
//!
//! - O0: No optimization (skip all passes)
//! - O1: Basic optimizations (constant folding, simplification, copy propagation, DCE)
//! - O2: Standard optimizations (O1 + GVN/CSE, repeated passes)
//! - O3: Aggressive optimizations (O2 + more iterations)

pub mod constant_fold;
pub mod copy_prop;
pub mod dce;
pub mod gvn;
pub mod simplify;

use crate::ir::ir::IrModule;

/// Run all optimization passes on the module based on the optimization level.
///
/// The pass pipeline is:
/// 1. Copy propagation (replace uses of copies with original values)
/// 2. Algebraic simplification (strength reduction)
/// 3. Constant folding (evaluate const exprs at compile time)
/// 4. GVN / CSE (local value numbering, O2+)
/// 5. Copy propagation (clean up copies from GVN/simplification)
/// 6. Dead code elimination (remove dead instructions)
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
        // Phase 1: Copy propagation (early - propagate copies from phi elimination
        // and lowering so subsequent passes see through them)
        copy_prop::run(module);

        // Phase 2: Algebraic simplification (x+0 => x, x*1 => x, etc.)
        simplify::run(module);

        // Phase 3: Constant folding (evaluate const exprs at compile time)
        constant_fold::run(module);

        // Phase 4: GVN / Common Subexpression Elimination
        if opt_level >= 2 {
            gvn::run(module);
        }

        // Phase 5: Copy propagation again (clean up copies created by GVN/simplify)
        copy_prop::run(module);

        // Phase 6: Dead code elimination (clean up dead instructions including dead copies)
        dce::run(module);
    }
}
