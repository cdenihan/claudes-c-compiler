# Optimization Passes

SSA-based optimization passes that improve the IR before code generation.

## Available Passes

- **cfg_simplify.rs** - CFG simplification: dead block elimination, jump chain threading, redundant branch simplification
- **constant_fold.rs** - Evaluates constant expressions at compile time (e.g., `3 + 4` -> `7`)
- **copy_prop.rs** - Copy propagation: replaces uses of copies with original values, follows transitive chains
- **dce.rs** - Dead code elimination: removes instructions whose results are never used
- **gvn.rs** - Dominator-based global value numbering: eliminates redundant BinOp, UnaryOp, Cmp, Cast, and GetElementPtr computations across all dominated blocks
- **licm.rs** - Loop-invariant code motion: hoists loop-invariant computations and safe loads to loop preheaders. Includes load hoisting for non-address-taken alloca-based loads that are not modified within the loop (e.g., function parameter loads). Requires single-entry preheaders for soundness
- **simplify.rs** - Algebraic simplification: identity removal (`x + 0` -> `x`), strength reduction (`x * 2` -> `x << 1`), boolean simplification

## Pass Pipeline

Passes run in a fixed pipeline with iteration count based on `-O` level:

- `-O0`: No passes run
- `-O1`: 1 iteration
- `-O2`: 2 iterations
- `-O3`: 3 iterations

Each iteration runs: CFG simplify -> copy prop -> simplify -> constant fold -> GVN/CSE -> LICM (O2+) -> copy prop -> DCE -> CFG simplify.

## Architecture

- All passes use `IrModule::for_each_function()` to iterate over defined functions
- `Instruction::dest()` provides the canonical way to extract a value defined by an instruction
- `IrConst::is_zero()`, `IrConst::is_one()`, `IrConst::zero(ty)`, `IrConst::one(ty)` provide shared constant helpers
- `IrBinOp`, `IrUnaryOp`, `IrCmpOp` derive `Hash`/`Eq` so they can be used directly as HashMap keys (e.g., in GVN)
- `IrConst::to_hash_key()` converts float constants to bit-pattern keys for hashing
- `IrBinOp::is_commutative()` identifies commutative ops for canonical ordering in CSE

## Adding New Passes

Each pass implements `fn run(module: &mut IrModule) -> usize` returning the count of changes made.
Use `module.for_each_function(|func| { ... })` to skip declarations.
Register new passes in `mod.rs::run_passes()` at the appropriate pipeline position.
