# Optimization Passes

This document describes the design and implementation of the optimization pass
pipeline. The pipeline transforms the compiler's intermediate representation (IR)
to produce better machine code by eliminating redundant computation, simplifying
control flow, and replacing expensive operations with cheaper equivalents.

All optimization levels (`-O0` through `-O3`, `-Os`, `-Oz`) currently run the
same full set of passes. This simplifies the compiler while it matures, avoiding
subtle bugs where code works at one optimization level but breaks at another.

## Table of Contents

1. [Pipeline Overview](#pipeline-overview)
2. [Phase Structure](#phase-structure)
3. [Dirty Tracking and Iteration Strategy](#dirty-tracking-and-iteration-strategy)
4. [Shared CFG Analysis](#shared-cfg-analysis)
5. [Pass Descriptions](#pass-descriptions)
6. [Pass Dependency Graph](#pass-dependency-graph)
7. [Disabling Individual Passes](#disabling-individual-passes)
8. [Files](#files)

---

## Pipeline Overview

The optimizer executes in four sequential phases:

```
 Phase 0: Inlining
     inline
     (convert gnu_inline defs to declarations)
     mem2reg
     constant_fold
     copy_prop
     simplify
     constant_fold
     copy_prop
     resolve_asm
         |
         v
 Phase 0.5: IsConstant Resolution
     resolve_remaining_is_constant
         |
         v
 Main Loop (up to 3 iterations, with dirty tracking)
   +-------------------------------------------------------+
   |  1.  cfg_simplify                                     |
   |  2.  copy_prop                                        |
   |  2a. div_by_const         (iteration 0 only, disabled on i686) |
   |  2b. narrow                                           |
   |  3.  simplify                                         |
   |  4.  constant_fold                                    |
   |  5.  gvn            \                                 |
   |  6.  licm             > shared CFG analysis           |
   |  6a. iv_strength_reduce (iteration 0 only)            |
   |  7.  if_convert          /                            |
   |  8.  copy_prop           (second round)               |
   |  9.  dce                                              |
   | 10.  cfg_simplify        (second round)               |
   | 10.5 ipcp                (interprocedural)            |
   +-------------------------------------------------------+
         |
         v
 Phase 11: Dead Static Elimination
     dead_statics
```

## Phase Structure

### Phase 0 -- Inlining

Function inlining runs first, before the main optimization loop. The inliner
substitutes the bodies of small static and `static inline` functions (as well as
`__attribute__((always_inline))` functions) into their call sites. After
inlining, `extern inline` functions with the `gnu_inline` attribute are
converted to declarations: their bodies existed only for the inliner, and
emitting them as definitions would cause infinite recursion when their internal
calls resolve to the local definition instead of the intended external library
symbol.

After inlining, a sequence of cleanup passes runs once on the freshly inlined
code:

1. **mem2reg** -- promotes stack allocations created during inlining back into
   SSA registers.
2. **constant_fold** -- folds constants that became visible after inlining
   (e.g., an inlined function that returns a constant).
3. **copy_prop** -- propagates copies introduced by phi elimination and mem2reg.
4. **simplify** -- applies algebraic identities to the simplified code.
5. **constant_fold** (again) -- catches additional folding opportunities exposed
   by simplification.
6. **copy_prop** (again) -- cleans up any remaining copies.
7. **resolve_asm** -- resolves inline assembly symbol references that became
   computable after inlining with constant arguments.

### Phase 0.5 -- IsConstant Resolution

After inlining and its cleanup passes, any `__builtin_constant_p` call whose
operand became a compile-time constant has already been resolved to `1` (true)
by constant folding. The remaining `IsConstant` instructions have operands that
are definitively not compile-time constants (parameters, globals, etc.), so this
phase resolves them all to `0` (false).

This resolution is critical: it allows `cfg_simplify` in the main loop to fold
conditional branches that test `__builtin_constant_p` results and eliminate dead
code paths. Without it, unreachable function calls guarded by
`__builtin_constant_p` would survive into the object file and cause linker
errors for intentionally undefined symbols (e.g., the Linux kernel's
`__bad_udelay()`, which is left undefined to generate a link error for invalid
`udelay()` arguments).

### Main Loop

The main optimization loop runs for up to 3 iterations. Each iteration executes
the full sequence of intraprocedural passes plus one interprocedural pass (IPCP)
at the end. The loop uses per-function dirty tracking and per-pass skip logic to
avoid redundant work, and it terminates early when either:

- No pass made any changes (the IR has reached a fixed point), or
- The iteration produced fewer than 5% as many changes as the first iteration
  (diminishing returns), unless IPCP made changes that require a full cleanup
  iteration.

Two passes run only during the first iteration: **div_by_const** (the expansion
is a one-time rewrite, and subsequent iterations clean up the generated code) and
**iv_strength_reduce** (a loop transformation that creates work for later cleanup
rather than benefiting from iteration).

### Phase 11 -- Dead Static Elimination

After all intraprocedural optimizations are complete, a final interprocedural
pass removes internal-linkage (`static`) functions and `static const` globals
that are no longer referenced by any live symbol. This is essential for `static
inline` functions from headers: after optimization eliminates dead code paths,
some callees become completely unreferenced and can be removed. Without this
cleanup, dead functions may reference undefined external symbols and cause
linker errors.

---

## Dirty Tracking and Iteration Strategy

The optimizer maintains two boolean vectors, each with one entry per function in
the module: **dirty** and **changed**.

- **dirty** indicates which functions should be visited during the current
  iteration. At the start of the first iteration, every function is dirty.
- **changed** accumulates which functions were modified by any pass during the
  current iteration.

The helper function `run_on_visited` enforces the dirty-tracking contract: it
skips declaration-only functions entirely, skips non-dirty functions, and marks
a function as changed whenever a pass reports modifications.

At the end of each iteration, `dirty` and `changed` are swapped: only functions
that were actually modified become dirty for the next iteration. This ensures
that functions which have already converged are not revisited.

### Per-Pass Skip Logic

In addition to per-function dirty tracking, the pipeline tracks how many changes
each pass made in the previous iteration. Before running a pass, the pipeline
checks whether the pass itself or any of its upstream dependencies made changes
last time. If neither the pass nor its upstream passes produced any changes, the
pass is skipped entirely for the current iteration.

This is driven by an explicit dependency graph encoded in the `should_run!`
macro. For example, `constant_fold` depends on `copy_prop`, `narrow`,
`simplify`, `if_convert`, and the second `copy_prop`; if none of those passes
made changes in the previous iteration, constant folding is skipped.

On the first iteration, all previous-change counts are set to `MAX` so every
pass runs unconditionally.

### Diminishing Returns and Early Exit

The pipeline tracks the total change count (excluding DCE) from the first
iteration as a baseline. If a subsequent iteration produces fewer than 5% of
that baseline, and IPCP did not make changes, the loop exits early. DCE is
excluded from this comparison because it is a cleanup pass whose large change
count (often thousands of removed dead instructions) would inflate the baseline
and make productive later iterations look like diminishing returns.

An exception is made for IPCP: if interprocedural constant propagation made
changes in an iteration, the pipeline always runs at least one more iteration
regardless of diminishing returns, because IPCP changes (constant arguments,
dead call elimination) require a full round of constant folding, DCE, and CFG
simplification to clean up.

The pipeline also guarantees at least two full iterations before the diminishing-
returns check can trigger, because multi-step constant propagation chains
(e.g., switch folding through layers of inlined helper functions) need at least
two iterations to fully propagate.

---

## Shared CFG Analysis

Three passes in the main loop -- GVN, LICM, and IVSR -- all require the same
expensive CFG analysis: label maps, predecessor/successor adjacency, dominator
trees, and natural loop detection. Rather than computing this independently in
each pass, the function `run_gvn_licm_ivsr_shared` builds a single
`CfgAnalysis` per function and passes it to all three.

This sharing is sound because:

- **GVN** only replaces operands within existing instructions; it does not add,
  remove, or reorder blocks, so the CFG structure is unchanged after GVN.
- **LICM** hoists instructions into preheader blocks but does not add or remove
  blocks from the CFG, so the analysis remains valid for IVSR.

For single-block functions, only GVN runs (via a fast path that skips CFG
analysis entirely), since LICM and IVSR require at least two blocks to form a
loop.

---

## Pass Descriptions

### cfg_simplify -- CFG Simplification

Simplifies the control flow graph through several transformations: folding
known-constant conditional branches and switches to unconditional branches,
converting conditional branches where both targets are identical to unconditional
branches, threading jump chains (redirecting branches through empty intermediate
blocks directly to the final target), removing unreachable blocks with no
predecessors, simplifying trivial phi nodes (single incoming value or all values
identical) to copies, and merging single-predecessor blocks into their
predecessor. Runs to a fixpoint within each invocation, since one simplification
can enable others. Jump chain threading is depth-limited to 32 hops to prevent
pathological cases.

This pass appears twice in each iteration of the main loop: once at the
beginning (to clean up from the previous iteration's constant folding and DCE)
and once at the end (to clean up after the current iteration's DCE and
if-conversion).

### copy_prop -- Copy Propagation

Replaces uses of a `Copy` instruction's destination with the `Copy`'s source
operand, transitively following chains of copies. This is important because many
other passes produce copies: phi elimination, mem2reg, algebraic simplification
(when an identity like `x + 0` reduces to `x`), and GVN (when a redundant
computation is replaced with a reference to an earlier equivalent). Without copy
propagation, each copy becomes a redundant load-store pair in code generation.

Uses a flat `Vec<Option<Operand>>` indexed by value ID for O(1) lookups, taking
advantage of the dense sequential nature of SSA value IDs. After copy
propagation, the dead `Copy` instructions themselves are cleaned up by DCE.

This pass also appears twice in each iteration: once early (after CFG
simplification) and once late (after GVN, LICM, and if-conversion), to
propagate copies generated by intermediate passes.

### div_by_const -- Division by Constant Strength Reduction

Replaces integer division and modulo by compile-time constants with equivalent
multiply-and-shift sequences. On x86, `div`/`idiv` instructions cost 20-90
cycles, while the replacement sequence costs 3-5 cycles, making this one of the
most impactful single optimizations for integer-heavy code.

Supported transformations:

- **Unsigned division** (`x /u C`): replaced with a widened multiply by a magic
  number followed by a right shift, using the algorithm from Hacker's Delight
  by Henry S. Warren Jr.
- **Signed division by power-of-2** (`x /s 2^k`): replaced with a biased
  arithmetic right shift that handles negative rounding correctly.
- **Signed division by constant** (`x /s C`): replaced with a magic-number
  multiply, shift, and sign correction.
- **Modulo** (`x % C`): rewritten as `x - (x / C) * C` using the optimized
  division from above.

Only 32-bit divisions are currently optimized. The pass is disabled on i686
targets because the generated 64-bit multiply sequences cannot be executed
correctly by the 32-bit backend. It runs only during the first iteration of the
main loop; subsequent iterations clean up the expanded instruction sequences.

### narrow -- Integer Narrowing

Eliminates unnecessary widening introduced by C's integer promotion rules. The
compiler's frontend promotes sub-64-bit operands to `I64` before performing
arithmetic, then narrows the result back. This creates a
widen-operate-narrow pattern that generates redundant sign/zero-extension
instructions.

The pass detects patterns like:

```
%w = Cast %x, I32 -> I64       (widen)
%r = BinOp add %w, %y, I64    (operate at I64)
%n = Cast %r, I64 -> I32       (narrow)
```

And rewrites them to operate directly at the narrow type:

```
%r = BinOp add %x, narrow(%y), I32
```

This is safe for arithmetic operations (add, sub, mul, and, or, xor, shl)
because the narrowing cast truncates the result, and the low bits are identical
regardless of operation width. Right shifts are safe when the extension type
matches the shift type (arithmetic shift with sign extension, logical shift with
zero extension). Comparisons where both operands are widened from the same type
can also be narrowed, since extension preserves ordering.

### simplify -- Algebraic Simplification

Applies algebraic identities and strength reductions to individual instructions.
Identity simplifications include `x + 0 => x`, `x * 1 => x`, `x & x => x`,
`x ^ 0 => x`, and many others for all binary operators. Strength reductions
convert expensive operations to cheaper equivalents: `x * 2^k => x << k`,
`x * 2 => x + x`, `x * (-1) => -x`, and `x /u 2^k => x >> k`.

The pass also performs constant reassociation: when the left operand of a binary
operation is itself a binary operation with a constant, the two constants are
combined. For example, `(x + 3) + 5` becomes `x + 8`, and similarly for
subtraction, multiplication, and bitwise operations.

Float-unsafe simplifications (such as `x + 0.0` and `x * 0.0`) are restricted
to integer types to preserve IEEE 754 semantics around signed zeros, NaN
propagation, and infinity arithmetic.

### constant_fold -- Constant Folding

Evaluates operations whose operands are all compile-time constants, replacing the
instruction with the computed result. This covers binary operations, unary
operations, comparisons, casts, `GetElementPtr` with constant base and offset,
and `Select` with a constant condition. The folded constants then enable further
optimizations: constant branches become foldable by CFG simplification, and dead
branches create opportunities for DCE.

This pass also handles the `IsConstant` unary operation (implementing
`__builtin_constant_p`): if the operand is a constant, the instruction is folded
to `1`; otherwise it is left for the Phase 0.5 resolution step.

### gvn -- Global Value Numbering

A dominator-based common subexpression elimination (CSE) pass. It walks the
dominator tree in depth-first order, maintaining scoped hash tables that map
expression keys to previously computed values. When an instruction computes an
expression that has already been computed in a dominating block, the instruction
is replaced with a copy of the earlier result.

Value-numbered expression types:

- **BinOp** -- with commutative operand canonicalization, so `a + b` and `b + a`
  receive the same value number.
- **UnaryOp** -- unary operations.
- **Cmp** -- comparisons.
- **Cast** -- type conversions keyed by source and destination type.
- **GetElementPtr** -- base + offset address computations.
- **Load** -- redundant load elimination: two loads from the same pointer with
  the same type produce the same value if no intervening memory modification
  occurs. Load value numbers are invalidated by stores, calls, and other
  memory-clobbering instructions.

The scoped hash tables are restored to their previous state when the dominator
tree walk backtracks, using the same scoping pattern as the SSA rename phase in
mem2reg.

### licm -- Loop-Invariant Code Motion

Identifies natural loops in the CFG and hoists loop-invariant instructions to
preheader blocks that execute before the loop. An instruction is loop-invariant
if all its operands are constants, defined outside the loop, or themselves
defined by loop-invariant instructions.

This is important for array index computations, address calculations that depend
on outer loop variables, and casts/extensions of values that do not change within
the loop.

Safety rules:

- **Pure instructions** (arithmetic, casts, GEP) are always hoisted.
- **Loads** are hoisted only when the memory location is provably unmodified
  inside the loop: loads from non-address-taken allocas with no in-loop stores,
  and loads from global addresses when the loop contains no calls and no stores
  to any global target.
- **Address-taken allocas** are never hoisted because stores through derived
  pointers may not be tracked.

The pass requires loops to have a single-entry preheader block; loops with
multiple outside predecessors are skipped.

### iv_strength_reduce -- Induction Variable Strength Reduction

Transforms expensive per-iteration index computations in loops into cheaper
pointer increment operations. For a typical array access pattern:

```c
for (int i = 0; i < n; i++) sum += arr[i];
```

The IR computes `base + i * sizeof(int)` every iteration via a cast, shift (or
multiply), and GEP. After strength reduction, the loop maintains a running
pointer that is incremented by `sizeof(int)` each iteration, eliminating the
per-iteration multiply and cast.

The pass identifies basic induction variables (phi nodes with a constant additive
step), then finds derived expressions (multiplications or shifts by constants)
that feed into GEP instructions. Each eligible derived expression is replaced
with a new phi-based pointer induction variable. The dead original computations
are removed by subsequent DCE. Stride is limited to 1024 bytes to avoid
transforming unusual access patterns, and cast chains are followed up to a depth
of 10 to find the root induction variable.

Runs only during the first iteration of the main loop. Uses shared CFG analysis
with GVN and LICM.

### if_convert -- If-Conversion

Converts simple diamond-shaped branch-and-phi patterns into `Select`
instructions, which lower to conditional moves (`cmov` on x86, `csel` on
AArch64). The target pattern is:

```
pred:
    condbranch %cond, true_block, false_block
true_block:                          false_block:
    (0-1 simple instructions)           (0-1 simple instructions)
    branch merge                        branch merge
merge:
    %result = phi [true_val, true_block], [false_val, false_block]
```

This is rewritten to:

```
pred:
    %result = select %cond, true_val, false_val
    branch merge
```

Only converts when both arms are side-effect-free (no stores, calls, or memory
operations), ensuring that the `Select` semantics (both operands are evaluated)
are safe. This is critical for performance in tight loops with simple
conditionals, such as the sliding-window hash function in zlib.

The pass iterates to a fixpoint within each invocation, since converting one
diamond may expose another.

### ipcp -- Interprocedural Constant Propagation

An interprocedural pass that performs three optimizations across function
boundaries:

1. **Constant return propagation**: identifies static functions that return the
   same constant on every return path and replaces calls to those functions with
   the constant value.
2. **Dead call elimination**: removes calls to side-effect-free void functions
   with empty bodies (stubs). This eliminates references to symbols that would
   otherwise cause linker errors.
3. **Constant argument propagation**: when all call sites of a defined function
   pass the same constant for a given parameter, replaces the `ParamRef` in the
   function body with a copy of that constant. Subsequent constant folding, DCE,
   and CFG simplification then eliminate dead code guarded by that parameter.

IPCP runs at the end of every iteration (not just the first), because earlier
passes within the same iteration may simplify call arguments to constants (e.g.,
phi nodes collapsed after CFG simplification resolves dead branches). When IPCP
makes changes, the pipeline always runs at least one more iteration to allow
cleanup passes to process the newly exposed constants.

### dce -- Dead Code Elimination

Removes instructions whose results are never used by any other instruction or
terminator. Uses a use-count-based worklist algorithm with O(n) complexity:

1. Build use counts for all values in a single forward pass.
2. Build a definition map from value IDs to their defining instruction
   locations, excluding side-effecting instructions (stores, calls).
3. Seed the worklist with instructions that have zero use count.
4. Process the worklist: for each dead instruction, decrement its operands' use
   counts; if any operand's count drops to zero, add its defining instruction to
   the worklist.
5. Sweep all dead instructions in a single pass.

Side-effecting instructions are never removed regardless of whether their result
is used.

### dead_statics -- Dead Static Function and Global Elimination

Removes internal-linkage (`static`) functions and `static const` globals that
are unreachable from any externally visible symbol. Uses a BFS reachability
analysis starting from roots (all non-static symbols and address-taken
functions), following references through function bodies and global initializers.
Any static symbol not reached during BFS is removed from the module.

### resolve_asm -- Inline Assembly Symbol Resolution

A post-inlining fixup pass that resolves symbolic references in inline assembly
operands. After inlining a function like `_static_cpu_has(u16 bit)` with a
constant argument, the IR may contain a `GlobalAddr` + `GEP` chain that
represents a specific offset into a global variable. This pass traces those
definition chains and sets the appropriate `input_symbols` entry (e.g.,
`"boot_cpu_data+74"`) so the backend can emit correct symbol references in the
assembly output.

### loop_analysis -- Shared Loop Analysis Utilities

A utility module (not a standalone pass) providing natural loop detection and
loop body computation used by both LICM and IVSR. It identifies natural loops by
finding back edges in the CFG (edges where the target dominates the source),
computes loop bodies by collecting all blocks that can reach the back-edge source
without going through the header, and merges loops that share the same header
block into a single combined loop.

---

## Pass Dependency Graph

The following diagram shows which passes create optimization opportunities for
which other passes. An arrow from A to B means "changes made by A may enable
further changes in B."

```
cfg_simplify ---------> copy_prop, gvn, licm, dce, if_convert
copy_prop ------------> simplify, constant_fold, gvn, licm, narrow
narrow ---------------> simplify, constant_fold
simplify -------------> constant_fold, copy_prop, gvn
constant_fold --------> cfg_simplify, copy_prop, dce, if_convert
gvn ------------------> copy_prop, licm, dce
licm -----------------> copy_prop, dce
if_convert -----------> copy_prop, constant_fold, cfg_simplify, dce
dce ------------------> cfg_simplify
ipcp -----------------> constant_fold, dce, cfg_simplify
```

Note: The edges above reflect the actual `should_run!` macro dependencies in
`mod.rs`. An arrow from A to B means "B's `should_run!` check includes A as
an upstream dependency." For example, `licm` depends on `cfg_simplify`,
`copy_prop`, and `gvn` -- if none of those made changes, LICM is skipped.

This dependency graph is encoded in the `should_run!` macro invocations in
`mod.rs` and drives the per-pass skip logic described above.

---

## Disabling Individual Passes

Individual passes can be disabled at runtime for debugging by setting the
`CCC_DISABLE_PASSES` environment variable to a comma-separated list of pass
names:

```
CCC_DISABLE_PASSES=gvn,licm ./ccc input.c -o output.o
```

Recognized names: `all`, `inline`, `cfg`, `copyprop`, `narrow`, `simplify`,
`constfold`, `gvn`, `licm`, `ifconv`, `dce`, `ipcp`, `divconst`, `ivsr`.

Setting the variable to `all` skips the entire optimization pipeline.

Pass timing information can be enabled with:

```
CCC_TIME_PASSES=1 ./ccc input.c -o output.o
```

This prints per-pass, per-function timing and change counts to stderr, which is
useful for identifying performance bottlenecks and understanding convergence
behavior across iterations.

---

## Files

| File                     | Description                                            |
|--------------------------|--------------------------------------------------------|
| `mod.rs`                 | Pipeline orchestration, dirty tracking, shared analysis |
| `cfg_simplify.rs`        | CFG simplification (branch folding, jump threading)    |
| `constant_fold.rs`       | Constant expression evaluation at compile time         |
| `copy_prop.rs`           | Copy propagation                                       |
| `dce.rs`                 | Dead code elimination (use-count worklist)              |
| `dead_statics.rs`        | Dead static function/global elimination (BFS)          |
| `div_by_const.rs`        | Division by constant strength reduction                |
| `gvn.rs`                 | Dominator-based global value numbering / CSE           |
| `if_convert.rs`          | Branch+phi diamond to Select conversion                |
| `inline.rs`              | Function inlining                                      |
| `ipcp.rs`                | Interprocedural constant propagation                   |
| `iv_strength_reduce.rs`  | Loop induction variable strength reduction             |
| `licm.rs`                | Loop-invariant code motion                             |
| `loop_analysis.rs`       | Shared loop detection and body computation utilities   |
| `narrow.rs`              | Integer narrowing (C promotion pattern elimination)    |
| `resolve_asm.rs`         | Post-inline assembly symbol resolution                 |
| `simplify.rs`            | Algebraic simplification and strength reduction        |
