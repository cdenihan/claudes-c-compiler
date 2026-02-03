# IR Subsystem -- Design Document

This document describes the intermediate representation (IR) used by the
compiler. The IR is a target-independent, SSA-based representation that sits
between the C language AST and the platform-specific backend code generators.

## Overview

The IR subsystem has three components:

1. **Core IR types** -- data structures that define the in-memory representation
   of a compiled translation unit (`ir.rs`, `module.rs`, `instruction.rs`,
   `ops.rs`, `constants.rs`, `intrinsics.rs`, `analysis.rs`).
2. **Lowering** (`lowering/`) -- translates the AST into alloca-based IR where
   every local variable lives in a stack slot.
3. **mem2reg** (`mem2reg/`) -- promotes eligible stack allocas to SSA registers
   by inserting phi nodes using dominance frontiers.

These components form a pipeline:

```
  AST
   |
   |  lowering/
   v
  Alloca-based IR  (every local is a stack slot: alloca + load/store)
   |
   |  mem2reg/
   v
  SSA IR           (scalars promoted to virtual registers with phi nodes)
   |
   |  optimization passes, then phi elimination
   v
  Backend-ready IR (phi nodes lowered to copies)
```

## Module Structure

```
IrModule
 |
 |-- functions: Vec<IrFunction>
 |    |
 |    |-- name, return_type, params: Vec<IrParam>
 |    |-- blocks: Vec<BasicBlock>
 |    |    |
 |    |    |-- label: BlockId(u32)         -- e.g. .LBB0, .LBB5
 |    |    |-- instructions: Vec<Instruction>
 |    |    |-- terminator: Terminator
 |    |    +-- source_spans: Vec<Span>     -- parallel to instructions (debug info)
 |    |
 |    |-- is_variadic, is_declaration, is_static, is_inline, ...
 |    +-- next_value_id: u32              -- cached upper bound on Value IDs
 |
 |-- globals: Vec<IrGlobal>
 |    |-- name, ty, size, align
 |    |-- init: GlobalInit                -- Zero | Scalar | Array | String | ...
 |    +-- is_static, is_extern, is_weak, is_thread_local, ...
 |
 |-- string_literals: Vec<(label, value)>
 |-- wide_string_literals, char16_string_literals
 |-- constructors, destructors           -- __attribute__((constructor/destructor))
 |-- aliases                             -- __attribute__((alias("target")))
 |-- toplevel_asm                        -- top-level asm("...") directives
 +-- symbol_attrs                        -- .weak / .hidden / .protected directives
```

### Key Identifiers

| Type      | Representation | Purpose                                         |
|-----------|----------------|--------------------------------------------------|
| `Value`   | `Value(u32)`   | Names an SSA value (virtual register)            |
| `BlockId` | `BlockId(u32)` | Names a basic block; formats as `.LBB{id}`       |
| `Operand` | enum           | Either a `Value` reference or an inline `IrConst` |

Values are created by instructions that produce a result (loads, binary ops,
casts, etc.). Instructions that only produce side effects (stores, fences,
memcpy) have no destination value. The method `Instruction::dest()` returns
`Some(Value)` for defining instructions and `None` for effectful-only ones.

## Type System

The IR uses a small, target-independent type vocabulary defined by `IrType`
(in `common/types`):

| IR Type    | Width   | Meaning                                  |
|------------|---------|------------------------------------------|
| `I8`       | 8-bit   | `char`, `_Bool`, byte-sized signed       |
| `I16`      | 16-bit  | `short` (signed)                         |
| `I32`      | 32-bit  | `int` (signed)                           |
| `I64`      | 64-bit  | `long`, `long long` (signed)             |
| `I128`     | 128-bit | `__int128` (signed)                      |
| `U8`       | 8-bit   | `unsigned char`                          |
| `U16`      | 16-bit  | `unsigned short`                         |
| `U32`      | 32-bit  | `unsigned int`                           |
| `U64`      | 64-bit  | `unsigned long`, `unsigned long long`    |
| `U128`     | 128-bit | `unsigned __int128`                      |
| `F32`      | 32-bit  | `float`                                  |
| `F64`      | 64-bit  | `double`                                 |
| `F128`     | 128-bit | `long double` (quad precision)           |
| `Ptr`      | target  | All pointer types (opaque)               |
| `Void`     | 0       | Void type (used for void returns, etc.)  |

Pointers are opaque at the IR level. The `IrType` attached to load/store
instructions determines the access width; the pointer itself carries no
pointee type information.

## Constants

`IrConst` represents compile-time constant values:

- **Integer**: `I8(i8)`, `I16(i16)`, `I32(i32)`, `I64(i64)`, `I128(i128)`
- **Float**: `F32(f32)`, `F64(f64)`
- **Long double**: `LongDouble(f64, [u8; 16])` -- carries both an f64
  approximation (for constant folding where full precision is not critical)
  and the raw IEEE 754 binary128 bytes (for exact code emission). On AArch64
  and RISC-V, the f128 bytes are used directly. On x86, they are converted to
  x87 80-bit extended precision at emission time.
- **Zero**: `Zero` -- the zero-initializer for any type.

`ConstHashKey` provides a hashable wrapper that uses raw bit patterns for
floating-point values, enabling constants to serve as keys in hash maps for
value numbering.

The `constants` module also provides float encoding utilities:
`f64_to_f128_bytes` converts an f64 to IEEE 754 binary128 (quad-precision)
for AArch64/RISC-V, and `f64_to_x87_bytes` converts to x86 80-bit extended.

## Instruction Set

The `Instruction` enum defines 38 variants organized into the following
categories.

### Memory

| Instruction     | Form                                    | Description                          |
|-----------------|-----------------------------------------|--------------------------------------|
| `Alloca`        | `%d = alloca ty, size, align`           | Static stack slot allocation         |
| `DynAlloca`     | `%d = dynalloca size_op, align`         | Dynamic stack allocation (`__builtin_alloca`) |
| `Load`          | `%d = load ptr, ty`                     | Load from memory                     |
| `Store`         | `store val, ptr, ty`                    | Store to memory                      |
| `Memcpy`        | `memcpy dest, src, size`                | Block memory copy                    |
| `GetElementPtr` | `%d = gep base, offset, ty`             | Pointer arithmetic                   |
| `GlobalAddr`    | `%d = globaladdr "name"`                | Address of a global symbol           |
| `StackSave`     | `%d = stacksave`                        | Capture stack pointer (VLA support)  |
| `StackRestore`  | `stackrestore ptr`                      | Restore stack pointer (VLA support)  |

`Load` and `Store` accept an `AddressSpace` field (`seg_override`) for x86
segment register prefixes (`%gs:`, `%fs:`) used with named address spaces.

The `Alloca` instruction has a `volatile` flag that prevents mem2reg from
promoting it to an SSA register. This is required for `volatile`-qualified
locals that must survive `setjmp`/`longjmp`.

### Arithmetic and Logic

| Instruction | Form                             | Description                         |
|-------------|----------------------------------|-------------------------------------|
| `BinOp`     | `%d = op lhs, rhs, ty`          | Binary arithmetic/logic/shift       |
| `UnaryOp`   | `%d = op src, ty`                | Unary operations                    |
| `Cmp`       | `%d = cmp op lhs, rhs, ty`      | Comparison (produces boolean)       |
| `Cast`      | `%d = cast src, from_ty, to_ty`  | Type conversion                     |
| `Copy`      | `%d = copy src`                  | Value copy (identity)               |
| `Select`    | `%d = select cond, t, f, ty`    | Conditional select (branchless)     |

**Binary operations** (`IrBinOp`): `Add`, `Sub`, `Mul`, `SDiv`, `UDiv`,
`SRem`, `URem`, `And`, `Or`, `Xor`, `Shl`, `AShr`, `LShr`. Each operation
carries methods `is_commutative()` and `can_trap()` (the latter is true for
division and remainder, which may cause SIGFPE and therefore must not be
speculatively executed by if-conversion). Constant folding is provided by
`eval_i64` and `eval_i128` methods that return `None` for division by zero.

**Unary operations** (`IrUnaryOp`): `Neg`, `Not`, `Clz`, `Ctz`, `Bswap`,
`Popcount`, `IsConstant` (for `__builtin_constant_p`, resolved after inlining
and constant propagation).

**Comparison operations** (`IrCmpOp`): `Eq`, `Ne`, `Slt`, `Sle`, `Sgt`,
`Sge`, `Ult`, `Ule`, `Ugt`, `Uge`. Signed variants use native ordering;
unsigned variants reinterpret bits. Constant evaluation is provided by
`eval_i64`, `eval_i128`, and `eval_f64`.

### Control Flow (Terminators)

Each basic block ends with exactly one `Terminator`:

| Terminator       | Form                                     | Description                        |
|------------------|------------------------------------------|------------------------------------|
| `Return`         | `ret [val]`                              | Function return                    |
| `Branch`         | `br label`                               | Unconditional jump                 |
| `CondBranch`     | `br cond, true_label, false_label`       | Conditional branch                 |
| `IndirectBranch` | `indirectbr target, [labels...]`         | Computed goto (`goto *ptr`)        |
| `Switch`         | `switch val, default, [(v, label)...]`   | Multi-way dispatch (jump table)    |
| `Unreachable`    | `unreachable`                            | After noreturn calls               |

### Function Calls

| Instruction     | Form                                    | Description                        |
|-----------------|-----------------------------------------|------------------------------------|
| `Call`          | `%d = call "func"(args...)`             | Direct function call               |
| `CallIndirect`  | `%d = call_indirect ptr(args...)`       | Indirect call through pointer      |

Both share a `CallInfo` struct that consolidates ABI metadata: argument types,
return type, variadic flag, number of fixed (non-variadic) parameters, struct
argument sizes/alignments, per-eightbyte SysV AMD64 classifications, RISC-V
float classifications, sret flag, fastcall flag, and return struct eightbyte
classifications.

### Variadic Argument Support

| Instruction    | Description                                            |
|----------------|--------------------------------------------------------|
| `VaStart`      | Initialize a `va_list`                                 |
| `VaEnd`        | Clean up a `va_list`                                   |
| `VaCopy`       | Copy one `va_list` to another                          |
| `VaArg`        | Extract the next scalar variadic argument              |
| `VaArgStruct`  | Extract a struct variadic argument into a buffer       |

`VaArgStruct` carries SysV AMD64 eightbyte classifications so the backend can
determine whether a struct's eightbytes come from register save areas or the
overflow area. The ABI rule that multi-eightbyte structs must be entirely in
registers or entirely on the stack is enforced at the backend level using
these classifications.

### Atomics

| Instruction       | Description                                               |
|-------------------|-----------------------------------------------------------|
| `AtomicRmw`       | Atomic read-modify-write (`fetch_add`, `fetch_xor`, etc.) |
| `AtomicCmpxchg`   | Compare-and-exchange (returns old value or success bool)   |
| `AtomicLoad`      | Atomic load with ordering                                 |
| `AtomicStore`     | Atomic store with ordering                                |
| `Fence`           | Memory fence                                              |

Atomic operations carry an `AtomicOrdering` (`Relaxed`, `Acquire`, `Release`,
`AcqRel`, `SeqCst`). The `AtomicRmwOp` enum covers `Add`, `Sub`, `And`, `Or`,
`Xor`, `Nand`, `Xchg`, and `TestAndSet`.

`AtomicCmpxchg` has a `returns_bool` flag: when true, the destination receives
a success/failure boolean (for `__atomic_compare_exchange_n`); when false, it
receives the old value (for `__sync_val_compare_and_swap`).

### SSA

| Instruction | Form                                    | Description                        |
|-------------|-----------------------------------------|------------------------------------|
| `Phi`       | `%d = phi [(val, block), ...]`          | Merge values at control flow joins |
| `ParamRef`  | `%d = paramref idx, ty`                 | Reference incoming parameter value |

`Phi` nodes are inserted by mem2reg and consumed by phi elimination before
backend codegen. `ParamRef` is emitted during lowering in the entry block
alongside parameter allocas; it makes the incoming parameter value explicit
in the IR so that mem2reg can promote parameter allocas to SSA.

### Complex Number Returns

| Instruction              | Description                                       |
|--------------------------|---------------------------------------------------|
| `GetReturnF64Second`     | Read second f64 from a `_Complex double` return   |
| `SetReturnF64Second`     | Set second f64 before returning `_Complex double`  |
| `GetReturnF32Second`     | Read second f32 from a `_Complex float` return     |
| `SetReturnF32Second`     | Set second f32 before returning `_Complex float`   |
| `GetReturnF128Second`    | Read second f128 from a `_Complex long double` return |
| `SetReturnF128Second`    | Set second f128 before returning `_Complex long double` |

These instructions handle the platform-specific multi-register return
conventions for C `_Complex` types. `Get` variants must appear immediately
after a `Call`/`CallIndirect`; `Set` variants must appear immediately before
a `Return` terminator.

### Inline Assembly and Intrinsics

| Instruction  | Description                                                |
|--------------|------------------------------------------------------------|
| `InlineAsm`  | Inline assembly with constraints, clobbers, and goto labels |
| `Intrinsic`  | Target-independent intrinsic operation                      |
| `LabelAddr`  | Address of a label (GCC `&&label` extension)               |

The `InlineAsm` instruction carries a template string, output/input operands
with constraints, a clobber list, goto labels (for `asm goto`), per-operand
symbol names (for `%P`/`%a` modifiers), and segment override information.
Goto labels also generate implicit CFG edges in `build_cfg`.

**Intrinsics** (`IntrinsicOp`) cover a wide range of target-independent
operations that each backend maps to native instructions:

- **Fences/barriers**: `Lfence`, `Mfence`, `Sfence`, `Pause`, `Clflush`
- **Non-temporal stores**: `Movnti`, `Movnti64`, `Movntdq`, `Movntpd`
- **SSE2 packed integer**: `Pcmpeqb128`, `Pcmpeqd128`, `Psubusb128`, `Por128`,
  `Pand128`, `Pxor128`, `Pmovmskb128`, `SetEpi8`, `SetEpi32`
- **SSE2 shuffle/pack/unpack**: `Pshufd`, `Packssdw`, `Punpcklbw`, etc.
- **SSE2/SSE4.1 insert/extract**: `Pinsrw`, `Pextrw`, `Pinsrd`, etc.
- **128-bit load/store**: `Loaddqu`, `Storedqu`
- **AES-NI**: `Aesenc128`, `Aesenclast128`, `Aesdec128`, `Aesdeclast128`,
  `Aesimc128`, `Aeskeygenassist128`
- **CLMUL**: `Pclmulqdq128`
- **CRC32**: `Crc32_8`, `Crc32_16`, `Crc32_32`, `Crc32_64`
- **Scalar math**: `SqrtF32`, `SqrtF64`, `FabsF32`, `FabsF64`
- **Frame/return address**: `FrameAddress`, `ReturnAddress`, `ThreadPointer`

## Global Variables

`IrGlobal` represents a global variable with:

- **Identity**: name, type (`IrType`), size in bytes, alignment in bytes.
- **Initializer** (`GlobalInit`): one of `Zero` (`.bss`), `Scalar`,
  `Array`, `String` (with null terminator), `WideString` (u32 `wchar_t`
  values), `Char16String` (u16 values), `GlobalAddr` (address of another
  symbol), `GlobalAddrOffset` (symbol + byte offset, for `&arr[3]` or
  `&s.field`), `Compound` (nested initializer sequence for arrays/structs
  containing address expressions), or `GlobalLabelDiff` (label difference
  `&&lab1 - &&lab2` for computed goto dispatch tables).
- **Linkage**: `is_static`, `is_extern`, `is_common`
  (`__attribute__((common))`), `is_weak`.
- **Placement**: optional `section` (`__attribute__((section(...)))`),
  `is_const` (placed in `.rodata`), `is_thread_local` (placed in
  `.tdata`/`.tbss`).
- **Visibility**: optional `visibility` (`hidden`, `default`, `protected`).
- **Other**: `is_used` (`__attribute__((used))`), `has_explicit_align`
  (prevents auto-promotion to 16-byte alignment).

`GlobalInit` provides a `for_each_ref` method to visit all symbol names
referenced by an initializer (recursing into `Compound` children), and
`emitted_byte_size` to compute the total bytes that will be emitted.

## SSA Construction: The Two-Phase Approach

The compiler uses a standard two-phase approach to SSA construction, which
avoids the complexity of building SSA directly during lowering.

### Phase 1: Alloca-Based Lowering

The lowering pass (`lowering/`) translates AST nodes into IR where every local
variable is represented as a stack allocation:

```
entry:
    %1 = alloca i32, 4, 4       // int x;
    %2 = alloca i32, 4, 4       // int y;
    %3 = paramref 0, i32        // incoming parameter value
    store %3, %1, i32           // x = param0
    ...
    %5 = load %1, i32           // read x
    %6 = binop add %5, const(1) // x + 1
    store %6, %2, i32           // y = x + 1
```

This representation is simple to generate because every variable reference
translates to a load or store through the alloca pointer, regardless of
control flow complexity. No SSA renaming or phi placement is needed at this
stage.

Parameters are handled by emitting a `ParamRef` instruction followed by a
store to the parameter's alloca, making the initial value visible in the IR.

See [lowering/README.md](lowering/README.md) for detailed documentation of the
lowering pass.

### Phase 2: mem2reg (SSA Promotion)

The mem2reg pass (`mem2reg/`) promotes eligible stack allocas to SSA virtual
registers, inserting phi nodes where needed. The algorithm proceeds in five
steps:

1. **Identify promotable allocas.** An alloca is promotable if it is scalar
   (at most `MAX_PROMOTABLE_ALLOCA_SIZE` = 8 bytes), is only accessed through
   direct loads and stores (not address-taken by GEP, memcpy, va_start, inline
   asm memory-only constraints, etc.), and is not marked `volatile`.

2. **Build the CFG and compute the dominator tree.** The CFG is built from
   block terminators (and inline asm goto edges). The dominator tree is
   computed using the Cooper-Harvey-Kennedy algorithm (see the Analysis
   section below).

3. **Compute dominance frontiers.** For each block, the dominance frontier
   is the set of blocks where its dominance ends -- the join points in the
   CFG where phi nodes may be needed.

4. **Insert phi nodes at iterated dominance frontiers.** For each promotable
   alloca, phi nodes are placed at the iterated dominance frontier of its
   defining blocks (blocks containing stores to that alloca).

5. **Rename variables via dominator-tree DFS.** Walk the dominator tree
   depth-first, maintaining a stack of reaching definitions for each alloca.
   Replace loads with the current reaching definition and update the stack at
   stores. Fill in phi node operands from predecessor reaching definitions.

After mem2reg, the original alloca/load/store sequences are replaced with
direct value references and phi nodes, yielding proper SSA form.

The pass runs twice in the compilation pipeline:

- **Before inlining** (`promote_allocas`): promotes non-parameter allocas.
  Parameter allocas are kept because the inliner assumes they exist for
  argument passing.
- **After inlining** (`promote_allocas_with_params`): promotes all allocas
  including parameters, since inlining is complete and parameter values are
  now explicit via `ParamRef` + `Store`.

See [mem2reg/README.md](mem2reg/README.md) for detailed documentation of the
mem2reg and phi elimination passes.

### Phi Elimination

After all SSA-based optimization passes complete, phi nodes must be lowered
before backend code generation. The phi elimination pass
(`mem2reg/phi_eliminate.rs`) converts each phi into copy instructions placed in
predecessor blocks.

The pass handles two cases:

- **Non-conflicting phis** (the common case): a direct `Copy` instruction is
  placed at the end of each predecessor block, before the terminator.
- **Conflicting phis** (cycles, e.g., swap patterns): the pass analyzes the
  copy graph per predecessor to detect cycles and introduces shared temporaries
  with a two-phase copy sequence to avoid the lost-copy problem. First, source
  values involved in cycles are saved to temporaries in the predecessor. Then,
  at the top of the target block, destinations are filled from the temporaries.

**Critical edge splitting**: when a predecessor has multiple successors (e.g.,
a `CondBranch`) and the target has phis, placing copies at the predecessor's
end would execute them on all outgoing paths, corrupting values on other edges.
The pass inserts a trampoline block on the critical edge that contains only the
phi copies and branches unconditionally to the target. Trampoline block IDs
are allocated from a global counter to avoid label collisions across functions.

## CFG Analysis (`analysis.rs`)

The `analysis` module provides shared control flow and dominator analysis used
by mem2reg and optimization passes (GVN, LICM, if-conversion, IVSR).

### Data Structures

**`FlatAdj` (Compressed Sparse Row adjacency list).** The CFG is stored as a
CSR-format flat adjacency list instead of `Vec<Vec<usize>>`. This reduces
`n+1` heap allocations to exactly 2 per `build_cfg` call and improves cache
locality. The structure uses two arrays: `offsets[i]..offsets[i+1]` gives the
range of indices into `data` for the neighbors of node `i`. Access is via
`row(i)`, which returns a `&[u32]` slice of neighbors.

**`CfgAnalysis`.** A cached bundle of pre-computed analysis results
(predecessors, successors, immediate dominators, dominator tree children, and
block count) shared across multiple optimization passes within a single
pipeline iteration. Since passes like GVN only replace instruction operands
without modifying the CFG, these results remain valid across GVN, LICM, and
IVSR, avoiding redundant recomputation. Loop analysis is performed separately
by the `loop_analysis` module when needed by LICM and IVSR.

### Algorithms

| Function                       | Description                                       |
|--------------------------------|---------------------------------------------------|
| `build_label_map`              | Map from `BlockId` to block index                 |
| `build_cfg`                    | Build predecessor and successor `FlatAdj` lists   |
| `compute_reverse_postorder`    | DFS-based reverse postorder traversal             |
| `compute_dominators`           | Immediate dominator array (Cooper-Harvey-Kennedy)  |
| `compute_dominance_frontiers`  | Dominance frontier sets for phi insertion          |
| `build_dom_tree_children`      | Dominator tree children lists from idom array     |

**CFG construction** (`build_cfg`) iterates over each block's terminator to
extract successor edges (`Branch`, `CondBranch`, `IndirectBranch`, `Switch`).
It also scans instructions for `InlineAsm` goto labels, which create implicit
control flow edges. The function returns both predecessor and successor
adjacency lists in CSR format, using only 4 heap allocations total.

**Reverse postorder** (`compute_reverse_postorder`) performs a DFS from the
entry block (block 0) and collects blocks in postorder, then reverses. This
ordering is required by the dominator algorithm and ensures that each block is
processed after its dominator.

**Dominator computation** (`compute_dominators`) uses the Cooper-Harvey-Kennedy
algorithm from "A Simple, Fast Dominance Algorithm" (2001). It iterates over
blocks in reverse postorder, computing the immediate dominator of each block as
the intersection of its already-processed predecessors' dominators. The
`intersect` function walks two fingers up the dominator tree, guided by RPO
numbering, until they meet at the common dominator. The algorithm converges in
a small number of iterations for reducible CFGs (typically 2-3 passes over the
RPO). The entry block is its own dominator (`idom[0] = 0`). Unreachable blocks
are marked with `usize::MAX`.

**Dominance frontiers** (`compute_dominance_frontiers`) are computed from the
predecessor lists and the idom array. For each join point (block with 2 or more
predecessors), a runner walks up the dominator tree from each predecessor,
adding the join point to every block's frontier along the way, stopping when
it reaches the join point's immediate dominator. The result is
`Vec<FxHashSet<usize>>` where `df[b]` is the dominance frontier of block `b`.

**Dominator tree children** (`build_dom_tree_children`) inverts the idom array
into a children list. `children[b]` contains all blocks whose immediate
dominator is `b`. This tree is used by mem2reg's variable renaming DFS and by
GVN's dominator-tree-ordered traversal.

## Key Design Decisions

- **Alloca-based lowering, then mem2reg.** This separates concerns: the
  lowering pass handles C semantics without worrying about SSA, and mem2reg
  handles SSA construction without knowing C. This is the same strategy used
  by LLVM at `-O0` and above.

- **Numeric value IDs.** Values are identified by `Value(u32)`, not string
  names. This simplifies the IR, avoids name collision issues, and makes value
  comparison a simple integer comparison.

- **Target-independent types.** The IR uses abstract types (`I8`, `I16`,
  `I32`, `I64`, `Ptr`) and delegates ABI details (calling conventions,
  register assignment, struct layout) to the backend.

- **Dual-representation long double.** `IrConst::LongDouble(f64, [u8; 16])`
  carries both an f64 approximation for constant folding and the raw f128
  bytes for precise code emission. This avoids losing precision during
  optimization while keeping arithmetic simple.

- **Canonical value visitor.** `Instruction::for_each_used_value()` is the
  canonical way to iterate over all `Value` references used by an instruction.
  All passes should use it rather than duplicating the match arms.

- **CSR adjacency lists.** The CFG uses flat CSR storage (`FlatAdj`) rather
  than `Vec<Vec<usize>>` because `build_cfg` is called per-function by
  multiple passes. The flat layout reduces allocation overhead and improves
  cache locality.

## File Inventory

| File                        | Purpose                                                    |
|-----------------------------|------------------------------------------------------------|
| `mod.rs`                    | Module declarations                                        |
| `ir.rs`                     | Re-export hub for all IR types                             |
| `module.rs`                 | `IrModule`, `IrFunction`, `IrParam`, `IrGlobal`, `GlobalInit` |
| `instruction.rs`            | `Instruction`, `Terminator`, `BasicBlock`, `BlockId`, `Value`, `Operand`, `CallInfo` |
| `ops.rs`                    | `IrBinOp`, `IrUnaryOp`, `IrCmpOp`, `AtomicRmwOp`, `AtomicOrdering` with eval methods |
| `constants.rs`              | `IrConst`, `ConstHashKey`, float encoding utilities        |
| `intrinsics.rs`             | `IntrinsicOp` (SIMD, crypto, math, fences)                 |
| `analysis.rs`               | `FlatAdj`, CFG construction, dominator tree, dominance frontiers, `CfgAnalysis` |
| `lowering/`                 | AST-to-IR translation (see [lowering/README.md](lowering/README.md)) |
| `mem2reg/`                  | SSA promotion and phi elimination (see [mem2reg/README.md](mem2reg/README.md)) |
