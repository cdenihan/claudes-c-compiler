# IR Lowering

Translates the parsed AST into the IR representation. This is the largest module
because it handles every C language construct.

## Module Organization

- **lowering.rs** - Core `Lowerer` struct with sub-structs (`SwitchFrame`, `FunctionMeta`),
  `lower()` entry point (multi-pass: typedefs → signatures → enums → lowering),
  function lowering (`lower_function`), global declaration processing, VLA parameter
  stride computation, and pointer type resolution utilities. Also provides shared
  helpers: `resolve_typedef_derived()`, `register_function_meta()`, `intern_string_literal()`.
- **global_init.rs** - Global initializer lowering subsystem. Handles struct/union
  initializers (nested, designated, bitfield, flexible array members), array initializers
  (multi-dimensional, flat, pointer arrays), compound literals, and scalar initializers.
  Entry point is `lower_global_init()`. Uses `push_zero_bytes()` for padding emission.
- **expr.rs** - Core expression lowering. `lower_expr()` dispatches all AST `Expr` nodes.
  Contains: literals, identifiers, binary/unary ops, pointer arithmetic, casts, ternary,
  compound literals, sizeof, address-of, deref, member access, short-circuit (&&/||),
  inc/dec, and utility helpers (type inference, common_type, implicit casts).
- **expr_builtins.rs** - Builtin function intrinsics: `try_lower_builtin_call()` dispatches
  `__builtin_*` functions via the `BuiltinKind` enum. Includes FP classification builtins
  (fpclassify, isnan, isinf, isfinite, isnormal, signbit) using bit manipulation, and
  intrinsic helpers for CLZ/CTZ/bswap/popcount/parity.
- **expr_atomics.rs** - Atomic builtin lowering: `try_lower_atomic_builtin()` handles all
  `__atomic_*` and `__sync_*` operations. Uses table-driven dispatch for fetch-op and
  op-fetch families (via `classify_fetch_op`/`classify_op_fetch`), plus sequential matching
  for exchange, compare-exchange, load, store, test-and-set, fences, and lock-free queries.
- **expr_calls.rs** - Function call lowering: `lower_function_call()` handles argument
  evaluation with implicit casts (`lower_call_arguments`), call dispatch to direct/indirect
  calls (`emit_call_instruction`), sret/two-register return classification, and variadic
  function detection. Extracts shared `classify_struct_return()` helper.
- **expr_assign.rs** - Assignment and compound assignment: `lower_assign()` (including
  struct copy via memcpy), `lower_compound_assign()` (complex, scalar+complex-RHS, and
  standard scalar paths). Bitfield helpers: `resolve_bitfield_lvalue`, `store_bitfield`,
  `extract_bitfield`, `truncate_to_bitfield_value`. Arithmetic conversion helpers:
  `usual_arithmetic_conversions`, `promote_for_op`, `narrow_from_op`.
- **stmt.rs** - Statement lowering: control flow (if/while/for/switch/goto), declarations,
  and array initializer list processing
- **lvalue.rs** - Lvalue resolution (what has an address) and array address computation
- **types.rs** - `TypeSpecifier` → `IrType` mapping, sizeof/alignof (via `scalar_type_size_align`),
  constant expression evaluation, struct layout computation (`compute_struct_union_layout`)
- **structs.rs** - Struct/union layout cache, member offset resolution, struct base address computation
- **complex.rs** - Complex number (`_Complex`) lowering for arithmetic, assignment, and casts

## Architecture

The `Lowerer` struct groups its state into logical sub-structs:
- `VarInfo` - shared type metadata (ty, elem_size, is_array, pointee_type, struct_layout,
  is_struct, array_dim_strides, c_type) embedded in both `LocalInfo` and `GlobalInfo` via
  `Deref`. This enables `lookup_var_info(name)` to unify the common "check locals then
  globals" pattern into a single lookup returning `&VarInfo`.
- `LocalInfo` - local variable info: wraps `VarInfo` plus alloca, alloc_size, is_bool,
  static_global_name, vla_strides, vla_size. Derefs to `VarInfo`.
- `GlobalInfo` - global variable info: wraps `VarInfo`. Derefs to `VarInfo`.
- `SwitchFrame` - nested switch context stack (cases, default label, expression type)
- `FunctionMeta` - known function signatures (return types, param types, variadic flags, sret info)
- `DeclAnalysis` - shared declaration analysis result (type properties, array/pointer/struct info)
  used by both `lower_local_decl` and `lower_global_decl` to avoid duplicating ~80 lines
  of type analysis logic. Computed by `analyze_declaration()`, consumed by
  `VarInfo::from_analysis()`, then wrapped by `LocalInfo::from_analysis()` and
  `GlobalInfo::from_analysis()` builders.

### Unified Lookup Helpers

- `lookup_var_info(name)` → `Option<&VarInfo>`: checks locals then globals for shared metadata
- `resolve_field_ctype(base, field, is_ptr)` → `Option<CType>`: unified struct field CType
  resolution that dispatches to `resolve_member_field_ctype` or `resolve_pointer_member_field_ctype`

## How Lowering Works

1. **Pass 0**: scan typedefs so return/param types can be resolved
2. **Pass 1**: scan function prototypes and global declarations to register signatures
3. **Pass 2**: lower each function body and global initializer

Each function is lowered into basic blocks. The lowerer maintains:
- Scope tracking local variables and their alloca'd stack slots
- Struct layout cache mapping struct types to computed field offsets
- Break/continue/switch target stacks for control flow lowering
- Goto/label tracking for forward references

## Key Design Decisions

- **Alloca-based lowering**: All locals start as `alloca + load/store`. A future `mem2reg`
  pass will promote these to proper SSA with phi nodes.
- **Implicit cast insertion**: `emit_implicit_cast()` inserts `Cast` instructions at
  call sites, binary ops, and assignments for C's implicit type promotion rules.
- **Pointer arithmetic scaling**: `try_lower_pointer_arithmetic()` multiplies integer
  offsets by element size. Handles ptr+int, int+ptr, and ptr-ptr.
- **Short-circuit via control flow**: `&&` and `||` use conditional branches (not
  boolean ops) to implement short-circuit evaluation correctly.
- **Bitfield operations**: Unified through `resolve_bitfield_lvalue()` helper which
  deduplicates the common lvalue resolution logic across assign/compound-assign/inc-dec.

## Relationship to Other Modules

```
parser/AST + sema/types  →  lowering  →  ir::Module  →  passes + codegen
```

Reads from: AST types, CType/StructLayout, builtin function table.
Produces: `ir::Module` with `IrFunction`s containing basic blocks of instructions.
