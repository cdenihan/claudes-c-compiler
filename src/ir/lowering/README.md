# IR Lowering

Translates the parsed AST into alloca-based IR. This is the largest module because it
handles every C language construct. The `mem2reg` pass later promotes allocas to SSA.

## Module Organization

| File | Responsibility |
|---|---|
| `lowering.rs` | `Lowerer` struct, `lower()` entry point, `lower_function`, scope management, `DeclAnalysis` |
| `stmt.rs` | Statement dispatch (`lower_stmt`), `lower_local_decl`, `emit_struct_init`, control flow |
| `stmt_init.rs` | Local variable init helpers: expr-init, list-init, extern/func-decl handling |
| `stmt_return.rs` | Return statement: sret, two-reg struct, complex decomposition, scalar returns |
| `expr.rs` | Expression lowering: binary/unary ops, casts, ternary, sizeof, pointer arithmetic |
| `expr_builtins.rs` | `__builtin_*` intrinsics (fpclassify, clz, ctz, bswap, popcount, etc.) |
| `expr_atomics.rs` | `__atomic_*` and `__sync_*` operations via table-driven dispatch |
| `expr_calls.rs` | Function call lowering with ABI handling (sret, complex args, struct passing) |
| `expr_assign.rs` | Assignment/compound-assign, bitfield read-modify-write, type promotions |
| `expr_types.rs` | Expression type inference (`get_expr_type`, `expr_ctype`) |
| `lvalue.rs` | L-value resolution and array address computation |
| `types.rs` | `TypeSpecifier` to `IrType`/`CType`, sizeof/alignof |
| `structs.rs` | Struct/union layout cache, field offset resolution |
| `complex.rs` | `_Complex` arithmetic, assignment, and conversions |
| `global_init.rs` | Global initializer dispatch (struct, array, scalar, compound literal) |
| `global_init_bytes.rs` | Byte-level global init (struct/array to byte arrays) |
| `global_init_compound.rs` | Compound global init (multi-dimensional, designated) |
| `const_eval.rs` | Compile-time constant expression evaluation |
| `pointer_analysis.rs` | Tracks expressions producing struct addresses vs packed data |
| `ref_collection.rs` | Pre-pass to collect referenced static/extern symbols |

## Architecture

The `Lowerer` processes a `TranslationUnit` in multiple passes:

1. **Pass 0**: Collect typedefs so function signatures can resolve aliased types
2. **Pass 1**: Register function signatures (return types, param types, sret/variadic)
3. **Pass 2**: Lower each function body and global initializer into IR

### Key Types

- **`VarInfo`** - Shared type metadata (ty, elem_size, is_array, struct_layout, etc.)
  embedded in both `LocalInfo` and `GlobalInfo` via `Deref`
- **`DeclAnalysis`** - Computed once per declaration, bundles all type properties.
  Used by both local and global lowering to avoid duplicating type analysis
- **`FunctionMeta`** - Per-function metadata (return types, param types, sret info)
- **`ScopeFrame`** - Records additions/shadows per block scope. `pop_scope()` undoes
  changes in O(changes) rather than cloning entire HashMaps

### Key Helpers

- `shadow_local_for_scope(name)` - Remove local and track for scope restoration
- `register_block_func_meta(name, ...)` - Register function metadata for block-scope declarations
- `lower_return_expr(e)` - Handles all return conventions (sret, two-reg, complex, scalar)
- `lower_local_init_expr(...)` / `lower_local_init_list(...)` - Dispatch init by type

## Design Decisions

- **Alloca-based**: Every local gets an `Alloca`. `mem2reg` promotes to SSA later.
- **Scope stack**: Push/pop `ScopeFrame`s instead of cloning HashMaps at block boundaries.
- **Complex ABI**: `_Complex double` decomposes to two FP registers, `_Complex float`
  packs into I64, larger types use sret. Handled in `stmt_return.rs`.
- **Short-circuit**: `&&`/`||` use conditional branches, not boolean ops.

## Relationship to Other Modules

```
parser/AST + sema/types  →  lowering  →  ir::Module  →  mem2reg → passes → codegen
```
