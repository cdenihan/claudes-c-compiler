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
- **expr.rs** - Expression lowering. `lower_expr()` dispatches to focused helpers:
  - Binary ops: `lower_binary_op` → `try_lower_pointer_arithmetic` / `lower_arithmetic_binop`
  - Calls: `lower_function_call` → `try_lower_builtin_call` / `try_lower_atomic_builtin`
  - Intrinsics: `lower_unary_intrinsic` (CLZ/CTZ/Popcount), `lower_bswap_intrinsic`, `lower_parity_intrinsic`
  - Member access: `lower_member_access_impl` (unified for `.` and `->`)
  - Bitfield ops: `resolve_bitfield_lvalue` → `store_bitfield` / `extract_bitfield` / `mask_to_bitwidth`
  - Short-circuit (&&, ||), ternary, casts, compound literals, address-of, deref
- **stmt.rs** - Statement lowering: control flow (if/while/for/switch/goto), declarations,
  and array initializer list processing
- **lvalue.rs** - Lvalue resolution (what has an address) and array address computation
- **types.rs** - `TypeSpecifier` → `IrType` mapping, sizeof/alignof (via `scalar_type_size_align`),
  constant expression evaluation, struct layout computation (`compute_struct_union_layout`)
- **structs.rs** - Struct/union layout cache, member offset resolution, struct base address computation
- **complex.rs** - Complex number (`_Complex`) lowering for arithmetic, assignment, and casts

## Architecture

The `Lowerer` struct groups its state into logical sub-structs:
- `SwitchFrame` - nested switch context stack (cases, default label, expression type)
- `FunctionMeta` - known function signatures (return types, param types, variadic flags, sret info)
- `LocalInfo` / `GlobalInfo` - variable metadata for locals and globals

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
