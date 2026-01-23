# IR Lowering

Translates the parsed AST into the IR representation. This is the largest module
because it handles every C language construct.

## Module Organization

- **lowering.rs** - Core `Lowerer` struct, `lower()` entry point, function/global scaffolding,
  and shared emission helpers (`emit_string_to_alloca`, `zero_init_alloca`, `emit_array_element_store`)
- **expr.rs** - Expression lowering. `lower_expr()` is a thin dispatch to focused helpers:
  - `lower_binary_op` → `try_lower_pointer_arithmetic` / `lower_arithmetic_binop`
  - `lower_function_call` → `try_lower_builtin_call` / `lower_call_arguments` / `emit_call_instruction`
  - `lower_cast`, `lower_compound_literal`, `lower_address_of`, `lower_deref`
  - `lower_member_access`, `lower_pointer_member_access`
  - `lower_short_circuit` (&&, ||), `lower_conditional` (ternary)
  - `init_struct_fields` (shared between compound literals and local decl init)
- **stmt.rs** - Statement lowering: control flow (if/while/for/switch/goto), declarations,
  and array initializer list processing (`lower_array_init_recursive`)
- **lvalue.rs** - Lvalue resolution (what has an address) and array address computation
- **types.rs** - `TypeSpecifier` → `IrType` mapping, sizeof, constant expression evaluation
- **structs.rs** - Struct/union layout computation and member offset resolution

## How Lowering Works

1. **First pass**: scan top-level declarations to register globals and function signatures
2. **Second pass**: lower each function body and global initializer

Each function is lowered into basic blocks. The lowerer maintains:
- Scope stack tracking local variables and their alloca'd stack slots
- Struct layout cache mapping struct types to computed field offsets
- Break/continue target stacks for loop lowering
- Goto/label tracking for forward references

## Key Design Decisions

- **Alloca-based lowering**: All locals start as `alloca + load/store`. The `mem2reg`
  pass later promotes these to proper SSA with phi nodes.
- **Implicit cast insertion**: `emit_implicit_cast()` inserts `Cast` instructions at
  call sites, binary ops, and assignments for C's implicit type promotion rules.
- **Pointer arithmetic scaling**: `try_lower_pointer_arithmetic()` multiplies integer
  offsets by element size. Handles ptr+int, int+ptr, and ptr-ptr.
- **Struct init sharing**: `init_struct_fields()` is shared between compound literal
  init and local variable init lists, with designator support.
- **Short-circuit via control flow**: `&&` and `||` use conditional branches (not
  boolean ops) to implement short-circuit evaluation correctly.

## Relationship to Other Modules

```
parser/AST + sema/types  →  lowering  →  ir::Module  →  passes + codegen
```

Reads from: AST types, CType/StructLayout, builtin function table.
Produces: `ir::Module` with `IrFunction`s containing basic blocks of SSA instructions.
