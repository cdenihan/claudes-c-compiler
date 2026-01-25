Task: Fix flat initialization of static struct arrays with pointer fields
Status: Complete
Branch: master

Problem:
Static struct arrays initialized with a flat (brace-less) initializer list that
contain pointer fields were only initializing the first field of the first
struct element. Additionally, string literal initializers for char array fields
within structs containing pointer members produced all-zero data.

Example of flat init that was broken:
  static struct S arr[2] = {1, &g, 3, &g};  // Only arr[0].a=1 was set
  static struct S arr[2] = {"hello", &g1, "world", &g2};  // Strings were zeros

Root cause:
Two bugs in the "compound" initialization path (used when structs have pointer
fields):

1. fill_struct_array_sequential() treated each Expr item in a flat initializer
   as a complete struct element, writing it only to field 0. This meant for a
   struct with 2 fields, items after the first were treated as separate struct
   elements rather than sequential fields of the same struct.

2. write_expr_to_bytes_or_ptrs() had no handling for Expr::StringLiteral when
   the target type is a char array. String literals for char array fields in
   structs with pointer members were silently dropped (producing zero bytes).

The byte-level path (used when structs have no pointer fields) handled both
cases correctly via fill_struct_global_bytes() and write_string_to_bytes().

Fix:
1. Rewrote the flat-init Expr branch in fill_struct_array_sequential() to
   consume items field-by-field using an index-based loop, matching the
   behavior of fill_struct_global_bytes() in the byte-level path.

2. Added Expr::StringLiteral handling in write_expr_to_bytes_or_ptrs() to
   copy string bytes into char array fields via write_string_to_bytes().

Tests fixed:
- compiler_suite_0011_0176 (all 28 checks now pass on x86, ARM, RISC-V)
- compiler_suite_0005_0061 (x86)

Files changed:
- src/ir/lowering/global_init_compound.rs: Both fixes applied
