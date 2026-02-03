# Common Module -- Design Document

The `common` module is the shared foundation of the compiler. It contains
types, data structures, and algorithms that are used across multiple compiler
phases -- from the preprocessor and parser through semantic analysis, IR
lowering, optimization passes, and code generation. Nothing in `common`
depends on any specific phase; all dependencies flow inward.

This document describes the 12 sub-modules, explains the key design decisions,
and provides enough context to understand the system without reading code.

---

## Table of Contents

1. [Module Overview](#module-overview)
2. [Dependency Diagram](#dependency-diagram)
3. [The Dual Type System: CType vs IrType](#the-dual-type-system-ctype-vs-irtype)
4. [Module Reference](#module-reference)
   - [types.rs -- Type Representations](#typesrs----type-representations)
   - [error.rs -- Diagnostic Infrastructure](#errorrs----diagnostic-infrastructure)
   - [source.rs -- Source Location Tracking](#sourcers----source-location-tracking)
   - [symbol_table.rs -- Scoped Symbol Table](#symbol_tablers----scoped-symbol-table)
   - [type_builder.rs -- Shared Type Resolution](#type_builderrs----shared-type-resolution)
   - [const_arith.rs -- Constant Arithmetic Primitives](#const_arithrs----constant-arithmetic-primitives)
   - [const_eval.rs -- Shared Constant Expression Evaluation](#const_evalrs----shared-constant-expression-evaluation)
   - [long_double.rs -- Long Double Precision Support](#long_doublers----long-double-precision-support)
   - [encoding.rs -- Non-UTF-8 Source File Handling](#encodingrs----non-utf-8-source-file-handling)
   - [asm_constraints.rs -- Inline Assembly Constraint Classification](#asm_constraintsrs----inline-assembly-constraint-classification)
   - [fx_hash.rs -- Fast Non-Cryptographic Hash](#fx_hashrs----fast-non-cryptographic-hash)
   - [temp_files.rs -- RAII Temporary File Handling](#temp_filesrs----raii-temporary-file-handling)
5. [Eliminating Duplication: The const_eval / const_arith Pattern](#eliminating-duplication-the-const_eval--const_arith-pattern)

---

## Module Overview

| Module              | Purpose                                              | Primary Consumers                              |
|---------------------|------------------------------------------------------|------------------------------------------------|
| `types.rs`          | CType, IrType, StructLayout, ABI classification      | Every phase                                    |
| `error.rs`          | Diagnostic engine with GCC-compatible output          | Parser, sema, driver                           |
| `source.rs`         | Span, SourceLocation, SourceManager                  | Lexer, parser, sema, backend                   |
| `symbol_table.rs`   | Scoped symbol table for name resolution              | Sema, sema const_eval                          |
| `type_builder.rs`   | Shared AST-to-CType conversion trait                 | Sema, lowering                                 |
| `const_arith.rs`    | Low-level integer/float constant arithmetic          | Sema const_eval, lowering const_eval           |
| `const_eval.rs`     | Shared constant expression evaluation logic          | Sema const_eval, lowering const_eval           |
| `long_double.rs`    | x87 80-bit and IEEE binary128 arithmetic/conversion  | const_arith, constant_fold pass, IR constants  |
| `encoding.rs`       | PUA encoding for non-UTF-8 source bytes              | Lexer                                          |
| `asm_constraints.rs`| Inline asm constraint classification                 | Inline pass, cfg_simplify, backend             |
| `fx_hash.rs`        | FxHashMap / FxHashSet type aliases                   | Every phase                                    |
| `temp_files.rs`     | RAII temp file management                            | Driver, backend (assembler invocation)          |

---

## Dependency Diagram

The following diagram shows which compiler phases use which common modules.
Arrows point from consumer to provider.

```
                          +--------------------------------------------------+
                          |                  common/                          |
                          |                                                  |
  Compiler Phases         |  Modules                                         |
  ===============         |  =======                                         |
                          |                                                  |
  Preprocessor  --------->|  fx_hash, encoding                               |
                          |                                                  |
  Lexer  ---------------->|  source (Span), encoding (decode_pua_byte)       |
                          |                                                  |
  Parser  --------------->|  source (Span), error (DiagnosticEngine),        |
                          |  fx_hash, types (AddressSpace)                   |
                          |                                                  |
  Sema  ----------------->|  types (CType, StructLayout), symbol_table,      |
                          |  type_builder, const_arith, const_eval,          |
                          |  error, fx_hash, long_double                     |
                          |                                                  |
  IR Lowering  ---------->|  types (CType, IrType, StructLayout),            |
                          |  type_builder, const_arith, const_eval,          |
                          |  source (Span), fx_hash, long_double             |
                          |                                                  |
  Optimization Passes --->|  types (IrType), fx_hash, long_double,           |
                          |  asm_constraints                                 |
                          |                                                  |
  Backend  -------------->|  types (IrType, EightbyteClass, RiscvFloatClass, |
                          |        AddressSpace), source, fx_hash,           |
                          |  asm_constraints, temp_files                     |
                          |                                                  |
  Driver  --------------->|  error (DiagnosticEngine, WarningConfig,         |
                          |        ColorMode), source (SourceManager),       |
                          |  temp_files                                      |
                          +--------------------------------------------------+
```

Key observations:

- `types.rs` and `fx_hash.rs` are truly universal -- imported by every phase.
- `const_arith.rs` and `const_eval.rs` are consumed by exactly two callers
  (sema and lowering), which is their entire reason for existing.
- `encoding.rs` is used only by the lexer.
- `temp_files.rs` is used only by the driver and backend for assembler/linker
  invocation.

---

## The Dual Type System: CType vs IrType

One of the most important design decisions in the compiler is maintaining two
separate type representations. Understanding why they exist and how they relate
is essential for working on any phase.

### CType -- The C-Level Type

`CType` represents types as the C programmer sees them. It preserves all
semantic distinctions that matter for type checking, sizeof, and ABI:

```
CType::Void, Bool, Char, UChar, Short, UShort, Int, UInt,
       Long, ULong, LongLong, ULongLong, Int128, UInt128,
       Float, Double, LongDouble,
       ComplexFloat, ComplexDouble, ComplexLongDouble,
       Pointer(Box<CType>, AddressSpace),
       Array(Box<CType>, Option<usize>),
       Function(Box<FunctionType>),
       Struct(RcStr), Union(RcStr), Enum(EnumType),
       Vector(Box<CType>, usize)
```

CType distinguishes `int` from `long` even when the underlying sizes match
(e.g., both are 32 bits on ILP32), because C requires them to be distinct
types for type compatibility checks and correct format specifier warnings.
CType also carries struct/union identity
through `RcStr` keys (e.g., `"struct.Foo"`) that index into a layout table.

CType is the primary type during parsing, semantic analysis, and the early
stages of IR lowering. It is used for:

- Type checking and implicit conversion rules
- sizeof / alignof evaluation
- Struct layout computation (field offsets, padding, bitfields)
- ABI classification (determining register vs. stack passing)

### IrType -- The IR-Level Type

`IrType` is a flat enumeration of machine-level types with no nesting:

```
IrType::I8, I16, I32, I64, I128,
        U8, U16, U32, U64, U128,
        F32, F64, F128,
        Ptr, Void
```

IrType is what the IR instructions, optimization passes, and code generator
work with. It collapses C-level distinctions that are irrelevant at the
machine level:

- `int` and `long` on LP64 both become `I64`
- All pointer types become `Ptr`
- Struct and array types are decomposed into sequences of scalar loads/stores

IrType knows its own size and alignment, which vary by target. On i686, for
example, `I64` is 4-byte aligned (not 8), and `F128` (long double) is 12
bytes with 4-byte alignment. These platform differences are captured by
thread-local target configuration (`set_target_ptr_size`,
`set_target_long_double_is_f128`) that the driver sets at startup.

### Why Two Systems?

The lowering phase is the bridge. It reads CType information from the AST and
sema, computes struct layouts, performs ABI classification, and produces IR
instructions annotated with IrType. Once lowering is complete, CType is no
longer needed -- the optimization passes and backend work exclusively with
IrType.

If there were only one type system, it would either be too detailed for the
backend (carrying struct names and C-level distinctions through every
optimization pass) or too coarse for semantic analysis (losing the ability to
distinguish `int*` from `long*`).

### Target Configuration

Both type systems depend on target parameters stored in thread-locals:

- `TARGET_PTR_SIZE` (4 for i686/ILP32, 8 for x86-64/AArch64/RISC-V LP64)
- `TARGET_LONG_DOUBLE_IS_F128` (true for AArch64/RISC-V, false for x86)

The driver calls `set_target_ptr_size()` and `set_target_long_double_is_f128()`
before compilation begins. Helper functions like `target_int_ir_type()` and
`widened_op_type()` use these to return target-appropriate types, preventing
hardcoded assumptions from leaking into phase-specific code.

---

## Module Reference

### types.rs -- Type Representations

The largest module in `common/`. Contains:

**CType enum** -- All C-level types, from `Void` through `Vector`. Struct and
union variants carry an `RcStr` (reference-counted string) key that indexes
into a `StructLayout` table. This makes cloning a struct type a cheap
refcount bump instead of a heap allocation.

**IrType enum** -- Flat machine-level types. Methods include `size()`,
`align()`, `is_signed()`, `is_unsigned()`, `is_float()`, `is_long_double()`,
and `is_128bit()`.

**StructLayout** -- Computed layout of a struct or union: field offsets,
total size, alignment, and `is_union` / `is_transparent_union` flags.
Fields are represented as `StructFieldLayout` entries, each carrying a name,
byte offset, CType, and optional bitfield offset/width. Layout computation
follows the System V ABI rules, including:
- Natural alignment with padding
- Bitfield packing across storage units (both standard and packed modes)
- `__attribute__((packed))` and `#pragma pack(N)` support
- `_Alignas` / `__attribute__((aligned(N)))` per-field overrides
- Zero-width bitfield alignment forcing

A `StructLayoutBuilder` handles the stateful layout algorithm, tracking the
current byte offset, maximum alignment, and bitfield accumulation state.

**EnumType** -- Stores variant name/value pairs and a `is_packed` flag. The
`packed_size()` method computes the minimum integer size needed to hold all
variant values (1, 2, 4, or 8 bytes), or returns 4 for non-packed enums
(with a GCC extension to use 8 bytes for values exceeding 32-bit range).

**FunctionType** -- Return type, parameter list with optional names, and
variadic flag.

**ABI classification types**:
- `EightbyteClass` -- System V AMD64 ABI classification (NoClass, Sse, Integer)
  with a `merge()` method implementing the standard merging rules: NoClass
  yields to anything, Integer dominates, and Sse merges with itself.
- `RiscvFloatClass` -- RISC-V LP64D hardware floating-point calling convention
  classification. Variants: `OneFloat`, `TwoFloats`, `FloatAndInt`,
  `IntAndFloat` -- each carrying field-level details like whether a component
  is `double` vs `float`, and the byte offsets and sizes of components.
- `AddressSpace` -- GCC named address space extension for x86 segment-relative
  addressing: `Default`, `SegGs` (`__seg_gs`), `SegFs` (`__seg_fs`).

**StructLayoutProvider trait** -- Abstraction that lets CType methods compute
sizes and alignments for struct types without depending on the lowering module.
Both `TypeContext` (in sema) and `FxHashMap<String, RcLayout>` implement this
trait.

**Target helpers** -- `set_target_ptr_size()`, `target_ptr_size()`,
`target_is_32bit()`, `set_target_long_double_is_f128()`,
`target_long_double_is_f128()`, `target_int_ir_type()`, `widened_op_type()`.
The `widened_op_type()` function returns the machine-word type for arithmetic
widening: on LP64 everything widens to I64, while on i686 most types widen to
I32 but I64/U64 stay at I64 (requiring register pairs).

### error.rs -- Diagnostic Infrastructure

A complete diagnostic system that renders GCC-compatible error messages with
source snippets, caret highlighting, and ANSI color output.

**Severity** -- Three levels: `Error`, `Warning`, `Note`.

**Diagnostic** -- A single diagnostic message with:
- Severity and message text
- Optional `Span` for source location
- Optional `WarningKind` for filtering and `-Werror` promotion
- Attached `notes` vector (sub-diagnostics providing additional context)
- Optional fix-it hint text (rendered below the snippet)
- Optional explicit location string (for preprocessor-phase diagnostics
  where the SourceManager is not yet available)

**WarningKind** -- Enumeration of warning categories matching GCC flag names:
`Undeclared`, `ImplicitFunctionDeclaration`, `Cpp`, `ReturnType`. Each
variant maps to a `-W<name>` flag (e.g., `-Wimplicit-function-declaration`).
Supports `-Wall` and `-Wextra` groupings. The `Undeclared` variant is
retained for CLI flag parsing compatibility but is no longer emitted as a
warning (undeclared variables are now hard errors).

**WarningConfig** -- Per-warning enabled/disabled and error-promotion state.
Processes CLI flags left-to-right so that later flags override earlier ones:
`-Wall -Wno-return-type` enables all warnings except `return-type`. Supports:
- `-Werror` (global promotion of all warnings to errors)
- `-Werror=<name>` (per-warning promotion, which also implicitly enables it)
- `-Wno-error=<name>` (demotion of a specific warning back from error)
- `-Wno-<name>` (disable a specific warning entirely)

**ColorMode** -- Three modes matching `-fdiagnostics-color={auto,always,never}`.
The `auto` mode detects whether stderr is a terminal. Color scheme matches GCC:
bold red for errors, bold magenta for warnings, bold cyan for notes, bold green
for carets and fix-it hints, bold white for file:line:col locations.

**DiagnosticEngine** -- Central diagnostic collector. Tracks error and warning
counts, holds a `WarningConfig` and optional `SourceManager` reference. Emits
diagnostics to stderr with source snippets and include-chain traces (GCC-style
"In file included from X:Y:" headers, with deduplication to avoid repeating the
same chain for consecutive errors in the same included file). The source manager
is attached after preprocessing creates it, so early-phase diagnostics use
explicit location strings instead.

### source.rs -- Source Location Tracking

Maps byte offsets in compiler output back to original source file locations.

**Span** -- A byte-offset range (`start`, `end`, `file_id`) in source code.
Compact (12 bytes) and cheap to copy. A `Span::dummy()` constructor provides
a zero-valued placeholder for generated code.

**SourceLocation** -- Human-readable location: filename, line number, column.
Implements `Display` as `file:line:col`.

**SourceManager** -- Resolves spans to locations. Operates in two modes:

1. *Simple mode*: A single file registered via `add_file()`. Spans are resolved
   by binary-searching a precomputed line-offset table.
2. *Line-map mode*: Preprocessed output with embedded `# linenum "filename"`
   markers. The line map is built by `build_line_map()` and maps byte offsets
   in the preprocessed stream back to original source files and line numbers.

The source manager also tracks:
- **Include chains** (`IncludeOrigin`): When a `# 1 "file.h" 1` marker
  appears (flag 1 = enter-include), it records that `file.h` was included
  from the previously active file at a specific line. This enables "In file
  included from X:Y:" traces in error diagnostics.
- **Macro expansion info** (`MacroExpansionInfo`): Records which
  preprocessed-output lines involved macro expansion, enabling "in expansion
  of macro 'X'" diagnostic notes. Stored sorted by preprocessed line number
  for binary search lookup.

Internally, filenames are deduplicated into a table indexed by `u16`, so
`LineMapEntry` structs are compact (10 bytes: `pp_offset` as u32 +
`filename_idx` as u16 + `orig_line` as u32) even for large translation units.

### symbol_table.rs -- Scoped Symbol Table

A simple, stack-based symbol table for lexical scoping during semantic analysis.

**Symbol** -- A declared symbol with a name, CType, and optional explicit
alignment (from `_Alignas` or `__attribute__((aligned(N)))`). The explicit
alignment is needed because `_Alignof(var)` must return the declared
alignment per C11 6.2.8p3, not the natural type alignment.

**SymbolTable** -- A stack of `Scope` objects, each containing an
`FxHashMap<String, Symbol>`. Operations:
- `push_scope()` / `pop_scope()` -- Enter/leave a lexical scope.
- `declare(symbol)` -- Insert a symbol into the current (innermost) scope.
- `lookup(name)` -- Search scopes from innermost to outermost, returning the
  first match. This implements C's name shadowing rules.

The table is initialized with a single scope (file scope) and is used by the
semantic analysis phase and its constant expression evaluator.

### type_builder.rs -- Shared Type Resolution

Prevents divergence in how AST type syntax is converted to CType between sema
and lowering.

**TypeConvertContext trait** -- A trait with one large default method
(`resolve_type_spec_to_ctype`) and five required methods. The default method
handles all shared cases:

- 22 primitive C types (Void through ComplexLongDouble)
- Pointer, Array, and FunctionPointer construction
- TypeofType and AutoType

Only four cases differ between sema and lowering and must be implemented by
each:
1. `resolve_typedef(name)` -- Typedef name resolution (lowering also checks
   function pointer typedefs for richer type info).
2. `resolve_struct_or_union(...)` -- Struct/union layout computation (lowering
   has caching and forward-declaration logic).
3. `resolve_enum(...)` -- Enum type resolution (sema preserves `CType::Enum`,
   lowering collapses to `CType::Int`).
4. `resolve_typeof_expr(expr)` -- typeof(expr) evaluation (sema returns
   `CType::Int` as a placeholder; lowering evaluates the full expression type).

A fifth required method, `eval_const_expr_as_usize(expr)`, handles
compile-time evaluation of array dimension expressions.

This design guarantees that the mapping from `TypeSpecifier::Int` to
`CType::Int` (and all 21 other primitives) is defined in exactly one place.

### const_arith.rs -- Constant Arithmetic Primitives

Low-level arithmetic functions for compile-time constant evaluation with
proper C semantics.

The functions here handle pure arithmetic: given `IrConst` operands and
width/signedness parameters, they compute the result. Callers (sema and
lowering) determine width and signedness from their own type systems before
calling these shared functions.

Key internal helpers:
- `wrap_result(v, is_32bit)` -- Truncate an i64 to 32-bit width when needed,
  preserving C truncation semantics (cast to i32, then sign-extend back).
- `unsigned_op(l, r, is_32bit, op)` -- Apply an operation in the unsigned
  domain with correct width handling.
- `bool_to_i64(b)` -- Convert boolean to 0/1.

Key evaluators:
- `eval_const_binop_int(op, l, r, is_32bit, is_unsigned)` -- (module-private)
  Integer binary operations (add, sub, mul, div, mod, shifts, bitwise,
  comparisons) with wrapping, division-by-zero checking, and proper width
  truncation.
- `eval_const_binop_float(op, l, r)` -- (public) Floating-point binary
  operations on f64 values.
- Additional helpers for i128 operations and long double constant arithmetic,
  delegating to the `long_double` module for full-precision results.

### const_eval.rs -- Shared Constant Expression Evaluation

Higher-level constant evaluation logic extracted from the near-identical
implementations that previously existed in both `sema::const_eval` and
`ir::lowering::const_eval`.

The functions are parameterized by closures that abstract over the
sema-vs-lowering differences, allowing both callers to share the same
evaluation logic for:

- **Literal evaluation** (`eval_literal`) -- Converts `Expr::IntLiteral`,
  `Expr::FloatLiteral`, `Expr::CharLiteral`, and all other literal kinds to
  `IrConst` values. Integer literals produce `IrConst::I32` when the value
  fits in 32 bits, otherwise `IrConst::I64`. Char literals are sign-extended
  from `signed char` to `int`, matching GCC behavior. Long double literals
  produce `IrConst::long_double_with_bytes` with full-precision byte storage.

- **Builtin constant folding** -- Evaluates `__builtin_bswap`,
  `__builtin_clz`, and similar builtins at compile time when their arguments
  are constants.

- **Sub-int promotion** -- Handles unary operations on types narrower than
  `int`, applying C integer promotion rules.

- **Bit-width evaluation through cast chains** -- Tracks type width through
  sequences of casts.

Functions that require caller-specific state (global address resolution,
sizeof/alignof, binary operations with type inference) remain in the
respective callers. See the final section for the full explanation of how this
eliminates duplication.

### long_double.rs -- Long Double Precision Support

Full-precision support for the two `long double` formats used across
target architectures:

| Architecture     | Format              | Storage                | Exponent Bias |
|------------------|---------------------|------------------------|---------------|
| x86-64, i686     | x87 80-bit extended | 16 bytes (6 padding)   | 16383         |
| AArch64, RISC-V  | IEEE 754 binary128  | 16 bytes               | 16383         |

Both formats use 15-bit exponent fields with bias 16383. The key structural
difference is that x87 has an explicit integer bit in the mantissa (64 bits
total significand), while binary128 uses an implicit leading 1 with 112
stored mantissa bits.

**Parsing:**
- `parse_long_double_to_f128_bytes(text)` -- Parses a decimal or hex float
  string to IEEE binary128 bytes. Handles infinity, NaN, hex floats with
  binary exponents, and decimal floats via a big-integer algorithm that
  preserves all 112 bits of mantissa precision. Uses a pure-Rust `BigUint`
  implementation with no external dependencies.

Internal preprocessing (`preparse_long_double`) strips suffixes, detects
signs, and classifies the input as hex, infinity, NaN, or decimal, shared
by both x87 and f128 parsing paths.

**Format conversion:**
- `x87_bytes_to_f128_bytes(x87)` -- Convert x87 80-bit to IEEE binary128.
- `x87_bytes_to_f64(bytes)` -- Convert x87 80-bit to f64.
- `f128_bytes_to_x87_bytes(f128_bytes)` -- Convert IEEE binary128 to x87.
- `f128_bytes_to_f64(f128_bytes)` -- Convert IEEE binary128 to f64.
- `f64_to_x87_bytes_simple(val)` -- Convert f64 to x87 bytes.
- `f64_to_f128_bytes_lossless(val)` -- Convert f64 to IEEE binary128 losslessly.

**Integer-to-float conversion:**
- `i64_to_f128_bytes(val)`, `u64_to_f128_bytes(val)` -- Signed/unsigned 64-bit
  integer to binary128.
- `i128_to_f128_bytes(val)`, `u128_to_f128_bytes(val)` -- Signed/unsigned
  128-bit integer to binary128.

**Float-to-integer conversion** (all return `Option`, `None` on overflow):
- `f128_bytes_to_i64(bytes)`, `f128_bytes_to_u64(bytes)` -- Binary128 to
  64-bit integer.
- `f128_bytes_to_i128(bytes)`, `f128_bytes_to_u128(bytes)` -- Binary128 to
  128-bit integer.

Note that all float-to-integer conversions operate on f128 bytes. Code working
with x87 values should first convert to f128 via `x87_bytes_to_f128_bytes`,
then call the appropriate `f128_bytes_to_*` function.

**x87 arithmetic (x86-64 inline assembly):**
These functions use `fld` / `fstp` x87 FPU instructions via Rust inline
assembly to perform operations at full 80-bit precision on x86-64 hosts:
- `x87_add(a, b)`, `x87_sub(a, b)`, `x87_mul(a, b)`, `x87_div(a, b)`
- `x87_rem(a, b)` -- Uses `fprem1` for IEEE remainder.
- `x87_neg(a)` -- Uses `fchs`.
- `x87_cmp(a, b)` -- Uses `fucomip` for unordered comparison. Returns
  negative, zero, or positive (following the C `strcmp` convention).

On non-x86 hosts cross-compiling for x86, fallback software implementations
are provided.

**f128 arithmetic (pure Rust software implementation):**
For AArch64/RISC-V targets (or cross-compilation on non-x86 hosts), all
binary128 operations are implemented in pure Rust without inline assembly:
- `f128_add(a, b)`, `f128_sub(a, b)`, `f128_mul(a, b)`, `f128_div(a, b)`
- `f128_rem(a, b)`, `f128_cmp(a, b)`

These decompose the 128-bit representation into sign, exponent, and mantissa
components using shared `f128_decompose` helpers, then perform the operation
with correct rounding.

The constant folding optimization pass uses these arithmetic functions to
evaluate long double expressions at compile time without precision loss. The
choice between x87 and f128 functions is determined by the
`target_long_double_is_f128()` flag.

### encoding.rs -- Non-UTF-8 Source File Handling

C source files may contain non-UTF-8 bytes in string and character literals
(e.g., EUC-JP, Latin-1 encoded files). Since Rust strings must be valid
UTF-8, this module provides a round-trip encoding scheme using Unicode
Private Use Area (PUA) code points.

**Encoding scheme:** Byte `0x80+n` maps to code point `U+E080+n` (PUA range
`U+E080..U+E0FF`).

**Public API:**
- `bytes_to_string(bytes)` -- Converts raw file bytes to a valid UTF-8 String.
  If the input is already valid UTF-8, it is returned as-is (zero-copy fast
  path). Otherwise, bytes are processed one at a time: valid UTF-8 multi-byte
  sequences are preserved, and invalid bytes `0x80-0xFF` are encoded as PUA
  code points. A UTF-8 BOM at the start of the file is stripped, matching
  GCC/Clang behavior.
- `decode_pua_byte(input: &[u8], pos: usize) -> (u8, usize)` -- Recovers the
  original byte from a PUA-encoded UTF-8 sequence in a byte slice. Returns
  the decoded byte and the number of input bytes consumed. Used by the lexer
  when processing string/character literals to emit the correct raw bytes
  into the compiled output.

### asm_constraints.rs -- Inline Assembly Constraint Classification

A single function shared by three consumers: the inline pass (symbol
resolution after inlining), cfg_simplify (dead block detection), and the
backend (operand emission).

**`constraint_is_immediate_only(constraint)`** -- Returns `true` for
constraints that accept only compile-time constants (`"i"`, `"n"`, `"e"`,
and x86-specific letters like `"K"`, `"M"`, `"G"`, `"H"`, `"J"`, `"L"`,
`"O"`). Returns `false` for multi-alternative constraints that also accept
registers or memory (`"ri"`, `"g"`, `"Ir"`, etc.).

The function strips output/early-clobber modifiers (`=`, `+`, `&`, `%`)
before classification, and rejects named operand references (`[name]`).
It checks that at least one immediate-class letter is present, and that no
register-class letters (`r`, `q`, `a`, `b`, `c`, `d`, etc.), memory-class
letters (`m`, `o`, `V`, `p`, `Q`), or general-class letters (`g`) appear.
Numeric digit references (operand reuse constraints) also cause a `false`
return.

### fx_hash.rs -- Fast Non-Cryptographic Hash

A copy of the FxHash algorithm used by the Rust compiler (rustc). Replaces
the default SipHash in `HashMap`/`HashSet` with a much faster hash for
compiler workloads where DoS resistance is unnecessary.

**Type aliases:**
- `FxHashMap<K, V>` -- `HashMap` with `FxHasher`.
- `FxHashSet<V>` -- `HashSet` with `FxHasher`.

The `FxHasher` struct implements `std::hash::Hasher`. The hash function uses
rotate-left-5-XOR followed by multiply-by-`0x517cc1b727220a95`. It is used
throughout the entire compiler for all hash maps and hash sets.

### temp_files.rs -- RAII Temporary File Handling

Provides safe temporary file management with automatic cleanup.

**`temp_dir()`** -- Returns the platform temp directory, respecting `$TMPDIR`
on Unix (falling back to `/tmp`).

**`make_temp_path(prefix, stem, extension)`** -- Generates a unique temp file
path. The filename includes the process ID and an atomic counter to avoid
collisions in parallel builds and multi-file compilations. Format:
`{prefix}_{pid}_{stem}.{counter}.{extension}`.

**`TempFile`** -- An RAII guard that deletes the file when dropped. Ensures
cleanup happens even on early returns, panics, or errors. Has a `keep` flag
for debugging that prevents deletion on drop. Used by the driver when invoking
external assemblers and by the backend for intermediate object files.

---

## Eliminating Duplication: The const_eval / const_arith Pattern

Compile-time constant expression evaluation is needed in two distinct phases:

1. **Semantic analysis (sema)** -- Evaluates constant expressions for
   `_Static_assert`, array dimensions, enum values, and `case` labels. Works
   with `CType` to determine operand width and signedness.

2. **IR lowering** -- Evaluates constant expressions for global variable
   initializers, constant folding during lowering, and static address
   computation. Works with `IrType`.

Before extraction, both phases contained near-identical implementations of
constant binary operations, literal evaluation, and builtin folding. The
`common` module eliminates this duplication with a two-layer design:

```
  sema::const_eval                  ir::lowering::const_eval
       |                                   |
       |  (determines width/signedness     |  (determines width/signedness
       |   from CType)                     |   from IrType)
       |                                   |
       +--------->  common::const_eval  <--+
                         |
                         | (pure evaluation logic,
                         |  parameterized by closures)
                         |
                    common::const_arith
                         |
                         | (pure arithmetic: wrapping,
                         |  unsigned ops, width truncation)
                         |
                    common::long_double
                         |
                         | (f128/x87 arithmetic for
                         |  long double constants)
```

**const_arith** is the bottom layer. Its functions like
`eval_const_binop_int(op, l, r, is_32bit, is_unsigned)` take explicit
width/signedness flags and return `IrConst` results. The callers compute
`is_32bit` and `is_unsigned` from their own type systems:
- Sema: `(ctype_size <= 4, ctype.is_unsigned())`
- Lowering: `(ir_type.size() <= 4, ir_type.is_unsigned())`

**const_eval** is the upper layer. Its functions like `eval_literal(expr)` and
builtin evaluation take AST expressions and return `IrConst` values directly.
Functions that need type information are parameterized by closures, so sema
passes CType-based logic and lowering passes IrType-based logic.

This structure means that:
- Adding a new integer literal kind requires changing only `eval_literal`.
- Adding a new binary operator requires changing only `eval_const_binop_int`.
- Adding a new builtin requires changing only the shared builtin evaluator.
- The two callers remain thin wrappers that supply type resolution and delegate
  to the shared layer.

Functions that inherently require phase-specific context -- global address
resolution (lowering only), sizeof/alignof evaluation (different symbol tables),
and type inference for binary operations -- remain in their respective callers.
The boundary is drawn precisely at the point where the logic diverges.
