# i686 Backend -- 32-bit x86 Code Generator

## Overview

The i686 backend targets 32-bit x86 (IA-32) processors, emitting AT&T-syntax
GNU assembly.  It implements the `ArchCodegen` trait that the shared code
generation framework dispatches to, producing one `.s` file per translation
unit.

The default calling convention is **cdecl** (System V i386 ABI): all arguments
are passed on the stack, pushed right-to-left, and the caller cleans up.
Return values are placed in `%eax` (32-bit scalars), `%eax:%edx` (64-bit
integers), or `st(0)` (float, double, long double).  Two alternative calling
conventions are also supported: **`-mregparm=N`** (first 1--3 integer
arguments in `%eax`, `%edx`, `%ecx`) and **`__attribute__((fastcall))`**
(first two DWORD-or-smaller arguments in `%ecx`, `%edx`, callee cleans
the stack).

The backend operates as an *accumulator machine*: intermediate results flow
through `%eax` (and `%edx` for the upper half of 64-bit values), with a
lightweight register allocator that promotes hot IR values into callee-saved
registers.  A post-emission peephole optimizer cleans up the redundant
store/load traffic this style produces.

---

## ILP32 Type Model

The i686 target uses the ILP32 data model, which differs from LP64 (x86-64)
in several important ways:

| Type | i686 (ILP32) | x86-64 (LP64) |
|------|:------------:|:--------------:|
| `char` | 1 | 1 |
| `short` | 2 | 2 |
| `int` | 4 | 4 |
| `long` | **4** | 8 |
| `long long` | 8 | 8 |
| pointer | **4** | 8 |
| `size_t` | **4** | 8 |
| `float` | 4 | 4 |
| `double` | 8 | 8 |
| `long double` | **12** (80-bit x87) | 16 (80-bit x87, padded) |

The key consequences for code generation:

- **Pointers are 4 bytes.**  Address arithmetic, GEP offsets, and pointer
  loads/stores all use `movl` and 32-bit registers.  The assembler pointer
  directive is `.long` (not `.quad`).
- **`long` is 4 bytes**, so `long` and `int` are identical in size.
  This means `long` function parameters need no special treatment relative
  to `int`.
- **`long long` (64-bit) does not fit in a single register** and must be
  split across `%eax:%edx` pairs, creating the register-pair splitting
  described below.
- **`long double` is native 80-bit x87 extended precision**, stored in
  12-byte stack slots (10 bytes of data, 2 bytes padding).  It is loaded
  and stored with `fldt`/`fstpt` directly, not via software emulation.

---

## File Inventory

All code generation logic lives under `src/backend/i686/codegen/`:

| File | Responsibility |
|------|---------------|
| `codegen.rs` | `I686Codegen` struct, `ArchCodegen` trait impl, accumulator load/store helpers (`operand_to_eax`, `operand_to_ecx`, `store_eax_to`), x87 FPU load helpers, 64-bit atomic helpers (`cmpxchg8b`), runtime stubs (`__x86.get_pc_thunk.bx`, `__divdi3`/`__udivdi3`/`__moddi3`/`__umoddi3`) |
| `prologue.rs` | Stack frame setup: `calculate_stack_space`, `emit_prologue`/`emit_epilogue`, parameter storage, register allocator integration, frame pointer omission logic |
| `calls.rs` | Call ABI: stack argument layout, `regparm` register argument emission, call instruction emission (direct/indirect/PLT), result retrieval (`%eax`, `%eax:%edx`, `st(0)`) |
| `memory.rs` | Load/store overrides for 64-bit and F128 types, constant-offset load/store, GEP address computation, memcpy emission, alloca alignment |
| `alu.rs` | Integer ALU: `add`/`sub`/`mul`/`and`/`or`/`xor`/`shl`/`shr`/`sar`, signed and unsigned division (`idivl`/`divl`), LEA strength reduction for multiply by 3/5/9, immediate-operand fast paths |
| `i128_ops.rs` | 64-bit register-pair operations (called "i128" in the shared trait): `add`/`adc`, `sub`/`sbb`, `mul`, `shld`/`shrd` shifts, comparisons, `__divdi3`/`__udivdi3` calls, float conversions via x87 `fildq`/`fisttpq` |
| `comparison.rs` | Float comparisons (SSE `ucomiss` for F32, x87 `fucomip` for F64/F128), integer comparisons (`cmpl` + `setCC`), fused compare-and-branch, `select` (CMOVcc) |
| `casts.rs` | Type conversion: integer widening/narrowing, float-to-int and int-to-float via x87 (including F64 source/dest requiring 8-byte stack temporaries), F128 conversions via `fldt`/`fstpt`, unsigned-to-float fixup for values with the sign bit set |
| `returns.rs` | Return value placement: 64-bit in `%eax:%edx`, F32/F64/F128 in `st(0)`, 32-bit scalars in `%eax` |
| `float_ops.rs` | F128 negation (`fchs` on x87) |
| `globals.rs` | Global/label address loading: absolute mode (`movl $name`), PIC mode (`@GOT`/`@GOTOFF` relative to `%ebx`), TLS (`@NTPOFF`/`@GOTNTPOFF`) |
| `variadic.rs` | `va_start`, `va_arg`, `va_copy` -- on i686 `va_list` is a simple pointer into the stack frame, advanced by the argument size |
| `atomics.rs` | Atomic RMW (`lock xadd`, `lock cmpxchg` loop), cmpxchg, atomic load/store, fence; 64-bit atomics via `lock cmpxchg8b` loops |
| `intrinsics.rs` | SSE/AES-NI/CRC32 intrinsics, memory fences, non-temporal stores, x87 FPU math (`fsqrt`, `fabs`), frame/return address intrinsics |
| `inline_asm.rs` | Inline assembly template substitution (delegates to shared x86 parser), GCC constraint classification (`r`, `q`, `a`, `b`, `S`, `D`, `m`, `i`, `t`, `u`, etc.), operand formatting with size modifiers (`%b`, `%w`, `%k`) |
| `asm_emitter.rs` | `InlineAsmEmitter` trait impl: scratch register allocation, operand loading/storing, constraint-to-register mapping for 32-bit GP and XMM registers |
| `peephole.rs` | Post-emission assembly optimizer (see dedicated section below) |
| `mod.rs` | Module re-exports |

---

## Calling Convention

### cdecl (Default)

The standard System V i386 ABI:

```
   Caller's frame
   ┌──────────────────────┐  higher addresses
   │  arg N               │  ← pushed first (right-to-left)
   │  ...                 │
   │  arg 1               │
   │  arg 0               │
   │  return address       │  ← pushed by CALL
   ├──────────────────────┤
   │  saved %ebp          │  ← pushed by prologue (unless -fomit-frame-pointer)
   │  saved callee-saved   │  ← %ebx, %esi, %edi (as needed)
   │  local variables      │
   │  spill slots          │
   └──────────────────────┘  ← %esp (16-byte aligned at call sites)
```

- All arguments are on the stack.  The caller adjusts `%esp` after the call
  to remove them.
- The stack is aligned to 16 bytes at the `call` instruction (modern i386 ABI
  requirement).
- `%eax`, `%ecx`, `%edx` are caller-saved (scratch).
- `%ebx`, `%esi`, `%edi`, `%ebp` are callee-saved.

### `-mregparm=N` (N = 1, 2, or 3)

Passes the first N integer/pointer arguments in registers instead of on the
stack.  The register order is `%eax`, `%edx`, `%ecx`.  This is used
extensively by the Linux kernel.  The ABI configuration simply sets
`max_int_regs` to N in the shared call classifier, and `emit_call_reg_args`
loads the arguments into the appropriate registers in reverse order to avoid
clobbering `%eax` (the accumulator) prematurely.

### `__attribute__((fastcall))`

Passes the first two DWORD-or-smaller integer/pointer arguments in `%ecx` and
`%edx`.  The callee pops the *stack* arguments on return (callee-cleanup).
Implemented via `is_fastcall`, `fastcall_reg_param_count`, and
`fastcall_stack_cleanup` fields on the codegen struct.

### Return Values

| Type | Location |
|------|----------|
| `int`, `long`, pointer | `%eax` |
| `long long` / 64-bit | `%eax` (low), `%edx` (high) |
| `float` | `st(0)` (pushed onto x87 stack from `%eax` bit pattern) |
| `double` | `st(0)` (loaded from 8-byte `%eax:%edx` pair via `fldl`) |
| `long double` (F128) | `st(0)` (loaded via `fldt`) |
| 128-bit integer | `%eax` (low), `%edx` (next 32 bits) |

---

## 64-bit Operation Splitting

Because every general-purpose register is 32 bits wide, 64-bit values
(`long long`, `double` bit patterns, `uint64_t`) must be represented as
register pairs or 8-byte stack slots.

### Register Pair Convention

The canonical register pair is `%eax:%edx` (low:high).  For 64-bit
arithmetic:

| Operation | Instruction sequence |
|-----------|---------------------|
| Add | `addl` low, `adcl` high |
| Subtract | `subl` low, `sbbl` high |
| Multiply | Cross-multiply with `mull` + `imull`, accumulate partial products into `%edx` |
| Left shift | `shldl %cl, %eax, %edx` / `shll %cl, %eax` with branch on `%cl >= 32` |
| Logical right shift | `shrdl %cl, %edx, %eax` / `shrl %cl, %edx` with branch on `%cl >= 32` |
| Arithmetic right shift | `shrdl %cl, %edx, %eax` / `sarl %cl, %edx` with sign-extend fixup |
| Bitwise ops | Pair of `andl`/`orl`/`xorl` on both halves |
| Negate | `notl` both halves, `addl $1` low, `adcl $0` high |
| Compare (eq/ne) | `cmpl` + `sete` on each half, `andb` the results |
| Compare (ordered) | Compare high halves first; if equal, compare low halves (unsigned for low half, signed/unsigned for high depending on the comparison) |

The right-hand operand is pushed onto the stack before the operation and
popped afterward, since all scratch registers are occupied by the result pair.

### 64-bit Division and Modulo

Hardware `divl`/`idivl` only supports 32-bit divisors.  For 64-bit
division, the backend calls runtime helper functions (`__divdi3`,
`__udivdi3`, `__moddi3`, `__umoddi3`) following the cdecl convention -- both
the dividend and divisor are pushed as 8-byte pairs.  The compiler emits
`.weak` implementations of these helpers (based on compiler-rt's algorithms)
so that standalone builds without libgcc can link successfully, while builds
that do link libgcc naturally use its versions instead.

---

## Register Allocation

The i686 backend has only **6 usable general-purpose registers** in total
(excluding `%esp`), of which three are caller-saved scratch:

| Register | Role |
|----------|------|
| `%eax` | Accumulator -- all intermediate results flow through here |
| `%ecx` | Secondary operand register (shift counts, RHS of binary ops) |
| `%edx` | Upper half of 64-bit results; `idivl`/`divl` remainder |
| `%ebx` | Callee-saved, allocatable (PhysReg 0); GOT base in PIC mode |
| `%esi` | Callee-saved, allocatable (PhysReg 1) |
| `%edi` | Callee-saved, allocatable (PhysReg 2) |
| `%ebp` | Frame pointer (callee-saved; allocatable as PhysReg 3 only with `-fomit-frame-pointer`) |

The register allocator runs before stack space computation and assigns
frequently-used IR values to the callee-saved registers `%ebx`, `%esi`,
`%edi` (and `%ebp` when the frame pointer is omitted).  Values assigned to
physical registers are loaded/stored with `movl %reg, ...` instead of going
through stack slots, eliminating memory traffic for the hottest values.

In **PIC mode**, `%ebx` is reserved as the GOT base pointer (loaded via
`__x86.get_pc_thunk.bx` + `_GLOBAL_OFFSET_TABLE_`) and is excluded from
the allocatable set.  It is still saved/restored as a callee-saved register.

Inline assembly clobber lists are integrated into allocation: if an `asm`
block clobbers `%esi`, the allocator will not place values in `%esi` across
that block.  Generic constraints (`r`, `q`, `g`) conservatively mark all
callee-saved registers as clobbered, since the scratch allocator might pick
any of them.

---

## Stack Frame Layout

### With Frame Pointer (default)

```
   higher addresses
   ┌──────────────────────┐
   │  arg 1               │  12(%ebp)
   │  arg 0               │   8(%ebp)
   │  return address       │   4(%ebp)
   ├──────────────────────┤
   │  saved %ebp          │   0(%ebp)  ← %ebp points here
   │  saved %ebx          │  -4(%ebp)
   │  saved %esi          │  -8(%ebp)
   │  ...                 │
   │  local slot 0         │  -N(%ebp)
   │  local slot 1         │  -(N+4)(%ebp)
   │  ...                 │
   └──────────────────────┘  ← %esp (16-byte aligned)
```

All local slots are referenced as negative offsets from `%ebp`.  The total
frame size (the `subl $N, %esp` in the prologue) is rounded up so that
`%esp` is 16-byte aligned, accounting for the saved `%ebp`, return address,
and callee-saved register pushes.

Stack slots are 4-byte granularity by default.  64-bit values get 8-byte
slots; F128 (long double) gets 12-byte slots; 128-bit integers get 16-byte
slots.  Over-aligned allocas (e.g., `__attribute__((aligned(16)))`) get
extra space and are dynamically aligned at access time with
`leal`/`addl`/`andl` sequences.

### Without Frame Pointer (`-fomit-frame-pointer`)

When the frame pointer is omitted, `%ebp` is freed as a fourth callee-saved
register (PhysReg 3).  All stack references use `%esp`-relative addressing
instead.  The `slot_ref` helper converts the EBP-relative offsets stored in
`StackSlot` values to ESP-relative offsets by adding `frame_base_offset +
esp_adjust`:

- `frame_base_offset` = `callee_saved_bytes + frame_size` (set once in the
  prologue)
- `esp_adjust` tracks temporary ESP changes during code generation (e.g.,
  `subl $N, %esp` for call arguments, `pushl` for temporaries)

This bookkeeping is critical for correctness: every `subl`/`pushl` that
modifies `%esp` increments `esp_adjust`, and every `addl`/`popl` decrements
it, keeping slot references accurate throughout the function body.

Dynamic allocas (`alloca` / VLAs) force the frame pointer to remain enabled,
since ESP changes by runtime-computed amounts that cannot be statically
tracked.

---

## F128 / Long Double via x87 FPU

On i686, `long double` maps to the x87 80-bit extended precision format
(10 bytes of data, stored in 12-byte aligned slots).  Unlike x86-64, where
F128 is often software-emulated via `__float128` library calls, the i686
backend uses the x87 FPU natively:

- **Load:** `fldt offset(%ebp)` pushes the 80-bit value onto `st(0)`.
- **Store:** `fstpt offset(%ebp)` pops `st(0)` and writes 10 bytes.
- **Arithmetic:** `faddp`, `fsubp`, `fmulp`, `fdivp` operate on the x87
  stack.
- **Negation:** `fchs` negates `st(0)`.
- **Comparison:** Two values are loaded onto the x87 stack; `fucomip`
  compares `st(0)` with `st(1)` and sets EFLAGS directly (P6+ feature),
  followed by `fstp %st(0)` to pop the remaining operand.
- **Conversions:** Integer-to-F128 uses `fildl`/`fildq` (load integer from
  memory to x87); F128-to-integer uses `fisttpq` (truncate and store).

Constants are materialized by constructing the 80-bit x87 byte representation
on the stack with `movl`/`movw` and then loading with `fldt`.  The
`f128_bytes_to_x87_bytes` helper converts from IEEE binary128 to x87
extended format.

Tracking which values are "directly" in F128 slots (vs. loaded through a
pointer) is maintained via the `f128_direct_slots` set in `CodegenState`.

---

## 128-bit Integer Operations

The shared code generation framework models 64-bit integer operations on
i686 using the same "i128" trait methods that other architectures use for
actual 128-bit values.  On i686, these operate on `%eax:%edx` register
pairs representing 64-bit values.

The pattern for binary operations:

1. Load the RHS pair into `%eax:%edx`.
2. Push both halves onto the stack (`pushl %edx; pushl %eax`).
3. Load the LHS pair into `%eax:%edx`.
4. Operate: e.g., `addl (%esp), %eax; adcl 4(%esp), %edx`.
5. Pop the stack (`addl $8, %esp`).
6. Store the result pair to the destination slot.

Shift operations use the double-precision shift instructions `shldl` and
`shrdl`, which shift a 64-bit value formed by concatenating two 32-bit
registers.  A `testb $32, %cl` / conditional branch handles the case where
the shift amount crosses the 32-bit boundary.  Constant shifts are expanded
inline without branches, using different sequences for amounts < 32,
== 32, and >= 32.

Multiplication uses the classic schoolbook decomposition:

```
(A_hi : A_lo) * (B_hi : B_lo) =
    A_lo * B_lo                  (mull: full 64-bit result in %edx:%eax)
  + (A_hi * B_lo) << 32          (imull: low 32 bits into high accumulator)
  + (A_lo * B_hi) << 32          (imull: low 32 bits into high accumulator)
```

---

## Division-by-Constant Optimization (Disabled)

The IR-level `div_by_const` pass, which replaces integer division by
compile-time constants with multiply-and-shift sequences, is **disabled for
the i686 target**.  The replacement sequences use `MulHigh` (upper-half
multiply) operations that the IR expresses as 64-bit arithmetic.  The i686
backend truncates 64-bit operations to 32 bits in its accumulator, producing
incorrect results for these sequences.

Until a 32-bit-aware variant is implemented (using single-operand `imull`
for the upper-half multiply), the backend falls back to hardware
`idivl`/`divl` instructions for all division and modulo operations.  The
guard is `!target.is_32bit()` in the optimization pipeline.

---

## Frame Pointer Omission (`-fomit-frame-pointer`)

When `-fomit-frame-pointer` is passed, the backend:

1. Skips the `pushl %ebp` / `movl %esp, %ebp` prologue sequence.
2. Adds `%ebp` (PhysReg 3) to the callee-saved allocatable set, giving the
   register allocator a fourth register.
3. Converts all stack references from `offset(%ebp)` to `offset(%esp)`.
4. Tracks `esp_adjust` to account for temporary ESP modifications.

The epilogue correspondingly skips `popl %ebp` and uses `addl` to restore
`%esp` instead of `leal -N(%ebp), %esp`.

Parameter references require a small correction: without the pushed `%ebp`,
parameters are 4 bytes closer to the current stack frame.  The `param_ref`
helper subtracts 4 from the EBP-relative offset before adding the
ESP-relative base.

Stack alignment calculations use a different bias (12 instead of 8) to
account for the absence of the pushed `%ebp` when ensuring 16-byte-aligned
slots.

---

## Peephole Optimizer

After all assembly text is emitted, the entire function is processed by a
multi-pass peephole optimizer (`peephole.rs`) that eliminates redundancies
inherent in the accumulator-based code generation style.

### Pass Structure

1. **Local passes** (iterative, up to 8 rounds):
   - **Store/load elimination:** A `movl %eax, -8(%ebp)` immediately
     followed by `movl -8(%ebp), %eax` -- the load is removed.
   - **Self-move elimination:** `movl %eax, %eax` is deleted.
   - **Redundant jump elimination:** An unconditional `jmp` to the
     immediately following label is removed.
   - **Branch inversion:** A conditional jump over an unconditional jump is
     inverted to eliminate the unconditional jump.
   - **Reverse move elimination:** A `movl %ecx, %eax` followed by
     `movl %eax, %ecx` -- the second is removed.

2. **Global passes** (single pass):
   - **Dead register move elimination:** A `movl %eax, %ecx` where `%ecx`
     is never read before being overwritten is removed.
   - **Dead store elimination:** A `movl %eax, -8(%ebp)` where the slot is
     written again before being read is removed.
   - **Compare+branch fusion:** Detects patterns where a comparison result
     is stored, reloaded, and tested, fusing them into a single
     compare-and-branch.
   - **Memory operand folding:** Replaces a load-from-slot + ALU-with-register
     sequence with a single ALU-with-memory-operand instruction.

3. **Local cleanup** (up to 4 rounds): Re-runs local and global passes to
   clean up opportunities exposed by the previous round.

4. **Never-read store elimination:** A global analysis removes stores to
   stack slots that are never read anywhere in the function.

### Line Classification

Every assembly line is classified into a `LineKind` enum (`StoreEbp`,
`LoadEbp`, `Move`, `Label`, `Jmp`, `CondJmp`, `Call`, `Ret`, `Push`, `Pop`,
`SetCC`, `Cmp`, `Directive`, `Other`) for efficient pattern matching.
Register operands are mapped to family IDs (0--7 for `%eax` through `%edi`)
so that sub-register aliases (`%al`, `%ax`, `%eax`) are treated as the same
physical register.

---

## Code16gcc Support

The `-m16` flag prepends `.code16gcc` to the assembly output.  This GNU
assembler directive causes all subsequent 32-bit instructions to be emitted
with operand-size and address-size override prefixes, allowing the code to
execute in 16-bit real mode while being written in 32-bit syntax.

This is used by the Linux kernel's early boot code, which runs in real mode
but is compiled with a 32-bit toolchain.  The backend does not change its
code generation; the assembler handles the prefix insertion transparently.
The `.code16gcc` directive is prepended in the top-level module dispatch
after peephole optimization completes.

---

## Key Design Decisions and Challenges

### The Accumulator Bottleneck

With only 6 GPRs total (3 scratch, 3 callee-saved), the i686 backend cannot
use a general-purpose register allocator the way x86-64 can with its 15
GPRs.  Instead, it uses `%eax` as a universal accumulator: every expression
evaluation flows through `%eax`, with `%ecx` as the secondary operand
register for binary operations and `%edx` as the implicit upper-half
register for multiply/divide/64-bit pairs.

This design is simple and correct, but produces excessive memory traffic
(store to stack, reload from stack).  The register allocator mitigates this
by assigning the most frequently used values to `%ebx`, `%esi`, `%edi`
(and `%ebp` when available), and the peephole optimizer eliminates the
remaining redundant store/load pairs.

A register cache (`reg_cache`) tracks what IR value is currently in `%eax`,
allowing `operand_to_eax` to skip the load when the value is already present.
This simple one-entry cache eliminates a significant fraction of redundant
loads without the complexity of a full register allocator.

### 64-bit Values on a 32-bit Machine

Every 64-bit operation requires careful orchestration of register pairs.
The difficulty is compounded by the scarcity of registers: with `%eax:%edx`
holding the result and `%ecx` needed for shift counts, there are no scratch
registers left for the second operand.  The backend resolves this by pushing
the RHS onto the stack and operating against `(%esp)`.

64-bit comparisons are particularly tricky: ordered comparisons must first
check the high halves, then branch to check the low halves only if the high
halves are equal.  This requires careful label management and different
condition codes for the high (signed) and low (unsigned) halves.

### ESP Tracking for Frame Pointer Omission

Without `%ebp` as a stable reference point, every temporary ESP adjustment
(pushing call arguments, pushing temporaries for x87 conversions, etc.)
shifts all stack slot addresses.  The `esp_adjust` field is meticulously
incremented and decremented around every `pushl`/`subl` and `popl`/`addl`
that modifies `%esp`, and `slot_ref` adds it to every stack access.  A
single missed update would silently corrupt all subsequent memory references.

### PIC Mode and `%ebx` Reservation

Position-independent code on i686 requires a GOT base register.  The
backend reserves `%ebx` for this purpose, loading it in the prologue via
`call __x86.get_pc_thunk.bx` / `addl $_GLOBAL_OFFSET_TABLE_, %ebx`.
Global address references use `@GOT(%ebx)` for external symbols and
`@GOTOFF(%ebx)` for local symbols.  The `__x86.get_pc_thunk.bx` helper is
emitted as a COMDAT section so that the linker deduplicates it across
translation units.

### Standalone 64-bit Division Runtime

Programs that link without libgcc (e.g., musl libc) need compiler-provided
implementations of `__divdi3`, `__udivdi3`, `__moddi3`, and `__umoddi3`.
The backend emits these as `.weak` symbols in the `.text` section, based on
the compiler-rt i386 division algorithms using normalized-divisor estimation.
If libgcc is linked, its strong symbols take precedence.  The stubs are only
emitted when 64-bit division is actually used (`needs_divdi3_helpers` flag).

### 64-bit Atomic Operations

The i686 ISA has no 64-bit atomic load/store instructions.  The backend uses
`lock cmpxchg8b` loops for all 64-bit atomic operations (RMW, cmpxchg,
load, store).  This requires `%ebx` and `%ecx` for the desired value and
`%eax:%edx` for the expected/old value, consuming all scratch registers
and `%ebx`.  The backend saves `%ebx` and `%esi` on the stack and uses
`%esi` as the pointer register for the duration of the atomic operation.
