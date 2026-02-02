# i686 Backend

Code generation targeting 32-bit x86 (i686) with the cdecl calling convention.

## Key Differences from x86-64

- **ILP32 type model**: pointers are 4 bytes, `long` is 4 bytes, `size_t` is `unsigned int`
- **cdecl calling convention**: all arguments passed on the stack by default. With `-mregparm=N` (N=1..3), the first N integer args are passed in EAX, EDX, ECX. Return values in `eax` (32-bit) or `eax:edx` (64-bit). Also supports `__attribute__((fastcall))` (first two DWORD args in ECX/EDX)
- **No native 64-bit arithmetic**: 64-bit operations are split into 32-bit register pairs (`eax:edx`). Division-by-constant strength reduction is disabled because the generated 64-bit multiply sequences cannot be executed correctly
- **x87 FPU for long double**: same as x86-64, F128 operations use the x87 FPU stack
- **Limited register pool**: 6 general-purpose registers (eax, ecx, edx, ebx, esi, edi) with ebx/esi/edi callee-saved

## Structure

| File | Responsibility |
|------|---------------|
| `codegen.rs` | Main `I686Codegen` struct implementing `ArchCodegen`. The largest backend file due to 64-bit operation splitting |
| `alu.rs` | Integer arithmetic and bitwise operations |
| `atomics.rs` | Atomic operations |
| `calls.rs` | Function call emission (cdecl, fastcall, regparm) |
| `casts.rs` | Type casts |
| `comparison.rs` | Compare operations |
| `float_ops.rs` | Floating-point operations |
| `globals.rs` | Global variable access |
| `i128_ops.rs` | 128-bit integer operations (4-register sequences) |
| `inline_asm.rs` | i686 inline asm template substitution and register formatting |
| `intrinsics.rs` | SSE2 intrinsic emission |
| `memory.rs` | Load/store operations |
| `peephole.rs` | Post-codegen peephole optimizer |
| `prologue.rs` | Function prologue/epilogue |
| `returns.rs` | Return value handling |
| `variadic.rs` | va_list / va_arg implementation |
| `asm_emitter.rs` | `InlineAsmEmitter` trait implementation |

## Known Limitations

- Division-by-constant pass disabled (generates 64-bit arithmetic the backend can't handle)
