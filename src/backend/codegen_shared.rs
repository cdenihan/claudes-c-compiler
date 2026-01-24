//! Shared code generation framework for all backends.
//!
//! All three backends (x86-64, AArch64, RISC-V 64) use the same stack-based
//! code generation strategy: each IR value is assigned a stack slot, and
//! instructions move values through a primary accumulator register.
//!
//! This module extracts the shared instruction dispatch and function generation
//! logic. Each architecture implements `ArchCodegen` to provide its specific
//! register names, instruction mnemonics, and ABI details.

use std::collections::{HashMap, HashSet};
use crate::ir::ir::*;
use crate::common::types::IrType;
use super::common::{self, AsmOutput, PtrDirective};

/// Stack slot location for a value. Interpretation varies by arch:
/// - x86: negative offset from %rbp
/// - ARM: positive offset from sp
/// - RISC-V: negative offset from s0
#[derive(Debug, Clone, Copy)]
pub struct StackSlot(pub i64);

/// Shared codegen state, used by all backends.
pub struct CodegenState {
    pub out: AsmOutput,
    pub stack_offset: i64,
    pub value_locations: HashMap<u32, StackSlot>,
    /// Values that are allocas (their stack slot IS the data, not a pointer to data).
    pub alloca_values: HashSet<u32>,
    /// Type associated with each alloca (for type-aware loads/stores).
    pub alloca_types: HashMap<u32, IrType>,
    /// Values that are 128-bit integers (need 16-byte copy).
    pub i128_values: HashSet<u32>,
    /// Counter for generating unique labels (e.g., memcpy loops).
    label_counter: u32,
    /// Whether position-independent code (PIC) generation is enabled.
    pub pic_mode: bool,
    /// Set of symbol names that are locally defined (not extern) and have internal
    /// linkage (static) — these can use direct addressing even in PIC mode.
    pub local_symbols: HashSet<String>,
}

impl CodegenState {
    pub fn new() -> Self {
        Self {
            out: AsmOutput::new(),
            stack_offset: 0,
            value_locations: HashMap::new(),
            alloca_values: HashSet::new(),
            alloca_types: HashMap::new(),
            i128_values: HashSet::new(),
            label_counter: 0,
            pic_mode: false,
            local_symbols: HashSet::new(),
        }
    }

    pub fn next_label_id(&mut self) -> u32 {
        let id = self.label_counter;
        self.label_counter += 1;
        id
    }

    /// Generate a fresh label with the given prefix.
    pub fn fresh_label(&mut self, prefix: &str) -> String {
        let id = self.next_label_id();
        format!(".L{}_{}", prefix, id)
    }

    pub fn emit(&mut self, s: &str) {
        self.out.emit(s);
    }

    pub fn reset_for_function(&mut self) {
        self.stack_offset = 0;
        self.value_locations.clear();
        self.alloca_values.clear();
        self.alloca_types.clear();
        self.i128_values.clear();
    }

    pub fn is_alloca(&self, v: u32) -> bool {
        self.alloca_values.contains(&v)
    }

    pub fn get_slot(&self, v: u32) -> Option<StackSlot> {
        self.value_locations.get(&v).copied()
    }

    pub fn is_i128_value(&self, v: u32) -> bool {
        self.i128_values.contains(&v)
    }

    /// Returns true if the given symbol needs GOT indirection in PIC mode.
    /// A symbol needs GOT if PIC is enabled AND it's not a local (static) symbol.
    /// Local labels (starting with '.') are always PIC-safe via RIP-relative.
    pub fn needs_got(&self, name: &str) -> bool {
        if !self.pic_mode {
            return false;
        }
        // Local labels (.Lxxx) never need GOT
        if name.starts_with('.') {
            return false;
        }
        // Symbols defined with internal linkage (static) don't need GOT
        !self.local_symbols.contains(name)
    }

    /// Returns true if a function call needs PLT indirection in PIC mode.
    pub fn needs_plt(&self, name: &str) -> bool {
        self.needs_got(name)
    }
}

/// Trait that each architecture implements to provide its specific code generation.
///
/// The shared framework calls these methods during instruction dispatch.
/// Each method should emit the appropriate assembly instructions.
pub trait ArchCodegen {
    /// Mutable access to the shared codegen state.
    fn state(&mut self) -> &mut CodegenState;
    /// Immutable access to the shared codegen state.
    fn state_ref(&self) -> &CodegenState;

    /// The pointer directive for this architecture's data emission.
    fn ptr_directive(&self) -> PtrDirective;

    /// Calculate stack space and assign locations for all values in the function.
    /// Returns the raw stack space needed (before alignment).
    fn calculate_stack_space(&mut self, func: &IrFunction) -> i64;

    /// Emit function prologue (save frame pointer, allocate stack).
    fn emit_prologue(&mut self, func: &IrFunction, frame_size: i64);

    /// Emit function epilogue (restore frame pointer, deallocate stack).
    fn emit_epilogue(&mut self, frame_size: i64);

    /// Store function parameters from argument registers to their stack slots.
    fn emit_store_params(&mut self, func: &IrFunction);

    /// Load an operand into the primary accumulator register (rax/x0/t0).
    fn emit_load_operand(&mut self, op: &Operand);

    /// Store the primary accumulator to a value's stack slot.
    fn emit_store_result(&mut self, dest: &Value);

    /// Emit a store instruction: store val to the address in ptr.
    fn emit_store(&mut self, val: &Operand, ptr: &Value, ty: IrType);

    /// Emit a load instruction: load from the address in ptr to dest.
    fn emit_load(&mut self, dest: &Value, ptr: &Value, ty: IrType);

    /// Emit a binary operation. Default: dispatches float/integer ops via
    /// `emit_float_binop` and `emit_int_binop` primitives.
    fn emit_binop(&mut self, dest: &Value, op: IrBinOp, lhs: &Operand, rhs: &Operand, ty: IrType) {
        if ty.is_float() {
            let float_op = classify_float_binop(op)
                .unwrap_or_else(|| panic!("unsupported float binop: {:?} on type {:?}", op, ty));
            self.emit_float_binop(dest, float_op, lhs, rhs, ty);
            return;
        }
        self.emit_int_binop(dest, op, lhs, rhs, ty);
    }

    /// Emit a float binary operation (add/sub/mul/div).
    /// The default `emit_binop` dispatches here after classifying the operation.
    fn emit_float_binop(&mut self, dest: &Value, op: FloatOp, lhs: &Operand, rhs: &Operand, ty: IrType);

    /// Emit an integer binary operation (all IrBinOp variants).
    /// The default `emit_binop` dispatches here for non-float types.
    fn emit_int_binop(&mut self, dest: &Value, op: IrBinOp, lhs: &Operand, rhs: &Operand, ty: IrType);

    /// Emit a unary operation.
    fn emit_unaryop(&mut self, dest: &Value, op: IrUnaryOp, src: &Operand, ty: IrType);

    /// Emit a comparison operation.
    fn emit_cmp(&mut self, dest: &Value, op: IrCmpOp, lhs: &Operand, rhs: &Operand, ty: IrType);

    /// Emit a function call (direct or indirect).
    /// `arg_types` provides the IR type of each argument for proper calling convention handling.
    /// `is_variadic` indicates the called function uses variadic arguments (affects calling convention).
    /// `num_fixed_args` is the number of named (non-variadic) parameters. For variadic calls on
    /// AArch64, arguments beyond this index must be passed in GP registers even if they are floats.
    fn emit_call(&mut self, args: &[Operand], arg_types: &[IrType], direct_name: Option<&str>,
                 func_ptr: Option<&Operand>, dest: Option<Value>, return_type: IrType,
                 is_variadic: bool, num_fixed_args: usize);

    /// Emit a global address load.
    fn emit_global_addr(&mut self, dest: &Value, name: &str);

    /// Emit a get-element-pointer (base + offset).
    fn emit_gep(&mut self, dest: &Value, base: &Value, offset: &Operand);

    /// Emit architecture-specific instructions for a type cast, after the source
    /// value has been loaded into the accumulator. Does NOT load/store—only emits
    /// the conversion instructions.
    fn emit_cast_instrs(&mut self, from_ty: IrType, to_ty: IrType);

    /// Emit a type cast. Default: load source → emit_cast_instrs → store result.
    /// x86-64 overrides this to handle 128-bit widening/narrowing.
    fn emit_cast(&mut self, dest: &Value, src: &Operand, from_ty: IrType, to_ty: IrType) {
        self.emit_load_operand(src);
        self.emit_cast_instrs(from_ty, to_ty);
        self.emit_store_result(dest);
    }

    /// Emit a memory copy: copy `size` bytes from src address to dest address.
    fn emit_memcpy(&mut self, dest: &Value, src: &Value, size: usize);

    /// Emit va_arg: extract next variadic argument from va_list and store to dest.
    fn emit_va_arg(&mut self, dest: &Value, va_list_ptr: &Value, result_ty: IrType);

    /// Emit va_start: initialize a va_list for variadic argument access.
    fn emit_va_start(&mut self, va_list_ptr: &Value);

    /// Emit va_end: clean up a va_list. No-op on all current targets (x86-64, AArch64, RISC-V).
    fn emit_va_end(&mut self, _va_list_ptr: &Value) {
        // va_end is a no-op on all supported architectures
    }

    /// Emit va_copy: copy src va_list to dest va_list.
    fn emit_va_copy(&mut self, dest_ptr: &Value, src_ptr: &Value);

    /// Emit an atomic read-modify-write operation.
    fn emit_atomic_rmw(&mut self, dest: &Value, op: AtomicRmwOp, ptr: &Operand, val: &Operand, ty: IrType, ordering: AtomicOrdering);

    /// Emit an atomic compare-and-exchange operation.
    fn emit_atomic_cmpxchg(&mut self, dest: &Value, ptr: &Operand, expected: &Operand, desired: &Operand, ty: IrType, success_ordering: AtomicOrdering, failure_ordering: AtomicOrdering, returns_bool: bool);

    /// Emit an atomic load.
    fn emit_atomic_load(&mut self, dest: &Value, ptr: &Operand, ty: IrType, ordering: AtomicOrdering);

    /// Emit an atomic store.
    fn emit_atomic_store(&mut self, ptr: &Operand, val: &Operand, ty: IrType, ordering: AtomicOrdering);

    /// Emit a memory fence.
    fn emit_fence(&mut self, ordering: AtomicOrdering);

    /// Emit inline assembly.
    fn emit_inline_asm(&mut self, template: &str, outputs: &[(String, Value, Option<String>)], inputs: &[(String, Operand, Option<String>)], clobbers: &[String], operand_types: &[IrType]);

    /// Emit a return terminator.
    fn emit_return(&mut self, val: Option<&Operand>, frame_size: i64);

    // ---- Architecture-specific instruction primitives ----
    // These small methods provide the building blocks for default implementations
    // of higher-level codegen methods, avoiding duplication across backends.

    /// The unconditional jump mnemonic for this architecture.
    /// x86: "jmp", ARM: "b", RISC-V: "j"
    fn jump_mnemonic(&self) -> &'static str;

    /// The trap/unreachable instruction for this architecture.
    /// x86: "ud2", ARM: "brk #0", RISC-V: "ebreak"
    fn trap_instruction(&self) -> &'static str;

    /// Emit a branch-if-nonzero instruction: if the accumulator register is
    /// nonzero, branch to `label`. Does NOT emit the preceding operand load.
    /// x86: "testq %rax, %rax; jne label", ARM: "cbnz x0, label", RISC-V: "bnez t0, label"
    fn emit_branch_nonzero(&mut self, label: &str);

    /// Emit an indirect jump through the accumulator register.
    /// x86: "jmpq *%rax", ARM: "br x0", RISC-V: "jr t0"
    fn emit_jump_indirect(&mut self);

    // ---- Default implementations for simple terminators and branches ----
    // These use the primitives above to provide shared control flow logic.

    /// Emit an unconditional branch. Default uses jump_mnemonic().
    fn emit_branch(&mut self, label: &str) {
        let mnemonic = self.jump_mnemonic();
        self.state().emit(&format!("    {} {}", mnemonic, label));
    }

    /// Emit an unreachable trap instruction. Default uses trap_instruction().
    fn emit_unreachable(&mut self) {
        let trap = self.trap_instruction();
        self.state().emit(&format!("    {}", trap));
    }

    /// Emit a conditional branch: load cond, branch to true_label if nonzero,
    /// fall through to false_label. Default uses emit_load_operand + emit_branch_nonzero + emit_branch.
    fn emit_cond_branch(&mut self, cond: &Operand, true_label: &str, false_label: &str) {
        self.emit_load_operand(cond);
        self.emit_branch_nonzero(true_label);
        self.emit_branch(false_label);
    }

    /// Emit an indirect branch (computed goto: goto *addr).
    /// Default: load target into accumulator, emit indirect jump.
    fn emit_indirect_branch(&mut self, target: &Operand) {
        self.emit_load_operand(target);
        self.emit_jump_indirect();
    }

    /// Emit a label address load (GCC &&label extension for computed goto).
    /// Default delegates to emit_global_addr since on all current backends,
    /// loading a label address uses the same mechanism as loading a global symbol address.
    fn emit_label_addr(&mut self, dest: &Value, label: &str) {
        self.emit_global_addr(dest, label);
    }

    /// Emit code to capture the second F64 return value after a function call.
    /// On x86-64: read xmm1 into dest. On ARM64: read d1. On RISC-V: read fa1.
    fn emit_get_return_f64_second(&mut self, dest: &Value);

    /// Emit code to set the second F64 return value before a return.
    /// On x86-64: write xmm1. On ARM64: write d1. On RISC-V: write fa1.
    fn emit_set_return_f64_second(&mut self, src: &Operand);

    /// Emit the function directive for the function type attribute.
    /// x86 uses "@function", ARM uses "%function".
    fn function_type_directive(&self) -> &'static str { "@function" }

    /// Emit dynamic stack allocation: subtract size from stack pointer,
    /// align the result, and store the pointer in dest.
    fn emit_dyn_alloca(&mut self, dest: &Value, size: &Operand, align: usize);

    /// Emit a 128-bit value copy (src -> dest, both 16-byte stack slots).
    /// Default: truncates to 64-bit (used by ARM/RISC-V where i128 is not fully supported).
    /// x86-64 overrides this to use rax:rdx register pair.
    fn emit_copy_i128(&mut self, dest: &Value, src: &Operand) {
        self.emit_load_operand(src);
        self.emit_store_result(dest);
    }

    /// Frame size including alignment and saved registers.
    fn aligned_frame_size(&self, raw_space: i64) -> i64;

    /// Emit an X86 SSE operation. Default is no-op (non-x86 targets).
    fn emit_x86_sse_op(&mut self, _dest: &Option<Value>, _op: &X86SseOpKind, _dest_ptr: &Option<Value>, _args: &[Operand]) {}
}

/// Generate assembly for a module using the given architecture's codegen.
pub fn generate_module(cg: &mut dyn ArchCodegen, module: &IrModule) -> String {
    // Build the set of locally-defined symbols for PIC mode.
    // In PIC mode, only symbols with internal linkage (static) are guaranteed
    // not to be interposed. Non-static globals/functions need GOT/PLT because
    // they could be preempted by another shared library (ELF symbol preemption).
    // String literal labels (starting with .L) are always local.
    {
        let state = cg.state();
        for func in &module.functions {
            if func.is_static {
                state.local_symbols.insert(func.name.clone());
            }
        }
        for global in &module.globals {
            if global.is_static {
                state.local_symbols.insert(global.name.clone());
            }
        }
        // String/wide string literal labels are always local (compiler-generated .L labels)
        for (label, _) in &module.string_literals {
            state.local_symbols.insert(label.clone());
        }
        for (label, _) in &module.wide_string_literals {
            state.local_symbols.insert(label.clone());
        }
    }

    // Emit data sections
    let ptr_dir = cg.ptr_directive();
    common::emit_data_sections(&mut cg.state().out, module, ptr_dir);

    // Text section
    cg.state().emit(".section .text");
    for func in &module.functions {
        if !func.is_declaration {
            generate_function(cg, func);
        }
    }

    // Emit .init_array for constructor functions
    for ctor in &module.constructors {
        cg.state().emit("");
        cg.state().emit(".section .init_array,\"aw\",@init_array");
        cg.state().emit(".align 8");
        cg.state().emit(&format!("{} {}", ptr_dir.as_str(), ctor));
    }

    // Emit .fini_array for destructor functions
    for dtor in &module.destructors {
        cg.state().emit("");
        cg.state().emit(".section .fini_array,\"aw\",@fini_array");
        cg.state().emit(".align 8");
        cg.state().emit(&format!("{} {}", ptr_dir.as_str(), dtor));
    }

    // Emit .note.GNU-stack section to indicate non-executable stack
    cg.state().emit("");
    cg.state().emit(".section .note.GNU-stack,\"\",@progbits");

    std::mem::take(&mut cg.state().out.buf)
}

/// Generate code for a single function.
fn generate_function(cg: &mut dyn ArchCodegen, func: &IrFunction) {
    cg.state().reset_for_function();

    let type_dir = cg.function_type_directive();
    if !func.is_static {
        cg.state().emit(&format!(".globl {}", func.name));
    }
    cg.state().emit(&format!(".type {}, {}", func.name, type_dir));
    cg.state().emit(&format!("{}:", func.name));

    // Calculate stack space and emit prologue
    let raw_space = cg.calculate_stack_space(func);
    let frame_size = cg.aligned_frame_size(raw_space);
    cg.emit_prologue(func, frame_size);

    // Store parameters
    cg.emit_store_params(func);

    // Generate basic blocks
    for block in &func.blocks {
        if block.label != "entry" {
            cg.state().emit(&format!("{}:", block.label));
        }
        for inst in &block.instructions {
            generate_instruction(cg, inst);
        }
        generate_terminator(cg, &block.terminator, frame_size);
    }

    cg.state().emit(&format!(".size {}, .-{}", func.name, func.name));
    cg.state().emit("");
}

/// Dispatch a single IR instruction to the appropriate arch method.
fn generate_instruction(cg: &mut dyn ArchCodegen, inst: &Instruction) {
    match inst {
        Instruction::Alloca { .. } => {
            // Space already allocated in prologue
        }
        Instruction::DynAlloca { dest, size, align } => {
            cg.emit_dyn_alloca(dest, size, *align);
        }
        Instruction::Store { val, ptr, ty } => {
            cg.emit_store(val, ptr, *ty);
        }
        Instruction::Load { dest, ptr, ty } => {
            cg.emit_load(dest, ptr, *ty);
        }
        Instruction::BinOp { dest, op, lhs, rhs, ty } => {
            cg.emit_binop(dest, *op, lhs, rhs, *ty);
        }
        Instruction::UnaryOp { dest, op, src, ty } => {
            cg.emit_unaryop(dest, *op, src, *ty);
        }
        Instruction::Cmp { dest, op, lhs, rhs, ty } => {
            cg.emit_cmp(dest, *op, lhs, rhs, *ty);
        }
        Instruction::Call { dest, func, args, arg_types, return_type, is_variadic, num_fixed_args } => {
            cg.emit_call(args, arg_types, Some(func), None, *dest, *return_type, *is_variadic, *num_fixed_args);
        }
        Instruction::CallIndirect { dest, func_ptr, args, arg_types, return_type, is_variadic, num_fixed_args } => {
            cg.emit_call(args, arg_types, None, Some(func_ptr), *dest, *return_type, *is_variadic, *num_fixed_args);
        }
        Instruction::GlobalAddr { dest, name } => {
            cg.emit_global_addr(dest, name);
        }
        Instruction::Copy { dest, src } => {
            // Check if this is a 128-bit value copy
            let is_i128_copy = match src {
                Operand::Value(v) => cg.state_ref().is_i128_value(v.0),
                Operand::Const(IrConst::I128(_)) => true,
                _ => false,
            };
            if is_i128_copy {
                // Mark dest as i128 too so subsequent copies from it are also 128-bit
                cg.state().i128_values.insert(dest.0);
                cg.emit_copy_i128(dest, src);
            } else {
                cg.emit_load_operand(src);
                cg.emit_store_result(dest);
            }
        }
        Instruction::Cast { dest, src, from_ty, to_ty } => {
            cg.emit_cast(dest, src, *from_ty, *to_ty);
        }
        Instruction::GetElementPtr { dest, base, offset, .. } => {
            cg.emit_gep(dest, base, offset);
        }
        Instruction::Memcpy { dest, src, size } => {
            cg.emit_memcpy(dest, src, *size);
        }
        Instruction::VaArg { dest, va_list_ptr, result_ty } => {
            cg.emit_va_arg(dest, va_list_ptr, *result_ty);
        }
        Instruction::VaStart { va_list_ptr } => {
            cg.emit_va_start(va_list_ptr);
        }
        Instruction::VaEnd { va_list_ptr } => {
            cg.emit_va_end(va_list_ptr);
        }
        Instruction::VaCopy { dest_ptr, src_ptr } => {
            cg.emit_va_copy(dest_ptr, src_ptr);
        }
        Instruction::AtomicRmw { dest, op, ptr, val, ty, ordering } => {
            cg.emit_atomic_rmw(dest, *op, ptr, val, *ty, *ordering);
        }
        Instruction::AtomicCmpxchg { dest, ptr, expected, desired, ty, success_ordering, failure_ordering, returns_bool } => {
            cg.emit_atomic_cmpxchg(dest, ptr, expected, desired, *ty, *success_ordering, *failure_ordering, *returns_bool);
        }
        Instruction::AtomicLoad { dest, ptr, ty, ordering } => {
            cg.emit_atomic_load(dest, ptr, *ty, *ordering);
        }
        Instruction::AtomicStore { ptr, val, ty, ordering } => {
            cg.emit_atomic_store(ptr, val, *ty, *ordering);
        }
        Instruction::Fence { ordering } => {
            cg.emit_fence(*ordering);
        }
        Instruction::Phi { .. } => {
            // Phi nodes are resolved before codegen by lowering them to copies
            // at the end of predecessor blocks. This case should not be reached
            // in normal operation, but is a no-op for safety.
        }
        Instruction::LabelAddr { dest, label } => {
            cg.emit_label_addr(dest, label);
        }
        Instruction::GetReturnF64Second { dest } => {
            cg.emit_get_return_f64_second(dest);
        }
        Instruction::SetReturnF64Second { src } => {
            cg.emit_set_return_f64_second(src);
        }
        Instruction::InlineAsm { template, outputs, inputs, clobbers, operand_types } => {
            cg.emit_inline_asm(template, outputs, inputs, clobbers, operand_types);
        }
        Instruction::X86SseOp { dest, op, dest_ptr, args } => {
            cg.emit_x86_sse_op(dest, op, dest_ptr, args);
        }
    }
}

/// Dispatch a terminator to the appropriate arch method.
fn generate_terminator(cg: &mut dyn ArchCodegen, term: &Terminator, frame_size: i64) {
    match term {
        Terminator::Return(val) => {
            cg.emit_return(val.as_ref(), frame_size);
        }
        Terminator::Branch(label) => {
            cg.emit_branch(label);
        }
        Terminator::CondBranch { cond, true_label, false_label } => {
            cg.emit_cond_branch(cond, true_label, false_label);
        }
        Terminator::IndirectBranch { target, .. } => {
            cg.emit_indirect_branch(target);
        }
        Terminator::Unreachable => {
            cg.emit_unreachable();
        }
    }
}

/// Check if an IR type is a 128-bit integer type (I128 or U128).
/// Used by all backends to detect 128-bit operands.
pub fn is_i128_type(ty: IrType) -> bool {
    matches!(ty, IrType::I128 | IrType::U128)
}

/// Shared stack space calculation: iterates over all instructions, assigns stack
/// slots for allocas and value results. Arch-specific offset direction is handled
/// by the `assign_slot` closure.
///
/// `initial_offset`: starting offset (e.g., 0 for x86, 16 for ARM/RISC-V to skip saved regs)
/// `assign_slot`: maps (current_space, raw_alloca_size) -> (slot_offset, new_space)
///   - raw_alloca_size is the alloca byte count (0 means pointer-sized, i.e., 8)
///   - For non-alloca values, raw_alloca_size is always 8
///   - The closure is responsible for alignment
pub fn calculate_stack_space_common(
    state: &mut CodegenState,
    func: &IrFunction,
    initial_offset: i64,
    assign_slot: impl Fn(i64, i64) -> (i64, i64),
) -> i64 {
    let mut space = initial_offset;
    for block in &func.blocks {
        for inst in &block.instructions {
            if let Instruction::Alloca { dest, size, ty, .. } = inst {
                let raw_size = if *size == 0 { 8 } else { *size as i64 };
                let (slot, new_space) = assign_slot(space, raw_size);
                state.value_locations.insert(dest.0, StackSlot(slot));
                state.alloca_values.insert(dest.0);
                state.alloca_types.insert(dest.0, *ty);
                space = new_space;
            } else if let Some(dest) = inst.dest() {
                // Use 16-byte slots for I128/U128 result types, 8 bytes for everything else
                let is_i128 = matches!(inst.result_type(), Some(IrType::I128) | Some(IrType::U128));
                let slot_size = if is_i128 { 16 } else { 8 };
                let (slot, new_space) = assign_slot(space, slot_size);
                state.value_locations.insert(dest.0, StackSlot(slot));
                if is_i128 {
                    state.i128_values.insert(dest.0);
                }
                space = new_space;
            }
        }
    }
    space
}

// ============================================================================
// Shared cast classification
// ============================================================================

/// Classification of type casts. All three backends use the same control flow
/// to decide which kind of cast to emit; only the actual instructions differ.
/// By classifying the cast once in shared code, we eliminate duplicated
/// Ptr-normalization and F128-reduction logic from each backend.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CastKind {
    /// No conversion needed (same type, or Ptr <-> I64/U64, or F128 <-> F64).
    Noop,
    /// Float to signed integer (from_ty is F32 or F64).
    FloatToSigned { from_f64: bool },
    /// Float to unsigned integer (from_ty is F32 or F64).
    FloatToUnsigned { from_f64: bool, to_u64: bool },
    /// Signed integer to float (to_ty is F32 or F64).
    SignedToFloat { to_f64: bool },
    /// Unsigned integer to float. `from_u64` indicates U64 source needing
    /// special overflow handling on x86.
    UnsignedToFloat { to_f64: bool, from_u64: bool },
    /// Float-to-float conversion (F32 <-> F64).
    FloatToFloat { widen: bool },
    /// Integer widening: sign- or zero-extend a smaller type to a larger one.
    IntWiden { from_ty: IrType, to_ty: IrType },
    /// Integer narrowing: truncate a larger type to a smaller one.
    IntNarrow { to_ty: IrType },
    /// Same-size signed-to-unsigned (need to mask/clear upper bits).
    SignedToUnsignedSameSize { to_ty: IrType },
}

/// Classify a cast between two IR types. This captures the shared decision logic
/// that all three backends use identically. Backends then match on the returned
/// `CastKind` to emit architecture-specific instructions.
///
/// Handles Ptr normalization (Ptr treated as U64) and F128 reduction (F128 treated
/// as F64 for computation purposes) before classification.
pub fn classify_cast(from_ty: IrType, to_ty: IrType) -> CastKind {
    if from_ty == to_ty {
        return CastKind::Noop;
    }

    // F128 (long double) is computed as F64. Treat F128 <-> F64 as noop,
    // and F128 <-> other as F64 <-> other.
    if from_ty == IrType::F128 || to_ty == IrType::F128 {
        let effective_from = if from_ty == IrType::F128 { IrType::F64 } else { from_ty };
        let effective_to = if to_ty == IrType::F128 { IrType::F64 } else { to_ty };
        if effective_from == effective_to {
            return CastKind::Noop;
        }
        return classify_cast(effective_from, effective_to);
    }

    // Ptr is equivalent to U64 on all 64-bit targets.
    if (from_ty == IrType::Ptr || to_ty == IrType::Ptr) && !from_ty.is_float() && !to_ty.is_float() {
        let effective_from = if from_ty == IrType::Ptr { IrType::U64 } else { from_ty };
        let effective_to = if to_ty == IrType::Ptr { IrType::U64 } else { to_ty };
        if effective_from == effective_to || (effective_from.size() == 8 && effective_to.size() == 8) {
            return CastKind::Noop;
        }
        return classify_cast(effective_from, effective_to);
    }

    // Float-to-int
    if from_ty.is_float() && !to_ty.is_float() {
        let is_unsigned_dest = to_ty.is_unsigned() || to_ty == IrType::Ptr;
        let from_f64 = from_ty == IrType::F64;
        if is_unsigned_dest {
            let to_u64 = to_ty == IrType::U64 || to_ty == IrType::Ptr;
            return CastKind::FloatToUnsigned { from_f64, to_u64 };
        } else {
            return CastKind::FloatToSigned { from_f64 };
        }
    }

    // Int-to-float
    if !from_ty.is_float() && to_ty.is_float() {
        let is_unsigned_src = from_ty.is_unsigned();
        let to_f64 = to_ty == IrType::F64;
        if is_unsigned_src {
            let from_u64 = from_ty == IrType::U64;
            return CastKind::UnsignedToFloat { to_f64, from_u64 };
        } else {
            return CastKind::SignedToFloat { to_f64 };
        }
    }

    // Float-to-float
    if from_ty.is_float() && to_ty.is_float() {
        let widen = from_ty == IrType::F32 && to_ty == IrType::F64;
        return CastKind::FloatToFloat { widen };
    }

    // Integer-to-integer
    let from_size = from_ty.size();
    let to_size = to_ty.size();

    if from_size == to_size {
        // Same size, different signedness
        if from_ty.is_signed() && to_ty.is_unsigned() {
            return CastKind::SignedToUnsignedSameSize { to_ty };
        }
        return CastKind::Noop;
    }

    if to_size > from_size {
        return CastKind::IntWiden { from_ty, to_ty };
    }

    CastKind::IntNarrow { to_ty }
}

// ============================================================================
// Shared float operation helpers
// ============================================================================

/// Classify a binary operation on floats. Returns None if the operation is not
/// meaningful on floats (e.g., bitwise And, Or, Xor, shifts, integer remainder).
/// This prevents the latent bug where all three backends silently emit `fadd`
/// for unsupported float operations.
pub fn classify_float_binop(op: IrBinOp) -> Option<FloatOp> {
    match op {
        IrBinOp::Add => Some(FloatOp::Add),
        IrBinOp::Sub => Some(FloatOp::Sub),
        IrBinOp::Mul => Some(FloatOp::Mul),
        IrBinOp::SDiv | IrBinOp::UDiv => Some(FloatOp::Div),
        // Bitwise ops, shifts, and remainder are not meaningful for floats.
        // Falling through to Add was a latent bug in all three backends.
        _ => None,
    }
}

/// Float arithmetic operations that all three backends support.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatOp {
    Add,
    Sub,
    Mul,
    Div,
}

/// Find the nth alloca instruction in the entry block (used for parameter storage).
pub fn find_param_alloca(func: &IrFunction, param_idx: usize) -> Option<(Value, IrType)> {
    func.blocks.first().and_then(|block| {
        block.instructions.iter()
            .filter(|i| matches!(i, Instruction::Alloca { .. }))
            .nth(param_idx)
            .and_then(|inst| {
                if let Instruction::Alloca { dest, ty, .. } = inst {
                    Some((*dest, *ty))
                } else {
                    None
                }
            })
    })
}

// ============================================================================
// Shared inline assembly framework
// ============================================================================

/// Operand classification for inline asm. Each backend classifies its constraints
/// into these categories so the shared framework can orchestrate register
/// assignment, tied operand resolution, and GCC numbering.
#[derive(Debug, Clone, PartialEq)]
pub enum AsmOperandKind {
    /// General-purpose register (e.g., x86 "r", ARM "r", RISC-V "r").
    GpReg,
    /// Floating-point register (RISC-V "f").
    FpReg,
    /// Memory operand (all arches "m").
    Memory,
    /// Specific named register (x86 "a"→"rax", RISC-V "a0", etc.).
    Specific(String),
    /// Tied to another operand by index (e.g., "0", "1").
    Tied(usize),
    /// Immediate value (RISC-V "I", "i", "n").
    Immediate,
    /// Address for atomic ops (RISC-V "A").
    Address,
    /// Zero-or-register (RISC-V "rJ", "J").
    ZeroOrReg,
}

/// Per-operand state tracked by the shared inline asm framework.
/// Backends populate arch-specific fields (mem_addr, mem_offset, imm_value)
/// during constraint classification.
#[derive(Debug, Clone)]
pub struct AsmOperand {
    pub kind: AsmOperandKind,
    pub reg: String,
    pub name: Option<String>,
    /// x86: memory address string like "offset(%rbp)".
    pub mem_addr: String,
    /// RISC-V/ARM: stack offset for memory/address operands.
    pub mem_offset: i64,
    /// Immediate value for "I"/"i" constraints.
    pub imm_value: Option<i64>,
}

impl AsmOperand {
    pub fn new(kind: AsmOperandKind, name: Option<String>) -> Self {
        Self { kind, reg: String::new(), name, mem_addr: String::new(), mem_offset: 0, imm_value: None }
    }
}

/// Trait that backends implement to provide architecture-specific inline asm behavior.
/// The shared `emit_inline_asm_common` function calls these methods to handle the
/// architecture-dependent parts of inline assembly processing.
pub trait InlineAsmEmitter {
    /// Mutable access to the codegen state (for emitting instructions).
    fn asm_state(&mut self) -> &mut CodegenState;
    /// Immutable access to the codegen state.
    fn asm_state_ref(&self) -> &CodegenState;

    /// Classify a constraint string into an AsmOperandKind, and optionally
    /// return the specific register name for Specific constraints.
    fn classify_constraint(&self, constraint: &str) -> AsmOperandKind;

    /// Set up arch-specific operand metadata after classification.
    /// Called once per operand. For memory/address operands, set mem_addr or mem_offset.
    /// For immediate operands, set imm_value. The `val` is the IR value associated
    /// with this operand (output pointer for outputs, input value for inputs).
    fn setup_operand_metadata(&self, op: &mut AsmOperand, val: &Operand, is_output: bool);

    /// Assign the next available scratch register for the given operand kind.
    /// Returns the register name. Called during Phase 1 for operands without
    /// specific register assignments.
    fn assign_scratch_reg(&mut self, kind: &AsmOperandKind) -> String;

    /// Load an input value into its assigned register. Called during Phase 2.
    fn load_input_to_reg(&mut self, op: &AsmOperand, val: &Operand, constraint: &str);

    /// Pre-load a read-write ("+") output's current value into its register.
    fn preload_readwrite_output(&mut self, op: &AsmOperand, ptr: &Value);

    /// Substitute operand references in a single template line and return the result.
    fn substitute_template_line(&self, line: &str, operands: &[AsmOperand], gcc_to_internal: &[usize], operand_types: &[IrType]) -> String;

    /// Store an output register value back to its stack slot after the asm executes.
    fn store_output_from_reg(&mut self, op: &AsmOperand, ptr: &Value, constraint: &str);

    /// Reset scratch register allocation state (called at start of each inline asm).
    fn reset_scratch_state(&mut self);
}

/// Shared inline assembly emission logic. All three backends call this from their
/// `emit_inline_asm` implementation, providing an `InlineAsmEmitter` to handle
/// arch-specific details.
///
/// The 4-phase structure is:
/// 1. Classify constraints, assign registers (specific first, then scratch), resolve ties
/// 2. Load input values into registers, pre-load read-write outputs
/// 3. Substitute operand references in template and emit
/// 4. Store output registers back to stack slots
pub fn emit_inline_asm_common(
    emitter: &mut dyn InlineAsmEmitter,
    template: &str,
    outputs: &[(String, Value, Option<String>)],
    inputs: &[(String, Operand, Option<String>)],
    operand_types: &[IrType],
) {
    emitter.reset_scratch_state();
    let total_operands = outputs.len() + inputs.len();

    // Phase 1: Classify all operands and assign registers
    let mut operands: Vec<AsmOperand> = Vec::with_capacity(total_operands);

    // Classify outputs
    for (constraint, ptr, name) in outputs {
        let kind = emitter.classify_constraint(constraint);
        let mut op = AsmOperand::new(kind, name.clone());
        // Set up specific register if classified as Specific
        if let AsmOperandKind::Specific(ref reg) = op.kind {
            op.reg = reg.clone();
        }
        let val = Operand::Value(*ptr);
        emitter.setup_operand_metadata(&mut op, &val, true);
        operands.push(op);
    }

    // Track which inputs are tied (to avoid assigning scratch regs)
    let mut input_tied_to: Vec<Option<usize>> = Vec::with_capacity(inputs.len());

    // Classify inputs
    for (constraint, val, name) in inputs {
        let kind = emitter.classify_constraint(constraint);
        let mut op = AsmOperand::new(kind.clone(), name.clone());
        if let AsmOperandKind::Specific(ref reg) = op.kind {
            op.reg = reg.clone();
        }
        if let AsmOperandKind::Tied(idx) = &kind {
            input_tied_to.push(Some(*idx));
        } else {
            input_tied_to.push(None);
        }
        emitter.setup_operand_metadata(&mut op, val, false);
        operands.push(op);
    }

    // Assign scratch registers to operands that need them (not specific, not memory,
    // not immediate, not tied)
    for i in 0..total_operands {
        if !operands[i].reg.is_empty() {
            continue;
        }
        match &operands[i].kind {
            AsmOperandKind::Memory | AsmOperandKind::Immediate => continue,
            AsmOperandKind::Tied(_) => continue,
            kind => {
                // Check if this is a tied input
                let is_tied = if i >= outputs.len() {
                    input_tied_to[i - outputs.len()].is_some()
                } else {
                    false
                };
                if !is_tied {
                    operands[i].reg = emitter.assign_scratch_reg(kind);
                }
            }
        }
    }

    // Resolve tied operands: copy register and metadata from the target operand
    for i in 0..total_operands {
        if let AsmOperandKind::Tied(tied_to) = operands[i].kind.clone() {
            if tied_to < operands.len() {
                operands[i].reg = operands[tied_to].reg.clone();
                operands[i].mem_addr = operands[tied_to].mem_addr.clone();
                operands[i].mem_offset = operands[tied_to].mem_offset;
                if matches!(operands[tied_to].kind, AsmOperandKind::Memory) {
                    operands[i].kind = AsmOperandKind::Memory;
                } else if matches!(operands[tied_to].kind, AsmOperandKind::Address) {
                    operands[i].kind = AsmOperandKind::Address;
                }
            }
        }
    }
    // Also check via input_tied_to for x86-style tied operands (not classified as Tied kind)
    for (i, tied_to) in input_tied_to.iter().enumerate() {
        if let Some(tied_to) = tied_to {
            let op_idx = outputs.len() + i;
            if *tied_to < operands.len() && operands[op_idx].reg.is_empty() {
                operands[op_idx].reg = operands[*tied_to].reg.clone();
                operands[op_idx].mem_addr = operands[*tied_to].mem_addr.clone();
                operands[op_idx].mem_offset = operands[*tied_to].mem_offset;
                if matches!(operands[*tied_to].kind, AsmOperandKind::Memory) {
                    operands[op_idx].kind = AsmOperandKind::Memory;
                }
            }
        }
    }

    // Handle "+" read-write constraints: synthetic inputs share the output's register.
    // The IR lowering adds synthetic inputs at the BEGINNING of the inputs list
    // (one per "+" output, in order).
    let mut plus_idx = 0;
    for (i, (constraint, _, _)) in outputs.iter().enumerate() {
        if constraint.contains('+') {
            let plus_input_idx = outputs.len() + plus_idx;
            if plus_input_idx < total_operands {
                operands[plus_input_idx].reg = operands[i].reg.clone();
                operands[plus_input_idx].kind = operands[i].kind.clone();
                operands[plus_input_idx].mem_addr = operands[i].mem_addr.clone();
                operands[plus_input_idx].mem_offset = operands[i].mem_offset;
            }
            plus_idx += 1;
        }
    }

    // Build GCC operand number → internal index mapping.
    // GCC numbers: outputs first, then EXPLICIT inputs (synthetic "+" inputs are hidden).
    let num_plus = outputs.iter().filter(|(c,_,_)| c.contains('+')).count();
    let num_gcc_operands = outputs.len() + (inputs.len() - num_plus);
    let mut gcc_to_internal: Vec<usize> = Vec::with_capacity(num_gcc_operands);
    for i in 0..outputs.len() {
        gcc_to_internal.push(i);
    }
    for i in num_plus..inputs.len() {
        gcc_to_internal.push(outputs.len() + i);
    }

    // Phase 2: Load input values into their assigned registers
    for (i, (constraint, val, _)) in inputs.iter().enumerate() {
        let op_idx = outputs.len() + i;
        match &operands[op_idx].kind {
            AsmOperandKind::Memory | AsmOperandKind::Immediate => continue,
            _ => {}
        }
        if operands[op_idx].reg.is_empty() {
            continue;
        }
        emitter.load_input_to_reg(&operands[op_idx], val, constraint);
    }

    // Pre-load read-write output values
    for (i, (constraint, ptr, _)) in outputs.iter().enumerate() {
        if constraint.contains('+') {
            if !matches!(operands[i].kind, AsmOperandKind::Memory) {
                emitter.preload_readwrite_output(&operands[i], ptr);
            }
        }
    }

    // Phase 3: Substitute operand references in template and emit
    let lines: Vec<&str> = template.split('\n').collect();
    for line in &lines {
        let line = line.trim().trim_start_matches('\t').trim();
        if line.is_empty() {
            continue;
        }
        let resolved = emitter.substitute_template_line(line, &operands, &gcc_to_internal, operand_types);
        emitter.asm_state().emit(&format!("    {}", resolved));
    }

    // Phase 4: Store output register values back to their stack slots
    for (i, (constraint, ptr, _)) in outputs.iter().enumerate() {
        if constraint.contains('=') || constraint.contains('+') {
            emitter.store_output_from_reg(&operands[i], ptr, constraint);
        }
    }
}
