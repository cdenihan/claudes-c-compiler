use std::collections::{HashMap, HashSet};
use crate::ir::ir::*;
use crate::common::types::IrType;

/// RISC-V 64 code generator. Produces assembly text from IR.
pub struct RiscvCodegen {
    output: String,
    stack_offset: i64,
    value_locations: HashMap<u32, i64>,
    /// Track which Values are allocas (their stack slot IS the data, not a pointer to data).
    alloca_values: HashSet<u32>,
}

impl RiscvCodegen {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            stack_offset: 0,
            value_locations: HashMap::new(),
            alloca_values: HashSet::new(),
        }
    }

    pub fn generate(mut self, module: &IrModule) -> String {
        if !module.string_literals.is_empty() {
            self.emit(".section .rodata");
            for (label, value) in &module.string_literals {
                self.emit(&format!("{}:", label));
                self.emit_string_data(value);
            }
            self.emit("");
        }

        // Global variables
        self.emit_globals(&module.globals);

        self.emit(".section .text");

        for func in &module.functions {
            if func.is_declaration {
                continue;
            }
            self.generate_function(func);
        }

        self.output
    }

    fn emit_globals(&mut self, globals: &[crate::ir::ir::IrGlobal]) {
        use crate::ir::ir::{GlobalInit, IrGlobal};

        let mut has_data = false;
        let mut has_bss = false;

        // Emit initialized globals in .data section
        for g in globals {
            if matches!(g.init, GlobalInit::Zero) {
                continue;
            }
            if !has_data {
                self.emit(".section .data");
                has_data = true;
            }
            self.emit_global_def(g);
        }

        if has_data {
            self.emit("");
        }

        // Emit zero-initialized globals in .bss section
        for g in globals {
            if !matches!(g.init, GlobalInit::Zero) {
                continue;
            }
            if !has_bss {
                self.emit(".section .bss");
                has_bss = true;
            }
            if !g.is_static {
                self.emit(&format!(".globl {}", g.name));
            }
            self.emit(&format!(".align {}", g.align));
            self.emit(&format!(".type {}, @object", g.name));
            self.emit(&format!(".size {}, {}", g.name, g.size));
            self.emit(&format!("{}:", g.name));
            self.emit(&format!("    .zero {}", g.size));
        }

        if has_bss {
            self.emit("");
        }
    }

    fn emit_global_def(&mut self, g: &crate::ir::ir::IrGlobal) {
        use crate::ir::ir::GlobalInit;

        if !g.is_static {
            self.emit(&format!(".globl {}", g.name));
        }
        self.emit(&format!(".align {}", g.align));
        self.emit(&format!(".type {}, @object", g.name));
        self.emit(&format!(".size {}, {}", g.name, g.size));
        self.emit(&format!("{}:", g.name));

        match &g.init {
            GlobalInit::Zero => {
                self.emit(&format!("    .zero {}", g.size));
            }
            GlobalInit::Scalar(c) => {
                self.emit_const_data(c, g.ty);
            }
            GlobalInit::Array(values) => {
                for val in values {
                    self.emit_const_data(val, g.ty);
                }
            }
            GlobalInit::String(s) => {
                self.emit(&format!("    .asciz \"{}\"", Self::escape_string(s)));
            }
            GlobalInit::GlobalAddr(label) => {
                self.emit(&format!("    .dword {}", label));
            }
        }
    }

    fn emit_const_data(&mut self, c: &IrConst, ty: IrType) {
        match c {
            IrConst::I8(v) => self.emit(&format!("    .byte {}", v)),
            IrConst::I16(v) => self.emit(&format!("    .short {}", v)),
            IrConst::I32(v) => self.emit(&format!("    .long {}", v)),
            IrConst::I64(v) => {
                match ty {
                    IrType::I8 => self.emit(&format!("    .byte {}", *v as i8)),
                    IrType::I16 => self.emit(&format!("    .short {}", *v as i16)),
                    IrType::I32 => self.emit(&format!("    .long {}", *v as i32)),
                    _ => self.emit(&format!("    .dword {}", v)),
                }
            }
            IrConst::F32(v) => {
                self.emit(&format!("    .long {}", v.to_bits()));
            }
            IrConst::F64(v) => {
                self.emit(&format!("    .dword {}", v.to_bits()));
            }
            IrConst::Zero => {
                let size = ty.size();
                self.emit(&format!("    .zero {}", if size == 0 { 4 } else { size }));
            }
        }
    }

    fn escape_string(s: &str) -> String {
        let mut result = String::new();
        for c in s.chars() {
            match c {
                '\\' => result.push_str("\\\\"),
                '"' => result.push_str("\\\""),
                '\n' => result.push_str("\\n"),
                '\t' => result.push_str("\\t"),
                '\r' => result.push_str("\\r"),
                '\0' => result.push_str("\\0"),
                c if c.is_ascii_graphic() || c == ' ' => result.push(c),
                c => {
                    for b in c.to_string().bytes() {
                        result.push_str(&format!("\\{:03o}", b));
                    }
                }
            }
        }
        result
    }

    fn emit(&mut self, s: &str) {
        self.output.push_str(s);
        self.output.push('\n');
    }

    fn emit_string_data(&mut self, s: &str) {
        self.output.push_str("    .byte ");
        let bytes: Vec<String> = s.bytes()
            .chain(std::iter::once(0u8))
            .map(|b| format!("{}", b))
            .collect();
        self.output.push_str(&bytes.join(", "));
        self.output.push('\n');
    }

    fn generate_function(&mut self, func: &IrFunction) {
        self.stack_offset = 0;
        self.value_locations.clear();
        self.alloca_values.clear();

        self.emit(&format!(".globl {}", func.name));
        self.emit(&format!(".type {}, @function", func.name));
        self.emit(&format!("{}:", func.name));

        // Calculate stack space
        let stack_space = self.calculate_stack_space(func);
        let frame_size = ((stack_space + 15) & !15) + 16; // +16 for ra/s0

        // Prologue
        self.emit(&format!("    addi sp, sp, -{}", frame_size));
        self.emit(&format!("    sd ra, {}(sp)", frame_size - 8));
        self.emit(&format!("    sd s0, {}(sp)", frame_size - 16));
        self.emit(&format!("    addi s0, sp, {}", frame_size));

        // Store parameters (a0-a7)
        let arg_regs = ["a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"];
        for (i, param) in func.params.iter().enumerate() {
            if i < 8 && !param.name.is_empty() {
                self.store_param(func, i, &arg_regs);
            }
        }

        // Generate blocks
        for block in &func.blocks {
            if block.label != "entry" {
                self.emit(&format!("{}:", block.label));
            }

            for inst in &block.instructions {
                self.generate_instruction(inst);
            }

            self.generate_terminator(&block.terminator, frame_size);
        }

        self.emit(&format!(".size {}, .-{}", func.name, func.name));
        self.emit("");
    }

    fn calculate_stack_space(&mut self, func: &IrFunction) -> i64 {
        // Start at -16 to skip past saved ra (-8) and s0 (-16) from s0
        let mut space: i64 = 16;
        for block in &func.blocks {
            for inst in &block.instructions {
                if let Some(dest) = self.get_dest(inst) {
                    let size = if let Instruction::Alloca { size, dest: alloca_dest, .. } = inst {
                        self.alloca_values.insert(alloca_dest.0);
                        ((*size as i64 + 7) & !7).max(8)
                    } else {
                        8
                    };
                    space += size;
                    self.value_locations.insert(dest.0, -(space as i64));
                }
            }
        }
        space
    }

    fn get_dest(&self, inst: &Instruction) -> Option<Value> {
        match inst {
            Instruction::Alloca { dest, .. } | Instruction::Load { dest, .. }
            | Instruction::BinOp { dest, .. } | Instruction::UnaryOp { dest, .. }
            | Instruction::Cmp { dest, .. } | Instruction::GetElementPtr { dest, .. }
            | Instruction::Cast { dest, .. } | Instruction::Copy { dest, .. }
            | Instruction::GlobalAddr { dest, .. } => Some(*dest),
            Instruction::Call { dest, .. } => *dest,
            Instruction::Store { .. } => None,
        }
    }

    fn store_param(&mut self, func: &IrFunction, arg_idx: usize, arg_regs: &[&str]) {
        if let Some(block) = func.blocks.first() {
            let alloca = block.instructions.iter()
                .filter(|i| matches!(i, Instruction::Alloca { .. }))
                .nth(arg_idx);
            if let Some(Instruction::Alloca { dest, .. }) = alloca {
                if let Some(&offset) = self.value_locations.get(&dest.0) {
                    self.emit(&format!("    sd {}, {}(s0)", arg_regs[arg_idx], offset));
                }
            }
        }
    }

    fn generate_instruction(&mut self, inst: &Instruction) {
        match inst {
            Instruction::Alloca { .. } => {}
            Instruction::Store { val, ptr, ty } => {
                self.operand_to_t0(val);
                if let Some(&offset) = self.value_locations.get(&ptr.0) {
                    if self.alloca_values.contains(&ptr.0) {
                        // Store directly to the alloca's stack slot with type-aware size
                        let store_instr = Self::store_for_type(*ty);
                        self.emit(&format!("    {} t0, {}(s0)", store_instr, offset));
                    } else {
                        // ptr is a computed address (e.g., from GEP).
                        // Load the pointer, then store through it.
                        self.emit("    mv t3, t0"); // save value
                        self.emit(&format!("    ld t4, {}(s0)", offset)); // load pointer
                        let store_instr = Self::store_for_type(*ty);
                        self.emit(&format!("    {} t3, 0(t4)", store_instr));
                    }
                }
            }
            Instruction::Load { dest, ptr, ty } => {
                if let Some(&ptr_off) = self.value_locations.get(&ptr.0) {
                    if self.alloca_values.contains(&ptr.0) {
                        // Load directly from the alloca's stack slot with type-aware size
                        let load_instr = Self::load_for_type(*ty);
                        self.emit(&format!("    {} t0, {}(s0)", load_instr, ptr_off));
                    } else {
                        // ptr is a computed address. Load the pointer, then deref.
                        self.emit(&format!("    ld t0, {}(s0)", ptr_off)); // load pointer
                        let load_instr = Self::load_for_type(*ty);
                        self.emit(&format!("    {} t0, 0(t0)", load_instr));
                    }
                    if let Some(&dest_off) = self.value_locations.get(&dest.0) {
                        self.emit(&format!("    sd t0, {}(s0)", dest_off));
                    }
                }
            }
            Instruction::BinOp { dest, op, lhs, rhs, .. } => {
                self.operand_to_t0(lhs);
                self.emit("    mv t1, t0");
                self.operand_to_t0(rhs);
                self.emit("    mv t2, t0");

                match op {
                    IrBinOp::Add => self.emit("    add t0, t1, t2"),
                    IrBinOp::Sub => self.emit("    sub t0, t1, t2"),
                    IrBinOp::Mul => self.emit("    mul t0, t1, t2"),
                    IrBinOp::SDiv => self.emit("    div t0, t1, t2"),
                    IrBinOp::UDiv => self.emit("    divu t0, t1, t2"),
                    IrBinOp::SRem => self.emit("    rem t0, t1, t2"),
                    IrBinOp::URem => self.emit("    remu t0, t1, t2"),
                    IrBinOp::And => self.emit("    and t0, t1, t2"),
                    IrBinOp::Or => self.emit("    or t0, t1, t2"),
                    IrBinOp::Xor => self.emit("    xor t0, t1, t2"),
                    IrBinOp::Shl => self.emit("    sll t0, t1, t2"),
                    IrBinOp::AShr => self.emit("    sra t0, t1, t2"),
                    IrBinOp::LShr => self.emit("    srl t0, t1, t2"),
                }

                if let Some(&offset) = self.value_locations.get(&dest.0) {
                    self.emit(&format!("    sd t0, {}(s0)", offset));
                }
            }
            Instruction::UnaryOp { dest, op, src, .. } => {
                self.operand_to_t0(src);
                match op {
                    IrUnaryOp::Neg => self.emit("    neg t0, t0"),
                    IrUnaryOp::Not => self.emit("    not t0, t0"),
                }
                if let Some(&offset) = self.value_locations.get(&dest.0) {
                    self.emit(&format!("    sd t0, {}(s0)", offset));
                }
            }
            Instruction::Cmp { dest, op, lhs, rhs, .. } => {
                self.operand_to_t0(lhs);
                self.emit("    mv t1, t0");
                self.operand_to_t0(rhs);
                self.emit("    mv t2, t0");

                match op {
                    IrCmpOp::Eq => {
                        self.emit("    sub t0, t1, t2");
                        self.emit("    seqz t0, t0");
                    }
                    IrCmpOp::Ne => {
                        self.emit("    sub t0, t1, t2");
                        self.emit("    snez t0, t0");
                    }
                    IrCmpOp::Slt | IrCmpOp::Ult => self.emit("    slt t0, t1, t2"),
                    IrCmpOp::Sge | IrCmpOp::Uge => {
                        self.emit("    slt t0, t1, t2");
                        self.emit("    xori t0, t0, 1");
                    }
                    IrCmpOp::Sgt | IrCmpOp::Ugt => self.emit("    slt t0, t2, t1"),
                    IrCmpOp::Sle | IrCmpOp::Ule => {
                        self.emit("    slt t0, t2, t1");
                        self.emit("    xori t0, t0, 1");
                    }
                }

                if let Some(&offset) = self.value_locations.get(&dest.0) {
                    self.emit(&format!("    sd t0, {}(s0)", offset));
                }
            }
            Instruction::Call { dest, func, args, .. } => {
                let arg_regs = ["a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"];
                for (i, arg) in args.iter().enumerate() {
                    if i < 8 {
                        self.operand_to_t0(arg);
                        self.emit(&format!("    mv {}, t0", arg_regs[i]));
                    }
                }
                self.emit(&format!("    call {}", func));
                if let Some(dest) = dest {
                    if let Some(&offset) = self.value_locations.get(&dest.0) {
                        self.emit(&format!("    sd a0, {}(s0)", offset));
                    }
                }
            }
            Instruction::GlobalAddr { dest, name } => {
                self.emit(&format!("    la t0, {}", name));
                if let Some(&offset) = self.value_locations.get(&dest.0) {
                    self.emit(&format!("    sd t0, {}(s0)", offset));
                }
            }
            Instruction::Copy { dest, src } => {
                self.operand_to_t0(src);
                if let Some(&offset) = self.value_locations.get(&dest.0) {
                    self.emit(&format!("    sd t0, {}(s0)", offset));
                }
            }
            Instruction::Cast { dest, src, .. } => {
                self.operand_to_t0(src);
                if let Some(&offset) = self.value_locations.get(&dest.0) {
                    self.emit(&format!("    sd t0, {}(s0)", offset));
                }
            }
            Instruction::GetElementPtr { dest, base, offset, .. } => {
                // For alloca values, use ADDI to compute address from s0.
                // For non-alloca values (loaded pointers), load the pointer first.
                if let Some(&base_off) = self.value_locations.get(&base.0) {
                    if self.alloca_values.contains(&base.0) {
                        // Alloca: compute address as s0 + base_off
                        self.emit(&format!("    addi t1, s0, {}", base_off));
                    } else {
                        // Non-alloca: load the pointer value
                        self.emit(&format!("    ld t1, {}(s0)", base_off));
                    }
                }
                self.operand_to_t0(offset);
                self.emit("    add t0, t1, t0");
                if let Some(&dest_off) = self.value_locations.get(&dest.0) {
                    self.emit(&format!("    sd t0, {}(s0)", dest_off));
                }
            }
        }
    }

    fn generate_terminator(&mut self, term: &Terminator, frame_size: i64) {
        match term {
            Terminator::Return(val) => {
                if let Some(val) = val {
                    self.operand_to_t0(val);
                    self.emit("    mv a0, t0");
                }
                // Epilogue
                self.emit(&format!("    ld ra, {}(sp)", frame_size - 8));
                self.emit(&format!("    ld s0, {}(sp)", frame_size - 16));
                self.emit(&format!("    addi sp, sp, {}", frame_size));
                self.emit("    ret");
            }
            Terminator::Branch(label) => {
                self.emit(&format!("    j {}", label));
            }
            Terminator::CondBranch { cond, true_label, false_label } => {
                self.operand_to_t0(cond);
                self.emit(&format!("    bnez t0, {}", true_label));
                self.emit(&format!("    j {}", false_label));
            }
            Terminator::Unreachable => {
                self.emit("    ebreak");
            }
        }
    }

    /// Get the store instruction for a given type (RISC-V).
    fn store_for_type(ty: IrType) -> &'static str {
        match ty {
            IrType::I8 => "sb",
            IrType::I16 => "sh",
            IrType::I32 => "sw",
            IrType::I64 | IrType::Ptr => "sd",
            _ => "sd",
        }
    }

    /// Get the load instruction for a given type (with sign extension, RISC-V).
    fn load_for_type(ty: IrType) -> &'static str {
        match ty {
            IrType::I8 => "lb",     // sign-extend byte
            IrType::I16 => "lh",    // sign-extend halfword
            IrType::I32 => "lw",    // sign-extend word to 64-bit
            IrType::I64 | IrType::Ptr => "ld",
            _ => "ld",
        }
    }

    fn operand_to_t0(&mut self, op: &Operand) {
        match op {
            Operand::Const(c) => {
                match c {
                    IrConst::I8(v) => self.emit(&format!("    li t0, {}", v)),
                    IrConst::I16(v) => self.emit(&format!("    li t0, {}", v)),
                    IrConst::I32(v) => self.emit(&format!("    li t0, {}", v)),
                    IrConst::I64(v) => self.emit(&format!("    li t0, {}", v)),
                    IrConst::F32(_) | IrConst::F64(_) => self.emit("    li t0, 0"),
                    IrConst::Zero => self.emit("    li t0, 0"),
                }
            }
            Operand::Value(v) => {
                if let Some(&offset) = self.value_locations.get(&v.0) {
                    self.emit(&format!("    ld t0, {}(s0)", offset));
                } else {
                    self.emit("    li t0, 0");
                }
            }
        }
    }
}

impl Default for RiscvCodegen {
    fn default() -> Self {
        Self::new()
    }
}
