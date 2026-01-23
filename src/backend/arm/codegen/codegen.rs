use std::collections::{HashMap, HashSet};
use crate::ir::ir::*;
use crate::common::types::IrType;

/// AArch64 code generator. Produces assembly text from IR.
pub struct ArmCodegen {
    output: String,
    stack_offset: i64,
    value_locations: HashMap<u32, i64>, // value -> stack offset from sp
    /// Track which Values are allocas (their stack slot IS the data, not a pointer to data).
    alloca_values: HashSet<u32>,
}

impl ArmCodegen {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            stack_offset: 0,
            value_locations: HashMap::new(),
            alloca_values: HashSet::new(),
        }
    }

    pub fn generate(mut self, module: &IrModule) -> String {
        // Data section for string literals
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
                self.emit(&format!("    .xword {}", label));
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
                    _ => self.emit(&format!("    .xword {}", v)),
                }
            }
            IrConst::F32(v) => {
                self.emit(&format!("    .long {}", v.to_bits()));
            }
            IrConst::F64(v) => {
                self.emit(&format!("    .xword {}", v.to_bits()));
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
        self.emit(&format!(".type {}, %function", func.name));
        self.emit(&format!("{}:", func.name));

        // Calculate stack space
        let stack_space = self.calculate_stack_space(func);
        let aligned_space = ((stack_space + 15) & !15) + 16; // +16 for fp/lr

        // Prologue: save fp, lr and allocate stack
        self.emit(&format!("    stp x29, x30, [sp, #-{}]!", aligned_space));
        self.emit("    mov x29, sp");

        // Store parameters (x0-x7 for first 8 args)
        let arg_regs = ["x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"];
        for (i, param) in func.params.iter().enumerate() {
            if i < 8 && !param.name.is_empty() {
                self.store_param(func, i, &arg_regs);
            }
        }

        // Generate code for each basic block
        for block in &func.blocks {
            if block.label != "entry" {
                self.emit(&format!("{}:", block.label));
            }

            for inst in &block.instructions {
                self.generate_instruction(inst);
            }

            self.generate_terminator(&block.terminator, aligned_space);
        }

        self.emit(&format!(".size {}, .-{}", func.name, func.name));
        self.emit("");
    }

    fn calculate_stack_space(&mut self, func: &IrFunction) -> i64 {
        let mut space: i64 = 16; // start after fp/lr save area
        for block in &func.blocks {
            for inst in &block.instructions {
                if let Some(dest) = self.get_dest(inst) {
                    let size = if let Instruction::Alloca { size, dest: alloca_dest, .. } = inst {
                        self.alloca_values.insert(alloca_dest.0);
                        ((*size as i64 + 7) & !7).max(8)
                    } else {
                        8
                    };
                    self.value_locations.insert(dest.0, space);
                    space += size;
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
            let alloca_idx = block.instructions.iter()
                .filter(|i| matches!(i, Instruction::Alloca { .. }))
                .nth(arg_idx);
            if let Some(Instruction::Alloca { dest, .. }) = alloca_idx {
                if let Some(&offset) = self.value_locations.get(&dest.0) {
                    self.emit(&format!("    str {}, [sp, #{}]", arg_regs[arg_idx], offset));
                }
            }
        }
    }

    fn generate_instruction(&mut self, inst: &Instruction) {
        match inst {
            Instruction::Alloca { .. } => {}
            Instruction::Store { val, ptr, ty } => {
                self.operand_to_x0(val);
                if let Some(&offset) = self.value_locations.get(&ptr.0) {
                    if self.alloca_values.contains(&ptr.0) {
                        // Store directly to the alloca's stack slot with type-aware size
                        let store_instr = Self::str_for_type(*ty);
                        let reg = Self::reg_for_type("x0", *ty);
                        self.emit(&format!("    {} {}, [sp, #{}]", store_instr, reg, offset));
                    } else {
                        // ptr is a computed address (e.g., from GEP).
                        // Load the pointer, then store through it.
                        self.emit("    mov x1, x0"); // save value
                        self.emit(&format!("    ldr x2, [sp, #{}]", offset)); // load pointer
                        let store_instr = Self::str_for_type(*ty);
                        let reg = Self::reg_for_type("x1", *ty);
                        self.emit(&format!("    {} {}, [x2]", store_instr, reg));
                    }
                }
            }
            Instruction::Load { dest, ptr, ty } => {
                if let Some(&ptr_off) = self.value_locations.get(&ptr.0) {
                    if self.alloca_values.contains(&ptr.0) {
                        // Load directly from the alloca's stack slot with type-aware size
                        let load_instr = Self::ldr_for_type(*ty);
                        self.emit(&format!("    {} x0, [sp, #{}]", load_instr, ptr_off));
                    } else {
                        // ptr is a computed address. Load the pointer, then deref.
                        self.emit(&format!("    ldr x0, [sp, #{}]", ptr_off)); // load pointer
                        let load_instr = Self::ldr_for_type(*ty);
                        self.emit(&format!("    {} x0, [x0]", load_instr));
                    }
                    if let Some(&dest_off) = self.value_locations.get(&dest.0) {
                        self.emit(&format!("    str x0, [sp, #{}]", dest_off));
                    }
                }
            }
            Instruction::BinOp { dest, op, lhs, rhs, .. } => {
                self.operand_to_x0(lhs);
                self.emit("    mov x1, x0");
                self.operand_to_x0(rhs);
                self.emit("    mov x2, x0");

                match op {
                    IrBinOp::Add => self.emit("    add x0, x1, x2"),
                    IrBinOp::Sub => self.emit("    sub x0, x1, x2"),
                    IrBinOp::Mul => self.emit("    mul x0, x1, x2"),
                    IrBinOp::SDiv => self.emit("    sdiv x0, x1, x2"),
                    IrBinOp::UDiv => self.emit("    udiv x0, x1, x2"),
                    IrBinOp::SRem | IrBinOp::URem => {
                        self.emit("    sdiv x3, x1, x2");
                        self.emit("    msub x0, x3, x2, x1");
                    }
                    IrBinOp::And => self.emit("    and x0, x1, x2"),
                    IrBinOp::Or => self.emit("    orr x0, x1, x2"),
                    IrBinOp::Xor => self.emit("    eor x0, x1, x2"),
                    IrBinOp::Shl => self.emit("    lsl x0, x1, x2"),
                    IrBinOp::AShr => self.emit("    asr x0, x1, x2"),
                    IrBinOp::LShr => self.emit("    lsr x0, x1, x2"),
                }

                if let Some(&offset) = self.value_locations.get(&dest.0) {
                    self.emit(&format!("    str x0, [sp, #{}]", offset));
                }
            }
            Instruction::UnaryOp { dest, op, src, .. } => {
                self.operand_to_x0(src);
                match op {
                    IrUnaryOp::Neg => self.emit("    neg x0, x0"),
                    IrUnaryOp::Not => self.emit("    mvn x0, x0"),
                }
                if let Some(&offset) = self.value_locations.get(&dest.0) {
                    self.emit(&format!("    str x0, [sp, #{}]", offset));
                }
            }
            Instruction::Cmp { dest, op, lhs, rhs, .. } => {
                self.operand_to_x0(lhs);
                self.emit("    mov x1, x0");
                self.operand_to_x0(rhs);
                self.emit("    cmp x1, x0");

                let cond = match op {
                    IrCmpOp::Eq => "eq",
                    IrCmpOp::Ne => "ne",
                    IrCmpOp::Slt => "lt",
                    IrCmpOp::Sle => "le",
                    IrCmpOp::Sgt => "gt",
                    IrCmpOp::Sge => "ge",
                    IrCmpOp::Ult => "lo",
                    IrCmpOp::Ule => "ls",
                    IrCmpOp::Ugt => "hi",
                    IrCmpOp::Uge => "hs",
                };
                self.emit(&format!("    cset x0, {}", cond));

                if let Some(&offset) = self.value_locations.get(&dest.0) {
                    self.emit(&format!("    str x0, [sp, #{}]", offset));
                }
            }
            Instruction::Call { dest, func, args, .. } => {
                let arg_regs = ["x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"];
                // Use temporary registers x9-x15 to avoid clobbering args
                let tmp_regs = ["x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16"];
                let num_args = args.len().min(8);
                // First, load all args into temp registers
                for (i, arg) in args.iter().enumerate().take(num_args) {
                    self.operand_to_x0(arg);
                    self.emit(&format!("    mov {}, x0", tmp_regs[i]));
                }
                // Then move from temps to arg registers
                for i in 0..num_args {
                    self.emit(&format!("    mov {}, {}", arg_regs[i], tmp_regs[i]));
                }
                // TODO: stack args for > 8 args
                self.emit(&format!("    bl {}", func));
                if let Some(dest) = dest {
                    if let Some(&offset) = self.value_locations.get(&dest.0) {
                        self.emit(&format!("    str x0, [sp, #{}]", offset));
                    }
                }
            }
            Instruction::GlobalAddr { dest, name } => {
                self.emit(&format!("    adrp x0, {}", name));
                self.emit(&format!("    add x0, x0, :lo12:{}", name));
                if let Some(&offset) = self.value_locations.get(&dest.0) {
                    self.emit(&format!("    str x0, [sp, #{}]", offset));
                }
            }
            Instruction::Copy { dest, src } => {
                self.operand_to_x0(src);
                if let Some(&offset) = self.value_locations.get(&dest.0) {
                    self.emit(&format!("    str x0, [sp, #{}]", offset));
                }
            }
            Instruction::Cast { dest, src, .. } => {
                self.operand_to_x0(src);
                if let Some(&offset) = self.value_locations.get(&dest.0) {
                    self.emit(&format!("    str x0, [sp, #{}]", offset));
                }
            }
            Instruction::GetElementPtr { dest, base, offset, .. } => {
                // For alloca values, use ADD to compute address from SP.
                // For non-alloca values (loaded pointers), load the pointer first.
                if let Some(&base_off) = self.value_locations.get(&base.0) {
                    if self.alloca_values.contains(&base.0) {
                        // Alloca: compute address as sp + offset
                        self.emit(&format!("    add x1, sp, #{}", base_off));
                    } else {
                        // Non-alloca: load the pointer value
                        self.emit(&format!("    ldr x1, [sp, #{}]", base_off));
                    }
                }
                self.operand_to_x0(offset);
                self.emit("    add x0, x1, x0");
                if let Some(&dest_off) = self.value_locations.get(&dest.0) {
                    self.emit(&format!("    str x0, [sp, #{}]", dest_off));
                }
            }
        }
    }

    fn generate_terminator(&mut self, term: &Terminator, frame_size: i64) {
        match term {
            Terminator::Return(val) => {
                if let Some(val) = val {
                    self.operand_to_x0(val);
                }
                // Epilogue: restore fp/lr and deallocate stack
                self.emit(&format!("    ldp x29, x30, [sp], #{}", frame_size));
                self.emit("    ret");
            }
            Terminator::Branch(label) => {
                self.emit(&format!("    b {}", label));
            }
            Terminator::CondBranch { cond, true_label, false_label } => {
                self.operand_to_x0(cond);
                self.emit(&format!("    cbnz x0, {}", true_label));
                self.emit(&format!("    b {}", false_label));
            }
            Terminator::Unreachable => {
                self.emit("    brk #0");
            }
        }
    }

    /// Get the store instruction for a given type.
    fn str_for_type(ty: IrType) -> &'static str {
        match ty {
            IrType::I8 => "strb",
            IrType::I16 => "strh",
            IrType::I32 => "str",   // str w-reg
            IrType::I64 | IrType::Ptr => "str",   // str x-reg
            _ => "str",
        }
    }

    /// Get the load instruction for a given type (with sign extension).
    fn ldr_for_type(ty: IrType) -> &'static str {
        match ty {
            IrType::I8 => "ldrsb",   // sign-extend byte
            IrType::I16 => "ldrsh",  // sign-extend halfword
            IrType::I32 => "ldrsw",  // sign-extend word to 64-bit
            IrType::I64 | IrType::Ptr => "ldr",
            _ => "ldr",
        }
    }

    /// Get the appropriate register name for a given base and type.
    fn reg_for_type(base: &str, ty: IrType) -> &'static str {
        // On AArch64, w0 is the 32-bit alias of x0, etc.
        // For byte/halfword stores, we still use w-reg (strb/strh use 32-bit reg)
        match base {
            "x0" => match ty {
                IrType::I8 | IrType::I16 | IrType::I32 => "w0",
                _ => "x0",
            },
            "x1" => match ty {
                IrType::I8 | IrType::I16 | IrType::I32 => "w1",
                _ => "x1",
            },
            _ => "x0",
        }
    }

    fn operand_to_x0(&mut self, op: &Operand) {
        match op {
            Operand::Const(c) => {
                match c {
                    IrConst::I8(v) => self.emit(&format!("    mov x0, #{}", v)),
                    IrConst::I16(v) => self.emit(&format!("    mov x0, #{}", v)),
                    IrConst::I32(v) => {
                        if *v >= 0 && *v <= 65535 {
                            self.emit(&format!("    mov x0, #{}", v));
                        } else if *v < 0 && *v >= -65536 {
                            self.emit(&format!("    mov x0, #{}", v));
                        } else {
                            self.emit(&format!("    mov x0, #{}", *v as u32 & 0xffff));
                            self.emit(&format!("    movk x0, #{}, lsl #16", (*v as u32 >> 16) & 0xffff));
                        }
                    }
                    IrConst::I64(v) => {
                        if *v >= 0 && *v <= 65535 {
                            self.emit(&format!("    mov x0, #{}", v));
                        } else if *v < 0 && *v >= -65536 {
                            self.emit(&format!("    mov x0, #{}", v));
                        } else {
                            // Load 64-bit constant
                            self.emit(&format!("    mov x0, #{}", *v as u64 & 0xffff));
                            if (*v as u64 >> 16) & 0xffff != 0 {
                                self.emit(&format!("    movk x0, #{}, lsl #16", (*v as u64 >> 16) & 0xffff));
                            }
                            if (*v as u64 >> 32) & 0xffff != 0 {
                                self.emit(&format!("    movk x0, #{}, lsl #32", (*v as u64 >> 32) & 0xffff));
                            }
                            if (*v as u64 >> 48) & 0xffff != 0 {
                                self.emit(&format!("    movk x0, #{}, lsl #48", (*v as u64 >> 48) & 0xffff));
                            }
                        }
                    }
                    IrConst::F32(_) | IrConst::F64(_) => {
                        self.emit("    mov x0, #0");
                    }
                    IrConst::Zero => self.emit("    mov x0, #0"),
                }
            }
            Operand::Value(v) => {
                if let Some(&offset) = self.value_locations.get(&v.0) {
                    self.emit(&format!("    ldr x0, [sp, #{}]", offset));
                } else {
                    self.emit("    mov x0, #0");
                }
            }
        }
    }
}

impl Default for ArmCodegen {
    fn default() -> Self {
        Self::new()
    }
}
