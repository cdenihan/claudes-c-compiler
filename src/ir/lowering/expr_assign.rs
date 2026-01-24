//! Assignment and compound assignment lowering, including bitfield operations.
//!
//! Extracted from expr.rs. This module handles:
//! - `lower_assign`: simple assignment (lhs = rhs)
//! - `lower_struct_assign`: struct/union copy via memcpy
//! - `lower_compound_assign`: compound assignment (+=, -=, *=, etc.)
//! - Bitfield helpers: resolve_bitfield_lvalue, store_bitfield, extract_bitfield
//! - Arithmetic conversion helpers: usual_arithmetic_conversions, promote_for_op, narrow_from_op

use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use crate::common::types::IrType;
use super::lowering::Lowerer;

impl Lowerer {
    pub(super) fn lower_assign(&mut self, lhs: &Expr, rhs: &Expr) -> Operand {
        // Complex assignment: copy real/imag parts
        let lhs_ct = self.expr_ctype(lhs);
        if lhs_ct.is_complex() {
            return self.lower_complex_assign(lhs, rhs, &lhs_ct);
        }

        if self.struct_value_size(lhs).is_some() {
            return self.lower_struct_assign(lhs, rhs);
        }

        // Check for bitfield assignment
        if let Some(result) = self.try_lower_bitfield_assign(lhs, rhs) {
            return result;
        }

        // When assigning a complex RHS to a non-complex LHS, extract the real part first
        let rhs_ct = self.expr_ctype(rhs);
        let lhs_ty = self.get_expr_type(lhs);
        let rhs_val = if rhs_ct.is_complex() && !lhs_ct.is_complex() {
            let complex_val = self.lower_expr(rhs);
            let ptr = self.operand_to_value(complex_val);
            let real_part = self.load_complex_real(ptr, &rhs_ct);
            let from_ty = Self::complex_component_ir_type(&rhs_ct);
            self.emit_implicit_cast(real_part, from_ty, lhs_ty)
        } else {
            let rhs_val = self.lower_expr(rhs);
            let rhs_ty = self.get_expr_type(rhs);
            self.emit_implicit_cast(rhs_val, rhs_ty, lhs_ty)
        };

        // _Bool variables clamp any value to 0 or 1
        let rhs_val = if self.is_bool_lvalue(lhs) {
            self.emit_bool_normalize(rhs_val)
        } else {
            rhs_val
        };

        if let Some(lv) = self.lower_lvalue(lhs) {
            self.store_lvalue_typed(&lv, rhs_val.clone(), lhs_ty);
            return rhs_val;
        }
        rhs_val
    }

    /// Lower struct/union assignment using memcpy.
    fn lower_struct_assign(&mut self, lhs: &Expr, rhs: &Expr) -> Operand {
        let struct_size = self.struct_value_size(lhs).unwrap_or(8);

        // For expressions producing packed struct data (small struct function
        // call returns, ternaries over them, etc.), store directly
        if self.expr_produces_packed_struct_data(rhs) && struct_size <= 8 {
            let rhs_val = self.lower_expr(rhs);
            if let Some(lv) = self.lower_lvalue(lhs) {
                let dest_addr = self.lvalue_addr(&lv);
                self.emit(Instruction::Store { val: rhs_val, ptr: dest_addr, ty: IrType::I64 });
                return Operand::Value(dest_addr);
            }
            return rhs_val;
        }

        let rhs_val = self.lower_expr(rhs);
        if let Some(lv) = self.lower_lvalue(lhs) {
            let dest_addr = self.lvalue_addr(&lv);
            let src_addr = self.operand_to_value(rhs_val);
            self.emit(Instruction::Memcpy { dest: dest_addr, src: src_addr, size: struct_size });
            return Operand::Value(dest_addr);
        }
        rhs_val
    }

    // -----------------------------------------------------------------------
    // Bitfield operations
    // -----------------------------------------------------------------------

    /// Resolve a bitfield member access expression, returning the field's address and
    /// bitfield metadata. Returns None if the expression is not a bitfield access.
    pub(super) fn resolve_bitfield_lvalue(&mut self, expr: &Expr) -> Option<(Value, IrType, u32, u32)> {
        let (base_expr, field_name, is_pointer) = match expr {
            Expr::MemberAccess(base, field, _) => (base.as_ref(), field.as_str(), false),
            Expr::PointerMemberAccess(base, field, _) => (base.as_ref(), field.as_str(), true),
            _ => return None,
        };

        let (field_offset, storage_ty, bitfield) = if is_pointer {
            self.resolve_pointer_member_access_full(base_expr, field_name)
        } else {
            self.resolve_member_access_full(base_expr, field_name)
        };

        let (bit_offset, bit_width) = bitfield?;

        let base_addr = if is_pointer {
            let ptr_val = self.lower_expr(base_expr);
            self.operand_to_value(ptr_val)
        } else {
            self.get_struct_base_addr(base_expr)
        };

        let field_addr = self.fresh_value();
        self.emit(Instruction::GetElementPtr {
            dest: field_addr, base: base_addr,
            offset: Operand::Const(IrConst::I64(field_offset as i64)),
            ty: storage_ty,
        });

        Some((field_addr, storage_ty, bit_offset, bit_width))
    }

    /// Truncate a value to bit_width bits and sign-extend if the bitfield is signed.
    pub(super) fn truncate_to_bitfield_value(&mut self, val: Operand, bit_width: u32, is_signed: bool) -> Operand {
        if bit_width >= 64 {
            return val;
        }
        if is_signed {
            let shl_amount = 64 - bit_width;
            let shifted = self.emit_binop_val(IrBinOp::Shl, val, Operand::Const(IrConst::I64(shl_amount as i64)), IrType::I64);
            let result = self.emit_binop_val(IrBinOp::AShr, Operand::Value(shifted), Operand::Const(IrConst::I64(shl_amount as i64)), IrType::I64);
            Operand::Value(result)
        } else {
            let mask = (1u64 << bit_width) - 1;
            let masked = self.emit_binop_val(IrBinOp::And, val, Operand::Const(IrConst::I64(mask as i64)), IrType::I64);
            Operand::Value(masked)
        }
    }

    /// Try to lower assignment to a bitfield member.
    fn try_lower_bitfield_assign(&mut self, lhs: &Expr, rhs: &Expr) -> Option<Operand> {
        let (field_addr, storage_ty, bit_offset, bit_width) = self.resolve_bitfield_lvalue(lhs)?;
        let rhs_val = self.lower_expr(rhs);
        self.store_bitfield(field_addr, storage_ty, bit_offset, bit_width, rhs_val.clone());
        Some(self.truncate_to_bitfield_value(rhs_val, bit_width, storage_ty.is_signed()))
    }

    /// Try to lower compound assignment to a bitfield member (e.g., s.bf += val).
    pub(super) fn try_lower_bitfield_compound_assign(&mut self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> Option<Operand> {
        let (field_addr, storage_ty, bit_offset, bit_width) = self.resolve_bitfield_lvalue(lhs)?;

        let loaded = self.fresh_value();
        self.emit(Instruction::Load { dest: loaded, ptr: field_addr, ty: storage_ty });
        let current_val = self.extract_bitfield(loaded, storage_ty, bit_offset, bit_width);

        let rhs_val = self.lower_expr(rhs);

        let is_unsigned = storage_ty.is_unsigned();
        let ir_op = Self::binop_to_ir(op.clone(), is_unsigned);
        let result = self.emit_binop_val(ir_op, current_val, rhs_val, IrType::I64);

        self.store_bitfield(field_addr, storage_ty, bit_offset, bit_width, Operand::Value(result));
        Some(self.truncate_to_bitfield_value(Operand::Value(result), bit_width, storage_ty.is_signed()))
    }

    /// Store a value into a bitfield: load storage unit, clear field bits, OR in new value, store back.
    pub(super) fn store_bitfield(&mut self, addr: Value, storage_ty: IrType, bit_offset: u32, bit_width: u32, val: Operand) {
        if bit_width >= 64 && bit_offset == 0 {
            self.emit(Instruction::Store { val, ptr: addr, ty: storage_ty });
            return;
        }

        let mask = if bit_width >= 64 { u64::MAX } else { (1u64 << bit_width) - 1 };
        let masked_val = self.emit_binop_val(IrBinOp::And, val, Operand::Const(IrConst::I64(mask as i64)), IrType::I64);

        let shifted_val = if bit_offset > 0 {
            self.emit_binop_val(IrBinOp::Shl, Operand::Value(masked_val), Operand::Const(IrConst::I64(bit_offset as i64)), IrType::I64)
        } else {
            masked_val
        };

        let old_val = self.fresh_value();
        self.emit(Instruction::Load { dest: old_val, ptr: addr, ty: storage_ty });

        let clear_mask = if bit_width >= 64 { 0u64 } else { !(mask << bit_offset) };
        let cleared = self.emit_binop_val(IrBinOp::And, Operand::Value(old_val), Operand::Const(IrConst::I64(clear_mask as i64)), IrType::I64);
        let new_val = self.emit_binop_val(IrBinOp::Or, Operand::Value(cleared), Operand::Value(shifted_val), IrType::I64);

        self.emit(Instruction::Store { val: Operand::Value(new_val), ptr: addr, ty: storage_ty });
    }

    /// Extract a bitfield value from a loaded storage unit.
    pub(super) fn extract_bitfield(&mut self, loaded: Value, storage_ty: IrType, bit_offset: u32, bit_width: u32) -> Operand {
        if bit_width >= 64 && bit_offset == 0 {
            return Operand::Value(loaded);
        }

        if storage_ty.is_signed() {
            let shl_amount = 64 - bit_offset - bit_width;
            let ashr_amount = 64 - bit_width;
            let mut val = Operand::Value(loaded);
            if shl_amount > 0 {
                let shifted = self.emit_binop_val(IrBinOp::Shl, val, Operand::Const(IrConst::I64(shl_amount as i64)), IrType::I64);
                val = Operand::Value(shifted);
            }
            if ashr_amount > 0 {
                let result = self.emit_binop_val(IrBinOp::AShr, val, Operand::Const(IrConst::I64(ashr_amount as i64)), IrType::I64);
                Operand::Value(result)
            } else {
                val
            }
        } else {
            let mut val = Operand::Value(loaded);
            if bit_offset > 0 {
                let shifted = self.emit_binop_val(IrBinOp::LShr, val, Operand::Const(IrConst::I64(bit_offset as i64)), IrType::I64);
                val = Operand::Value(shifted);
            }
            if bit_width >= 64 {
                val
            } else {
                let mask = (1u64 << bit_width) - 1;
                let masked = self.emit_binop_val(IrBinOp::And, val, Operand::Const(IrConst::I64(mask as i64)), IrType::I64);
                Operand::Value(masked)
            }
        }
    }

    // -----------------------------------------------------------------------
    // Compound assignment
    // -----------------------------------------------------------------------

    pub(super) fn lower_compound_assign(&mut self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> Operand {
        // Complex compound assignment
        let lhs_ct = self.expr_ctype(lhs);
        if lhs_ct.is_complex() && matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div) {
            return self.lower_complex_compound_assign(op, lhs, rhs, &lhs_ct);
        }

        // Non-complex LHS but complex RHS: extract real part
        let rhs_ct = self.expr_ctype(rhs);
        if !lhs_ct.is_complex() && rhs_ct.is_complex() {
            return self.lower_scalar_compound_assign_complex_rhs(op, lhs, rhs, &rhs_ct);
        }

        // Check for bitfield compound assignment
        if let Some(result) = self.try_lower_bitfield_compound_assign(op, lhs, rhs) {
            return result;
        }

        // Standard scalar compound assignment
        self.lower_scalar_compound_assign(op, lhs, rhs)
    }

    /// Complex compound assignment (z += w, z -= w, z *= w, z /= w).
    fn lower_complex_compound_assign(&mut self, op: &BinOp, lhs: &Expr, rhs: &Expr, lhs_ct: &crate::common::types::CType) -> Operand {
        let rhs_ct = self.expr_ctype(rhs);
        let result_ct = self.common_complex_type(lhs_ct, &rhs_ct);

        let lhs_ptr = self.lower_complex_lvalue(lhs);

        let op_lhs_ptr = if result_ct != *lhs_ct {
            let lhs_converted = self.convert_to_complex(Operand::Value(lhs_ptr), lhs_ct, &result_ct);
            self.operand_to_value(lhs_converted)
        } else {
            lhs_ptr
        };

        let rhs_val = self.lower_expr(rhs);
        let rhs_complex = self.convert_to_complex(rhs_val, &rhs_ct, &result_ct);
        let rhs_ptr = self.operand_to_value(rhs_complex);

        let result = match op {
            BinOp::Add => self.lower_complex_add(op_lhs_ptr, rhs_ptr, &result_ct),
            BinOp::Sub => self.lower_complex_sub(op_lhs_ptr, rhs_ptr, &result_ct),
            BinOp::Mul => self.lower_complex_mul(op_lhs_ptr, rhs_ptr, &result_ct),
            BinOp::Div => self.lower_complex_div(op_lhs_ptr, rhs_ptr, &result_ct),
            _ => unreachable!(),
        };

        let store_ptr = if result_ct != *lhs_ct {
            let result_ptr = self.operand_to_value(result);
            let converted_back = self.convert_to_complex(Operand::Value(result_ptr), &result_ct, lhs_ct);
            self.operand_to_value(converted_back)
        } else {
            self.operand_to_value(result)
        };
        let comp_size = Self::complex_component_size(lhs_ct);
        self.emit(Instruction::Memcpy { dest: lhs_ptr, src: store_ptr, size: comp_size * 2 });

        Operand::Value(lhs_ptr)
    }

    /// Scalar compound assignment with complex RHS: extract real part from RHS.
    fn lower_scalar_compound_assign_complex_rhs(&mut self, op: &BinOp, lhs: &Expr, rhs: &Expr, rhs_ct: &crate::common::types::CType) -> Operand {
        let rhs_val = self.lower_expr(rhs);
        let rhs_ptr = self.operand_to_value(rhs_val);
        let real_part = self.load_complex_real(rhs_ptr, rhs_ct);
        let real_ty = Self::complex_component_ir_type(rhs_ct);

        let ty = self.get_expr_type(lhs);
        let common_ty = if ty.is_float() || real_ty.is_float() {
            if ty == IrType::F128 || real_ty == IrType::F128 { IrType::F128 }
            else if ty == IrType::F64 || real_ty == IrType::F64 { IrType::F64 }
            else { IrType::F32 }
        } else {
            IrType::I64
        };
        let op_ty = if common_ty.is_float() { common_ty } else { IrType::I64 };

        let rhs_promoted = self.emit_implicit_cast(real_part, real_ty, op_ty);

        if let Some(lv) = self.lower_lvalue(lhs) {
            let loaded = self.load_lvalue_typed(&lv, ty);
            let loaded_promoted = self.emit_implicit_cast(loaded, ty, op_ty);
            let is_unsigned = self.infer_expr_type(lhs).is_unsigned();
            let ir_op = Self::binop_to_ir(op.clone(), is_unsigned);
            let result = self.emit_binop_val(ir_op, loaded_promoted, rhs_promoted, op_ty);
            let result_cast = self.emit_implicit_cast(Operand::Value(result), op_ty, ty);
            self.store_lvalue_typed(&lv, result_cast.clone(), ty);
            return result_cast;
        }
        Operand::Const(IrConst::I64(0))
    }

    /// Standard scalar compound assignment.
    fn lower_scalar_compound_assign(&mut self, op: &BinOp, lhs: &Expr, rhs: &Expr) -> Operand {
        let ty = self.get_expr_type(lhs);
        let lhs_ir_ty = self.infer_expr_type(lhs);
        let rhs_ir_ty = self.infer_expr_type(rhs);
        let rhs_ty = self.get_expr_type(rhs);

        let (common_ty, op_ty) = Self::usual_arithmetic_conversions(ty, rhs_ty, lhs_ir_ty, rhs_ir_ty);

        let rhs_val = self.lower_expr_with_type(rhs, op_ty);
        if let Some(lv) = self.lower_lvalue(lhs) {
            let loaded = self.load_lvalue_typed(&lv, ty);
            let is_shift = matches!(op, BinOp::Shl | BinOp::Shr);
            let loaded_promoted = self.promote_for_op(loaded, ty, lhs_ir_ty, op_ty, common_ty, is_shift);

            let is_unsigned = if is_shift {
                Self::integer_promote(lhs_ir_ty).is_unsigned()
            } else {
                common_ty.is_unsigned()
            };
            let ir_op = Self::binop_to_ir(op.clone(), is_unsigned);

            // Scale RHS for pointer += and -=
            let actual_rhs = if ty == IrType::Ptr && matches!(op, BinOp::Add | BinOp::Sub) {
                let elem_size = self.get_pointer_elem_size_from_expr(lhs);
                self.scale_index(rhs_val, elem_size)
            } else {
                rhs_val
            };

            let result = self.emit_binop_val(ir_op, loaded_promoted, actual_rhs, op_ty);

            let store_val = self.narrow_from_op(Operand::Value(result), ty, lhs_ir_ty, op_ty);
            let store_val = if self.is_bool_lvalue(lhs) {
                self.emit_bool_normalize(store_val)
            } else {
                store_val
            };
            self.store_lvalue_typed(&lv, store_val.clone(), ty);
            return store_val;
        }
        rhs_val
    }

    // -----------------------------------------------------------------------
    // Usual arithmetic conversions helpers
    // -----------------------------------------------------------------------

    /// Compute the common type and operation type for a binary operation
    /// using C's "usual arithmetic conversions".
    pub(super) fn usual_arithmetic_conversions(lhs_ty: IrType, rhs_ty: IrType, lhs_ir_ty: IrType, rhs_ir_ty: IrType) -> (IrType, IrType) {
        let common_ty = if lhs_ty.is_float() || rhs_ty.is_float() {
            if lhs_ty == IrType::F128 || rhs_ty == IrType::F128 { IrType::F128 }
            else if lhs_ty == IrType::F64 || rhs_ty == IrType::F64 { IrType::F64 }
            else { IrType::F32 }
        } else {
            Self::common_type(
                Self::integer_promote(lhs_ir_ty),
                Self::integer_promote(rhs_ir_ty),
            )
        };
        let op_ty = if common_ty.is_float() {
            common_ty
        } else if common_ty == IrType::I128 || common_ty == IrType::U128 {
            common_ty
        } else {
            IrType::I64
        };
        (common_ty, op_ty)
    }

    /// Promote a loaded value to the operation type for compound assignment.
    pub(super) fn promote_for_op(&mut self, loaded: Operand, val_ty: IrType, ir_ty: IrType, op_ty: IrType, common_ty: IrType, is_shift: bool) -> Operand {
        if val_ty != op_ty && op_ty.is_float() && !val_ty.is_float() {
            let cast_from = if ir_ty.is_unsigned() { IrType::U64 } else { IrType::I64 };
            Operand::Value(self.emit_cast_val(loaded, cast_from, op_ty))
        } else if val_ty != op_ty && val_ty.is_float() && op_ty.is_float() {
            Operand::Value(self.emit_cast_val(loaded, val_ty, op_ty))
        } else if !op_ty.is_float() && ir_ty.size() < 8 {
            let extend_unsigned = if is_shift {
                ir_ty.is_unsigned()
            } else {
                common_ty.is_unsigned() || ir_ty.is_unsigned()
            };
            let from_ty = if extend_unsigned {
                match ir_ty {
                    IrType::I32 => IrType::U32,
                    IrType::I16 => IrType::U16,
                    IrType::I8 => IrType::U8,
                    _ => ir_ty,
                }
            } else {
                ir_ty
            };
            Operand::Value(self.emit_cast_val(loaded, from_ty, IrType::I64))
        } else {
            loaded
        }
    }

    /// Narrow a result from operation type back to the target type.
    pub(super) fn narrow_from_op(&mut self, result: Operand, target_ty: IrType, target_ir_ty: IrType, op_ty: IrType) -> Operand {
        if op_ty.is_float() && !target_ty.is_float() {
            let cast_to = if target_ir_ty.is_unsigned() { IrType::U64 } else { IrType::I64 };
            let dest = self.emit_cast_val(result, op_ty, cast_to);
            if target_ir_ty.size() < 8 {
                Operand::Value(self.emit_cast_val(Operand::Value(dest), cast_to, target_ir_ty))
            } else {
                Operand::Value(dest)
            }
        } else if op_ty.is_float() && target_ty.is_float() && op_ty != target_ty {
            Operand::Value(self.emit_cast_val(result, op_ty, target_ty))
        } else if !op_ty.is_float() && target_ir_ty.size() < 8 {
            Operand::Value(self.emit_cast_val(result, IrType::I64, target_ir_ty))
        } else {
            result
        }
    }
}
