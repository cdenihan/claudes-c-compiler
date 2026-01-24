//! Builtin function call lowering: __builtin_* intrinsics and FP classification.
//!
//! Extracted from expr.rs to keep the expression lowering manageable.
//! This module handles:
//! - `try_lower_builtin_call`: main dispatch for all __builtin_* functions
//! - Intrinsic helpers (clz, ctz, bswap, popcount, parity)
//! - FP classification builtins (fpclassify, isnan, isinf, isfinite, isnormal, signbit)
//! - `creal_return_type`: return type for creal/cimag family
//! - `builtin_return_type` lookup (defined on Lowerer in lowering.rs)

use crate::frontend::parser::ast::*;
use crate::frontend::sema::builtins::{self, BuiltinKind, BuiltinIntrinsic};
use crate::ir::ir::*;
use crate::common::types::IrType;
use super::lowering::Lowerer;

impl Lowerer {
    /// Determine the return type for creal/crealf/creall/cimag/cimagf/cimagl based on function name.
    pub(super) fn creal_return_type(name: &str) -> IrType {
        match name {
            "crealf" | "__builtin_crealf" | "cimagf" | "__builtin_cimagf" => IrType::F32,
            "creall" | "__builtin_creall" | "cimagl" | "__builtin_cimagl" => IrType::F128,
            _ => IrType::F64, // creal, cimag, __builtin_creal, __builtin_cimag
        }
    }

    /// Try to lower a __builtin_* call. Returns Some(result) if handled.
    pub(super) fn try_lower_builtin_call(&mut self, name: &str, args: &[Expr]) -> Option<Operand> {
        // Handle alloca specially - it needs dynamic stack allocation
        match name {
            "__builtin_alloca" | "__builtin_alloca_with_align" => {
                if let Some(size_expr) = args.first() {
                    let size_operand = self.lower_expr(size_expr);
                    let align = if name == "__builtin_alloca_with_align" && args.len() >= 2 {
                        // align is in bits
                        if let Expr::IntLiteral(bits, _) = &args[1] {
                            (*bits as usize) / 8
                        } else {
                            16
                        }
                    } else {
                        16 // default alignment
                    };
                    let dest = self.fresh_value();
                    self.emit(Instruction::DynAlloca { dest, size: size_operand, align });
                    return Some(Operand::Value(dest));
                }
                return Some(Operand::Const(IrConst::I64(0)));
            }
            _ => {}
        }
        // Handle va_start/va_end/va_copy specially
        match name {
            "__builtin_va_start" => {
                if let Some(ap_expr) = args.first() {
                    let ap_val = self.lower_expr(ap_expr);
                    let ap_ptr = self.operand_to_value(ap_val);
                    self.emit(Instruction::VaStart { va_list_ptr: ap_ptr });
                }
                return Some(Operand::Const(IrConst::I64(0)));
            }
            "__builtin_va_end" => {
                if let Some(ap_expr) = args.first() {
                    let ap_val = self.lower_expr(ap_expr);
                    let ap_ptr = self.operand_to_value(ap_val);
                    self.emit(Instruction::VaEnd { va_list_ptr: ap_ptr });
                }
                return Some(Operand::Const(IrConst::I64(0)));
            }
            "__builtin_va_copy" => {
                if args.len() >= 2 {
                    let dest_val = self.lower_expr(&args[0]);
                    let src_val = self.lower_expr(&args[1]);
                    let dest_ptr = self.operand_to_value(dest_val);
                    let src_ptr = self.operand_to_value(src_val);
                    self.emit(Instruction::VaCopy { dest_ptr, src_ptr });
                }
                return Some(Operand::Const(IrConst::I64(0)));
            }
            _ => {}
        }

        // __builtin_prefetch(addr, [rw], [locality]) - no-op performance hint
        if name == "__builtin_prefetch" {
            for arg in args {
                self.lower_expr(arg);
            }
            return Some(Operand::Const(IrConst::I64(0)));
        }

        // Handle atomic builtins (delegated to expr_atomics.rs)
        if let Some(result) = self.try_lower_atomic_builtin(name, args) {
            return Some(result);
        }

        let builtin_info = builtins::resolve_builtin(name)?;
        match &builtin_info.kind {
            BuiltinKind::LibcAlias(libc_name) => {
                let arg_types: Vec<IrType> = args.iter().map(|a| self.get_expr_type(a)).collect();
                let arg_vals: Vec<Operand> = args.iter().map(|a| self.lower_expr(a)).collect();
                let dest = self.fresh_value();
                let variadic = self.func_meta.variadic.contains(libc_name.as_str());
                let n_fixed = if variadic {
                    self.func_meta.param_types.get(libc_name.as_str()).map(|p| p.len()).unwrap_or(arg_vals.len())
                } else { arg_vals.len() };
                let return_type = Self::builtin_return_type(name)
                    .or_else(|| self.func_meta.return_types.get(libc_name.as_str()).copied())
                    .unwrap_or(IrType::I64);
                self.emit(Instruction::Call {
                    dest: Some(dest), func: libc_name.clone(),
                    args: arg_vals, arg_types, return_type, is_variadic: variadic, num_fixed_args: n_fixed,
                });
                Some(Operand::Value(dest))
            }
            BuiltinKind::Identity => {
                Some(args.first().map_or(Operand::Const(IrConst::I64(0)), |a| self.lower_expr(a)))
            }
            BuiltinKind::ConstantI64(val) => Some(Operand::Const(IrConst::I64(*val))),
            BuiltinKind::ConstantF64(val) => {
                let is_float_variant = name == "__builtin_inff"
                    || name == "__builtin_huge_valf";
                if is_float_variant {
                    Some(Operand::Const(IrConst::F32(*val as f32)))
                } else {
                    Some(Operand::Const(IrConst::F64(*val)))
                }
            }
            BuiltinKind::Intrinsic(intrinsic) => {
                self.lower_builtin_intrinsic(intrinsic, name, args)
            }
        }
    }

    /// Lower a builtin intrinsic (the BuiltinKind::Intrinsic arm of try_lower_builtin_call).
    fn lower_builtin_intrinsic(&mut self, intrinsic: &BuiltinIntrinsic, name: &str, args: &[Expr]) -> Option<Operand> {
        match intrinsic {
            BuiltinIntrinsic::FpCompare => {
                if args.len() >= 2 {
                    let lhs_ty = self.get_expr_type(&args[0]);
                    let rhs_ty = self.get_expr_type(&args[1]);
                    let cmp_ty = if lhs_ty == IrType::F64 || rhs_ty == IrType::F64 {
                        IrType::F64
                    } else if lhs_ty == IrType::F32 || rhs_ty == IrType::F32 {
                        IrType::F32
                    } else {
                        IrType::F64
                    };
                    let mut lhs = self.lower_expr(&args[0]);
                    let mut rhs = self.lower_expr(&args[1]);
                    if lhs_ty != cmp_ty {
                        let conv = self.emit_cast_val(lhs, lhs_ty, cmp_ty);
                        lhs = Operand::Value(conv);
                    }
                    if rhs_ty != cmp_ty {
                        let conv = self.emit_cast_val(rhs, rhs_ty, cmp_ty);
                        rhs = Operand::Value(conv);
                    }
                    let cmp_op = match name {
                        "__builtin_isgreater" => IrCmpOp::Sgt,
                        "__builtin_isgreaterequal" => IrCmpOp::Sge,
                        "__builtin_isless" => IrCmpOp::Slt,
                        "__builtin_islessequal" => IrCmpOp::Sle,
                        "__builtin_islessgreater" => IrCmpOp::Ne,
                        "__builtin_isunordered" => IrCmpOp::Ne, // approximate
                        _ => IrCmpOp::Eq,
                    };
                    let dest = self.emit_cmp_val(cmp_op, lhs, rhs, cmp_ty);
                    return Some(Operand::Value(dest));
                }
                Some(Operand::Const(IrConst::I64(0)))
            }
            BuiltinIntrinsic::Clz => self.lower_unary_intrinsic(name, args, IrUnaryOp::Clz),
            BuiltinIntrinsic::Ctz => self.lower_unary_intrinsic(name, args, IrUnaryOp::Ctz),
            BuiltinIntrinsic::Bswap => self.lower_bswap_intrinsic(name, args),
            BuiltinIntrinsic::Popcount => self.lower_unary_intrinsic(name, args, IrUnaryOp::Popcount),
            BuiltinIntrinsic::Parity => self.lower_parity_intrinsic(name, args),
            BuiltinIntrinsic::ComplexReal => {
                if !args.is_empty() {
                    let arg_ctype = self.expr_ctype(&args[0]);
                    if arg_ctype.is_complex() {
                        Some(self.lower_complex_real_part(&args[0]))
                    } else {
                        let target_ty = Self::creal_return_type(name);
                        let val = self.lower_expr(&args[0]);
                        let val_ty = self.get_expr_type(&args[0]);
                        Some(self.emit_implicit_cast(val, val_ty, target_ty))
                    }
                } else {
                    Some(Operand::Const(IrConst::F64(0.0)))
                }
            }
            BuiltinIntrinsic::ComplexImag => {
                if !args.is_empty() {
                    let arg_ctype = self.expr_ctype(&args[0]);
                    if arg_ctype.is_complex() {
                        Some(self.lower_complex_imag_part(&args[0]))
                    } else {
                        let target_ty = Self::creal_return_type(name);
                        Some(match target_ty {
                            IrType::F32 => Operand::Const(IrConst::F32(0.0)),
                            IrType::F128 => Operand::Const(IrConst::LongDouble(0.0)),
                            _ => Operand::Const(IrConst::F64(0.0)),
                        })
                    }
                } else {
                    Some(Operand::Const(IrConst::F64(0.0)))
                }
            }
            BuiltinIntrinsic::ComplexConj => {
                if !args.is_empty() {
                    Some(self.lower_complex_conj(&args[0]))
                } else {
                    Some(Operand::Const(IrConst::F64(0.0)))
                }
            }
            BuiltinIntrinsic::Fence => {
                Some(Operand::Const(IrConst::I64(0)))
            }
            BuiltinIntrinsic::FpClassify => self.lower_builtin_fpclassify(args),
            BuiltinIntrinsic::IsNan => self.lower_builtin_isnan(args),
            BuiltinIntrinsic::IsInf => self.lower_builtin_isinf(args),
            BuiltinIntrinsic::IsFinite => self.lower_builtin_isfinite(args),
            BuiltinIntrinsic::IsNormal => self.lower_builtin_isnormal(args),
            BuiltinIntrinsic::SignBit => self.lower_builtin_signbit(args),
            BuiltinIntrinsic::IsInfSign => self.lower_builtin_isinf_sign(args),
            BuiltinIntrinsic::Alloca => {
                // Handled earlier in try_lower_builtin_call - should not reach here
                Some(Operand::Const(IrConst::I64(0)))
            }
            BuiltinIntrinsic::ComplexConstruct => {
                if args.len() >= 2 {
                    let real_val = self.lower_expr(&args[0]);
                    let imag_val = self.lower_expr(&args[1]);
                    let arg_ty = self.get_expr_type(&args[0]);
                    let (comp_ty, complex_size, comp_size) = if arg_ty == IrType::F32 {
                        (IrType::F32, 8usize, 4usize)
                    } else {
                        (IrType::F64, 16usize, 8usize)
                    };
                    let alloca = self.fresh_value();
                    self.emit(Instruction::Alloca { dest: alloca, ty: IrType::Ptr, size: complex_size });
                    self.emit(Instruction::Store { val: real_val, ptr: alloca, ty: comp_ty });
                    let imag_ptr = self.fresh_value();
                    self.emit(Instruction::GetElementPtr {
                        dest: imag_ptr, base: alloca,
                        offset: Operand::Const(IrConst::I64(comp_size as i64)),
                        ty: IrType::I8,
                    });
                    self.emit(Instruction::Store { val: imag_val, ptr: imag_ptr, ty: comp_ty });
                    Some(Operand::Value(alloca))
                } else {
                    Some(Operand::Const(IrConst::I64(0)))
                }
            }
            BuiltinIntrinsic::VaStart | BuiltinIntrinsic::VaEnd | BuiltinIntrinsic::VaCopy => {
                unreachable!("va builtins handled earlier by name match")
            }
        }
    }

    // -----------------------------------------------------------------------
    // Intrinsic helpers
    // -----------------------------------------------------------------------

    /// Determine the operand width for a suffix-encoded intrinsic (clz, ctz, popcount, etc.).
    fn intrinsic_type_from_suffix(name: &str) -> IrType {
        if name.ends_with("ll") || name.ends_with('l') { IrType::I64 } else { IrType::I32 }
    }

    /// Lower a simple unary intrinsic (CLZ, CTZ, Popcount) that takes one integer arg.
    fn lower_unary_intrinsic(&mut self, name: &str, args: &[Expr], ir_op: IrUnaryOp) -> Option<Operand> {
        if args.is_empty() {
            return Some(Operand::Const(IrConst::I64(0)));
        }
        let arg = self.lower_expr(&args[0]);
        let ty = Self::intrinsic_type_from_suffix(name);
        let dest = self.fresh_value();
        self.emit(Instruction::UnaryOp { dest, op: ir_op, src: arg, ty });
        Some(Operand::Value(dest))
    }

    /// Lower __builtin_bswap{16,32,64} - type determined by numeric suffix.
    fn lower_bswap_intrinsic(&mut self, name: &str, args: &[Expr]) -> Option<Operand> {
        if args.is_empty() {
            return Some(Operand::Const(IrConst::I64(0)));
        }
        let arg = self.lower_expr(&args[0]);
        let ty = if name.contains("64") {
            IrType::I64
        } else if name.contains("16") {
            IrType::I16
        } else {
            IrType::I32
        };
        let dest = self.fresh_value();
        self.emit(Instruction::UnaryOp { dest, op: IrUnaryOp::Bswap, src: arg, ty });
        Some(Operand::Value(dest))
    }

    /// Lower __builtin_parity{,l,ll} - implemented as popcount & 1.
    fn lower_parity_intrinsic(&mut self, name: &str, args: &[Expr]) -> Option<Operand> {
        if args.is_empty() {
            return Some(Operand::Const(IrConst::I64(0)));
        }
        let arg = self.lower_expr(&args[0]);
        let ty = Self::intrinsic_type_from_suffix(name);
        let pop = self.fresh_value();
        self.emit(Instruction::UnaryOp { dest: pop, op: IrUnaryOp::Popcount, src: arg, ty });
        let dest = self.emit_binop_val(IrBinOp::And, Operand::Value(pop), Operand::Const(IrConst::I64(1)), ty);
        Some(Operand::Value(dest))
    }

    // =========================================================================
    // Floating-point classification builtins
    // =========================================================================

    /// Helper: get the type of the last argument for fp classification builtins.
    fn lower_fp_classify_arg(&mut self, args: &[Expr]) -> (IrType, Operand) {
        let arg_expr = args.last().unwrap();
        let arg_ty = self.get_expr_type(arg_expr);
        let arg_val = self.lower_expr(arg_expr);
        (arg_ty, arg_val)
    }

    /// Bitcast a float/double to its integer bit representation via memory.
    /// F32 -> I32, F64 -> I64. Uses alloca+store+load for a true bitcast.
    fn bitcast_float_to_int(&mut self, val: Operand, float_ty: IrType) -> (Operand, IrType) {
        let int_ty = match float_ty {
            IrType::F32 => IrType::I32,
            _ => IrType::I64,
        };
        let size = if float_ty == IrType::F32 { 4 } else { 8 };

        let tmp_alloca = self.fresh_value();
        self.emit(Instruction::Alloca { dest: tmp_alloca, size, ty: float_ty });

        let val_v = self.operand_to_value(val);
        self.emit(Instruction::Store { val: Operand::Value(val_v), ptr: tmp_alloca, ty: float_ty });

        let result = self.fresh_value();
        self.emit(Instruction::Load { dest: result, ptr: tmp_alloca, ty: int_ty });

        (Operand::Value(result), int_ty)
    }

    /// Floating-point bit-layout constants for classification.
    /// Returns (abs_mask, exp_only, exp_shift, exp_field_max, mant_mask).
    pub(super) fn fp_masks(float_ty: IrType) -> (i64, i64, i64, i64, i64) {
        match float_ty {
            IrType::F32 => (
                0x7FFFFFFF_i64,
                0x7F800000_i64,
                23,
                0xFF,
                0x007FFFFF_i64,
            ),
            _ => (
                0x7FFFFFFFFFFFFFFF_i64,
                0x7FF0000000000000_u64 as i64,
                52,
                0x7FF,
                0x000FFFFFFFFFFFFF_u64 as i64,
            ),
        }
    }

    /// Compute the absolute value (sign-stripped) of float bits: bits & abs_mask.
    fn fp_abs_bits(&mut self, bits: &Operand, int_ty: IrType, abs_mask: i64) -> Value {
        self.emit_binop_val(IrBinOp::And, bits.clone(), Operand::Const(IrConst::I64(abs_mask)), int_ty)
    }

    /// Lower __builtin_fpclassify(nan_val, inf_val, norm_val, subnorm_val, zero_val, x).
    /// Uses arithmetic select: result = sum of (class_val[i] * is_class[i]).
    fn lower_builtin_fpclassify(&mut self, args: &[Expr]) -> Option<Operand> {
        if args.len() < 6 {
            return Some(Operand::Const(IrConst::I64(0)));
        }

        let class_vals: Vec<_> = (0..5).map(|i| self.lower_expr(&args[i])).collect();
        let arg_ty = self.get_expr_type(&args[5]);
        let arg_val = self.lower_expr(&args[5]);

        let (bits, int_ty) = self.bitcast_float_to_int(arg_val, arg_ty);
        let (_abs_mask, _exp_only, exp_shift, exp_field_max, mant_mask) = Self::fp_masks(arg_ty);

        let shifted = self.emit_binop_val(IrBinOp::LShr, bits.clone(), Operand::Const(IrConst::I64(exp_shift)), int_ty);
        let exponent = self.emit_binop_val(IrBinOp::And, Operand::Value(shifted), Operand::Const(IrConst::I64(exp_field_max)), int_ty);
        let mantissa = self.emit_binop_val(IrBinOp::And, bits, Operand::Const(IrConst::I64(mant_mask)), int_ty);

        let exp_is_max = self.emit_cmp_val(IrCmpOp::Eq, Operand::Value(exponent), Operand::Const(IrConst::I64(exp_field_max)), int_ty);
        let exp_is_zero = self.emit_cmp_val(IrCmpOp::Eq, Operand::Value(exponent), Operand::Const(IrConst::I64(0)), int_ty);
        let mant_is_zero = self.emit_cmp_val(IrCmpOp::Eq, Operand::Value(mantissa), Operand::Const(IrConst::I64(0)), int_ty);
        let mant_not_zero = self.emit_cmp_val(IrCmpOp::Ne, Operand::Value(mantissa), Operand::Const(IrConst::I64(0)), int_ty);

        let is_nan = self.emit_binop_val(IrBinOp::And, Operand::Value(exp_is_max), Operand::Value(mant_not_zero), IrType::I32);
        let is_inf = self.emit_binop_val(IrBinOp::And, Operand::Value(exp_is_max), Operand::Value(mant_is_zero), IrType::I32);
        let is_zero = self.emit_binop_val(IrBinOp::And, Operand::Value(exp_is_zero), Operand::Value(mant_is_zero), IrType::I32);
        let is_subnorm = self.emit_binop_val(IrBinOp::And, Operand::Value(exp_is_zero), Operand::Value(mant_not_zero), IrType::I32);

        let mut special = is_nan;
        for &flag in &[is_inf, is_zero, is_subnorm] {
            special = self.emit_binop_val(IrBinOp::Or, Operand::Value(special), Operand::Value(flag), IrType::I32);
        }
        let is_normal = self.emit_binop_val(IrBinOp::Xor, Operand::Value(special), Operand::Const(IrConst::I64(1)), IrType::I32);

        // Order: nan=0, inf=1, normal=2, subnormal=3, zero=4
        let class_flags = [is_nan, is_inf, is_normal, is_subnorm, is_zero];
        let contribs: Vec<_> = class_vals.iter().zip(class_flags.iter())
            .map(|(val, &flag)| (val.clone(), flag))
            .collect();
        let mut result = self.emit_binop_val(IrBinOp::Mul, contribs[0].0.clone(), Operand::Value(contribs[0].1), IrType::I64);
        for &(ref val, flag) in &contribs[1..] {
            let contrib = self.emit_binop_val(IrBinOp::Mul, val.clone(), Operand::Value(flag), IrType::I64);
            result = self.emit_binop_val(IrBinOp::Add, Operand::Value(result), Operand::Value(contrib), IrType::I64);
        }

        Some(Operand::Value(result))
    }

    /// Shared setup for FP classification builtins: validates args, lowers the
    /// float argument, bitcasts to integer bits, and retrieves the FP masks.
    /// The closure receives (self, bits, arg_ty, int_ty, masks) and returns the result Value.
    fn lower_fp_classify_with<F>(&mut self, args: &[Expr], classify: F) -> Option<Operand>
    where
        F: FnOnce(&mut Self, Operand, IrType, IrType, (i64, i64, i64, i64, i64)) -> Value,
    {
        if args.is_empty() { return Some(Operand::Const(IrConst::I64(0))); }
        let (arg_ty, arg_val) = self.lower_fp_classify_arg(args);
        let (bits, int_ty) = self.bitcast_float_to_int(arg_val, arg_ty);
        let masks = Self::fp_masks(arg_ty);
        let result = classify(self, bits, arg_ty, int_ty, masks);
        Some(Operand::Value(result))
    }

    /// __builtin_isnan(x) -> 1 if x is NaN. NaN: abs(bits) > exp_only.
    fn lower_builtin_isnan(&mut self, args: &[Expr]) -> Option<Operand> {
        self.lower_fp_classify_with(args, |s, bits, _, int_ty, (abs_mask, exp_only, _, _, _)| {
            let abs_val = s.fp_abs_bits(&bits, int_ty, abs_mask);
            s.emit_cmp_val(IrCmpOp::Ugt, Operand::Value(abs_val), Operand::Const(IrConst::I64(exp_only)), int_ty)
        })
    }

    /// __builtin_isinf(x) -> nonzero if +/-infinity. Inf: abs(bits) == exp_only.
    fn lower_builtin_isinf(&mut self, args: &[Expr]) -> Option<Operand> {
        self.lower_fp_classify_with(args, |s, bits, _, int_ty, (abs_mask, exp_only, _, _, _)| {
            let abs_val = s.fp_abs_bits(&bits, int_ty, abs_mask);
            s.emit_cmp_val(IrCmpOp::Eq, Operand::Value(abs_val), Operand::Const(IrConst::I64(exp_only)), int_ty)
        })
    }

    /// __builtin_isfinite(x) -> 1 if finite. Finite: (bits & exp_only) != exp_only.
    fn lower_builtin_isfinite(&mut self, args: &[Expr]) -> Option<Operand> {
        self.lower_fp_classify_with(args, |s, bits, _, int_ty, (_, exp_only, _, _, _)| {
            let exp_bits = s.emit_binop_val(IrBinOp::And, bits, Operand::Const(IrConst::I64(exp_only)), int_ty);
            s.emit_cmp_val(IrCmpOp::Ne, Operand::Value(exp_bits), Operand::Const(IrConst::I64(exp_only)), int_ty)
        })
    }

    /// __builtin_isnormal(x) -> 1 if normal. Normal: exp != 0 AND exp != max.
    fn lower_builtin_isnormal(&mut self, args: &[Expr]) -> Option<Operand> {
        self.lower_fp_classify_with(args, |s, bits, _, int_ty, (_, exp_only, exp_shift, exp_field_max, _)| {
            let exp_bits = s.emit_binop_val(IrBinOp::And, bits, Operand::Const(IrConst::I64(exp_only)), int_ty);
            let exponent = s.emit_binop_val(IrBinOp::LShr, Operand::Value(exp_bits), Operand::Const(IrConst::I64(exp_shift)), int_ty);
            let exp_nonzero = s.emit_cmp_val(IrCmpOp::Ne, Operand::Value(exponent), Operand::Const(IrConst::I64(0)), int_ty);
            let exp_not_max = s.emit_cmp_val(IrCmpOp::Ne, Operand::Value(exponent), Operand::Const(IrConst::I64(exp_field_max)), int_ty);
            s.emit_binop_val(IrBinOp::And, Operand::Value(exp_nonzero), Operand::Value(exp_not_max), IrType::I32)
        })
    }

    /// __builtin_signbit(x) -> nonzero if sign bit set.
    fn lower_builtin_signbit(&mut self, args: &[Expr]) -> Option<Operand> {
        self.lower_fp_classify_with(args, |s, bits, arg_ty, int_ty, _| {
            let sign_shift = if arg_ty == IrType::F32 { 31_i64 } else { 63_i64 };
            let shifted = s.emit_binop_val(IrBinOp::LShr, bits, Operand::Const(IrConst::I64(sign_shift)), int_ty);
            s.emit_binop_val(IrBinOp::And, Operand::Value(shifted), Operand::Const(IrConst::I64(1)), int_ty)
        })
    }

    /// __builtin_isinf_sign(x) -> -1 if -inf, +1 if +inf, 0 otherwise.
    fn lower_builtin_isinf_sign(&mut self, args: &[Expr]) -> Option<Operand> {
        self.lower_fp_classify_with(args, |s, bits, arg_ty, int_ty, (abs_mask, exp_only, _, _, _)| {
            let sign_shift = if arg_ty == IrType::F32 { 31_i64 } else { 63_i64 };
            let abs_val = s.fp_abs_bits(&bits, int_ty, abs_mask);
            let is_inf = s.emit_cmp_val(IrCmpOp::Eq, Operand::Value(abs_val), Operand::Const(IrConst::I64(exp_only)), int_ty);
            let sign_shifted = s.emit_binop_val(IrBinOp::LShr, bits, Operand::Const(IrConst::I64(sign_shift)), int_ty);
            let sign_bit = s.emit_binop_val(IrBinOp::And, Operand::Value(sign_shifted), Operand::Const(IrConst::I64(1)), int_ty);
            let sign_x2 = s.emit_binop_val(IrBinOp::Mul, Operand::Value(sign_bit), Operand::Const(IrConst::I64(2)), IrType::I64);
            let direction = s.emit_binop_val(IrBinOp::Sub, Operand::Const(IrConst::I64(1)), Operand::Value(sign_x2), IrType::I64);
            s.emit_binop_val(IrBinOp::Mul, Operand::Value(is_inf), Operand::Value(direction), IrType::I64)
        })
    }
}
