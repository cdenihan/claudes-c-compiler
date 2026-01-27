//! Control flow statement lowering: if/else, loops (while/for/do-while),
//! break/continue, goto (direct and computed), and labels.

use crate::frontend::parser::ast::*;
use crate::ir::ir::*;
use super::lowering::Lowerer;

impl Lowerer {
    pub(super) fn lower_if_stmt(&mut self, cond: &Expr, then_stmt: &Stmt, else_stmt: Option<&Stmt>) {
        let cond_val = self.lower_condition_expr(cond);
        let then_label = self.fresh_label();
        let else_label = self.fresh_label();
        let end_label = self.fresh_label();

        let false_target = if else_stmt.is_some() { else_label } else { end_label };
        self.terminate(Terminator::CondBranch {
            cond: cond_val,
            true_label: then_label,
            false_label: false_target,
        });

        self.start_block(then_label);
        self.lower_stmt(then_stmt);
        self.terminate(Terminator::Branch(end_label));

        if let Some(else_stmt) = else_stmt {
            self.start_block(else_label);
            self.lower_stmt(else_stmt);
            self.terminate(Terminator::Branch(end_label));
        }

        self.start_block(end_label);
    }

    pub(super) fn lower_while_stmt(&mut self, cond: &Expr, body: &Stmt) {
        let cond_label = self.fresh_label();
        let body_label = self.fresh_label();
        let end_label = self.fresh_label();

        let scope_depth = self.func().scope_stack.len();
        self.func_mut().break_labels.push((end_label, scope_depth));
        self.func_mut().continue_labels.push((cond_label, scope_depth));

        self.terminate(Terminator::Branch(cond_label));

        self.start_block(cond_label);
        let cond_val = self.lower_condition_expr(cond);
        self.terminate(Terminator::CondBranch {
            cond: cond_val,
            true_label: body_label,
            false_label: end_label,
        });

        self.start_block(body_label);
        self.lower_stmt(body);
        let continue_target = self.func().continue_labels.last().unwrap().0;
        self.terminate(Terminator::Branch(continue_target));

        self.func_mut().break_labels.pop();
        self.func_mut().continue_labels.pop();
        self.start_block(end_label);
    }

    pub(super) fn lower_for_stmt(
        &mut self,
        init: &Option<Box<ForInit>>,
        cond: &Option<Expr>,
        inc: &Option<Expr>,
        body: &Stmt,
    ) {
        // C99: for-init declarations have their own scope.
        let has_decl_init = init.as_ref().map_or(false, |i| matches!(i.as_ref(), ForInit::Declaration(_)));
        if has_decl_init {
            self.push_scope();
        }

        // Init
        if let Some(init) = init {
            match init.as_ref() {
                ForInit::Declaration(decl) => {
                    self.collect_enum_constants_scoped(&decl.type_spec);
                    self.lower_local_decl(decl);
                }
                ForInit::Expr(expr) => { self.lower_expr(expr); },
            }
        }

        let cond_label = self.fresh_label();
        let body_label = self.fresh_label();
        let inc_label = self.fresh_label();
        let end_label = self.fresh_label();

        let scope_depth = self.func().scope_stack.len();
        self.func_mut().break_labels.push((end_label, scope_depth));
        self.func_mut().continue_labels.push((inc_label, scope_depth));

        self.terminate(Terminator::Branch(cond_label));

        // Condition
        self.start_block(cond_label);
        if let Some(cond) = cond {
            let cond_val = self.lower_condition_expr(cond);
            self.terminate(Terminator::CondBranch {
                cond: cond_val,
                true_label: body_label,
                false_label: end_label,
            });
        } else {
            self.terminate(Terminator::Branch(body_label));
        }

        // Body
        self.start_block(body_label);
        self.lower_stmt(body);
        self.terminate(Terminator::Branch(inc_label));

        // Increment
        self.start_block(inc_label);
        if let Some(inc) = inc {
            self.lower_expr(inc);
        }
        self.terminate(Terminator::Branch(cond_label));

        self.func_mut().break_labels.pop();
        self.func_mut().continue_labels.pop();
        self.start_block(end_label);

        if has_decl_init {
            self.pop_scope();
        }
    }

    pub(super) fn lower_do_while_stmt(&mut self, body: &Stmt, cond: &Expr) {
        let body_label = self.fresh_label();
        let cond_label = self.fresh_label();
        let end_label = self.fresh_label();

        let scope_depth = self.func().scope_stack.len();
        self.func_mut().break_labels.push((end_label, scope_depth));
        self.func_mut().continue_labels.push((cond_label, scope_depth));

        self.terminate(Terminator::Branch(body_label));

        self.start_block(body_label);
        self.lower_stmt(body);
        self.terminate(Terminator::Branch(cond_label));

        self.start_block(cond_label);
        let cond_val = self.lower_condition_expr(cond);
        self.terminate(Terminator::CondBranch {
            cond: cond_val,
            true_label: body_label,
            false_label: end_label,
        });

        self.func_mut().break_labels.pop();
        self.func_mut().continue_labels.pop();
        self.start_block(end_label);
    }

    pub(super) fn lower_break_stmt(&mut self) {
        if let Some(&(label, scope_depth)) = self.func().break_labels.last() {
            // Emit cleanup calls for all scopes being exited by break
            let cleanups = self.collect_scope_cleanup_vars_above_depth(scope_depth);
            self.emit_cleanup_calls(&cleanups);
            self.terminate(Terminator::Branch(label));
            let dead = self.fresh_label();
            self.start_block(dead);
        }
    }

    pub(super) fn lower_continue_stmt(&mut self) {
        if let Some(&(label, scope_depth)) = self.func().continue_labels.last() {
            // Emit cleanup calls for all scopes being exited by continue
            let cleanups = self.collect_scope_cleanup_vars_above_depth(scope_depth);
            self.emit_cleanup_calls(&cleanups);
            self.terminate(Terminator::Branch(label));
            let dead = self.fresh_label();
            self.start_block(dead);
        }
    }

    pub(super) fn lower_goto_stmt(&mut self, label: &str) {
        // Emit cleanup calls for all active scopes before jumping.
        // TODO: This is conservative — it cleans up ALL active scopes, which is correct
        // when goto exits scopes but over-cleans when jumping within the same scope or
        // to a label in an enclosing scope. Proper handling would require tracking the
        // target label's scope depth, which is complex since labels can be forward refs.
        let all_cleanups = self.collect_all_scope_cleanup_vars();
        self.emit_cleanup_calls(&all_cleanups);
        // If the function has VLA declarations, restore the saved stack pointer before
        // jumping. This ensures VLA stack space is reclaimed on backward jumps (e.g.,
        // goto to a label before a VLA declaration in a loop).
        if let Some(save_val) = self.func().vla_stack_save {
            self.emit(Instruction::StackRestore { ptr: save_val });
        }
        let scoped_label = self.get_or_create_user_label(label);
        self.terminate(Terminator::Branch(scoped_label));
        let dead = self.fresh_label();
        self.start_block(dead);
    }

    pub(super) fn lower_goto_indirect_stmt(&mut self, expr: &Expr) {
        // Emit cleanup calls for all active scopes before indirect jump (same as goto).
        let all_cleanups = self.collect_all_scope_cleanup_vars();
        self.emit_cleanup_calls(&all_cleanups);
        // If the function has VLA declarations, restore the saved stack pointer before
        // indirect jumps too (computed gotos).
        if let Some(save_val) = self.func().vla_stack_save {
            self.emit(Instruction::StackRestore { ptr: save_val });
        }
        let target = self.lower_expr(expr);
        let possible_targets: Vec<BlockId> = self.func_mut().user_labels.values().copied().collect();
        self.terminate(Terminator::IndirectBranch { target, possible_targets });
        let dead = self.fresh_label();
        self.start_block(dead);
    }

    pub(super) fn lower_label_stmt(&mut self, name: &str, stmt: &Stmt) {
        let label = self.get_or_create_user_label(name);
        self.terminate(Terminator::Branch(label));
        self.start_block(label);
        self.lower_stmt(stmt);
    }
}
