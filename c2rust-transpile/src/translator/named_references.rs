#![deny(missing_docs)]
//! This module provides helper functions for generating rvalue and lvalues
//! corresponding to a single Rust expression.

use super::*;

impl<'c> Translation<'c> {
    /// Get back a Rust lvalue corresponding to the expression passed in.
    ///
    /// Do not use the output lvalue expression more than once.
    pub fn name_reference_write(
        &self,
        ctx: ExprContext,
        reference: CExprId,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        self.name_reference(ctx, reference, false)
            .map(|ws| ws.map(|(lvalue, _)| lvalue))
    }

    /// Get back a Rust (lvalue, rvalue) pair corresponding to the expression passed in.
    ///
    /// You may reuse either of these expressions.
    pub fn name_reference_write_read(
        &self,
        ctx: ExprContext,
        reference: CExprId,
    ) -> Result<WithStmts<(P<Expr>, P<Expr>)>, TranslationError> {
        let msg: &str = "When called with `uses_read = true`, `name_reference` should always \
                         return an rvalue (something from which to read the memory location)";

        self.name_reference(ctx, reference, true)
            .map(|ws| ws.map(|(lvalue, rvalue)| (lvalue, rvalue.expect(msg))))
    }

    /// This function transforms an expression that should refer to a memory location (a C lvalue)
    /// into a Rust lvalue for writing to that location.
    ///
    /// When called with `uses_read`, this function returns an rvalue too. The rvalue can be used to
    /// read multiple times without duplicating side-effects.
    ///
    /// NOTE: Use `name_reference_write` or `name_reference_write_read` instead of calling this
    ///       directly.
    fn name_reference(
        &self,
        ctx: ExprContext,
        reference: CExprId,
        uses_read: bool,
    ) -> Result<WithStmts<(P<Expr>, Option<P<Expr>>)>, TranslationError> {
        let reference_ty = self
            .ast_context
            .index(reference)
            .kind
            .get_qual_type()
            .ok_or_else(|| format_err!("bad reference type"))?;
        let WithStmts {
            val: reference,
            mut stmts,
        } = self.convert_expr(ctx.used(), reference)?;

        /// Check if something is a valid Rust lvalue. Inspired by `librustc::ty::expr_is_lval`.
        fn is_lvalue(e: &Expr) -> bool {
            match e.node {
                ExprKind::Path(..)
                | ExprKind::Unary(ast::UnOp::Deref, _)
                | ExprKind::Field(..)
                | ExprKind::Index(..) => true,
                _ => false,
            }
        }

        // Check if something is a side-effect free Rust lvalue.
        fn is_simple_lvalue(e: &Expr) -> bool {
            match e.node {
                ExprKind::Path(..) => true,
                ExprKind::Unary(ast::UnOp::Deref, ref e)
                | ExprKind::Field(ref e, _)
                | ExprKind::Index(ref e, _) => is_simple_lvalue(e),
                _ => false,
            }
        }

        // Given the LHS access to a variable, produce the RHS one
        let read = |write: P<Expr>| -> Result<P<Expr>, TranslationError> {
            if reference_ty.qualifiers.is_volatile {
                self.volatile_read(&write, reference_ty)
            } else {
                Ok(write)
            }
        };

        if !uses_read && is_lvalue(&*reference) {
            Ok(WithStmts {
                stmts,
                val: (reference, None),
            })
        } else if is_simple_lvalue(&*reference) {
            Ok(WithStmts {
                stmts,
                val: (reference.clone(), Some(read(reference)?)),
            })
        } else {
            // This is the case where we explicitly need to factor out possible side-effects.

            let ptr_name = self.renamer.borrow_mut().fresh();

            // let ref mut p = lhs;
            let compute_ref = mk().local_stmt(P(mk().local(
                mk().mutbl().ident_ref_pat(&ptr_name),
                None as Option<P<Ty>>,
                Some(reference),
            )));
            stmts.push(compute_ref);

            let write = mk().unary_expr(ast::UnOp::Deref, mk().ident_expr(&ptr_name));

            Ok(WithStmts {
                stmts,
                val: (write.clone(), Some(read(write)?)),
            })
        }
    }
}
