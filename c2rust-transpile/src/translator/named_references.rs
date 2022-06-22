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
    ) -> Result<WithStmts<Box<Expr>>, TranslationError> {
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
    ) -> Result<WithStmts<(Box<Expr>, Box<Expr>)>, TranslationError> {
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
    ) -> Result<WithStmts<(Box<Expr>, Option<Box<Expr>>)>, TranslationError> {
        let reference_ty = self
            .ast_context
            .index(reference)
            .kind
            .get_qual_type()
            .ok_or_else(|| format_err!("bad reference type"))?;
        let reference = self.convert_expr(ctx.used(), reference)?;
        reference.and_then(|reference| {
            /// Check if something is a valid Rust lvalue. Inspired by `librustc::ty::expr_is_lval`.
            fn is_lvalue(e: &Box<Expr>) -> bool {
                use Expr::*;
                matches!(
                    unparen(e).as_ref(),
                    Unary(ExprUnary {
                        op: syn::UnOp::Deref(_),
                        ..
                    }) | Path(..)
                        | Field(..)
                        | Index(..)
                )
            }

            // Check if something is a side-effect free Rust lvalue.
            fn is_simple_lvalue(e: &Box<Expr>) -> bool {
                match **unparen(e) {
                    Expr::Path(..) => true,
                    Expr::Unary(ExprUnary {
                        op: syn::UnOp::Deref(_),
                        ref expr,
                        ..
                    })
                    | Expr::Field(ExprField { base: ref expr, .. })
                    | Expr::Index(ExprIndex { ref expr, .. }) => is_simple_lvalue(expr),
                    _ => false,
                }
            }

            // Given the LHS access to a variable, produce the RHS one
            let read = |write: Box<Expr>| -> Result<Box<Expr>, TranslationError> {
                if reference_ty.qualifiers.is_volatile {
                    self.volatile_read(&write, reference_ty)
                } else {
                    Ok(write)
                }
            };

            if !uses_read && is_lvalue(&reference) {
                Ok(WithStmts::new_val((reference, None)))
            } else if is_simple_lvalue(&reference) {
                Ok(WithStmts::new_val((
                    reference.clone(),
                    Some(read(reference)?),
                )))
            } else {
                // This is the case where we explicitly need to factor out possible side-effects.

                let ptr_name = self.renamer.borrow_mut().fresh();

                // let ref mut p = lhs;
                let compute_ref = mk().local_stmt(Box::new(mk().local(
                    mk().mutbl().ident_ref_pat(&ptr_name),
                    None as Option<Box<Type>>,
                    Some(reference),
                )));

                let write =
                    mk().unary_expr(UnOp::Deref(Default::default()), mk().ident_expr(&ptr_name));

                Ok(WithStmts::new(
                    vec![compute_ref],
                    (write.clone(), Some(read(write)?)),
                ))
            }
        })
    }
}
