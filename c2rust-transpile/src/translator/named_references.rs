#![deny(missing_docs)]
//! This module provides helper functions for generating rvalue and lvalues
//! corresponding to a single Rust expression.

use super::*;

/// Check if something is a valid Rust lvalue. Inspired by `librustc::ty::expr_is_lval`.
fn is_lvalue(e: &Expr) -> bool {
    use Expr::*;
    matches!(
        unparen(e),
        Unary(ExprUnary {
            op: syn::UnOp::Deref(_),
            ..
        }) | Path(..)
            | Field(..)
            | Index(..)
    )
}

/// Check if something is a side-effect free Rust lvalue.
fn is_simple_lvalue(e: &Expr) -> bool {
    use Expr::*;
    match *unparen(e) {
        Path(..) => true,
        Unary(ExprUnary {
            op: syn::UnOp::Deref(_),
            ref expr,
            ..
        })
        | Field(ExprField { base: ref expr, .. })
        | Index(ExprIndex { ref expr, .. }) => is_simple_lvalue(expr),
        _ => false,
    }
}

pub struct NamedReference<R> {
    pub lvalue: Box<Expr>,
    pub rvalue: R,
}

impl<R> NamedReference<R> {
    pub fn map_rvalue<S, F: Fn(R) -> S>(self, f: F) -> NamedReference<S> {
        let Self { lvalue, rvalue } = self;
        NamedReference {
            lvalue,
            rvalue: f(rvalue),
        }
    }
}

impl<'c> Translation<'c> {
    /// Get back a Rust lvalue corresponding to the expression passed in.
    ///
    /// Do not use the output lvalue expression more than once.
    pub fn name_reference_write(
        &self,
        ctx: ExprContext,
        reference: CExprId,
    ) -> TranslationResult<WithStmts<NamedReference<()>>> {
        self.name_reference(ctx, reference, false)
            .map(|ws| ws.map(|named_ref| named_ref.map_rvalue(|_| ())))
    }

    /// Get back a Rust (lvalue, rvalue) pair corresponding to the expression passed in.
    ///
    /// You may reuse either of these expressions.
    pub fn name_reference_write_read(
        &self,
        ctx: ExprContext,
        reference: CExprId,
    ) -> TranslationResult<WithStmts<NamedReference<Box<Expr>>>> {
        let msg: &str = "When called with `uses_read = true`, `name_reference` should always \
                         return an rvalue (something from which to read the memory location)";

        self.name_reference(ctx, reference, true)
            .map(|ws| ws.map(|named_ref| named_ref.map_rvalue(|rvalue| rvalue.expect(msg))))
    }

    /// Given the LHS access to a variable, produce the RHS one
    fn read(&self, reference_ty: CQualTypeId, write: Box<Expr>) -> TranslationResult<Box<Expr>> {
        if reference_ty.qualifiers.is_volatile {
            self.volatile_read(write, reference_ty)
        } else {
            Ok(write)
        }
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
    ) -> TranslationResult<WithStmts<NamedReference<Option<Box<Expr>>>>> {
        let reference_ty = self
            .ast_context
            .index(reference)
            .kind
            .get_qual_type()
            .ok_or_else(|| format_err!("bad reference type"))?;
        let read = |write| self.read(reference_ty, write);
        let reference = self.convert_expr(ctx.used(), reference)?;
        reference.and_then(|reference| {
            if !uses_read && is_lvalue(&reference) {
                Ok(WithStmts::new_val(NamedReference {
                    lvalue: reference,
                    rvalue: None,
                }))
            } else if is_simple_lvalue(&reference) {
                Ok(WithStmts::new_val(NamedReference {
                    lvalue: reference.clone(),
                    rvalue: Some(read(reference)?),
                }))
            } else {
                // This is the case where we explicitly need to factor out possible side-effects.

                let ptr_name = self.renamer.borrow_mut().fresh();

                // let ref mut p = lhs;
                let compute_ref = mk().local_stmt(Box::new(mk().local(
                    mk().mutbl().ident_ref_pat(&ptr_name),
                    None,
                    Some(reference),
                )));

                let write =
                    mk().unary_expr(UnOp::Deref(Default::default()), mk().ident_expr(&ptr_name));

                Ok(WithStmts::new(
                    vec![compute_ref],
                    NamedReference {
                        lvalue: write.clone(),
                        rvalue: Some(read(write)?),
                    },
                ))
            }
        })
    }
}
