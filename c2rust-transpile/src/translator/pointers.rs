use std::ops::Index;

use c2rust_ast_builder::{mk, properties::Mutability};
use syn::{BinOp, Expr, Type, UnOp};

use crate::{
    diagnostics::{TranslationError, TranslationResult},
    translator::{cast_int, ExprContext, Translation},
    with_stmts::WithStmts,
    CExprId, CExprKind, CLiteral, CQualTypeId, CTypeId,
};

impl<'c> Translation<'c> {
    pub fn convert_address_of(
        &self,
        ctx: ExprContext,
        arg: Option<CExprId>,
        arg_cty: CQualTypeId,
        pointer_cty: CQualTypeId,
        mut val: WithStmts<Box<Expr>>,
        is_array_decay: bool,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let arg_expr_kind = arg.map(|arg| {
            let arg = self.ast_context.unwrap_predefined_ident(arg);
            &self.ast_context.index(arg).kind
        });
        let pointee_cty = self
            .ast_context
            .get_pointee_qual_type(pointer_cty.ctype)
            .ok_or_else(|| TranslationError::generic("Address-of should return a pointer"))?;
        let arg_is_macro = arg.map_or(false, |arg| {
            matches!(
                self.convert_const_macro_expansion(ctx, arg, None),
                Ok(Some(_))
            )
        });

        let mut needs_cast = false;
        let mut ref_cast_pointee_ty = None;
        let mutbl = if pointee_cty.qualifiers.is_const {
            Mutability::Immutable
        } else if ctx.is_static {
            // static variable initializers aren't able to use &mut, so we work around that
            // by using & and an extra cast through & to *const to *mut
            // TODO: Rust 1.83: Allowed, so this can be removed.
            needs_cast = true;
            Mutability::Immutable
        } else {
            Mutability::Mutable
        };

        // String literals are translated with a transmute, which produces a temporary.
        // Taking the address of a temporary leaves a dangling pointer. So instead,
        // cast the string literal directly so that its 'static lifetime is preserved.
        if let (
            Some(&CExprKind::Literal(literal_cty, CLiteral::String(ref bytes, element_size @ 1))),
            false,
        ) = (arg_expr_kind, arg_is_macro)
        {
            let bytes_padded = self.string_literal_bytes(literal_cty.ctype, bytes, element_size);
            let len = bytes_padded.len();
            val = WithStmts::new_val(mk().lit_expr(bytes_padded));

            if is_array_decay {
                ref_cast_pointee_ty = Some(mk().ident_ty("u8"));
            } else {
                ref_cast_pointee_ty =
                    Some(mk().array_ty(mk().ident_ty("u8"), mk().lit_expr(len as u128)));
            }
            needs_cast = true;
        } else {
            let arg_cty_kind = &self.ast_context.resolve_type(arg_cty.ctype).kind;

            if is_array_decay {
                let method = match mutbl {
                    Mutability::Mutable => "as_mut_ptr",
                    Mutability::Immutable => "as_ptr",
                };
                val = val.map(|val| mk().method_call_expr(val, method, vec![]));

                // If the target pointee type is different from the source element type,
                // then we need to cast the ptr type as well.
                if arg_cty_kind.element_ty().map_or(false, |arg_element_cty| {
                    arg_element_cty != pointee_cty.ctype
                }) {
                    needs_cast = true;
                }
            } else {
                val = val.map(|val| mk().set_mutbl(mutbl).addr_of_expr(val));

                // Add an intermediate reference-to-pointer cast if the context needs
                // reference-to-pointer decay, or if another cast follows.
                if ctx.decay_ref.is_yes() || needs_cast {
                    ref_cast_pointee_ty = Some(self.convert_pointee_type(arg_cty.ctype)?);
                }
            }
        }

        // Perform an intermediate reference-to-pointer cast if needed.
        // TODO: Rust 1.76: Use `ptr::from_ref`.
        if let Some(pointee_ty) = ref_cast_pointee_ty {
            val = val.map(|val| mk().cast_expr(val, mk().set_mutbl(mutbl).ptr_ty(pointee_ty)));
        }

        // Perform a final cast to the target type if needed.
        if needs_cast {
            let pointer_ty = self.convert_type(pointer_cty.ctype)?;
            val = val.map(|val| mk().cast_expr(val, pointer_ty));
        }

        Ok(val)
    }

    /// Construct an expression for a NULL at any type, including forward declarations,
    /// function pointers, and normal pointers.
    pub fn null_ptr(&self, type_id: CTypeId, is_static: bool) -> TranslationResult<Box<Expr>> {
        if self.ast_context.is_function_pointer(type_id) {
            return Ok(mk().path_expr(vec!["None"]));
        }

        let pointee = self
            .ast_context
            .get_pointee_qual_type(type_id)
            .ok_or_else(|| TranslationError::generic("null_ptr requires a pointer"))?;
        let ty = self.convert_type(type_id)?;
        let mut zero = mk().lit_expr(mk().int_unsuffixed_lit(0));
        if is_static && !pointee.qualifiers.is_const {
            let ty_ = self.convert_pointee_type(pointee.ctype)?;
            zero = mk().cast_expr(zero, mk().ptr_ty(ty_));
        }
        Ok(mk().cast_expr(zero, ty))
    }

    fn convert_pointee_type(&self, type_id: CTypeId) -> TranslationResult<Box<Type>> {
        self.import_type(type_id);

        self.type_converter
            .borrow_mut()
            .convert_pointee(&self.ast_context, type_id)
    }
}

/// Pointer offset that casts its argument to isize
pub fn pointer_offset(
    ptr: Box<Expr>,
    offset: Box<Expr>,
    multiply_by: Option<Box<Expr>>,
    neg: bool,
    mut deref: bool,
) -> Box<Expr> {
    let mut offset = cast_int(offset, "isize", false);

    if let Some(mul) = multiply_by {
        let mul = cast_int(mul, "isize", false);
        offset = mk().binary_expr(BinOp::Mul(Default::default()), offset, mul);
        deref = false;
    }

    if neg {
        offset = mk().unary_expr(UnOp::Neg(Default::default()), offset);
    }

    let res = mk().method_call_expr(ptr, "offset", vec![offset]);
    if deref {
        mk().unary_expr(UnOp::Deref(Default::default()), res)
    } else {
        res
    }
}
