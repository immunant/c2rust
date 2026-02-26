use std::ops::Index;

use c2rust_ast_builder::mk;
use proc_macro2::Span;
use syn::{Expr, ExprCast, Type};

use crate::{
    c_ast,
    diagnostics::TranslationResult,
    translator::{signed_int_expr, unparen, ConvertedDecl, Translation},
    with_stmts::WithStmts,
    CDeclKind, CEnumConstantId, CEnumId, CExprId, CExprKind, CLiteral, CQualTypeId, CTypeId,
    CTypeKind, ConstIntExpr,
};

impl<'c> Translation<'c> {
    pub fn convert_enum(
        &self,
        enum_id: CEnumId,
        span: Span,
        integral_type: CQualTypeId,
    ) -> TranslationResult<ConvertedDecl> {
        let enum_name = &self
            .type_converter
            .borrow()
            .resolve_decl_name(enum_id)
            .expect("Enums should already be renamed");
        let ty = self.convert_type(integral_type.ctype)?;
        Ok(ConvertedDecl::Item(
            mk().span(span).pub_().type_item(enum_name, ty),
        ))
    }

    pub fn convert_enum_constant(
        &self,
        enum_constant_id: CEnumConstantId,
        span: Span,
        value: ConstIntExpr,
    ) -> TranslationResult<ConvertedDecl> {
        let name = self
            .renamer
            .borrow_mut()
            .get(&enum_constant_id)
            .expect("Enum constant not named");
        let enum_id = self.ast_context.parents[&enum_constant_id];
        let enum_name = self
            .type_converter
            .borrow()
            .resolve_decl_name(enum_id)
            .expect("Enums should already be renamed");
        self.add_import(enum_id, &enum_name);

        let ty = mk().ident_ty(enum_name);
        let val = match value {
            ConstIntExpr::I(value) => signed_int_expr(value),
            ConstIntExpr::U(value) => mk().lit_expr(mk().int_unsuffixed_lit(value as u128)),
        };

        Ok(ConvertedDecl::Item(
            mk().span(span).pub_().const_item(name, ty, val),
        ))
    }

    pub fn convert_enum_zero_initializer(&self, type_id: CTypeId) -> WithStmts<Box<Expr>> {
        WithStmts::new_val(self.enum_for_i64(type_id, 0))
    }

    /// Translate a cast where the source type, but not the target type, is an `enum` type.
    pub fn convert_cast_from_enum(
        &self,
        target_cty: CTypeId,
        val: Box<Expr>,
    ) -> TranslationResult<Box<Expr>> {
        // Convert it to the expected integral type.
        // When modifying this, look at `Translation::enum_cast` -
        // this function assumes `DeclRef`'s to `EnumConstants`'s will translate to casts.
        let ty = self.convert_type(target_cty)?;
        Ok(mk().cast_expr(val, ty))
    }

    /// Translate a cast where the target type is an `enum` type.
    ///
    /// When translating variable references to `EnumConstant`s, we always insert casts to the
    /// expected type. In C, `EnumConstant`s have some integral type, _not_ the enum type. However,
    /// if we then immediately have a cast to convert this variable back into an enum type, we would
    /// like to produce Rust with _no_ casts. This function handles this simplification.
    pub fn convert_cast_to_enum(
        &self,
        enum_type_id: CTypeId,
        enum_id: CEnumId,
        expr: CExprId,
        val: WithStmts<Box<Expr>>,
        target_ty: Box<Type>,
    ) -> WithStmts<Box<Expr>> {
        // Extract the IDs of the `EnumConstant` decls underlying the enum.
        match self.ast_context.index(expr).kind {
            // This is the case of finding a variable which is an `EnumConstant` of the same enum
            // we are casting to. Here, we can just remove the extraneous cast instead of generating
            // a new one.
            CExprKind::DeclRef(_, enum_constant_id, _)
                if self.is_variant_of_enum(enum_id, enum_constant_id) =>
            {
                return val.map(|x| match *unparen(&x) {
                    Expr::Cast(ExprCast { ref expr, .. }) => expr.clone(),
                    // If this DeclRef expanded to a const macro, we actually need to insert a cast,
                    // because the translation of a const macro skips implicit casts in its context.
                    Expr::Path(..) => mk().cast_expr(x, target_ty),
                    _ => panic!(
                        "DeclRef {:?} of enum {:?} is not cast: {x:?}",
                        expr, enum_id
                    ),
                });
            }

            CExprKind::Literal(_, CLiteral::Integer(i, _)) => {
                return val.map(|_| self.enum_for_i64(enum_type_id, i as i64));
            }

            CExprKind::Unary(_, c_ast::UnOp::Negate, subexpr_id, _) => {
                if let &CExprKind::Literal(_, CLiteral::Integer(i, _)) =
                    &self.ast_context[subexpr_id].kind
                {
                    return val.map(|_| self.enum_for_i64(enum_type_id, -(i as i64)));
                }
            }

            // In all other cases, a cast to an enum requires a `transmute` - Rust enums cannot be
            // converted into integral types as easily as C ones.
            _ => {}
        }

        val.map(|x| mk().cast_expr(x, target_ty))
    }

    /// Given an integer value this attempts to either generate the corresponding enum
    /// variant directly, otherwise it transmutes a number to the enum type.
    fn enum_for_i64(&self, enum_type_id: CTypeId, value: i64) -> Box<Expr> {
        let enum_id = match self.ast_context.resolve_type(enum_type_id).kind {
            CTypeKind::Enum(enum_id) => enum_id,
            _ => panic!("{:?} does not point to an `enum` type", enum_type_id),
        };

        let variants = match self.ast_context[enum_id].kind {
            CDeclKind::Enum { ref variants, .. } => variants,
            _ => panic!("{:?} does not point to an `enum` declaration", enum_id),
        };

        for &variant_id in variants {
            match self.ast_context[variant_id].kind {
                CDeclKind::EnumConstant { value: v, .. } => {
                    if v == ConstIntExpr::I(value) || v == ConstIntExpr::U(value as u64) {
                        let name = self.renamer.borrow().get(&variant_id).unwrap();

                        // Import the enum variant if needed
                        self.add_import(variant_id, &name);
                        return mk().path_expr(vec![name]);
                    }
                }
                _ => panic!("{:?} does not point to an enum variant", variant_id),
            }
        }

        let underlying_type_id = self.enum_integral_type(enum_id);
        let value = match self.ast_context.resolve_type(underlying_type_id.ctype).kind {
            CTypeKind::UInt => mk().lit_expr(mk().int_unsuffixed_lit((value as u32) as u128)),
            CTypeKind::ULong => mk().lit_expr(mk().int_unsuffixed_lit((value as u64) as u128)),
            _ => signed_int_expr(value),
        };

        let target_ty = self.convert_type(enum_type_id).unwrap();
        mk().cast_expr(value, target_ty)
    }

    fn is_variant_of_enum(&self, enum_id: CEnumId, enum_constant_id: CEnumConstantId) -> bool {
        let variants = match self.ast_context[enum_id].kind {
            CDeclKind::Enum { ref variants, .. } => variants,
            _ => panic!("{:?} does not point to an `enum` declaration", enum_id),
        };

        variants.contains(&enum_constant_id)
    }

    fn enum_integral_type(&self, enum_id: CEnumId) -> CQualTypeId {
        match self.ast_context[enum_id].kind {
            CDeclKind::Enum {
                integral_type: Some(integral_type),
                ..
            } => integral_type,
            _ => panic!(
                "{:?} does not point to an integral `enum` declaration",
                enum_id
            ),
        }
    }
}
