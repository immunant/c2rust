use c2rust_ast_builder::mk;
use proc_macro2::Span;
use syn::{Expr, ImplItem};

use crate::{
    c_ast::{self, iterators::SomeId},
    diagnostics::TranslationResult,
    translator::{signed_int_expr, ConvertedDecl, ExprContext, Translation},
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
        variants: &[CEnumConstantId],
    ) -> TranslationResult<ConvertedDecl> {
        let enum_name = &self
            .type_converter
            .borrow()
            .resolve_decl_name(enum_id)
            .expect("Enums should already be renamed");

        let field = mk()
            .pub_()
            .enum_field(self.convert_type(integral_type.ctype)?);
        let item = mk()
            .span(span)
            .call_attr("derive", vec!["Clone", "Copy"])
            .call_attr("repr", vec!["transparent"])
            .pub_()
            .struct_item(enum_name, vec![field], true);

        if variants.is_empty() {
            return Ok(ConvertedDecl::Item(item));
        }

        let self_ty = mk().ident_ty("Self");
        let constants: Vec<ImplItem> = variants
            .iter()
            .map(|&enum_constant_id| {
                let span = self
                    .get_span(SomeId::Decl(enum_constant_id))
                    .unwrap_or_else(Span::call_site);

                let (name, value) = match self.ast_context[enum_constant_id].kind {
                    CDeclKind::EnumConstant {
                        ref name, value, ..
                    } => (name, value),
                    _ => panic!("{:?} does not point to an enum variant", enum_constant_id),
                };
                let name = self.type_converter.borrow_mut().declare_field_name(
                    enum_id,
                    enum_constant_id,
                    name,
                );

                let val = match value {
                    ConstIntExpr::I(value) => signed_int_expr(value),
                    ConstIntExpr::U(value) => mk().lit_expr(mk().int_unsuffixed_lit(value as u128)),
                };
                let init = self.enum_constructor_expr(enum_id, val, true);

                mk().span(span)
                    .pub_()
                    .const_impl_item(name, self_ty.clone(), init)
            })
            .collect();
        let impl_block = mk().impl_item(mk().ident_ty(enum_name), constants);

        Ok(ConvertedDecl::Items(vec![item, impl_block]))
    }

    pub fn convert_enum_zero_initializer(&self, enum_id: CEnumId) -> WithStmts<Box<Expr>> {
        WithStmts::new_val(self.enum_for_i64(enum_id, 0))
    }

    /// Translates a `DeclRef` for an `EnumConstant`.
    pub fn convert_enum_constant_decl_ref(
        &self,
        enum_constant_id: CEnumConstantId,
        target_cty: CTypeId,
    ) -> TranslationResult<Box<Expr>> {
        let enum_id = self.ast_context.parents[&enum_constant_id];
        let val = self.enum_constant_expr(enum_id, enum_constant_id);
        self.convert_cast_from_enum(target_cty, val)
    }

    /// Translate a cast where the source type, but not the target type, is an `enum` type.
    pub fn convert_cast_from_enum(
        &self,
        target_cty: CTypeId,
        mut val: Box<Expr>,
    ) -> TranslationResult<Box<Expr>> {
        // Convert it to the expected integral type.
        val = self.integer_from_enum(val);

        let ty = self.convert_type(target_cty)?;
        val = mk().cast_expr(val, ty);

        Ok(val)
    }

    /// Gets the inner integral value of an enum value.
    pub fn integer_from_enum(&self, val: Box<Expr>) -> Box<Expr> {
        mk().anon_field_expr(val, 0)
    }

    /// Translates a cast where the target type is an `enum` type.
    pub fn convert_cast_to_enum(
        &self,
        ctx: ExprContext,
        enum_id: CEnumId,
        expr: Option<CExprId>,
        source_cty: CTypeId,
        mut val: Box<Expr>,
    ) -> TranslationResult<Box<Expr>> {
        if let Some(expr) = expr {
            match self.ast_context[expr].kind {
                // This is the case of finding a variable which is an `EnumConstant` of the same
                // enum we are casting to. Here, we can just remove the extraneous cast instead of
                // generating a new one.
                CExprKind::DeclRef(_, enum_constant_id, _)
                    if self.is_variant_of_enum(enum_id, enum_constant_id) =>
                {
                    // `enum`s shouldn't need portable `override_ty`s.
                    let expr_is_macro = matches!(
                        self.convert_const_macro_expansion(ctx, expr, None),
                        Ok(Some(_))
                    );

                    // If this DeclRef expanded to a const macro, we actually need to insert a cast,
                    // because the translation of a const macro skips implicit casts in its context.
                    if !expr_is_macro {
                        return Ok(self.enum_constant_expr(enum_id, enum_constant_id));
                    }
                }

                CExprKind::Literal(_, CLiteral::Integer(i, _)) => {
                    return Ok(self.enum_for_i64(enum_id, i as i64));
                }

                CExprKind::Unary(_, c_ast::UnOp::Negate, subexpr_id, _) => {
                    if let &CExprKind::Literal(_, CLiteral::Integer(i, _)) =
                        &self.ast_context[subexpr_id].kind
                    {
                        return Ok(self.enum_for_i64(enum_id, -(i as i64)));
                    }
                }

                _ => {}
            }
        }

        let integral_type = self.enum_integral_type(enum_id);
        let mut needs_cast = true;

        // C allows directly casting from enum to enum, but in Rust we need to use
        // the inner integer value as an intermediate.
        if let CTypeKind::Enum(source_enum_id) = self.ast_context.resolve_type(source_cty).kind {
            val = self.integer_from_enum(val);
            needs_cast = integral_type.ctype != self.enum_integral_type(source_enum_id).ctype;
        }

        if needs_cast {
            let ty = self.convert_type(integral_type.ctype)?;
            val = mk().cast_expr(val, ty);
        }

        Ok(self.enum_constructor_expr(enum_id, val, false))
    }

    /// Given an integer value this attempts to either generate the corresponding enum
    /// variant directly, otherwise it converts a number to the enum type.
    fn enum_for_i64(&self, enum_id: CEnumId, value: i64) -> Box<Expr> {
        if let Some(enum_constant_id) = self.enum_variant_for_i64(enum_id, value) {
            return self.enum_constant_expr(enum_id, enum_constant_id);
        }

        let underlying_type_id = self.enum_integral_type(enum_id);
        let value = match self.ast_context.resolve_type(underlying_type_id.ctype).kind {
            CTypeKind::UInt => mk().lit_expr(mk().int_unsuffixed_lit((value as u32) as u128)),
            CTypeKind::ULong => mk().lit_expr(mk().int_unsuffixed_lit((value as u64) as u128)),
            _ => signed_int_expr(value),
        };

        self.enum_constructor_expr(enum_id, value, false)
    }

    /// Returns the id of the variant of `enum_id` whose value matches `value`, if any.
    fn enum_variant_for_i64(&self, enum_id: CEnumId, value: i64) -> Option<CEnumConstantId> {
        let variants = match self.ast_context[enum_id].kind {
            CDeclKind::Enum { ref variants, .. } => variants,
            _ => panic!("{:?} does not point to an `enum` declaration", enum_id),
        };

        variants
            .iter()
            .copied()
            .find(|&variant_id| match self.ast_context[variant_id].kind {
                CDeclKind::EnumConstant { value: v, .. } => {
                    v == ConstIntExpr::I(value) || v == ConstIntExpr::U(value as u64)
                }
                _ => panic!("{:?} does not point to an enum variant", variant_id),
            })
    }

    fn enum_constructor_expr(
        &self,
        enum_id: CEnumId,
        value: Box<Expr>,
        use_self_ty: bool,
    ) -> Box<Expr> {
        let func = if use_self_ty {
            mk().ident_expr("Self")
        } else {
            let enum_name = self
                .type_converter
                .borrow()
                .resolve_decl_name(enum_id)
                .unwrap();
            self.add_import(enum_id, &enum_name);
            mk().ident_expr(enum_name)
        };

        mk().call_expr(func, vec![value])
    }

    fn enum_constant_expr(&self, enum_id: CEnumId, enum_constant_id: CEnumConstantId) -> Box<Expr> {
        let enum_name = self
            .type_converter
            .borrow()
            .resolve_decl_name(enum_id)
            .unwrap();
        let name = self
            .type_converter
            .borrow()
            .resolve_field_name(Some(enum_id), enum_constant_id)
            .unwrap();

        self.add_import(enum_id, &enum_name);
        mk().path_expr(vec![enum_name, name])
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
