use c2rust_ast_builder::mk;
use failure::format_err;
use proc_macro2::Span;
use syn::{Expr, Pat};

use crate::c_ast::iterators::SomeId;
use crate::translator::IdOrExpr;
use crate::{
    diagnostics::TranslationResult,
    translator::{signed_int_expr, ConvertedDecl, ExprContext, Translation, TranslationError},
    with_stmts::WithStmts,
    CDeclKind, CEnumConstantId, CEnumId, CQualTypeId, CTypeId, CTypeKind, ConstIntExpr,
};

impl<'c> Translation<'c> {
    pub fn convert_enum(
        &self,
        enum_id: CEnumId,
        span: Span,
        underlying_type_id: CQualTypeId,
        variants: &[CEnumConstantId],
    ) -> TranslationResult<ConvertedDecl> {
        let enum_name = &self
            .type_converter
            .borrow()
            .resolve_decl_name(enum_id)
            .expect("Enums should already be renamed");
        let underlying_type_rs = self.convert_type(underlying_type_id.ctype)?;
        let field = mk().pub_().enum_field(underlying_type_rs);
        let enum_item = mk()
            .span(span)
            .call_attr("derive", vec!["Clone", "Copy", "PartialEq", "Eq"])
            .call_attr("repr", vec!["transparent"])
            .pub_()
            .struct_item(enum_name, vec![field], true);

        if variants.is_empty() {
            return Ok(ConvertedDecl::Item(enum_item));
        }

        let underlying_type_kind = &self.ast_context.resolve_type(underlying_type_id.ctype).kind;
        let underlying_type_is_bool = underlying_type_kind.is_bool();
        let enum_type = mk().ident_ty("Self");
        let constants = variants
            .iter()
            .map(|&enum_constant_id| {
                let name = match self.ast_context[enum_constant_id].kind {
                    CDeclKind::EnumConstant { ref name, .. } => name,
                    _ => panic!("{:?} does not point to an enum variant", enum_constant_id),
                };
                let name_rs = self.type_converter.borrow_mut().declare_field_name(
                    enum_id,
                    enum_constant_id,
                    name,
                );
                let (span, init) =
                    self.make_enum_constant_init(enum_constant_id, underlying_type_is_bool);
                mk().span(span)
                    .pub_()
                    .const_impl_item(name_rs, enum_type.clone(), init)
            })
            .collect();

        let impl_block = mk()
            .span(span)
            .impl_item(mk().ident_ty(enum_name), constants);
        Ok(ConvertedDecl::Items(vec![enum_item, impl_block]))
    }

    fn make_enum_constant_init(
        &self,
        enum_constant_id: CEnumConstantId,
        is_bool: bool,
    ) -> (Span, Box<Expr>) {
        let value = match self.ast_context[enum_constant_id].kind {
            CDeclKind::EnumConstant { value, .. } => value,
            _ => panic!("{:?} does not point to an enum variant", enum_constant_id),
        };
        let value_rs = match value {
            ConstIntExpr::I(value) => signed_int_expr(value),
            ConstIntExpr::U(value) => {
                let lit = if is_bool {
                    mk().bool_lit(value != 0)
                } else {
                    mk().int_unsuffixed_lit(value as u128)
                };

                mk().lit_expr(lit)
            }
        };
        let enum_id = self.ast_context.parents[&enum_constant_id];
        let init = self.enum_constructor_expr(enum_id, value_rs, true);
        let span = self
            .get_span(SomeId::Decl(enum_constant_id))
            .unwrap_or_else(Span::call_site);

        (span, init)
    }

    pub fn convert_enum_zero_initializer(&self, enum_id: CEnumId) -> WithStmts<Box<Expr>> {
        WithStmts::new_val(self.enum_for_i64(enum_id, 0))
    }

    /// Translates a `DeclRef` for an `EnumConstant`.
    pub fn convert_enum_constant_decl_ref(
        &self,
        ctx: ExprContext,
        expr_type_id: CQualTypeId,
        enum_constant_id: CEnumConstantId,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let mut val = self.enum_constant_expr(enum_constant_id);

        if self.enum_constant_matches_type(expr_type_id.ctype, enum_constant_id) {
            return Ok(WithStmts::new_val(val));
        }

        val = self.make_enum_to_underlying_cast(val);

        // Add a cast to the expected integral type.
        let enum_id = self.ast_context.parents[&enum_constant_id];
        let underlying_type_id = self.enum_underlying_type(enum_id);
        self.make_cast(ctx, underlying_type_id, expr_type_id, val.into())
    }

    /// Translate a cast where the source type is an `enum` type.
    pub(crate) fn convert_cast_from_enum(
        &self,
        ctx: ExprContext,
        target_type_id: CQualTypeId,
        expr: impl Into<IdOrExpr>,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        if ctx.is_pattern {
            return Err(TranslationError::generic(
                "cast from enum is not supported in patterns",
            ));
        }

        let expr = expr.into();
        let source_type_id = expr
            .type_id(&self.ast_context)
            .ok_or_else(|| format_err!("Invalid expression type"))?;

        // First extract the enum's underlying type...
        let mut expr_rs = self.convert_expr(ctx, expr, None)?;
        expr_rs = expr_rs.map(|expr_rs| self.make_enum_to_underlying_cast(expr_rs));

        // Cast from the underlying type to the expected type.
        let enum_id = self.enum_id_from_type(source_type_id.ctype);
        let underlying_type_id = self.enum_underlying_type(enum_id);
        self.make_cast(ctx, underlying_type_id, target_type_id, expr_rs)
    }

    /// Gets the underlying value of an enum value.
    pub fn make_enum_to_underlying_cast(&self, val: Box<Expr>) -> Box<Expr> {
        mk().anon_field_expr(val, 0)
    }

    /// Translates a cast where the target type is an `enum` type.
    pub(crate) fn convert_cast_to_enum(
        &self,
        ctx: ExprContext,
        enum_id: CEnumId,
        expr: impl Into<IdOrExpr>,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let expr = expr.into();
        let source_type_id = expr
            .type_id(&self.ast_context)
            .ok_or_else(|| format_err!("Invalid expression type"))?;

        // First cast the value to the enum's underlying type.
        let mut expr_rs = self.convert_expr(ctx, expr, None)?;
        let underlying_type_id = self.enum_underlying_type(enum_id);
        expr_rs = self.make_cast(ctx, source_type_id, underlying_type_id, expr_rs)?;

        // Then construct an enum from the underlying value.
        Ok(expr_rs.map(|expr_rs| self.enum_constructor_expr(enum_id, expr_rs, false)))
    }

    /// Given an integer value this attempts to either generate the corresponding enum
    /// variant directly, otherwise it converts a number to the enum type.
    fn enum_for_i64(&self, enum_id: CEnumId, value: i64) -> Box<Expr> {
        if let Some(enum_constant_id) = self.enum_variant_for_i64(enum_id, value) {
            return self.enum_constant_expr(enum_constant_id);
        }

        let underlying_type_id = self.enum_underlying_type(enum_id);
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

    fn enum_constant_expr(&self, enum_constant_id: CEnumConstantId) -> Box<Expr> {
        let enum_id = self.ast_context.parents[&enum_constant_id];
        let enum_name = self
            .type_converter
            .borrow()
            .resolve_decl_name(enum_id)
            .unwrap();
        let enum_constant_name = self
            .type_converter
            .borrow()
            .resolve_field_name(Some(enum_id), enum_constant_id)
            .unwrap();

        self.add_import(enum_id, &enum_name);
        mk().path_expr(vec![enum_name, enum_constant_name])
    }

    pub(crate) fn enum_constructor_expr(
        &self,
        enum_id: CEnumId,
        value: Box<Expr>,
        use_self_type: bool,
    ) -> Box<Expr> {
        let func = if use_self_type {
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

    pub(crate) fn enum_constructor_pat(&self, enum_id: CEnumId, value: Pat) -> Pat {
        let enum_name = self
            .type_converter
            .borrow()
            .resolve_decl_name(enum_id)
            .unwrap();
        self.add_import(enum_id, &enum_name);

        mk().tuple_struct_pat(enum_name.as_str(), None, vec![value])
    }

    fn enum_id_from_type(&self, type_id: CTypeId) -> CEnumId {
        match self.ast_context.resolve_type(type_id).kind {
            CTypeKind::Enum(enum_id) => enum_id,
            _ => panic!("{type_id:?} is not an enum type"),
        }
    }

    pub(crate) fn enum_constant_matches_type(
        &self,
        type_id: CTypeId,
        enum_constant_id: CEnumConstantId,
    ) -> bool {
        let CTypeKind::Enum(type_enum_id) = self.ast_context.resolve_type(type_id).kind else {
            return false;
        };
        let constant_enum_id = self.ast_context.parents[&enum_constant_id];
        type_enum_id == constant_enum_id
    }

    pub(crate) fn enum_underlying_type(&self, enum_id: CEnumId) -> CQualTypeId {
        match self.ast_context[enum_id].kind {
            CDeclKind::Enum {
                underlying_type_id: Some(underlying_type_id),
                ..
            } => underlying_type_id,
            _ => panic!(
                "{:?} does not point to a non-extern `enum` declaration",
                enum_id
            ),
        }
    }
}
