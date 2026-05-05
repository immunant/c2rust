use c2rust_ast_builder::mk;
use proc_macro2::Span;
use syn::Expr;

use crate::c_ast::iterators::SomeId;
use crate::c_ast::CUnOp;
use crate::{
    diagnostics::TranslationResult,
    translator::{signed_int_expr, ConvertedDecl, EnumMode, ExprContext, Translation},
    with_stmts::WithStmts,
    CDeclKind, CEnumConstantId, CEnumId, CExprId, CExprKind, CLiteral, CQualTypeId, CTypeKind,
    ConstIntExpr,
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
        let integral_type_rs = self.convert_type(integral_type.ctype)?;
        let item = match self.tcfg.enum_mode {
            EnumMode::NewType => {
                let field = mk().pub_().enum_field(integral_type_rs);
                mk().span(span)
                    .call_attr("derive", vec!["Clone", "Copy"])
                    .call_attr("repr", vec!["transparent"])
                    .pub_()
                    .struct_item(enum_name, vec![field], true)
            }

            EnumMode::Consts => mk()
                .span(span)
                .pub_()
                .type_item(enum_name, integral_type_rs),
        };

        Ok(ConvertedDecl::Item(item))
    }

    pub fn convert_enum_constant(
        &self,
        enum_constant_id: CEnumConstantId,
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

        let ty = mk().ident_ty(enum_name);
        let (span, init) = self.make_enum_constant_init(enum_constant_id);

        Ok(ConvertedDecl::Item(
            mk().span(span).pub_().const_item(name, ty, init),
        ))
    }

    fn make_enum_constant_init(&self, enum_constant_id: CEnumConstantId) -> (Span, Box<Expr>) {
        let value = match self.ast_context[enum_constant_id].kind {
            CDeclKind::EnumConstant { value, .. } => value,
            _ => panic!("{:?} does not point to an enum variant", enum_constant_id),
        };
        let value_rs = match value {
            ConstIntExpr::I(value) => signed_int_expr(value),
            ConstIntExpr::U(value) => mk().lit_expr(mk().int_unsuffixed_lit(value as u128)),
        };
        let enum_id = self.ast_context.parents[&enum_constant_id];
        let init = self.enum_constructor_expr(enum_id, value_rs);
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
        enum_constant_id: CEnumConstantId,
        target_type_id: CQualTypeId,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let enum_id = self.ast_context.parents[&enum_constant_id];
        let val = self.enum_constant_expr(enum_constant_id);

        // Add a cast to the expected integral type.
        self.convert_cast_from_enum(ctx, enum_id, target_type_id, val)
    }

    /// Translate a cast where the source type, but not the target type, is an `enum` type.
    pub fn convert_cast_from_enum(
        &self,
        ctx: ExprContext,
        enum_id: CEnumId,
        target_cty: CQualTypeId,
        mut val: Box<Expr>,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        match self.tcfg.enum_mode {
            // First extract the enum's inner type...
            EnumMode::NewType => val = self.integer_from_enum(val),
            EnumMode::Consts => {}
        }

        // Cast from the enum's integral type to the expected integral type.
        let source_cty = self.enum_integral_type(enum_id);
        self.convert_cast(
            ctx,
            source_cty,
            target_cty,
            WithStmts::new_val(val),
            None,
            None,
            None,
        )
    }

    /// Gets the inner integral value of an enum value.
    pub fn integer_from_enum(&self, val: Box<Expr>) -> Box<Expr> {
        match self.tcfg.enum_mode {
            EnumMode::NewType => mk().anon_field_expr(val, 0),
            EnumMode::Consts => val,
        }
    }

    /// Translates a cast where the target type is an `enum` type.
    pub fn convert_cast_to_enum(
        &self,
        ctx: ExprContext,
        mut source_cty: CQualTypeId,
        enum_id: CEnumId,
        expr: Option<CExprId>,
        mut val: Box<Expr>,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        if let Some(expr) = expr {
            match self.ast_context[expr].kind {
                // This is the case of finding a variable which is an `EnumConstant` of the same
                // enum we are casting to. Here, we can just remove the extraneous cast instead of
                // generating a new one.
                CExprKind::DeclRef(_, enum_constant_id, _)
                    if self.is_variant_of_enum(enum_id, enum_constant_id) =>
                {
                    // `enum`s shouldn't need portable `override_ty`s.
                    let expr_is_macro = self.expr_is_expanded_macro(ctx, expr, None);

                    // If this DeclRef expanded to a const macro, we actually need to insert a cast,
                    // because the translation of a const macro skips implicit casts in its context.
                    if !expr_is_macro {
                        val = self.enum_constant_expr(enum_constant_id);
                        return Ok(WithStmts::new_val(val));
                    }
                }

                CExprKind::Literal(_, CLiteral::Integer(i, _)) => {
                    val = self.enum_for_i64(enum_id, i as i64);
                    return Ok(WithStmts::new_val(val));
                }

                CExprKind::Unary(_, CUnOp::Negate, subexpr_id, _) => {
                    if let &CExprKind::Literal(_, CLiteral::Integer(i, _)) =
                        &self.ast_context[subexpr_id].kind
                    {
                        val = self.enum_for_i64(enum_id, -(i as i64));
                        return Ok(WithStmts::new_val(val));
                    }
                }

                _ => {}
            }
        }

        // We could be casting from enum to enum...
        if let CTypeKind::Enum(source_enum_id) =
            self.ast_context.resolve_type(source_cty.ctype).kind
        {
            // Casting to ourselves, the audacity!
            if source_enum_id == enum_id {
                return Ok(WithStmts::new_val(val));
            }

            match self.tcfg.enum_mode {
                // Enum-to-enum casts need to be translated via the inner value as an intermediate.
                EnumMode::NewType => val = self.integer_from_enum(val),
                EnumMode::Consts => {}
            }

            source_cty = self.enum_integral_type(source_enum_id);
        }

        let enum_integral_type = self.enum_integral_type(enum_id);
        let mut val = WithStmts::new_val(val);

        match self.tcfg.enum_mode {
            EnumMode::NewType => {
                val =
                    self.convert_cast(ctx, source_cty, enum_integral_type, val, None, None, None)?;
                val = val.map(|val| self.enum_constructor_expr(enum_id, val));
            }

            EnumMode::Consts => {
                let source_type_kind = &self.ast_context.resolve_type(source_cty.ctype).kind;
                let enum_integral_type_kind =
                    &self.ast_context.resolve_type(enum_integral_type.ctype).kind;

                if source_type_kind != enum_integral_type_kind {
                    val = val.map(|val| self.enum_constructor_expr(enum_id, val));
                }
            }
        }

        Ok(val)
    }

    /// Given an integer value this attempts to either generate the corresponding enum
    /// variant directly, otherwise it converts a number to the enum type.
    fn enum_for_i64(&self, enum_id: CEnumId, value: i64) -> Box<Expr> {
        if let Some(enum_constant_id) = self.enum_variant_for_i64(enum_id, value) {
            return self.enum_constant_expr(enum_constant_id);
        }

        let underlying_type_id = self.enum_integral_type(enum_id);
        let value = match self.ast_context.resolve_type(underlying_type_id.ctype).kind {
            CTypeKind::UInt => mk().lit_expr(mk().int_unsuffixed_lit((value as u32) as u128)),
            CTypeKind::ULong => mk().lit_expr(mk().int_unsuffixed_lit((value as u64) as u128)),
            _ => signed_int_expr(value),
        };

        self.enum_constructor_expr(enum_id, value)
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
        let name = self.renamer.borrow().get(&enum_constant_id).unwrap();
        self.add_import(enum_constant_id, &name);
        mk().ident_expr(name)
    }

    fn enum_constructor_expr(&self, enum_id: CEnumId, value: Box<Expr>) -> Box<Expr> {
        let enum_name = self
            .type_converter
            .borrow()
            .resolve_decl_name(enum_id)
            .unwrap();
        self.add_import(enum_id, &enum_name);

        match self.tcfg.enum_mode {
            EnumMode::NewType => mk().call_expr(mk().ident_expr(enum_name), vec![value]),
            EnumMode::Consts => mk().cast_expr(value, mk().ident_ty(enum_name)),
        }
    }

    fn is_variant_of_enum(&self, enum_id: CEnumId, enum_constant_id: CEnumConstantId) -> bool {
        let variants = match self.ast_context[enum_id].kind {
            CDeclKind::Enum { ref variants, .. } => variants,
            _ => panic!("{:?} does not point to an `enum` declaration", enum_id),
        };

        variants.contains(&enum_constant_id)
    }

    pub fn enum_integral_type(&self, enum_id: CEnumId) -> CQualTypeId {
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
