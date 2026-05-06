use c2rust_ast_builder::mk;
use proc_macro2::Span;
use syn::Expr;

use crate::{
    diagnostics::TranslationResult,
    translator::{signed_int_expr, ConvertedDecl, Translation},
    with_stmts::WithStmts,
    CDeclKind, CEnumConstantId, CEnumId, CQualTypeId, CTypeId, CTypeKind, ConstIntExpr,
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
        let ty = self.convert_type(target_cty)?;
        Ok(mk().cast_expr(val, ty))
    }

    /// Translate a cast where the target type is an `enum` type.
    pub fn convert_cast_to_enum(
        &self,
        enum_type_id: CTypeId,
        val: Box<Expr>,
    ) -> TranslationResult<Box<Expr>> {
        let target_ty = self.convert_type(enum_type_id)?;
        Ok(mk().cast_expr(val, target_ty))
    }

    /// Given an integer value this attempts to either generate the corresponding enum
    /// variant directly, otherwise it converts a number to the enum type.
    pub fn enum_for_i64(&self, enum_type_id: CTypeId, value: i64) -> Box<Expr> {
        let enum_id = match self.ast_context.resolve_type(enum_type_id).kind {
            CTypeKind::Enum(enum_id) => enum_id,
            _ => panic!("{:?} does not point to an `enum` type", enum_type_id),
        };

        if let Some(enum_constant_id) = self.enum_variant_for_i64(enum_id, value) {
            return self.enum_constant_expr(enum_constant_id);
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

    pub fn enum_constant_matches_type(
        &self,
        type_id: CTypeId,
        enum_constant_id: CEnumConstantId,
    ) -> bool {
        let type_enum_id = match self.ast_context.resolve_type_no_typedef(type_id).kind {
            CTypeKind::Enum(enum_id) => enum_id,
            _ => return false,
        };
        let constant_enum_id = self.ast_context.parents[&enum_constant_id];
        type_enum_id == constant_enum_id
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
