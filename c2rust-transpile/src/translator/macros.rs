use c2rust_ast_builder::mk;
use failure::format_err;
use log::{info, trace};
use proc_macro2::{Span, TokenStream};
use syn::{Expr, MacroDelimiter};

use crate::c_ast::{CDeclId, CExprId, CQualTypeId, CTypeId, CTypeKind};
use crate::diagnostics::{TranslationError, TranslationResult};
use crate::translator::{ConvertedDecl, ConvertedMacro, ExprContext, Translation};
use crate::with_stmts::WithStmts;
use crate::TranslateMacros;

impl<'c> Translation<'c> {
    pub fn convert_macro(
        &self,
        ctx: ExprContext,
        decl_id: CDeclId,
        span: Span,
        name: &str,
    ) -> TranslationResult<ConvertedDecl> {
        trace!(
            "Expanding macro {:?}: {:?}",
            decl_id,
            self.ast_context[decl_id]
        );

        let maybe_replacement = self.recreate_const_macro_from_expansions(
            ctx.const_().set_converting_macro(decl_id),
            &self.ast_context.macro_expansions[&decl_id],
        );

        match maybe_replacement {
            Ok((replacement, result_type_id)) => {
                trace!("  to {:?}", replacement);

                let converted = ConvertedMacro { result_type_id };
                self.converted_macros
                    .borrow_mut()
                    .insert(decl_id, Some(converted));
                let result_type_rs = self.convert_type(result_type_id)?;

                Ok(ConvertedDecl::Item(mk().span(span).pub_().const_item(
                    name,
                    result_type_rs,
                    replacement,
                )))
            }
            Err(e) => {
                self.converted_macros.borrow_mut().insert(decl_id, None);
                info!("Could not expand macro {}: {}", name, e);
                Ok(ConvertedDecl::NoItem)
            }
        }
    }

    /// Given all of the expansions of a const macro,
    /// try to recreate a Rust `const` translation
    /// that is equivalent to every expansion.
    ///
    /// This may fail, in which case we simply don't emit a `const`
    /// and leave all of the expansions as fully inlined
    /// instead of referencing this `const`.
    ///
    /// For example, if the types of the macro expansion have no common type,
    /// which is required for a Rust `const` but not a C const macro,
    /// this can fail.  Or there could just be a feature we don't yet support.
    fn recreate_const_macro_from_expansions(
        &self,
        ctx: ExprContext,
        expansions: &[CExprId],
    ) -> TranslationResult<(Box<Expr>, CTypeId)> {
        let (val, ty) = expansions
            .iter()
            .try_fold::<Option<(WithStmts<Box<Expr>>, CTypeId)>, _, _>(
                None,
                |mut canonical, &expr_id| -> TranslationResult<_> {
                    self.can_convert_const_macro_expansion(expr_id)?;

                    let type_id = self.ast_context[expr_id]
                        .kind
                        .get_type()
                        .ok_or_else(|| format_err!("Invalid expression type"))?;
                    let val = self.convert_expr(ctx, expr_id, None)?;
                    let new = (val, type_id);

                    // Join ty and cur_ty to the smaller of the two types. If the
                    // types are not cast-compatible, abort the fold.
                    match &mut canonical {
                        Some(canonical) => {
                            let &mut (_, canon_type_id) = canonical;
                            let (_, new_type_id) = new;

                            let canon_type_kind =
                                self.ast_context.resolve_type(canon_type_id).kind.clone();
                            let new_type_kind =
                                self.ast_context.resolve_type(new_type_id).kind.clone();
                            let smaller_type_kind = CTypeKind::smaller_compatible_type(
                                canon_type_kind.clone(),
                                new_type_kind,
                            );
                            let Some(smaller_type_kind) = smaller_type_kind else {
                                return Err(
                                    format_err!("Not all macro expansions are compatible types")
                                    .into()
                                )
                            };

                            if smaller_type_kind != canon_type_kind {
                                *canonical = new;
                            }
                        }

                        None => canonical = Some(new),
                    }

                    Ok(canonical)
                },
            )?
            .ok_or_else(|| format_err!("Could not find a valid type for macro"))?;

        val.wrap_unsafe()
            .to_pure_expr()
            .map(|val| (val, ty))
            .ok_or_else(|| TranslationError::generic("Macro expansion is not a pure expression"))

        // TODO: Validate that all replacements are equivalent and pick the most
        // common type to minimize casts.
    }

    /// Determine if we're able to convert this const macro expansion.
    fn can_convert_const_macro_expansion(&self, expr_id: CExprId) -> TranslationResult<()> {
        match self.tcfg.translate_const_macros {
            TranslateMacros::None => Err(format_err!("translate_const_macros is None"))?,
            TranslateMacros::Conservative => {
                // TODO We still allow `CExprKind::ExplicitCast`s
                // even though they're broken (see #853).

                // This is a top-down, pessimistic/conservative analysis.
                // This is somewhat duplicative of `fn convert_expr` simply checking
                // `ExprContext::is_const` and returning errors where the expr is not `const`,
                // which is an non-conservative analysis scattered across all of the `fn convert_*`s.
                // That's what's done for `TranslateMacros::Experimental`,
                // as opposed to the conservative analysis done here for `TranslateMacros::Conservative`.
                // When the conservative analysis is incomplete,
                // it won't translate the macro, but the result will compile.
                // But when the non-conservative analysis is incomplete,
                // the resulting code may not transpile,
                // which is why the conservative analysis is used for `TranslateMacros::Conservative`.
                if !self.ast_context.is_const_expr(expr_id) {
                    Err(format_err!("non-const expr {expr_id:?}"))?;
                }
                Ok(())
            }
            TranslateMacros::Experimental => Ok(()),
        }
    }

    /// Convert the expansion of a const-like macro.
    ///
    /// See [`TranspilerConfig::translate_const_macros`].
    ///
    /// [`TranspilerConfig::translate_const_macros`]: crate::TranspilerConfig::translate_const_macros
    pub fn convert_const_macro_expansion(
        &self,
        ctx: ExprContext,
        expr_id: CExprId,
        override_ty: Option<CQualTypeId>,
    ) -> TranslationResult<Option<WithStmts<Box<Expr>>>> {
        let macros = match self.ast_context.macro_invocations.get(&expr_id) {
            Some(macros) => macros.as_slice(),
            None => return Ok(None),
        };

        // Find the first macro after the macro we're currently expanding, if any.
        let first_macro = macros
            .splitn(2, |macro_id| ctx.is_converting_macro(macro_id))
            .last()
            .unwrap()
            .first();
        let macro_id = match first_macro {
            Some(macro_id) => macro_id,
            None => return Ok(None),
        };

        trace!("  found macro expansion: {macro_id:?}");

        use std::cell::Ref;
        let get_converted = || -> Option<Option<Ref<'_, ConvertedMacro>>> {
            Ref::filter_map(self.converted_macros.borrow(), |converted_macros| {
                converted_macros.get(macro_id)
            })
            .ok()
            .map(|val| Ref::filter_map(val, Option::as_ref).ok())
        };

        // Ensure that we've converted this macro and that it has a valid definition.
        let mut converted = get_converted();
        if converted.is_none() {
            // We haven't tried to expand it yet.
            self.convert_decl(ctx, *macro_id)?;
            converted = get_converted();
        }
        let Some(Some(converted)) = converted else {
            // Expansion wasn't possible.
            return Ok(None);
        };

        let result_type_id = converted.result_type_id;
        let rust_name = self
            .renamer
            .borrow_mut()
            .get(macro_id)
            .ok_or_else(|| format_err!("Macro name not declared"))?;

        self.add_import(*macro_id, &rust_name);

        let val = WithStmts::new_val(mk().path_expr(vec![rust_name]));

        let expr_kind = &self.ast_context[expr_id].kind;
        // TODO We'd like to get rid of this cast eventually (see #1321).
        // Currently, const macros do not get the correct `override_ty` themselves,
        // so they aren't declared with the correct portable type,
        // but its uses are expecting the correct portable type,
        // so we need to cast it to the `override_ty` here.
        let expr_ty = override_ty.or_else(|| expr_kind.get_qual_type());
        if let Some(expr_ty) = expr_ty {
            self.convert_cast(
                ctx,
                CQualTypeId::new(result_type_id),
                expr_ty,
                val,
                None,
                None,
                None,
            )
            .map(Some)
        } else {
            Ok(Some(val))
        }

        // TODO: May need to handle volatile reads here.
        // See `DeclRef` below.
    }

    /// Convert the expansion of a function-like macro.
    ///
    /// See [`TranspilerConfig::translate_fn_macros`].
    ///
    /// [`TranspilerConfig::translate_fn_macros`]: crate::TranspilerConfig::translate_fn_macros
    pub fn convert_fn_macro_invocation(
        &self,
        _ctx: ExprContext,
        text: &str,
    ) -> Option<WithStmts<Box<Expr>>> {
        match self.tcfg.translate_fn_macros {
            TranslateMacros::None => return None,
            TranslateMacros::Conservative => return None, // Nothing is supported for `Conservative` yet.
            TranslateMacros::Experimental => {}
        }

        let mut split = text.splitn(2, '(');
        let ident = split.next()?.trim();
        let args = split.next()?.trim_end_matches(')');

        let ts: TokenStream = syn::parse_str(args).ok()?;
        Some(WithStmts::new_val(mk().mac_expr(mk().mac(
            mk().path(ident),
            ts,
            MacroDelimiter::Paren(Default::default()),
        ))))
    }

    pub fn expr_is_expanded_macro(
        &self,
        ctx: ExprContext,
        expr_id: CExprId,
        override_ty: Option<CQualTypeId>,
    ) -> bool {
        matches!(
            self.convert_const_macro_expansion(ctx, expr_id, override_ty),
            Ok(Some(_))
        )
    }
}
