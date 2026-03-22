use c2rust_ast_builder::mk;
use failure::format_err;
use log::{info, trace};
use proc_macro2::{Span, TokenStream};
use syn::{Expr, MacroDelimiter};

use crate::c_ast::{CDeclId, CExprId, CQualTypeId, CTypeId, CTypeKind};
use crate::diagnostics::{TranslationError, TranslationResult};
use crate::translator::{ConvertedDecl, ExprContext, MacroExpansion, Translation};
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
            ctx.const_().set_expanding_macro(decl_id),
            &self.ast_context.macro_expansions[&decl_id],
        );

        match maybe_replacement {
            Ok((replacement, ty)) => {
                trace!("  to {:?}", replacement);

                let expansion = MacroExpansion { ty };
                self.macro_expansions
                    .borrow_mut()
                    .insert(decl_id, Some(expansion));
                let ty = self.convert_type(ty)?;

                Ok(ConvertedDecl::Item(mk().span(span).pub_().const_item(
                    name,
                    ty,
                    replacement,
                )))
            }
            Err(e) => {
                self.macro_expansions.borrow_mut().insert(decl_id, None);
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
            .try_fold::<Option<(WithStmts<Box<Expr>>, CTypeId)>, _, _>(None, |canonical, &id| {
                self.can_convert_const_macro_expansion(id)?;

                let expr = self.convert_expr(ctx, id)?;
                let override_ty = self.expr_override_types.get(&id).copied();
                let ty = override_ty
                    .or_else(|| self.ast_context[id].kind.get_qual_type())
                    .ok_or_else(|| format_err!("Invalid expression type"))?
                    .ctype;

                // Join ty and cur_ty to the smaller of the two types. If the
                // types are not cast-compatible, abort the fold.
                let ty_kind = self.ast_context.resolve_type(ty).kind.clone();
                if let Some((canon_val, canon_ty)) = canonical {
                    let canon_ty_kind = self.ast_context.resolve_type(canon_ty).kind.clone();
                    if let Some(smaller_ty) =
                        CTypeKind::smaller_compatible_type(canon_ty_kind.clone(), ty_kind)
                    {
                        if smaller_ty == canon_ty_kind {
                            Ok(Some((canon_val, canon_ty)))
                        } else {
                            Ok(Some((expr, ty)))
                        }
                    } else {
                        Err(format_err!("Not all macro expansions are compatible types"))
                    }
                } else {
                    Ok(Some((expr, ty)))
                }
            })?
            .ok_or_else(|| format_err!("Could not find a valid type for macro"))?;

        val.to_unsafe_pure_expr()
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
            .splitn(2, |macro_id| ctx.expanding_macro(macro_id))
            .last()
            .unwrap()
            .first();
        let macro_id = match first_macro {
            Some(macro_id) => macro_id,
            None => return Ok(None),
        };

        trace!("  found macro expansion: {macro_id:?}");
        // Ensure that we've converted this macro and that it has a valid definition.
        let expansion = self.macro_expansions.borrow().get(macro_id).cloned();
        let macro_ty = match expansion {
            // Expansion exists.
            Some(Some(expansion)) => expansion.ty,

            // Expansion wasn't possible.
            Some(None) => return Ok(None),

            // We haven't tried to expand it yet.
            None => {
                self.convert_decl(ctx, *macro_id)?;
                if let Some(Some(expansion)) = self.macro_expansions.borrow().get(macro_id) {
                    expansion.ty
                } else {
                    return Ok(None);
                }
            }
        };
        let rust_name = self
            .renamer
            .borrow_mut()
            .get(macro_id)
            .ok_or_else(|| format_err!("Macro name not declared"))?;

        self.add_import(*macro_id, &rust_name);

        let val = WithStmts::new_val(mk().path_expr(vec![rust_name]));

        // Rust `const` variables have a single consistent type, determined by
        // `recreate_const_macro_from_expansions`, while in C each macro expansion has its own type,
        // determined by the surrounding context.
        // Since the expansion sites are expecting a particular type, we need to cast it here
        // if it differs from the `const` type.
        let expr_ty = override_ty.or_else(|| self.ast_context[expr_id].kind.get_qual_type());
        if let Some(expr_ty) = expr_ty {
            self.convert_cast(
                ctx,
                CQualTypeId::new(macro_ty),
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
