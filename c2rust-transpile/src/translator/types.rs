//! This module implements the translation and handling of types in general. Specific types are
//! handled in their own modules.

use c2rust_ast_builder::mk;
use c2rust_ast_exporter::clang_ast::SrcSpan;
use c2rust_rust_tools::RustEdition;
use failure::format_err;
use indexmap::IndexSet;
use log::warn;
use syn::{BinOp, Expr, Type};

use crate::c_ast::{
    CDeclId, CDeclKind, CExprId, CExprKind, CFieldId, CQualTypeId, CTypeId, CTypeKind, CUnOp,
    CastKind, TypedAstContext,
};
use crate::diagnostics::{TranslationError, TranslationErrorKind, TranslationResult};
use crate::translator::{cast_int, DecayRef, ExprContext, IdOrExpr, Import, Translation};
use crate::with_stmts::WithStmts;
use crate::{format_translation_err, ExternCrate};

impl<'c> Translation<'c> {
    pub(crate) fn convert_type(&self, type_id: CTypeId) -> TranslationResult<Box<Type>> {
        self.import_type(type_id);

        self.type_converter
            .borrow_mut()
            .convert(&self.ast_context, type_id)
    }

    pub(crate) fn import_type(&self, ctype: CTypeId) {
        self.add_imports(&self.imports_for_type(ctype))
    }

    pub(crate) fn imports_for_type(&self, ctype: CTypeId) -> IndexSet<Import> {
        use self::CTypeKind::*;

        let mut imports = IndexSet::default();

        let type_kind = &self.ast_context[ctype].kind;
        match type_kind {
            // libc can be accessed from anywhere as of Rust 2019 by full path
            Void | Char | SChar | UChar | Short | UShort | Int | UInt | Long | ULong | LongLong
            | ULongLong | Int128 | UInt128 | Half | BFloat16 | Float | Double | LongDouble
            | Float128 => {}
            // other libc types
            WChar | IntMax | UIntMax => {}
            // Built-in Rust sized types
            Int8 | Int16 | Int32 | Int64 | IntPtr | UInt8 | UInt16 | UInt32 | UInt64 | UIntPtr
            | Size | SSize | PtrDiff => {}
            // Bool uses the bool type, so no dependency on libc
            Bool => {}
            Paren(ctype)
            | Decayed(ctype)
            | IncompleteArray(ctype)
            | ConstantArray(ctype, _)
            | Elaborated(ctype)
            | Pointer(CQualTypeId { ctype, .. })
            | Attributed(CQualTypeId { ctype, .. }, _)
            | VariableArray(ctype, _)
            | Reference(CQualTypeId { ctype, .. })
            | BlockPointer(CQualTypeId { ctype, .. })
            | TypeOf(ctype)
            | Auto(ctype)
            | Complex(ctype) => imports.extend(self.imports_for_type(*ctype)),
            Enum(decl_id) | Typedef(decl_id) | Union(decl_id) | Struct(decl_id) => {
                let mut decl_id = *decl_id;
                // if the `decl` has been "squashed", get the corresponding `decl_id`
                if self.ast_context.prenamed_decls.contains_key(&decl_id) {
                    decl_id = *self.ast_context.prenamed_decls.get(&decl_id).unwrap();
                }

                let ident_name = self
                    .type_converter
                    .borrow()
                    .resolve_decl_name(decl_id)
                    .expect("Expected decl name");
                imports.insert(Import {
                    decl_id,
                    ident_name,
                });
            }
            Function(CQualTypeId { ctype, .. }, ref params, ..) => {
                // Return Type
                let type_kind = &self.ast_context[*ctype].kind;

                // Rust doesn't use void for return type, so skip
                if *type_kind != Void {
                    imports.extend(self.imports_for_type(*ctype));
                }

                // Param Types
                for param_id in params {
                    imports.extend(self.imports_for_type(param_id.ctype))
                }
            }
            Vector(..) => {
                // Handled in `import_simd_typedef`
            }
            TypeOfExpr(_) | BuiltinFn | Atomic(..) => {}
            UnhandledSveType => {
                // TODO: handle SVE types
            }
        }
        imports
    }

    // Compute the size of a type
    // Rust type: usize
    pub(crate) fn compute_size_of_type(
        &self,
        ctx: ExprContext,
        expected_type_id: Option<CQualTypeId>,
        result_type_id: CQualTypeId,
        type_id: CTypeId,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        if let CTypeKind::VariableArray(elts, len) = self.ast_context.resolve_type(type_id).kind {
            let len = len.expect("Sizeof a VLA type with count expression omitted");

            let elts = self.compute_size_of_type(ctx, expected_type_id, result_type_id, elts)?;
            return elts.and_then_try(|lhs| {
                let len = self.convert_expr(ctx.not_static(), len, expected_type_id)?;
                Ok(len.map(|len| {
                    let rhs = cast_int(len, "usize", true);
                    mk().binary_expr(BinOp::Mul(Default::default()), lhs, rhs)
                }))
            });
        }
        let ty = self.convert_type(type_id)?;
        let result = mk_size_of_ty_expr(ty)?;

        self.make_cast(
            ctx,
            result_type_id,
            expected_type_id.unwrap_or(result_type_id),
            result,
        )
    }

    pub(crate) fn compute_align_of_type(
        &self,
        ctx: ExprContext,
        expected_type_id: Option<CQualTypeId>,
        result_type_id: CQualTypeId,
        mut type_id: CTypeId,
        preferred: bool,
        src_loc: &Option<SrcSpan>,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        type_id = self.variable_array_base_type(type_id);

        let ty = self.convert_type(type_id)?;
        let tys = vec![ty];
        let mut path = vec![mk().path_segment("core")];
        // `core::intrinsics::pref_align_of` was removed in Rust 1.89
        // (https://github.com/rust-lang/rust/pull/141803).
        // There is no longer a notion of preferred alignment in Rust,
        // so in edition 2024, we no longer support
        // the non-standard `__alignof`/`__alignof__` extensions.
        // Normal alignment should always be a valid preferred alignment,
        // even if in a few cases, it is smaller,
        // so rather than having a hard error here,
        // we polyfill it with normal alignment.
        if preferred && self.tcfg.edition < RustEdition::Edition2024 {
            self.use_feature("core_intrinsics");
            path.push(mk().path_segment("intrinsics"));
            path.push(mk().path_segment_with_args("pref_align_of", mk().angle_bracketed_args(tys)));
        } else {
            if preferred {
                let msg = "using `core::mem::align_of` instead of `core::intrinsics::pref_align_of` \
                    for preferred alignment (`__alignof`/`__alignof__`) as the latter has been removed in Rust";
                if let Some(loc) = self.ast_context.display_loc(src_loc) {
                    warn!("{loc}: {msg}");
                } else {
                    warn!("{msg}");
                }
            }
            path.push(mk().path_segment("mem"));
            path.push(mk().path_segment_with_args("align_of", mk().angle_bracketed_args(tys)));
        }
        let call = mk().call_expr(mk().abs_path_expr(path), vec![]);
        self.make_cast(
            ctx,
            result_type_id,
            expected_type_id.unwrap_or(result_type_id),
            WithStmts::new_val(call),
        )
    }

    pub(crate) fn convert_cast(
        &self,
        mut ctx: ExprContext,
        override_ty: Option<CQualTypeId>,
        ty: CQualTypeId,
        expr: CExprId,
        kind: CastKind,
        opt_field_id: Option<CDeclId>,
        is_explicit: bool,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let source_ty = if let Some(func_decl) = self
            .ast_context
            .fn_declref_decl(expr)
            .filter(|_| is_explicit)
        {
            // If we're casting a function, look for its declared ty to use as a more
            // precise source type. The AST node's type will not preserve typedef arg types
            // but the function's declaration will.
            let kind_with_declared_args = self.ast_context.fn_decl_ty_with_declared_args(func_decl);
            let func_ty = self.ast_context.type_for_kind(&kind_with_declared_args);
            let func_ptr_ty = self
                .ast_context
                .type_for_kind(&CTypeKind::Pointer(CQualTypeId::new(func_ty)));

            CQualTypeId::new(func_ptr_ty)
        } else {
            self.ast_context
                .index_unwrap_parens(expr)
                .kind
                .get_qual_type()
                .ok_or_else(|| format_err!("bad source type"))?
        };
        let target_ty = override_ty.unwrap_or(ty);

        match kind {
            CastKind::LValueToRValue => {
                let val = if source_ty.qualifiers.is_volatile {
                    // If the expression is volatile and used as something that isn't an LValue,
                    // this constitutes a volatile read. A volatile read is a side effect, so it
                    // needs to be included even if the expression is unused.
                    let val = self
                        .convert_expr(ctx.used(), expr, None)?
                        .try_map(|val| self.volatile_read(val, source_ty))?;
                    self.convert_side_effects_expr(
                        ctx,
                        val,
                        "LValueToRValue value is not supposed to be used",
                    )
                } else {
                    self.convert_expr(ctx, expr, None)?
                };

                // if the context wants a different type, add a cast
                return self.make_cast(ctx, source_ty.not_volatile(), target_ty, val);
            }

            CastKind::IntegralToBoolean
            | CastKind::FloatingToBoolean
            | CastKind::PointerToBoolean => {
                return self.convert_scalar_to_bool_cast(ctx, expr, true);
            }

            _ => {}
        }

        // In general, if we are casting the result of an expression, then the inner
        // expression should be translated to whatever type it normally would.
        // But for some expression types, if we don't absolutely have to cast,
        // we would rather the expression is translated according to the type we're
        // expecting, and then we can skip the cast entirely.
        if self.can_propagate_cast(ctx, expr, target_ty, is_explicit) {
            return self.convert_expr(ctx, expr, Some(target_ty));
        }

        match kind {
            // A reference must be decayed if a bitcast is required. Const casts in
            // LLVM 8 are now NoOp casts, so we need to include it as well.
            CastKind::BitCast | CastKind::PointerToIntegral | CastKind::NoOp => {
                ctx.decay_ref = DecayRef::Yes
            }
            CastKind::ArrayToPointerDecay
            | CastKind::FunctionToPointerDecay
            | CastKind::BuiltinFnToFnPtr => {
                ctx.needs_address = true;
            }
            _ => {}
        }

        let mut val = self.convert_expr(ctx, expr, None)?;

        if is_explicit {
            let stmts = self.compute_variable_array_sizes(ctx, ty.ctype)?;
            val = val.prepend_stmts(stmts);
        }

        // Shuffle Vector "function" builtins will add a cast to the output of the
        // builtin call which is unnecessary for translation purposes
        if self.casting_simd_builtin_call(expr, is_explicit, kind) {
            return Ok(val);
        }

        self.make_cast_full(
            ctx,
            source_ty,
            target_ty,
            val,
            Some(expr),
            Some(kind),
            opt_field_id,
            is_explicit,
        )
    }

    fn can_propagate_cast(
        &self,
        ctx: ExprContext,
        expr_id: CExprId,
        target_type_id: CQualTypeId,
        is_explicit: bool,
    ) -> bool {
        // Always preserve explicit casts.
        if is_explicit {
            return false;
        }

        let mut expr_kind = &self.ast_context.index_unwrap_parens(expr_id).kind;

        // In patterns, skip over `ConstantExpr`s.
        if ctx.is_pattern {
            if let &CExprKind::ConstantExpr(_, expr_id, _) = expr_kind {
                expr_kind = &self.ast_context.index_unwrap_parens(expr_id).kind;
            }
        }

        match *expr_kind {
            // The inner expression is also an implicit cast. See if we can combine the casts.
            CExprKind::ImplicitCast(source_type_id, _, cast_kind, _, _) => {
                let source_type_kind = &self.ast_context.resolve_type(source_type_id.ctype).kind;
                let target_type_kind = &self.ast_context.resolve_type(target_type_id.ctype).kind;

                if let CTypeKind::Enum(target_enum_id) = *target_type_kind {
                    let target_underlying_type_id = self.enum_underlying_type(target_enum_id);
                    let target_underlying_type_kind = &self
                        .ast_context
                        .resolve_type(target_underlying_type_id.ctype)
                        .kind;

                    // We are casting to an enum type, from its underlying integral type.
                    // Skip the cast to the integral type and cast to the enum type directly.
                    if cast_kind == CastKind::IntegralCast
                        && source_type_kind == target_underlying_type_kind
                    {
                        return true;
                    }
                }
            }

            CExprKind::DeclRef(_, decl_id, _) => {
                if let CDeclKind::EnumConstant { .. } = self.ast_context[decl_id].kind {
                    // In C, `EnumConstant`s have some integral type, _not_ the enum type.
                    // However, if we then immediately have a cast to convert this variable back into
                    // the enum type, we would like to produce Rust with _no_ casts.
                    if self.enum_constant_matches_type(target_type_id.ctype, decl_id) {
                        return true;
                    }

                    let source_enum_id = self.ast_context.parents[&decl_id];
                    let source_underlying_type_id = self.enum_underlying_type(source_enum_id);
                    let target_type_resolved_id = self
                        .ast_context
                        .resolve_type_id_no_typedef(target_type_id.ctype);

                    // Likewise, if we are casting to the underlying type of the enum, then
                    // translate the enum constant directly as that.
                    if target_type_resolved_id == source_underlying_type_id.ctype {
                        return true;
                    }
                }
            }

            _ => {}
        }

        let mut literal_expr_kind = expr_kind;
        let mut is_negated = false;

        if let &CExprKind::Unary(_, CUnOp::Negate, subexpr_id, _) = literal_expr_kind {
            if !self.expr_is_expanded_macro(ctx, subexpr_id, None) {
                literal_expr_kind = &self.ast_context.index_unwrap_parens(subexpr_id).kind;
                is_negated = true;
            }
        }

        if let CExprKind::Literal(_, lit) = literal_expr_kind {
            // Does the inner literal fit in the type we're casting to?
            if self.literal_matches_ty(lit, target_type_id, is_negated) {
                return true;
            }
        }

        false
    }

    pub(crate) fn make_cast(
        &self,
        ctx: ExprContext,
        source_type_id: CQualTypeId,
        target_type_id: CQualTypeId,
        val: WithStmts<Box<Expr>>,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        self.make_cast_full(
            ctx,
            source_type_id,
            target_type_id,
            val,
            None,
            None,
            None,
            false,
        )
    }

    fn make_cast_full(
        &self,
        ctx: ExprContext,
        source_cty: CQualTypeId,
        target_cty: CQualTypeId,
        val: WithStmts<Box<Expr>>,
        expr: Option<CExprId>,
        kind: Option<CastKind>,
        opt_field_id: Option<CFieldId>,
        is_explicit: bool,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let source_ty_kind = &self.ast_context.resolve_type(source_cty.ctype).kind;
        let target_ty_kind = &self.ast_context.resolve_type(target_cty.ctype).kind;

        let kind = kind.unwrap_or_else(|| {
            CastKind::from_types(source_ty_kind, target_ty_kind).unwrap_or_else(|| {
                warn!(
                    "Unknown CastKind for {source_ty_kind:?} to {target_ty_kind:?} cast. \
                    Defaulting to BitCast",
                );

                CastKind::BitCast
            })
        });

        if self.ast_context.type_kinds_eq(
            source_ty_kind,
            target_ty_kind,
            &TypedAstContext::resolve_type_id,
        ) && kind != CastKind::LValueToRValue
        {
            return Ok(val);
        }

        if ctx.is_pattern && !matches!(kind, CastKind::ToVoid | CastKind::IntegralCast) {
            return Err(TranslationError::generic(
                "cast kind is not supported in patterns",
            ));
        }

        match kind {
            CastKind::IntegralCast
            | CastKind::FloatingCast
            | CastKind::FloatingToIntegral
            | CastKind::IntegralToFloating
            | CastKind::BooleanToSignedIntegral
            | CastKind::BitCast
            | CastKind::NoOp
            | CastKind::IntegralToPointer
            | CastKind::PointerToIntegral => {
                if ctx.is_pattern
                    && !(source_ty_kind.is_integral_type() && target_ty_kind.is_enum())
                {
                    return Err(TranslationError::generic(
                        "only casts from integers to enums are supported in patterns",
                    ));
                }

                match (source_ty_kind, target_ty_kind) {
                    (CTypeKind::Pointer(..), CTypeKind::Pointer(..)) => {
                        if !is_explicit
                            && self
                                .ast_context
                                .is_mut_to_const_pointer_cast(source_cty.ctype, target_cty.ctype)
                        {
                            Ok(val)
                        } else {
                            val.and_then_try(|val| {
                                self.convert_pointer_to_pointer_cast(
                                    ctx,
                                    target_cty,
                                    (val, source_cty),
                                )
                            })
                        }
                    }

                    (CTypeKind::Enum(..), _) => val.and_then_try(|val| {
                        self.convert_cast_from_enum(ctx, target_cty, (val, source_cty))
                    }),

                    (CTypeKind::Pointer(..), _) => val.and_then_try(|val| {
                        self.convert_pointer_to_integral_cast(ctx, target_cty, (val, source_cty))
                    }),

                    (_, &CTypeKind::Enum(enum_id)) => val.and_then_try(|val| {
                        self.convert_cast_to_enum(ctx, enum_id, (val, source_cty))
                    }),

                    (_, CTypeKind::Pointer(..)) => val.and_then_try(|val| {
                        self.convert_integral_to_pointer_cast(ctx, target_cty, (val, source_cty))
                    }),

                    (CTypeKind::Bool, _) if target_ty_kind.is_numeric() => {
                        val.and_then_try(|val| {
                            self.convert_numeric_to_numeric_cast(ctx, target_cty, (val, source_cty))
                        })
                    }

                    _ if source_ty_kind.is_numeric() && target_ty_kind.is_numeric() => val
                        .and_then_try(|val| {
                            self.convert_numeric_to_numeric_cast(ctx, target_cty, (val, source_cty))
                        }),

                    _ => Err(format_err!(
                        "cast between unsupported type kinds:\n\
                        source_type_kind = {source_ty_kind:?}\n\
                        target_type_kind = {target_ty_kind:?}"
                    )
                    .into()),
                }
            }

            CastKind::LValueToRValue => {
                panic!("LValueToRValue casts must be handled in convert_cast")
            }

            CastKind::ToVoid => Ok(val),

            CastKind::FunctionToPointerDecay | CastKind::BuiltinFnToFnPtr => {
                Ok(val.map(|x| mk().call_expr(mk().ident_expr("Some"), vec![x])))
            }

            CastKind::ArrayToPointerDecay => {
                self.convert_array_to_pointer_decay(ctx, source_cty, target_cty, val, expr)
            }

            CastKind::NullToPointer => {
                assert!(val.stmts().is_empty());
                Ok(WithStmts::new_val(self.null_ptr(target_cty.ctype)?))
            }

            CastKind::ToUnion => self.convert_cast_to_union(val, opt_field_id),

            CastKind::IntegralToBoolean
            | CastKind::FloatingToBoolean
            | CastKind::PointerToBoolean => val
                .and_then_try(|val| self.convert_scalar_to_bool_cast(ctx, (val, source_cty), true)),

            CastKind::FloatingRealToComplex
            | CastKind::FloatingComplexToIntegralComplex
            | CastKind::FloatingComplexCast
            | CastKind::FloatingComplexToReal
            | CastKind::IntegralComplexToReal
            | CastKind::IntegralRealToComplex
            | CastKind::IntegralComplexCast
            | CastKind::IntegralComplexToFloatingComplex
            | CastKind::IntegralComplexToBoolean => Err(TranslationError::generic(
                "TODO casts with complex numbers not supported",
            )),

            CastKind::VectorSplat => Err(TranslationError::generic(
                "TODO vector splat casts not supported",
            )),

            CastKind::AtomicToNonAtomic | CastKind::NonAtomicToAtomic => Ok(val),
        }
    }

    /// Converts `expr_id` as if it were wrapped in an `ImplicitCast` expression.
    pub(crate) fn convert_expr_with_cast(
        &self,
        ctx: ExprContext,
        target_type_id: CQualTypeId,
        expr_id: CExprId,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let source_type_id = self.ast_context[expr_id].kind.get_qual_type().unwrap();

        let source_type_kind = &self.ast_context.resolve_type(source_type_id.ctype).kind;
        let target_type_kind = &self.ast_context.resolve_type(target_type_id.ctype).kind;

        let kind = CastKind::from_types(source_type_kind, target_type_kind).unwrap_or_else(|| {
            warn!(
                "Unknown CastKind for {source_type_kind:?} to {target_type_kind:?} cast. \
                Defaulting to BitCast",
            );

            CastKind::BitCast
        });

        self.convert_cast(ctx, None, target_type_id, expr_id, kind, None, false)
    }

    pub(crate) fn convert_expr_with_optional_cast(
        &self,
        ctx: ExprContext,
        target_type_id: Option<CQualTypeId>,
        expr_id: CExprId,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        if let Some(target_type_id) = target_type_id {
            self.convert_expr_with_cast(ctx, target_type_id, expr_id)
        } else {
            self.convert_expr(ctx, expr_id, None)
        }
    }

    /// Converts a cast between two numeric types, or from `bool` to a numeric type.
    fn convert_numeric_to_numeric_cast(
        &self,
        ctx: ExprContext,
        target_type_id: CQualTypeId,
        expr: impl Into<IdOrExpr>,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let expr = expr.into();
        let source_type_id = expr
            .type_id(&self.ast_context)
            .ok_or_else(|| format_err!("Invalid expression type"))?;

        let source_type_kind = &self.ast_context.resolve_type(source_type_id.ctype).kind;
        let target_type_kind = &self.ast_context.resolve_type(target_type_id.ctype).kind;

        match (source_type_kind, target_type_kind) {
            (CTypeKind::Bool, _) if target_type_kind.is_floating_type() => {
                // `bool` can't be cast directly to a floating type. Cast it to `u8` first.
                let mut expr_rs = self.convert_expr(ctx, expr, None)?;

                let u8_type_id =
                    CQualTypeId::new(self.ast_context.type_for_kind(&CTypeKind::UInt8));
                let u8_type_rs = self.convert_type(u8_type_id.ctype)?;
                expr_rs = expr_rs.map(|expr_rs| mk().cast_expr(expr_rs, u8_type_rs));

                expr_rs.and_then_try(|expr_rs| {
                    self.convert_numeric_to_numeric_cast(ctx, target_type_id, (expr_rs, u8_type_id))
                })
            }

            (
                CTypeKind::LongDouble | CTypeKind::Float128,
                CTypeKind::LongDouble | CTypeKind::Float128,
            ) => {
                // These are both converted to `f128`, so a cast between the two should
                // just be a no-op.
                self.convert_expr(ctx, expr, None)
            }

            (CTypeKind::LongDouble | CTypeKind::Float128, _) => {
                let expr_rs = self.convert_expr(ctx, expr, None)?;

                let target_type_rs = self.convert_type(target_type_id.ctype)?;
                let type_args_rs = mk().angle_bracketed_args(vec![target_type_rs]);
                let callee_rs = mk().path_expr(vec![
                    mk().path_segment_with_args("Into", type_args_rs),
                    mk().path_segment("into"),
                ]);
                Ok(expr_rs.map(|expr_rs| mk().call_expr(callee_rs, vec![expr_rs])))
            }

            (_, CTypeKind::LongDouble | CTypeKind::Float128) => {
                if ctx.is_const {
                    return Err(format_translation_err!(
                        None,
                        "f128 cannot be used in constants because `f128::f128::new` is not `const`",
                    ));
                }

                let expr_rs = self.convert_expr(ctx, expr, None)?;

                self.use_crate(ExternCrate::F128);
                let fn_path = mk().abs_path_expr(vec!["f128", "f128", "new"]);
                Ok(expr_rs.map(|expr_rs| mk().call_expr(fn_path, vec![expr_rs])))
            }

            _ => {
                let expr_rs = self.convert_expr(ctx, expr, None)?;

                let target_type_rs = self.convert_type(target_type_id.ctype)?;
                Ok(expr_rs.map(|expr_rs| mk().cast_expr(expr_rs, target_type_rs)))
            }
        }
    }
}

pub(crate) fn mk_size_of_ty_expr(ty: Box<Type>) -> TranslationResult<WithStmts<Box<Expr>>> {
    let name = "size_of";
    let params = mk().angle_bracketed_args(vec![ty]);
    let path = vec![
        mk().path_segment("core"),
        mk().path_segment("mem"),
        mk().path_segment_with_args(name, params),
    ];
    let call = mk().call_expr(mk().abs_path_expr(path), vec![]);

    Ok(WithStmts::new_val(call))
}
