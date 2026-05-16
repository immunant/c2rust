use c2rust_ast_builder::{mk, properties::Mutability};
use c2rust_ast_exporter::clang_ast::LRValue;
use c2rust_rust_tools::RustEdition;
use failure::{err_msg, format_err};
use std::ops::Index;
use syn::{BinOp, Expr, Type, UnOp};

use crate::c_ast::CUnOp;
use crate::{
    diagnostics::{TranslationError, TranslationErrorKind, TranslationResult},
    format_translation_err,
    translator::{
        cast_int, neg_expr, transmute_expr, unwrap_function_pointer, ExprContext, Translation,
    },
    with_stmts::WithStmts,
    CExprId, CExprKind, CLiteral, CQualTypeId, CTypeId, CTypeKind, CastKind, ExternCrate,
};

impl<'c> Translation<'c> {
    pub fn convert_address_of(
        &self,
        mut ctx: ExprContext,
        cqual_type: CQualTypeId,
        arg: CExprId,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let arg_kind = &self.ast_context[self.ast_context.resolve_parens(arg)].kind;

        match arg_kind {
            // C99 6.5.3.2 para 4
            CExprKind::Unary(_, CUnOp::Deref, target, _) => {
                return self.convert_expr(ctx, *target, None)
            }
            // Array subscript functions as a deref too.
            &CExprKind::ArraySubscript(_, lhs, rhs, _) => {
                return self.convert_array_subscript(
                    ctx.used().set_needs_address(true),
                    lhs,
                    rhs,
                    LRValue::RValue, // if we bypass the deref, we stay an RValue
                    Some(cqual_type),
                    false, // don't deref, keep as pointer
                );
            }
            // An AddrOf DeclRef/Member is safe to not decay
            // if the translator isn't already giving a hard yes to decaying (ie, BitCasts).
            // So we only convert default to no decay.
            CExprKind::DeclRef(..) | CExprKind::Member(..) => ctx.decay_ref.set_default_to_no(),
            _ => (),
        }

        let val = self.convert_expr(ctx.used().set_needs_address(true), arg, None)?;

        // & becomes a no-op when applied to a function.
        if self.ast_context.is_function_pointer(cqual_type.ctype) {
            return Ok(val.map(|x| mk().call_expr(mk().ident_expr("Some"), vec![x])));
        }

        let arg_cty = arg_kind
            .get_qual_type()
            .ok_or_else(|| format_err!("bad source type"))?;

        self.convert_address_of_common(ctx, Some(arg), arg_cty, cqual_type, val, false)
    }

    pub fn convert_array_to_pointer_decay(
        &self,
        ctx: ExprContext,
        source_cty: CQualTypeId,
        target_cty: CQualTypeId,
        val: WithStmts<Box<Expr>>,
        expr: Option<CExprId>,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        // Because va_list is sometimes defined as a single-element
        // array in order for it to allocate memory as a local variable
        // and to be a pointer as a function argument we would get
        // spurious casts when trying to treat it like a VaList which
        // has reference semantics.
        if self.ast_context.is_va_list(target_cty.ctype) {
            return Ok(val);
        }

        let source_ty_kind = &self.ast_context.resolve_type(source_cty.ctype).kind;

        // Variable length arrays are already represented as pointers.
        if let CTypeKind::VariableArray(..) = source_ty_kind {
            return Ok(val);
        }

        self.convert_address_of_common(ctx, expr, source_cty, target_cty, val, true)
    }

    fn convert_address_of_common(
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
        let arg_is_macro = arg.map_or(false, |arg| self.expr_is_expanded_macro(ctx, arg, None));

        let mut needs_cast = false;
        let mut ref_cast_pointee_ty = None;
        let mutbl = if ctx.is_const && !pointee_cty.qualifiers.is_const {
            // const contexts aren't able to use &mut, so we work around that
            // by using & and an extra cast through & to *const to *mut
            // TODO: Rust 1.83: Allowed, so this can be removed.
            needs_cast = true;
            Mutability::Immutable
        } else {
            pointee_cty.mutability()
        };

        // Narrow string literals are translated directly as `[u8; N]` literals when their address
        // is taken, without the transmute. String/byte literals are already references in Rust.
        if let (
            Some(&CExprKind::Literal(literal_cty, CLiteral::String(_, element_size @ 1))),
            false,
        ) = (arg_expr_kind, arg_is_macro)
        {
            if is_array_decay {
                val = val.map(|val| mk().method_call_expr(val, "as_ptr", vec![]));
            } else {
                let size = self.ast_context.array_len(literal_cty.ctype) * element_size as usize;
                ref_cast_pointee_ty =
                    Some(mk().array_ty(mk().ident_ty("u8"), mk().lit_expr(size as u128)));
            }
            needs_cast = true;
        }
        // Values that translate into const temporaries can't be raw-borrowed in Rust.
        // They must be regular-borrowed first, which will extend the lifetime to static.
        else if arg_is_macro || matches!(arg_expr_kind, Some(CExprKind::Literal(..))) {
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
                val = val.map(|val| mk().set_mutbl(mutbl).borrow_expr(val));

                // Add an intermediate reference-to-pointer cast if the context needs
                // reference-to-pointer decay, or if another cast follows.
                if ctx.decay_ref.is_yes() || needs_cast {
                    ref_cast_pointee_ty = Some(self.convert_pointee_type(arg_cty.ctype)?);
                }
            }
        } else {
            self.use_feature("raw_ref_op");
            val = val.map(|val| mk().set_mutbl(mutbl).raw_borrow_expr(val));

            if is_array_decay {
                // TODO: Call `ptr::as_[mut]_ptr` instead once that is available.
                // (`array_ptr_get` feature added to nightly in January 2024)
                needs_cast = true;
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

    pub fn convert_deref(
        &self,
        ctx: ExprContext,
        cqual_type: CQualTypeId,
        arg: CExprId,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let arg_expr_kind = &self.ast_context.index(arg).kind;

        if let &CExprKind::Unary(_, CUnOp::AddressOf, arg, _) = arg_expr_kind {
            return self.convert_expr(ctx.used(), arg, None);
        }

        self.convert_expr(ctx.used(), arg, None)?
            .try_map(|val: Box<Expr>| {
                if let CTypeKind::Function(..) =
                    self.ast_context.resolve_type(cqual_type.ctype).kind
                {
                    Ok(unwrap_function_pointer(val))
                } else if let Some(_vla) = self.compute_size_of_expr(cqual_type.ctype) {
                    Ok(val)
                } else {
                    Ok(mk().unary_expr(UnOp::Deref(Default::default()), val))
                }
            })
    }

    pub fn convert_array_subscript(
        &self,
        ctx: ExprContext,
        lhs: CExprId,
        rhs: CExprId,
        lrvalue: LRValue,
        override_ty: Option<CQualTypeId>,
        deref: bool,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let lhs_node = &self.ast_context.index(lhs).kind;
        let rhs_node = &self.ast_context.index(rhs).kind;

        let lhs_node_type = lhs_node
            .get_type()
            .ok_or_else(|| format_err!("lhs node bad type"))?;
        let lhs_node_kind = &self.ast_context.resolve_type(lhs_node_type).kind;
        let lhs_is_indexable = lhs_node_kind.is_pointer() || lhs_node_kind.is_vector();

        // From here on in, the LHS is the pointer/array and the RHS the index
        let (lhs, rhs, lhs_node) = if lhs_is_indexable {
            (lhs, rhs, lhs_node)
        } else {
            (rhs, lhs, rhs_node)
        };

        let lhs_node_type = lhs_node
            .get_type()
            .ok_or_else(|| format_err!("lhs node bad type"))?;
        if self
            .ast_context
            .resolve_type(lhs_node_type)
            .kind
            .is_vector()
        {
            return Err(TranslationError::from(
                err_msg("Attempting to index a vector type")
                    .context(TranslationErrorKind::OldLLVMSimd),
            ));
        }

        let rhs = self.convert_expr(ctx.used(), rhs, None)?;
        rhs.and_then_try(|rhs| {
            let simple_index_array = if ctx.needs_address() {
                // We can't necessarily index into an array if we're using
                // that element to compute an address.
                None
            } else {
                match lhs_node {
                    &CExprKind::ImplicitCast(_, arr, CastKind::ArrayToPointerDecay, _, _) => {
                        match self.ast_context[arr].kind {
                            CExprKind::Member(_, _, field_decl, _, _)
                                if self
                                    .potential_flexible_array_members
                                    .borrow()
                                    .contains(&field_decl) =>
                            {
                                None
                            }
                            ref kind => {
                                let arr_type =
                                    kind.get_type().ok_or_else(|| format_err!("bad arr type"))?;
                                match self.ast_context.resolve_type(arr_type).kind {
                                    // These get translated to 0-element arrays, this avoids the bounds check
                                    // that using an array subscript in Rust would cause
                                    CTypeKind::IncompleteArray(_) => None,
                                    _ => Some(arr),
                                }
                            }
                        }
                    }
                    _ => None,
                }
            };

            if let Some(arr) = simple_index_array {
                // If the LHS just underwent an implicit cast from array to pointer, bypass that
                // to make an actual Rust indexing operation

                let t = self.ast_context[arr]
                    .kind
                    .get_type()
                    .ok_or_else(|| format_err!("bad arr type"))?;
                let var_elt_type_id = match self.ast_context.resolve_type(t).kind {
                    CTypeKind::ConstantArray(..) => None,
                    CTypeKind::IncompleteArray(..) => None,
                    CTypeKind::VariableArray(elt, _) => Some(elt),
                    ref other => panic!("Unexpected array type {:?}", other),
                };

                let lhs = self.convert_expr(ctx.used(), arr, None)?;
                Ok(lhs.and_then(|lhs| {
                    // stmts.extend(lhs.stmts_mut());
                    // is_unsafe = is_unsafe || lhs.is_unsafe();

                    // Don't dereference the offset if we're still within the variable portion
                    if let Some(elt_type_id) = var_elt_type_id {
                        self.convert_pointer_offset(lhs, rhs, elt_type_id, false, deref)
                    } else {
                        WithStmts::new_val(mk().index_expr(lhs, cast_int(rhs, "usize", false)))
                    }
                }))
            } else {
                // LHS must be ref decayed for the offset method call's self param
                let lhs = self.convert_expr(ctx.used().decay_ref(), lhs, None)?;
                lhs.and_then_try(|lhs| {
                    // stmts.extend(lhs.stmts_mut());
                    // is_unsafe = is_unsafe || lhs.is_unsafe();

                    let lhs_type_id = lhs_node
                        .get_type()
                        .ok_or_else(|| format_err!("bad lhs type"))?;

                    // Determine the type of element being indexed
                    let pointee_type_id = match self.ast_context.resolve_type(lhs_type_id).kind {
                        CTypeKind::Pointer(pointee_id) => pointee_id,
                        _ => {
                            return Err(
                                format_err!("Subscript applied to non-pointer: {:?}", lhs).into()
                            );
                        }
                    };

                    let mut val =
                        self.convert_pointer_offset(lhs, rhs, pointee_type_id.ctype, false, deref);
                    // if the context wants a different type, add a cast
                    if let Some(expected_ty) = override_ty {
                        if lrvalue.is_rvalue() && expected_ty != pointee_type_id {
                            let ty = self.convert_type(expected_ty.ctype)?;
                            val = val.map(|val| mk().cast_expr(val, ty));
                        }
                    }
                    Ok(val)
                })
            }
        })
    }

    /// Pointer offset that casts its argument to isize
    pub fn convert_pointer_offset(
        &self,
        ptr: Box<Expr>,
        offset: Box<Expr>,
        pointee_cty: CTypeId,
        neg: bool,
        mut deref: bool,
    ) -> WithStmts<Box<Expr>> {
        let mut offset = cast_int(offset, "isize", false);

        if let Some(mul) = self.compute_size_of_expr(pointee_cty) {
            let mul = cast_int(mul, "isize", false);
            offset = mk().binary_expr(BinOp::Mul(Default::default()), offset, mul);
            deref = false;
        }

        if neg {
            offset = neg_expr(offset);
        }

        let mut res = mk().method_call_expr(ptr, "offset", vec![offset]);

        if deref {
            res = mk().unary_expr(UnOp::Deref(Default::default()), res);
        }

        WithStmts::new_val(res).set_unsafe()
    }

    /// Construct an expression for a NULL at any type, including forward declarations,
    /// function pointers, and normal pointers.
    pub fn null_ptr(&self, type_id: CTypeId) -> TranslationResult<Box<Expr>> {
        if self.ast_context.is_function_pointer(type_id) {
            return Ok(mk().path_expr(vec!["None"]));
        }

        let pointer_qty = self
            .ast_context
            .get_pointee_qual_type(type_id)
            .ok_or_else(|| TranslationError::generic("null_ptr requires a pointer"))?;

        let func = if pointer_qty.qualifiers.is_const {
            "null"
        } else {
            "null_mut"
        };
        let pointee_ty = self.convert_pointee_type(pointer_qty.ctype)?;
        let type_args = mk().angle_bracketed_args(vec![pointee_ty.clone()]);
        let val = mk().call_expr(
            mk().abs_path_expr(vec![
                mk().path_segment("core"),
                mk().path_segment("ptr"),
                mk().path_segment_with_args(func, type_args),
            ]),
            vec![],
        );

        Ok(val)
    }

    fn convert_pointee_type(&self, type_id: CTypeId) -> TranslationResult<Box<Type>> {
        self.import_type(type_id);

        self.type_converter
            .borrow_mut()
            .convert_pointee(&self.ast_context, type_id)
    }

    pub fn convert_pointer_to_pointer_cast(
        &self,
        source_cty: CTypeId,
        target_cty: CTypeId,
        val: WithStmts<Box<Expr>>,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        if self.ast_context.is_function_pointer(target_cty)
            || self.ast_context.is_function_pointer(source_cty)
        {
            let source_ty = self
                .type_converter
                .borrow_mut()
                .convert(&self.ast_context, source_cty)?;
            let target_ty = self
                .type_converter
                .borrow_mut()
                .convert(&self.ast_context, target_cty)?;

            if source_ty == target_ty {
                return Ok(val);
            }

            self.import_type(source_cty);
            self.import_type(target_cty);

            Ok(val.and_then(|val| {
                WithStmts::new_val(transmute_expr(source_ty, target_ty, val)).set_unsafe()
            }))
        } else {
            // Normal case
            let target_ty = self.convert_type(target_cty)?;
            Ok(val.map(|val| mk().cast_expr(val, target_ty)))
        }
    }

    pub fn convert_integral_to_pointer_cast(
        &self,
        ctx: ExprContext,
        source_cty: CTypeId,
        target_cty: CTypeId,
        val: WithStmts<Box<Expr>>,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let source_ty_kind = &self.ast_context.resolve_type(source_cty).kind;
        let target_ty = self.convert_type(target_cty)?;

        if self.ast_context.is_function_pointer(target_cty) {
            if ctx.is_const {
                return Err(format_translation_err!(
                    None,
                    "cannot transmute integers to Option<fn ...> in `const` context",
                ));
            }

            self.use_crate(ExternCrate::Libc);
            Ok(val.and_then(|mut val| {
                // First cast the integer to pointer size
                let intptr_t = mk().abs_path_ty(vec!["libc", "intptr_t"]);
                val = mk().cast_expr(val, intptr_t.clone());

                WithStmts::new_val(transmute_expr(intptr_t, target_ty, val)).set_unsafe()
            }))
        } else
        // Rust 1.90: `const_strict_provenance` feature added
        // Rust 1.91: stabilized
        if ctx.is_const && self.tcfg.edition < RustEdition::Edition2024 {
            if source_ty_kind.is_bool() {
                self.use_crate(ExternCrate::Libc);
                Ok(val.map(|mut val| {
                    // First cast the boolean to pointer size
                    val = mk().cast_expr(val, mk().abs_path_ty(vec!["libc", "size_t"]));
                    mk().cast_expr(val, target_ty)
                }))
            } else if let &CTypeKind::Enum(..) = source_ty_kind {
                val.try_map(|val| self.convert_cast_from_enum(target_cty, val))
            } else {
                Ok(val.map(|val| mk().cast_expr(val, target_ty)))
            }
        } else {
            // First cast the value to `usize`.
            let source_type_kind = &self.ast_context.resolve_type(source_cty).kind;
            let size_type_id = self.ast_context.type_for_kind(&CTypeKind::Size);

            let val = if let &CTypeKind::Enum(..) = source_type_kind {
                val.try_map(|val| self.convert_cast_from_enum(size_type_id, val))?
            } else {
                let size_type_rs = self.convert_type(size_type_id)?;
                val.map(|val| mk().cast_expr(val, size_type_rs))
            };

            // Then convert the `usize` into a pointer.
            let pointee_type_id = self
                .ast_context
                .get_pointee_qual_type(target_cty)
                .expect("target type must be a pointer");
            let mutability = pointee_type_id.mutability();

            let fn_name = match self.tcfg.edition {
                RustEdition::Edition2021 => {
                    // Rust 1.76: feature name changed to `exposed_provenance[_mut]`
                    // Rust 1.84: stabilized
                    self.use_feature("strict_provenance");

                    // Rust 1.79: method name changed to `with_exposed_provenance[_mut]`
                    match mutability {
                        Mutability::Immutable => "from_exposed_addr",
                        Mutability::Mutable => "from_exposed_addr_mut",
                    }
                }
                RustEdition::Edition2024 => match mutability {
                    Mutability::Immutable => "with_exposed_provenance",
                    Mutability::Mutable => "with_exposed_provenance_mut",
                },
            };
            let pointee_type_rs = self.convert_pointee_type(pointee_type_id.ctype)?;
            let type_args = mk().angle_bracketed_args(vec![pointee_type_rs]);
            let fn_expr = mk().abs_path_expr(vec![
                mk().path_segment("core"),
                mk().path_segment("ptr"),
                mk().path_segment_with_args(fn_name, type_args),
            ]);
            let val = val.map(|val| mk().call_expr(fn_expr, vec![val]));

            Ok(val)
        }
    }

    pub fn convert_pointer_to_integral_cast(
        &self,
        ctx: ExprContext,
        source_cty: CTypeId,
        target_cty: CTypeId,
        val: WithStmts<Box<Expr>>,
        expr: Option<CExprId>,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        if ctx.is_const {
            return Err(format_translation_err!(
                None,
                "cannot observe pointer values in `const` context",
            ));
        }

        let target_type_rs = self.convert_type(target_cty)?;

        if self.ast_context.is_function_pointer(source_cty) {
            let source_ty = self.convert_type(source_cty)?;

            Ok(val.and_then(|val| {
                WithStmts::new_val(transmute_expr(source_ty, target_type_rs, val)).set_unsafe()
            }))
        } else {
            // First convert the pointer to `usize`.
            let method_name = match self.tcfg.edition {
                RustEdition::Edition2021 => {
                    // Rust 1.76: feature name changed to `exposed_provenance`
                    // Rust 1.84: stabilized
                    self.use_feature("strict_provenance");

                    // Rust 1.79: method name changed to `expose_provenance`
                    "expose_addr"
                }
                RustEdition::Edition2024 => "expose_provenance",
            };

            let val = val.map(|val| mk().method_call_expr(val, method_name, vec![]));

            // Then cast the `usize` to the target type.
            let target_ty_kind = &self.ast_context.resolve_type(target_cty).kind;

            if let &CTypeKind::Enum(enum_decl_id) = target_ty_kind {
                val.try_map(|val| {
                    self.convert_cast_to_enum(ctx, target_cty, enum_decl_id, expr, val)
                })
            } else {
                Ok(val.map(|val| mk().cast_expr(val, target_type_rs)))
            }
        }
    }

    pub fn convert_pointer_is_null(
        &self,
        ctx: ExprContext,
        ptr_type: CTypeId,
        val: Box<Expr>,
        is_null: bool,
    ) -> TranslationResult<Box<Expr>> {
        Ok(if self.ast_context.is_function_pointer(ptr_type) {
            let method = if is_null { "is_none" } else { "is_some" };
            mk().method_call_expr(val, method, vec![])
        } else {
            // TODO: `pointer::is_null` becomes stably const in Rust 1.84.
            if ctx.is_const {
                return Err(format_translation_err!(
                    None,
                    "cannot check nullity of pointer in `const` context",
                ));
            }
            let val = mk().method_call_expr(val, "is_null", vec![]);
            if !is_null {
                mk().unary_expr(UnOp::Not(Default::default()), val)
            } else {
                val
            }
        })
    }
}
