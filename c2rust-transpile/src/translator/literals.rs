#![deny(missing_docs)]
//! This code is used to generate literal expressions of various kinds.
//! These include integer, floating, array, struct, union, enum literals.

use failure::format_err;

use super::*;
use std::iter;

impl<'c> Translation<'c> {
    /// Generate an integer literal corresponding to the given type, value, and base.
    pub fn mk_int_lit(
        &self,
        ty: CQualTypeId,
        val: u64,
        base: IntBase,
        negative: bool,
    ) -> TranslationResult<Box<Expr>> {
        let type_resolved_id = self.ast_context.resolve_type_id(ty.ctype);

        if self.ast_context[type_resolved_id].kind.is_enum() {
            let mut val = val as i64;

            if negative {
                val = -val;
            }

            return Ok(self.enum_for_i64(type_resolved_id, val));
        }

        let lit = match base {
            IntBase::Dec => mk().int_unsuffixed_lit(val),
            IntBase::Hex => mk().float_unsuffixed_lit(&format!("0x{:x}", val)),
            IntBase::Oct => mk().float_unsuffixed_lit(&format!("0o{:o}", val)),
        };
        let mut expr = mk().lit_expr(lit);

        if negative {
            expr = neg_expr(expr);
        }

        let target_ty = self.convert_type(ty.ctype)?;
        Ok(mk().cast_expr(expr, target_ty))
    }

    /// Return whether the literal can be directly translated as this type.
    pub fn literal_matches_ty(&self, lit: &CLiteral, ty: CQualTypeId, is_negated: bool) -> bool {
        let ty_kind = &self.ast_context.resolve_type(ty.ctype).kind;
        match *lit {
            CLiteral::Integer(_, _) if ty_kind.is_enum() => true,
            CLiteral::Integer(value, _) | CLiteral::Character(value)
                if ty_kind.is_integral_type() && !ty_kind.is_bool() =>
            {
                ty_kind.guaranteed_integer_in_range(value)
                    && (!is_negated || ty_kind.is_signed_integral_type())
            }
            CLiteral::Floating(value, _) if ty_kind.is_floating_type() => {
                ty_kind.guaranteed_float_in_range(value)
            }
            _ => false,
        }
    }

    /// Convert a C literal expression to a Rust expression
    pub fn convert_literal(
        &self,
        ctx: ExprContext,
        ty: CQualTypeId,
        lit: &CLiteral,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        match *lit {
            CLiteral::Integer(val, base) => {
                Ok(WithStmts::new_val(self.mk_int_lit(ty, val, base, false)?))
            }

            CLiteral::Character(val) => {
                let val = val as u32;
                let expr = match char::from_u32(val) {
                    Some(c) => mk().lit_expr(c),
                    None => {
                        // Fallback for characters outside of the valid Unicode range
                        if (val as i32) < 0 {
                            neg_expr(mk().lit_expr(
                                mk().int_unsuffixed_lit((val as i32).unsigned_abs() as u128),
                            ))
                        } else {
                            mk().lit_expr(mk().int_unsuffixed_lit(val as u128))
                        }
                    }
                };

                let type_rs = self.convert_type(ty.ctype)?;
                Ok(WithStmts::new_val(mk().cast_expr(expr, type_rs)))
            }

            CLiteral::Floating(val, ref c_str) => {
                let str = if c_str.is_empty() {
                    let mut buffer = dtoa::Buffer::new();
                    buffer.format(val).to_string()
                } else {
                    c_str.to_owned()
                };
                let val = match self.ast_context.resolve_type(ty.ctype).kind {
                    CTypeKind::LongDouble | CTypeKind::Float128 => {
                        if ctx.is_const {
                            return Err(format_translation_err!(
                                None,
                                "f128 cannot be used in constants because `f128::f128::new` is not `const`",
                            ));
                        }

                        self.use_crate(ExternCrate::F128);

                        let fn_path = mk().abs_path_expr(vec!["f128", "f128", "new"]);
                        let args = vec![mk().lit_expr(mk().float_unsuffixed_lit(&str))];

                        mk().call_expr(fn_path, args)
                    }
                    CTypeKind::Double => mk().lit_expr(mk().float_lit(&str, "f64")),
                    CTypeKind::Float => mk().lit_expr(mk().float_lit(&str, "f32")),
                    ref k => panic!("Unsupported floating point literal type {:?}", k),
                };
                Ok(WithStmts::new_val(val))
            }

            CLiteral::String(ref bytes, element_size) => {
                if ctx.is_pattern {
                    return Err(TranslationError::generic(
                        "CLiteral::String is not supported in patterns",
                    ));
                }

                let bytes_padded = self.string_literal_bytes(ty.ctype, bytes, element_size);
                let len = bytes_padded.len();
                let val = mk().lit_expr(bytes_padded);

                if ctx.needs_address && element_size == 1 {
                    // Unlike in C, Rust string literals are already references by default.
                    // So if the address needs to be taken, just make a bare literal and let
                    // `convert_address_of_common` cast it to the appropriate type.
                    // Strings with element_size > 1 cannot be cast from a byte literal for
                    // alignment reasons, and need a transmute.
                    Ok(WithStmts::new_val(val))
                } else {
                    // std::mem::transmute::<[u8; size], ctype>(*b"xxxx")
                    let array_ty = mk().array_ty(mk().ident_ty("u8"), mk().lit_expr(len as u128));
                    let mut val = transmute_expr(
                        array_ty,
                        self.convert_type(ty.ctype)?,
                        mk().unary_expr(UnOp::Deref(Default::default()), val),
                    );

                    // A transmute creates a temporary, which cannot have its address taken without
                    // creating dangling pointers. Wrap it inside an inline `const` block, so that
                    // it will be const-promoted to 'static.
                    if ctx.needs_address {
                        self.use_feature("inline_const");
                        let stmts = vec![mk().expr_stmt(val)];
                        val = mk().const_block_expr(mk().const_block(stmts));
                    }

                    Ok(WithStmts::new_unsafe_val(val))
                }
            }
        }
    }

    /// Returns the bytes of a string literal, including any additional zero bytes to pad the
    /// literal to the expected size.
    pub fn string_literal_bytes(&self, ctype: CTypeId, bytes: &[u8], element_size: u8) -> Vec<u8> {
        let size = self.ast_context.array_len(ctype) * element_size as usize;
        let mut bytes_padded = Vec::with_capacity(size);
        bytes_padded.extend(bytes);
        bytes_padded.resize(size, 0);
        bytes_padded
    }

    /// Convert an initialization list into an expression. These initialization lists can be
    /// used as array literals, struct literals, and union literals in code.
    pub fn convert_init_list(
        &self,
        ctx: ExprContext,
        ty: CQualTypeId,
        ids: &[CExprId],
        opt_union_field_id: Option<CFieldId>,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        match self.ast_context.resolve_type(ty.ctype).kind {
            CTypeKind::ConstantArray(ty, n) => {
                // Convert all of the provided initializer values

                let to_array_element = |id: CExprId| -> TranslationResult<_> {
                    self.convert_expr(ctx.used(), id, None)?.result_map(|x| {
                        // Array literals require all of their elements to be
                        // the correct type; they will not use implicit casts to
                        // change mut to const. This becomes a problem when an
                        // array literal is used in a position where there is no
                        // type information available to force its type to the
                        // correct const or mut variation. To avoid this issue
                        // we manually insert the otherwise elided casts in this
                        // particular context.
                        if let CExprKind::ImplicitCast(ty, _, CastKind::ConstCast, _, _) =
                            self.ast_context[id].kind
                        {
                            let t = self.convert_type(ty.ctype)?;
                            Ok(mk().cast_expr(x, t))
                        } else {
                            Ok(x)
                        }
                    })
                };

                // We need to handle the 4 cases in `str_init.c` with identical initializers:
                // * `ptr_extra_braces`
                // * `array_extra_braces`
                // * `array_of_ptrs`
                // * `array_of_arrays`
                // All 4 have different types, but the same initializer,
                // which is possible because C allows extra braces around any initializer element.
                // For non-string literal elements, the clang AST already fixes this up,
                // but doesn't for string literals, so we need to handle them specially.
                // The existing logic below this special case handles all except `array_extra_braces`.
                // `array_extra_braces` is uniquely identified by:
                // * there being only one element in the initializer list
                // * the element type of the array being `CTypeKind::Char` (w/o this, `array_of_arrays` is included)
                // * the expr kind being a string literal (`CExprKind::Literal` of a `CLiteral::String`).
                let is_string_literal = |id: CExprId| {
                    let ty_kind = &self.ast_context.resolve_type(ty).kind;
                    let expr_kind = &self.ast_context.index(id).kind;
                    let is_char_array = matches!(*ty_kind, CTypeKind::Char);
                    let is_str_literal =
                        matches!(*expr_kind, CExprKind::Literal(_, CLiteral::String { .. }));
                    is_char_array && is_str_literal
                };

                let is_zero_literal = |id: CExprId| {
                    matches!(
                        self.ast_context.index(id).kind,
                        CExprKind::Literal(_, CLiteral::Integer(0, _base))
                    )
                };

                match ids {
                    [] => {
                        // This was likely a C array of the form `int x[16] = {}`.
                        // We'll emit that as [0; 16].
                        let len = mk().lit_expr(mk().int_unsuffixed_lit(n as u128));
                        let zeroed = self.implicit_default_expr(ctx, ty)?;
                        Ok(zeroed.map(|default_value| mk().repeat_expr(default_value, len)))
                    }
                    &[single] if is_string_literal(single) => {
                        // See comment on `is_string_literal`.
                        // This detects these cases from `str_init.c`:
                        // * `ptr_extra_braces`
                        // * `array_of_ptrs`
                        // * `array_of_arrays`
                        self.convert_expr(ctx.used(), single, None)
                    }
                    &[single] if is_zero_literal(single) && n > 1 => {
                        // This was likely a C array of the form `int x[16] = { 0 }`.
                        // We'll emit that as [0; 16].
                        let len = mk().lit_expr(mk().int_unsuffixed_lit(n as u128));
                        Ok(to_array_element(single)?
                            .map(|default_value| mk().repeat_expr(default_value, len)))
                    }
                    [..] => {
                        Ok(ids
                            .iter()
                            .copied()
                            .map(to_array_element)
                            .chain(
                                // Pad out the array literal with default values to the desired size
                                iter::repeat(self.implicit_default_expr(ctx, ty))
                                    .take(n - ids.len()),
                            )
                            .collect::<TranslationResult<WithStmts<_>>>()?
                            .map(|vals| mk().array_expr(vals)))
                    }
                }
            }
            CTypeKind::Struct(struct_id) => {
                self.convert_struct_literal(ctx, struct_id, ids.as_ref())
            }
            CTypeKind::Union(union_id) => {
                self.convert_union_literal(ctx, union_id, ids.as_ref(), ty, opt_union_field_id)
            }
            CTypeKind::Vector(CQualTypeId { ctype, .. }, len) => {
                self.vector_list_initializer(ctx, ids, ctype, len)
            }
            ref kind if kind.is_scalar() => {
                if let Some(&first) = ids.first() {
                    self.convert_expr(ctx.used(), first, None)
                } else {
                    self.implicit_default_expr(ctx.used(), ty.ctype)
                }
            }
            ref t => Err(format_err!("Init list not implemented for {:?}", t).into()),
        }
    }
}
