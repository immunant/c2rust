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
    ) -> TranslationResult<Box<Expr>> {
        let lit = match base {
            IntBase::Dec => mk().int_unsuffixed_lit(val),
            IntBase::Hex => mk().float_unsuffixed_lit(&format!("0x{:x}", val)),
            IntBase::Oct => mk().float_unsuffixed_lit(&format!("0o{:o}", val)),
        };

        let target_ty = self.convert_type(ty.ctype)?;
        Ok(mk().cast_expr(mk().lit_expr(lit), target_ty))
    }

    /// Return whether the literal can be directly translated as this type.
    pub fn literal_matches_ty(&self, lit: &CLiteral, ty: CQualTypeId) -> bool {
        let ty_kind = &self.ast_context.resolve_type(ty.ctype).kind;
        match *lit {
            CLiteral::Integer(value, _) if ty_kind.is_integral_type() && !ty_kind.is_bool() => {
                ty_kind.guaranteed_integer_in_range(value)
            }
            // `convert_literal` always casts these to i32.
            CLiteral::Character(_value) => matches!(ty_kind, CTypeKind::Int32),
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
            CLiteral::Integer(val, base) => Ok(WithStmts::new_val(self.mk_int_lit(ty, val, base)?)),

            CLiteral::Character(val) => {
                let val = val as u32;
                let expr = match char::from_u32(val) {
                    Some(c) => {
                        let expr = mk().lit_expr(c);
                        let i32_type = mk().path_ty(vec!["i32"]);
                        mk().cast_expr(expr, i32_type)
                    }
                    None => {
                        // Fallback for characters outside of the valid Unicode range
                        if (val as i32) < 0 {
                            mk().unary_expr(
                                UnOp::Neg(Default::default()),
                                mk().lit_expr(
                                    mk().int_lit((val as i32).unsigned_abs() as u128, "i32"),
                                ),
                            )
                        } else {
                            mk().lit_expr(mk().int_lit(val as u128, "i32"))
                        }
                    }
                };
                Ok(WithStmts::new_val(expr))
            }

            CLiteral::Floating(val, ref c_str) => {
                let str = if c_str.is_empty() {
                    let mut buffer = dtoa::Buffer::new();
                    buffer.format(val).to_string()
                } else {
                    c_str.to_owned()
                };
                let val = match self.ast_context.resolve_type(ty.ctype).kind {
                    CTypeKind::LongDouble => {
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
                let bytes_padded = self.string_literal_bytes(ty.ctype, bytes, element_size);

                // std::mem::transmute::<[u8; size], ctype>(*b"xxxx")
                let array_ty = mk().array_ty(
                    mk().ident_ty("u8"),
                    mk().lit_expr(bytes_padded.len() as u128),
                );
                let val = transmute_expr(
                    array_ty,
                    self.convert_type(ty.ctype)?,
                    mk().unary_expr(UnOp::Deref(Default::default()), mk().lit_expr(bytes_padded)),
                );
                Ok(WithStmts::new_unsafe_val(val))
            }
        }
    }

    /// Returns the bytes of a string literal, including any additional zero bytes to pad the
    /// literal to the expected size.
    pub fn string_literal_bytes(&self, ctype: CTypeId, bytes: &[u8], element_size: u8) -> Vec<u8> {
        let num_elems = match self.ast_context.resolve_type(ctype).kind {
            CTypeKind::ConstantArray(_, num_elems) => num_elems,
            ref kind => {
                panic!("String literal with unknown size: {bytes:?}, kind = {kind:?}")
            }
        };

        let size = num_elems * (element_size as usize);
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
                        let zeroed = self.implicit_default_expr(ty, ctx.is_static)?;
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
                                iter::repeat(self.implicit_default_expr(ty, ctx.is_static))
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
            CTypeKind::Pointer(_) => {
                let id = ids.first().unwrap();
                self.convert_expr(ctx.used(), *id, None)
            }
            CTypeKind::Enum(_) => {
                let id = ids.first().unwrap();
                self.convert_expr(ctx.used(), *id, None)
            }
            CTypeKind::Vector(CQualTypeId { ctype, .. }, len) => {
                self.vector_list_initializer(ctx, ids, ctype, len)
            }
            ref kind if kind.is_integral_type() => {
                let id = ids.first().unwrap();
                self.convert_expr(ctx.used(), *id, None)
            }
            ref t => Err(format_err!("Init list not implemented for {:?}", t).into()),
        }
    }

    fn convert_union_literal(
        &self,
        ctx: ExprContext,
        union_id: CRecordId,
        ids: &[CExprId],
        _ty: CQualTypeId,
        opt_union_field_id: Option<CFieldId>,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let union_field_id = opt_union_field_id.expect("union field ID");

        match self.ast_context.index(union_id).kind {
            CDeclKind::Union { .. } => {
                let union_name = self
                    .type_converter
                    .borrow()
                    .resolve_decl_name(union_id)
                    .unwrap();
                log::debug!("importing union {union_name}, id {union_id:?}");
                self.add_import(union_id, &union_name);
                match self.ast_context.index(union_field_id).kind {
                    CDeclKind::Field { typ: field_ty, .. } => {
                        let val = if ids.is_empty() {
                            self.implicit_default_expr(field_ty.ctype, ctx.is_static)?
                        } else {
                            self.convert_expr(ctx.used(), ids[0], None)?
                        };

                        Ok(val.map(|v| {
                            let name = vec![mk().path_segment(union_name)];
                            let field_name = self
                                .type_converter
                                .borrow()
                                .resolve_field_name(Some(union_id), union_field_id)
                                .unwrap();
                            let fields = vec![mk().field(field_name, v)];
                            mk().struct_expr(name, fields)
                        }))
                    }
                    _ => panic!("Union field decl mismatch"),
                }
            }
            _ => panic!("Expected union decl"),
        }
    }
}
