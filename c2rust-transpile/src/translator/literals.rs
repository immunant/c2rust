#![deny(missing_docs)]
//! This code is used to generate literal expressions of various kinds.
//! These include integer, floating, array, struct, union, enum literals.

use super::*;

impl<'c> Translation<'c> {
    /// Generate an integer literal corresponding to the given type, value, and base.
    pub fn mk_int_lit(&self, ty: CQualTypeId, val: u64, base: IntBase) -> P<Expr> {
        // Note that C doesn't have anything smaller than integer literals
        let (intty, suffix) = match self.ast_context.resolve_type(ty.ctype).kind {
            CTypeKind::Int => (LitIntType::Signed(IntTy::I32), "i32"),
            CTypeKind::Long => (LitIntType::Signed(IntTy::I64), "i64"),
            CTypeKind::LongLong => (LitIntType::Signed(IntTy::I64), "i64"),
            CTypeKind::UInt => (LitIntType::Unsigned(UintTy::U32), "u32"),
            CTypeKind::ULong => (LitIntType::Unsigned(UintTy::U64), "u64"),
            CTypeKind::ULongLong => (LitIntType::Unsigned(UintTy::U64), "u64"),
            _ => (LitIntType::Unsuffixed, ""),
        };

        let lit = match base {
            IntBase::Dec => mk().int_lit(val.into(), intty),
            IntBase::Hex => mk().float_unsuffixed_lit(format!("0x{:x}{}", val, suffix)),
            IntBase::Oct => mk().float_unsuffixed_lit(format!("0o{:o}{}", val, suffix)),
        };

        mk().lit_expr(lit)
    }

    /// Given an integer value this attempts to either generate the corresponding enum
    /// variant directly, otherwise it transmutes a number to the enum type.
    pub fn enum_for_i64(&self, enum_type_id: CTypeId, value: i64) -> P<Expr> {

        let def_id = match self.ast_context.resolve_type(enum_type_id).kind {
            CTypeKind::Enum(def_id) => def_id,
            _ => panic!("{:?} does not point to an `enum` type"),
        };

        let (variants, underlying_type_id) = match self.ast_context[def_id].kind {
            CDeclKind::Enum { ref variants, integral_type, .. } => (variants, integral_type),
            _ => panic!("{:?} does not point to an `enum` declaration")
        };

        for &variant_id in variants {
            match self.ast_context[variant_id].kind {
                CDeclKind::EnumConstant { value: v, .. } =>
                if v == ConstIntExpr::I(value) || v == ConstIntExpr::U(value as u64) {
                    let name = self.renamer.borrow().get(&variant_id).unwrap();
                    return mk().path_expr(vec![name])
                }
                _ => panic!("{:?} does not point to an enum variant", variant_id),
            }
        }

        let underlying_type_id = underlying_type_id.expect("Attempt to construct value of forward declared enum");
        let value = match self.ast_context.resolve_type(underlying_type_id.ctype).kind {
            CTypeKind::UInt => mk().lit_expr(mk().int_lit((value as u32) as u128, LitIntType::Unsuffixed)),
            CTypeKind::ULong => mk().lit_expr(mk().int_lit((value as u64) as u128, LitIntType::Unsuffixed)),
            _ => signed_int_expr(value),
        };

        let target_ty = self.convert_type(enum_type_id).unwrap();

        mk().cast_expr(value, target_ty)
    }

    /// Convert a C literal expression to a Rust expression
    pub fn convert_literal(
        &self,
        is_static: bool,
        ty: CQualTypeId,
        kind: &CLiteral,
    ) -> Result<WithStmts<P<Expr>>, String> {
        match *kind {
            CLiteral::Integer(val, base) => Ok(WithStmts::new(self.mk_int_lit(ty, val, base))),

            CLiteral::Character(val) => {
                let expr = match char::from_u32(val as u32) {
                    Some(c) => {
                        let lit = mk().char_lit(c);
                        let expr = mk().lit_expr(lit);
                        let i32_type = mk().path_ty(vec!["i32"]);
                        mk().cast_expr(expr, i32_type)
                    }
                    None => {
                        // Fallback for characters outside of the valid Unicode range
                        let lit = mk().int_lit(val as u128, LitIntType::Signed(IntTy::I32));
                        mk().lit_expr(lit)
                    }
                };
                Ok(WithStmts::new(expr))
            }

            CLiteral::Floating(val, ref c_str) => {
                let mut bytes: Vec<u8> = vec![];
                let str = if c_str.is_empty() {
                    dtoa::write(&mut bytes, val).unwrap();
                    String::from_utf8(bytes).unwrap()
                } else {
                    c_str.to_owned()
                };

                let float_ty = match self.ast_context.resolve_type(ty.ctype).kind {
                    CTypeKind::LongDouble => FloatTy::F64,
                    CTypeKind::Double => FloatTy::F64,
                    CTypeKind::Float => FloatTy::F32,
                    ref k => panic!("Unsupported floating point literal type {:?}", k),
                };
                Ok(WithStmts::new(mk().lit_expr(mk().float_lit(str, float_ty))))
            }

            CLiteral::String(ref val, width) => {
                let mut val = val.to_owned();

                match self.ast_context.resolve_type(ty.ctype).kind {
                    // Match the literal size to the expected size padding with zeros as needed
                    CTypeKind::ConstantArray(_, size) => val.resize(size * (width as usize), 0),

                    // Add zero terminator
                    _ => {
                        for _ in 0..width {
                            val.push(0);
                        }
                    }
                };
                if is_static {
                    let mut vals: Vec<P<Expr>> = vec![];
                    for c in val {
                        vals.push(mk().lit_expr(mk().int_lit(c as u128, LitIntType::Unsuffixed)));
                    }
                    let array = mk().array_expr(vals);
                    Ok(WithStmts::new(array))
                } else {
                    let u8_ty = mk().path_ty(vec!["u8"]);
                    let width_lit =
                        mk().lit_expr(mk().int_lit(val.len() as u128, LitIntType::Unsuffixed));
                    let array_ty = mk().array_ty(u8_ty, width_lit);
                    let source_ty = mk().ref_ty(array_ty);
                    let mutbl = if ty.qualifiers.is_const {
                        Mutability::Immutable
                    } else {
                        Mutability::Mutable
                    };
                    let target_ty = mk().set_mutbl(mutbl).ref_ty(self.convert_type(ty.ctype)?);
                    let byte_literal = mk().lit_expr(mk().bytestr_lit(val));
                    let pointer = transmute_expr(source_ty, target_ty, byte_literal);
                    let array = mk().unary_expr(ast::UnOp::Deref, pointer);
                    Ok(WithStmts::new(array))
                }
            }
        }
    }

    /// Convert an initialization list into an expresion. These initialization lists can be
    /// used as array literals, struct literals, and union literals in code.
    pub fn convert_init_list(
        &self,
        ctx: ExprContext,
        ty: CQualTypeId,
        ids: &[CExprId],
        opt_union_field_id: Option<CFieldId>,
    ) -> Result<WithStmts<P<Expr>>, String> {
        match self.ast_context.resolve_type(ty.ctype).kind {
            CTypeKind::ConstantArray(ty, n) => {
                // Convert all of the provided initializer values

                // Need to check to see if the next item is a string literal,
                // if it is need to treat it as a declaration, rather than
                // an init list. https://github.com/GaloisInc/C2Rust/issues/40
                let mut is_string = false;

                if ids.len() == 1 {
                    let v = ids.first().unwrap();
                    if let CExprKind::Literal(_, CLiteral::String { .. }) =
                        self.ast_context.index(*v).kind
                    {
                        is_string = true;
                    }
                }

                let mut stmts: Vec<Stmt> = vec![];
                let val: P<Expr> = if is_string {
                    let v = ids.first().unwrap();
                    let mut x = self.convert_expr(ctx.used(), *v)?;
                    stmts.append(&mut x.stmts);
                    x.val
                } else {
                    let mut vals: Vec<P<Expr>> = vec![];
                    for &v in ids {
                        let mut x = self.convert_expr(ctx.used(), v)?;

                        // Array literals require all of their elements to be the correct type; they
                        // will not use implicit casts to change mut to const. This becomes a problem
                        // when an array literal is used in a position where there is no type information
                        // available to force its type to the correct const or mut variation. To avoid
                        // this issue we manually insert the otherwise elided casts in this particular context.
                        if let CExprKind::ImplicitCast(ty, _, CastKind::ConstCast, _, _) =
                            self.ast_context[v].kind
                        {
                            let t = self.convert_type(ty.ctype)?;
                            x.val = mk().cast_expr(x.val, t)
                        }

                        stmts.append(&mut x.stmts);
                        vals.push(x.val);
                    }
                    // Pad out the array literal with default values to the desired size
                    for _i in ids.len()..n {
                        vals.push(self.implicit_default_expr(ty, ctx.is_static)?)
                    }
                    mk().array_expr(vals)
                };

                Ok(WithStmts { stmts, val })
            }
            CTypeKind::Struct(struct_id) => {
                self.convert_struct_literal(ctx, struct_id, ids.as_ref())
            }
            CTypeKind::Union(union_id) => self.convert_union_literal(
                ctx,
                union_id,
                ids.as_ref(),
                ty,
                opt_union_field_id,
            ),
            CTypeKind::Pointer(_) => {
                let id = ids.first().unwrap();
                let mut x = self.convert_expr(ctx.used(), *id);
                Ok(x.unwrap())
            }
            CTypeKind::Vector(CQualTypeId { ctype, .. }, len) => {
                self.vector_list_initializer(ctx, ids, ctype, len)
            }
            ref t => Err(format!("Init list not implemented for {:?}", t)),
        }
    }

    fn convert_union_literal(
        &self,
        ctx: ExprContext,
        union_id: CRecordId,
        ids: &[CExprId],
        _ty: CQualTypeId,
        opt_union_field_id: Option<CFieldId>,
    ) -> Result<WithStmts<P<Expr>>, String> {
        let union_field_id = opt_union_field_id.expect("union field ID");

        match self.ast_context.index(union_id).kind {
            CDeclKind::Union { .. } => {
                let union_name = self
                    .type_converter
                    .borrow()
                    .resolve_decl_name(union_id)
                    .unwrap();
                match self.ast_context.index(union_field_id).kind {
                    CDeclKind::Field { typ: field_ty, .. } => {
                        let val = if ids.is_empty() {
                            WithStmts {
                                stmts: vec![],
                                val: self.implicit_default_expr(field_ty.ctype, ctx.is_static)?,
                            }
                        } else {
                            self.convert_expr(ctx.used(), ids[0])?
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

    fn convert_struct_literal(
        &self,
        ctx: ExprContext,
        struct_id: CRecordId,
        ids: &[CExprId],
    ) -> Result<WithStmts<P<Expr>>, String> {
        let mut has_bitfields = false;
        let (field_decls, platform_byte_size) = match self.ast_context.index(struct_id).kind {
            CDeclKind::Struct { ref fields, platform_byte_size, .. } => {
                let mut fieldnames = vec![];

                let fields = match fields {
                    &Some(ref fields) => fields,
                    &None => return Err(format!("Attempted to construct forward-declared struct")),
                };

                for &x in fields {
                    let name = self
                        .type_converter
                        .borrow()
                        .resolve_field_name(Some(struct_id), x)
                        .unwrap();
                    if let CDeclKind::Field { typ, bitfield_width, platform_type_bitwidth, .. } = self.ast_context.index(x).kind {
                        has_bitfields |= bitfield_width.is_some();

                        fieldnames.push((name, typ, bitfield_width, platform_type_bitwidth));
                    } else {
                        panic!("Struct field decl type mismatch")
                    }
                }

                (fieldnames, platform_byte_size)
            }
            _ => panic!("Struct literal declaration mismatch"),
        };

        let struct_name = self
            .type_converter
            .borrow()
            .resolve_decl_name(struct_id)
            .unwrap();

        if has_bitfields {
            return self.convert_bitfield_struct_literal(struct_name, ids, field_decls, platform_byte_size, is_static);
        }

        let mut stmts: Vec<Stmt> = vec![];
        let mut fields: Vec<Field> = vec![];

        // Add specified record fields
        for i in 0usize..ids.len() {
            let v = ids[i];
            let &(ref field_name, _, _, _) = &field_decls[i];

            let mut x = self.convert_expr(ctx.used(), v)?;
            stmts.append(&mut x.stmts);
            fields.push(mk().field(field_name, x.val));
        }

        // Pad out remaining omitted record fields
        for i in ids.len()..fields.len() {
            let &(ref field_name, ty) = &field_decls[i];
            fields.push(mk().field(field_name, self.implicit_default_expr(ty.ctype, ctx.is_static)?));
        }

        Ok(WithStmts {
            stmts,
            val: mk().struct_expr(vec![mk().path_segment(struct_name)], fields),
        })
    }
}
