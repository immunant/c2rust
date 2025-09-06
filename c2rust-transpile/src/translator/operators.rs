//! This module provides translations of unary and binary operator expressions.

use super::*;

fn neg_expr(arg: Box<Expr>) -> Box<Expr> {
    mk().unary_expr(UnOp::Neg(Default::default()), arg)
}

fn wrapping_neg_expr(arg: Box<Expr>) -> Box<Expr> {
    mk().method_call_expr(arg, "wrapping_neg", vec![])
}

impl From<c_ast::BinOp> for BinOp {
    fn from(op: c_ast::BinOp) -> Self {
        match op {
            c_ast::BinOp::Multiply => BinOp::Mul(Default::default()),
            c_ast::BinOp::Divide => BinOp::Div(Default::default()),
            c_ast::BinOp::Modulus => BinOp::Rem(Default::default()),
            c_ast::BinOp::Add => BinOp::Add(Default::default()),
            c_ast::BinOp::Subtract => BinOp::Sub(Default::default()),
            c_ast::BinOp::ShiftLeft => BinOp::Shl(Default::default()),
            c_ast::BinOp::ShiftRight => BinOp::Shr(Default::default()),
            c_ast::BinOp::Less => BinOp::Lt(Default::default()),
            c_ast::BinOp::Greater => BinOp::Gt(Default::default()),
            c_ast::BinOp::LessEqual => BinOp::Le(Default::default()),
            c_ast::BinOp::GreaterEqual => BinOp::Ge(Default::default()),
            c_ast::BinOp::EqualEqual => BinOp::Eq(Default::default()),
            c_ast::BinOp::NotEqual => BinOp::Ne(Default::default()),
            c_ast::BinOp::BitAnd => BinOp::BitAnd(Default::default()),
            c_ast::BinOp::BitXor => BinOp::BitXor(Default::default()),
            c_ast::BinOp::BitOr => BinOp::BitOr(Default::default()),
            c_ast::BinOp::And => BinOp::And(Default::default()),
            c_ast::BinOp::Or => BinOp::Or(Default::default()),

            _ => panic!("C BinOp {:?} is not a valid Rust BinOp", op),
        }
    }
}

impl<'c> Translation<'c> {
    pub fn convert_binary_expr(
        &self,
        mut ctx: ExprContext,
        expr_type_id: CQualTypeId,
        op: c_ast::BinOp,
        lhs: CExprId,
        rhs: CExprId,
        opt_lhs_type_id: Option<CQualTypeId>,
        opt_res_type_id: Option<CQualTypeId>,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        // If we're not making an assignment, a binop will require parens
        // applied to ternary conditionals
        if !op.is_assignment() {
            ctx.ternary_needs_parens = true;
        }

        let lhs_loc = &self.ast_context[lhs].loc;
        let rhs_loc = &self.ast_context[rhs].loc;
        use c_ast::BinOp::*;
        match op {
            Comma => {
                // The value of the LHS of a comma expression is always discarded
                self.convert_expr(ctx.unused(), lhs, None)?
                    .and_then(|_| self.convert_expr(ctx, rhs, Some(expr_type_id)))
            }

            And | Or => {
                let lhs = self.convert_condition(ctx, true, lhs)?;
                let rhs = self.convert_condition(ctx, true, rhs)?;
                lhs.map(|x| bool_to_int(mk().binary_expr(BinOp::from(op), x, rhs.to_expr())))
                    .and_then(|out| {
                        if ctx.is_unused() {
                            Ok(WithStmts::new(
                                vec![mk().semi_stmt(out)],
                                self.panic_or_err("Binary expression is not supposed to be used"),
                            ))
                        } else {
                            Ok(WithStmts::new_val(out))
                        }
                    })
            }

            // No sequence-point cases
            AssignAdd | AssignSubtract | AssignMultiply | AssignDivide | AssignModulus
            | AssignBitXor | AssignShiftLeft | AssignShiftRight | AssignBitOr | AssignBitAnd
            | Assign => self.convert_assignment_operator(
                ctx,
                op,
                expr_type_id,
                lhs,
                rhs,
                opt_lhs_type_id,
                opt_res_type_id,
            ),

            _ => {
                // Comparing references to pointers isn't consistently supported by rust
                // and so we need to decay references to pointers to do so. See
                // https://github.com/rust-lang/rust/issues/53772. This might be removable
                // once the above issue is resolved.
                if op == c_ast::BinOp::EqualEqual || op == c_ast::BinOp::NotEqual {
                    ctx = ctx.decay_ref();
                }

                let ty = self.convert_type(expr_type_id.ctype)?;

                let lhs_kind = &self.ast_context.index(lhs).kind;
                let mut lhs_type_id = lhs_kind.get_qual_type().ok_or_else(|| {
                    format_translation_err!(
                        self.ast_context.display_loc(lhs_loc),
                        "bad lhs type for assignment"
                    )
                })?;
                let rhs_kind = &self.ast_context.index(rhs).kind;
                let mut rhs_type_id = rhs_kind.get_qual_type().ok_or_else(|| {
                    format_translation_err!(
                        self.ast_context.display_loc(rhs_loc),
                        "bad rhs type for assignment"
                    )
                })?;

                // If this operation will (in Rust) take args of the same type, then propagate our
                // expected type down to the translation of our argument expressions.
                let lhs_resolved_ty = self.ast_context.resolve_type(lhs_type_id.ctype);
                let rhs_resolved_ty = self.ast_context.resolve_type(rhs_type_id.ctype);
                let expr_ty_kind = &self.ast_context.index(expr_type_id.ctype).kind;
                // Addition and subtraction can accept one pointer argument for .offset(), in which
                // case we don't want to homogenize arg types.
                if !lhs_resolved_ty.kind.is_pointer()
                    && !rhs_resolved_ty.kind.is_pointer()
                    && !expr_ty_kind.is_pointer()
                {
                    if op.all_types_same() {
                        // Ops like division and bitxor accept inputs of their expected result type.
                        lhs_type_id = expr_type_id;
                        rhs_type_id = expr_type_id;
                    } else if op.input_types_same() && lhs_resolved_ty.kind != rhs_resolved_ty.kind
                    {
                        // Ops like comparisons require argument types to match, but the result type
                        // doesn't inform us what type to choose. Select a synthetic definition of a
                        // portable rust type (e.g. u64 or usize) if either arg is one.
                        trace!(
                            "Binary op arg types differ: {:?} vs {:?}",
                            lhs_resolved_ty.kind,
                            rhs_resolved_ty.kind
                        );
                        let ty = if CTypeKind::PULLBACK_KINDS.contains(&lhs_resolved_ty.kind) {
                            lhs_type_id
                        } else {
                            rhs_type_id
                        };
                        lhs_type_id = ty;
                        rhs_type_id = ty;
                    } else if matches!(op, ShiftLeft | ShiftRight) {
                        lhs_type_id = expr_type_id;
                    }
                }

                if ctx.is_unused() {
                    Ok(self
                        .convert_expr(ctx, lhs, Some(lhs_type_id))?
                        .and_then(|_| self.convert_expr(ctx, rhs, Some(rhs_type_id)))?
                        .map(|_| self.panic_or_err("Binary expression is not supposed to be used")))
                } else {
                    let rhs_ctx = ctx;

                    // When we use methods on pointers (ie wrapping_offset_from or offset)
                    // we must ensure we have an explicit raw ptr for the self param, as
                    // self references do not decay
                    if op == c_ast::BinOp::Subtract || op == c_ast::BinOp::Add {
                        let ty_kind = &self.ast_context.resolve_type(lhs_type_id.ctype).kind;

                        if let CTypeKind::Pointer(_) = ty_kind {
                            ctx = ctx.decay_ref();
                        }
                    }

                    self.convert_expr(ctx, lhs, Some(lhs_type_id))?
                        .and_then(|lhs_val| {
                            self.convert_expr(rhs_ctx, rhs, Some(rhs_type_id))?
                                .and_then(|rhs_val| {
                                    let expr_ids = Some((lhs, rhs));
                                    self.convert_binary_operator(
                                        op,
                                        ty,
                                        expr_type_id.ctype,
                                        lhs_type_id,
                                        rhs_type_id,
                                        lhs_val,
                                        rhs_val,
                                        expr_ids,
                                    )
                                })
                        })
                }
            }
        }
    }

    fn convert_assignment_operator_aux(
        &self,
        bin_op_kind: BinOp,
        bin_op: c_ast::BinOp,
        read: Box<Expr>,
        write: Box<Expr>,
        rhs: Box<Expr>,
        compute_lhs_type_id: CQualTypeId,
        compute_res_type_id: CQualTypeId,
        lhs_type_id: CQualTypeId,
        rhs_type_id: CQualTypeId,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        if self.ast_context.resolve_type_id(compute_lhs_type_id.ctype)
            == self.ast_context.resolve_type_id(lhs_type_id.ctype)
        {
            Ok(WithStmts::new_val(mk().assign_op_expr(
                bin_op_kind,
                write,
                rhs,
            )))
        } else {
            let compute_lhs_resolved_ty = &self.ast_context.resolve_type(compute_lhs_type_id.ctype);
            let lhs_type = self.convert_type(compute_lhs_type_id.ctype)?;

            // We can't simply as-cast into a non primitive like f128
            let lhs = if compute_lhs_resolved_ty.kind == CTypeKind::LongDouble {
                self.use_crate(ExternCrate::F128);

                let fn_path = mk().path_expr(vec!["f128", "f128", "from"]);
                let args = vec![read];

                mk().call_expr(fn_path, args)
            } else {
                mk().cast_expr(read, lhs_type.clone())
            };
            let ty = self.convert_type(compute_res_type_id.ctype)?;
            let mut val = self.convert_binary_operator(
                bin_op,
                ty,
                compute_res_type_id.ctype,
                compute_lhs_type_id,
                rhs_type_id,
                lhs,
                rhs,
                None,
            )?;

            let is_enum_result = self
                .ast_context
                .resolve_type(lhs_type_id.ctype)
                .kind
                .is_enum();
            let result_type = self.convert_type(lhs_type_id.ctype)?;
            let val = if is_enum_result {
                val.set_unsafe();
                val.map(|val| transmute_expr(lhs_type, result_type, val))
            } else {
                // We can't as-cast from a non primitive like f128 back to the result_type
                if compute_lhs_resolved_ty.kind == CTypeKind::LongDouble {
                    let resolved_lhs_kind = &self.ast_context.resolve_type(lhs_type_id.ctype).kind;

                    self.f128_cast_to(val, resolved_lhs_kind)?
                } else {
                    val.map(|val| mk().cast_expr(val, result_type))
                }
            };
            Ok(val.map(|val| mk().assign_expr(write.clone(), val)))
        }
    }

    /// Translate an assignment binary operator.
    ///
    /// `compute_lhs_ty` and `compute_res_ty` correspond to Clang's
    /// `CompoundAssignOperator::{CompLHSType,CompResultType}`; see the Clang docs:
    /// https://clang.llvm.org/doxygen/classclang_1_1CompoundAssignOperator.html#details
    fn convert_assignment_operator(
        &self,
        ctx: ExprContext,
        op: c_ast::BinOp,
        expr_type_id: CQualTypeId,
        lhs: CExprId,
        rhs: CExprId,
        compute_lhs_type_id: Option<CQualTypeId>,
        compute_res_type_id: Option<CQualTypeId>,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        if op == c_ast::BinOp::Assign {
            assert!(compute_lhs_type_id.is_none());
            assert!(compute_res_type_id.is_none());
        } else {
            assert!(compute_lhs_type_id.is_some());
            assert!(compute_res_type_id.is_some());
        }

        let rhs_type_id = self
            .ast_context
            .index(rhs)
            .kind
            .get_qual_type()
            .ok_or_else(|| format_err!("bad assignment rhs type"))?;
        let lhs_kind = &self.ast_context.index(lhs).kind;
        let lhs_type_id = lhs_kind
            .get_qual_type()
            .ok_or_else(|| format_err!("bad initial lhs type"))?;

        // First, translate the rhs. Then, if it must match the lhs but doesn't, add a cast.
        let mut rhs_translation = self.convert_expr(ctx.used(), rhs, Some(rhs_type_id))?;
        let lhs_rhs_types_must_match = {
            let lhs_resolved_ty = &self.ast_context.resolve_type(lhs_type_id.ctype);
            let rhs_resolved_ty = &self.ast_context.resolve_type(rhs_type_id.ctype);
            // Addition and subtraction can accept one pointer argument for .offset(), in which
            // case we don't want to homogenize arg types.
            let neither_ptr =
                !lhs_resolved_ty.kind.is_pointer() && !rhs_resolved_ty.kind.is_pointer();

            use c_ast::BinOp::*;
            match op.underlying_assignment() {
                Some(Add) => neither_ptr,
                Some(Subtract) => neither_ptr,
                Some(Multiply) => true,
                Some(Divide) => true,
                Some(Modulus) => true,
                Some(BitXor) => true,
                Some(ShiftLeft) => false,
                Some(ShiftRight) => false,
                Some(BitOr) => true,
                Some(BitAnd) => true,
                None => true,
                _ => unreachable!(),
            }
        };
        if lhs_rhs_types_must_match {
            // For compound assignment, use the compute type; for regular assignment, use lhs type
            let effective_lhs_ty = compute_lhs_type_id.unwrap_or(lhs_type_id);
            if effective_lhs_ty.ctype != rhs_type_id.ctype {
                let new_rhs_ty =
                    self.convert_type(compute_lhs_type_id.unwrap_or(lhs_type_id).ctype)?;
                rhs_translation = rhs_translation.map(|val| mk().cast_expr(val, new_rhs_ty));
            }
        }

        // Now that we've translated the rhs, finish translating the assignment operator.
        self.convert_assignment_operator_with_rhs(
            ctx,
            op,
            expr_type_id,
            lhs,
            rhs_type_id,
            rhs_translation,
            compute_lhs_type_id,
            compute_res_type_id,
        )
    }

    /// Translate an assignment binary operator, provided a pre-translated RHS expression
    fn convert_assignment_operator_with_rhs(
        &self,
        ctx: ExprContext,
        op: c_ast::BinOp,
        expr_type_id: CQualTypeId,
        lhs: CExprId,
        rhs_type_id: CQualTypeId,
        rhs_translation: WithStmts<Box<Expr>>,
        compute_lhs_type_id: Option<CQualTypeId>,
        compute_res_type_id: Option<CQualTypeId>,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        if op == c_ast::BinOp::Assign {
            assert!(compute_lhs_type_id.is_none());
            assert!(compute_res_type_id.is_none());
        } else {
            assert!(compute_lhs_type_id.is_some());
            assert!(compute_res_type_id.is_some());
        }

        let ty = self.convert_type(expr_type_id.ctype)?;

        let result_type_id = compute_res_type_id.unwrap_or(expr_type_id);
        let expr_or_comp_type_id = compute_lhs_type_id.unwrap_or(expr_type_id);
        let initial_lhs = &self.ast_context.index(lhs).kind;
        let initial_lhs_type_id = initial_lhs
            .get_qual_type()
            .ok_or_else(|| format_err!("bad initial lhs type"))?;

        let bitfield_id = match initial_lhs {
            CExprKind::Member(_, _, decl_id, _, _) => {
                let kind = &self.ast_context[*decl_id].kind;

                if let CDeclKind::Field {
                    bitfield_width: Some(_),
                    ..
                } = kind
                {
                    Some(decl_id)
                } else {
                    None
                }
            }
            _ => None,
        };

        if let Some(field_id) = bitfield_id {
            let rhs_expr = mk().cast_expr(rhs_translation.to_expr(), ty);
            return self.convert_bitfield_assignment_op_with_rhs(ctx, op, lhs, rhs_expr, *field_id);
        }

        let is_volatile = initial_lhs_type_id.qualifiers.is_volatile;
        let is_volatile_compound_assign = op.underlying_assignment().is_some() && is_volatile;

        let expr_resolved_ty = self.ast_context.resolve_type(expr_type_id.ctype);
        let compute_resolved_ty = &self.ast_context.resolve_type(expr_or_comp_type_id.ctype);

        let pointer_lhs = match &expr_resolved_ty.kind {
            &CTypeKind::Pointer(pointee) => Some(pointee),
            _ => None,
        };

        let is_unsigned_arith = match op {
            c_ast::BinOp::AssignAdd
            | c_ast::BinOp::AssignSubtract
            | c_ast::BinOp::AssignMultiply
            | c_ast::BinOp::AssignDivide
            | c_ast::BinOp::AssignModulus => compute_resolved_ty.kind.is_unsigned_integral_type(),
            _ => false,
        };

        let lhs_translation = if initial_lhs_type_id.ctype != expr_or_comp_type_id.ctype
            || ctx.is_used()
            || pointer_lhs.is_some()
            || is_volatile_compound_assign
            || is_unsigned_arith
        {
            self.name_reference_write_read(ctx, lhs)?
        } else {
            self.name_reference_write(ctx, lhs)?.map(|named_ref| {
                named_ref
                    .map_rvalue(|()| self.panic_or_err("Volatile value is not supposed to be read"))
            })
        };

        rhs_translation.and_then(|rhs| {
            lhs_translation.and_then(
                |NamedReference {
                     lvalue: write,
                     rvalue: read,
                 }| {
                    // Assignment expression itself
                    use c_ast::BinOp::*;
                    let assign_stmt = match op {
                        // Regular (possibly volatile) assignment
                        Assign if !is_volatile => WithStmts::new_val(mk().assign_expr(write, rhs)),
                        Assign => WithStmts::new_unsafe_val(self.volatile_write(
                            write,
                            initial_lhs_type_id,
                            rhs,
                        )?),

                        // Anything volatile needs to be desugared into explicit reads and writes
                        op if is_volatile || is_unsigned_arith => {
                            let op = op
                                .underlying_assignment()
                                .expect("Cannot convert non-assignment operator");

                            let val = if expr_or_comp_type_id.ctype == initial_lhs_type_id.ctype {
                                self.convert_binary_operator(
                                    op,
                                    ty,
                                    expr_type_id.ctype,
                                    initial_lhs_type_id,
                                    rhs_type_id,
                                    read.clone(),
                                    rhs,
                                    None,
                                )?
                            } else {
                                let lhs_type =
                                    self.convert_type(compute_lhs_type_id.unwrap().ctype)?;
                                let write_type = self.convert_type(expr_type_id.ctype)?;
                                let lhs = mk().cast_expr(read.clone(), lhs_type.clone());
                                let ty = self.convert_type(result_type_id.ctype)?;
                                let mut val = self.convert_binary_operator(
                                    op,
                                    ty,
                                    result_type_id.ctype,
                                    expr_or_comp_type_id,
                                    rhs_type_id,
                                    lhs,
                                    rhs,
                                    None,
                                )?;

                                let expr_resolved_ty =
                                    self.ast_context.resolve_type(expr_type_id.ctype);
                                let is_enum_result = expr_resolved_ty.kind.is_enum();
                                let expr_type = self.convert_type(expr_type_id.ctype)?;
                                let val = if is_enum_result {
                                    val.set_unsafe();
                                    val.map(|val| transmute_expr(lhs_type, expr_type, val))
                                } else {
                                    val.map(|val| mk().cast_expr(val, expr_type))
                                };
                                val.map(|val| mk().cast_expr(val, write_type))
                            };

                            let write = if is_volatile {
                                val.and_then(|val| {
                                    TranslationResult::Ok(WithStmts::new_unsafe_val(
                                        self.volatile_write(write, initial_lhs_type_id, val)?,
                                    ))
                                })?
                            } else {
                                val.map(|val| mk().assign_expr(write, val))
                            };
                            write
                        }

                        // Everything else
                        AssignAdd if pointer_lhs.is_some() => {
                            let mul = self.compute_size_of_expr(pointer_lhs.unwrap().ctype);
                            let ptr = pointer_offset(write.clone(), rhs, mul, false, false);
                            WithStmts::new_unsafe_val(mk().assign_expr(write, ptr))
                        }
                        AssignSubtract if pointer_lhs.is_some() => {
                            let mul = self.compute_size_of_expr(pointer_lhs.unwrap().ctype);
                            let ptr = pointer_offset(write.clone(), rhs, mul, true, false);
                            WithStmts::new_unsafe_val(mk().assign_expr(write, ptr))
                        }

                        _ => {
                            if let (AssignAdd | AssignSubtract, Some(pointer_lhs)) =
                                (op, pointer_lhs)
                            {
                                let mul = self.compute_size_of_expr(pointer_lhs.ctype);
                                let ptr = pointer_offset(
                                    write.clone(),
                                    rhs,
                                    mul,
                                    op == AssignSubtract,
                                    false,
                                );
                                WithStmts::new_unsafe_val(mk().assign_expr(write, ptr))
                            } else {
                                fn eq<Token: Default, F: Fn(Token) -> BinOp>(f: F) -> BinOp {
                                    f(Default::default())
                                }

                                let (bin_op, bin_op_kind) = match op {
                                    AssignAdd => (Add, eq(BinOp::AddAssign)),
                                    AssignSubtract => (Subtract, eq(BinOp::SubAssign)),
                                    AssignMultiply => (Multiply, eq(BinOp::MulAssign)),
                                    AssignDivide => (Divide, eq(BinOp::DivAssign)),
                                    AssignModulus => (Modulus, eq(BinOp::RemAssign)),
                                    AssignBitXor => (BitXor, eq(BinOp::BitXorAssign)),
                                    AssignShiftLeft => (ShiftLeft, eq(BinOp::ShlAssign)),
                                    AssignShiftRight => (ShiftRight, eq(BinOp::ShrAssign)),
                                    AssignBitOr => (BitOr, eq(BinOp::BitOrAssign)),
                                    AssignBitAnd => (BitAnd, eq(BinOp::BitAndAssign)),
                                    _ => panic!("Cannot convert non-assignment operator"),
                                };
                                self.convert_assignment_operator_aux(
                                    bin_op_kind,
                                    bin_op,
                                    read.clone(),
                                    write,
                                    rhs,
                                    compute_lhs_type_id.unwrap(),
                                    compute_res_type_id.unwrap(),
                                    expr_type_id,
                                    rhs_type_id,
                                )?
                            }
                        }
                    };

                    assign_stmt.and_then(|assign_stmt| {
                        Ok(WithStmts::new(vec![mk().semi_stmt(assign_stmt)], read))
                    })
                },
            )
        })
    }

    /// Translate a non-assignment binary operator. It is expected that the `lhs` and `rhs`
    /// arguments be usable as rvalues.
    fn convert_binary_operator(
        &self,
        op: c_ast::BinOp,
        ty: Box<Type>,
        ctype: CTypeId,
        lhs_type: CQualTypeId,
        rhs_type: CQualTypeId,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        lhs_rhs_ids: Option<(CExprId, CExprId)>,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let is_unsigned_integral_type = self
            .ast_context
            .resolve_type(ctype)
            .kind
            .is_unsigned_integral_type();

        Ok(WithStmts::new_val(match op {
            c_ast::BinOp::Add => return self.convert_addition(lhs_type, rhs_type, lhs, rhs),
            c_ast::BinOp::Subtract => {
                return self.convert_subtraction(ty, lhs_type, rhs_type, lhs, rhs)
            }

            c_ast::BinOp::Multiply if is_unsigned_integral_type => {
                mk().method_call_expr(lhs, "wrapping_mul", vec![rhs])
            }
            c_ast::BinOp::Multiply => mk().binary_expr(BinOp::Mul(Default::default()), lhs, rhs),

            c_ast::BinOp::Divide if is_unsigned_integral_type => {
                mk().method_call_expr(lhs, "wrapping_div", vec![rhs])
            }
            c_ast::BinOp::Divide => mk().binary_expr(BinOp::Div(Default::default()), lhs, rhs),

            c_ast::BinOp::Modulus if is_unsigned_integral_type => {
                mk().method_call_expr(lhs, "wrapping_rem", vec![rhs])
            }
            c_ast::BinOp::Modulus => mk().binary_expr(BinOp::Rem(Default::default()), lhs, rhs),

            c_ast::BinOp::BitXor => mk().binary_expr(BinOp::BitXor(Default::default()), lhs, rhs),

            c_ast::BinOp::ShiftRight => mk().binary_expr(BinOp::Shr(Default::default()), lhs, rhs),
            c_ast::BinOp::ShiftLeft => mk().binary_expr(BinOp::Shl(Default::default()), lhs, rhs),

            c_ast::BinOp::EqualEqual => {
                // Using is_none method for null comparison means we don't have to
                // rely on the PartialEq trait as much and is also more idiomatic
                let expr = if let Some((lhs_expr_id, rhs_expr_id)) = lhs_rhs_ids {
                    let fn_eq_null = self.ast_context.is_function_pointer(lhs_type.ctype)
                        && self.ast_context.is_null_expr(rhs_expr_id);
                    let null_eq_fn = self.ast_context.is_function_pointer(rhs_type.ctype)
                        && self.ast_context.is_null_expr(lhs_expr_id);

                    if fn_eq_null {
                        mk().method_call_expr(lhs, "is_none", vec![])
                    } else if null_eq_fn {
                        mk().method_call_expr(rhs, "is_none", vec![])
                    } else {
                        mk().binary_expr(BinOp::Eq(Default::default()), lhs, rhs)
                    }
                } else {
                    mk().binary_expr(BinOp::Eq(Default::default()), lhs, rhs)
                };

                bool_to_int(expr)
            }
            c_ast::BinOp::NotEqual => {
                // Using is_some method for null comparison means we don't have to
                // rely on the PartialEq trait as much and is also more idiomatic
                let expr = if let Some((lhs_expr_id, rhs_expr_id)) = lhs_rhs_ids {
                    let fn_eq_null = self.ast_context.is_function_pointer(lhs_type.ctype)
                        && self.ast_context.is_null_expr(rhs_expr_id);
                    let null_eq_fn = self.ast_context.is_function_pointer(rhs_type.ctype)
                        && self.ast_context.is_null_expr(lhs_expr_id);

                    if fn_eq_null {
                        mk().method_call_expr(lhs, "is_some", vec![])
                    } else if null_eq_fn {
                        mk().method_call_expr(rhs, "is_some", vec![])
                    } else {
                        mk().binary_expr(BinOp::Ne(Default::default()), lhs, rhs)
                    }
                } else {
                    mk().binary_expr(BinOp::Ne(Default::default()), lhs, rhs)
                };

                bool_to_int(expr)
            }
            c_ast::BinOp::Less => {
                bool_to_int(mk().binary_expr(BinOp::Lt(Default::default()), lhs, rhs))
            }
            c_ast::BinOp::Greater => {
                bool_to_int(mk().binary_expr(BinOp::Gt(Default::default()), lhs, rhs))
            }
            c_ast::BinOp::GreaterEqual => {
                bool_to_int(mk().binary_expr(BinOp::Ge(Default::default()), lhs, rhs))
            }
            c_ast::BinOp::LessEqual => {
                bool_to_int(mk().binary_expr(BinOp::Le(Default::default()), lhs, rhs))
            }

            c_ast::BinOp::BitAnd => mk().binary_expr(BinOp::BitAnd(Default::default()), lhs, rhs),
            c_ast::BinOp::BitOr => mk().binary_expr(BinOp::BitOr(Default::default()), lhs, rhs),

            op => unimplemented!("Translation of binary operator {:?}", op),
        }))
    }

    fn convert_addition(
        &self,
        lhs_type_id: CQualTypeId,
        rhs_type_id: CQualTypeId,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let lhs_type = &self.ast_context.resolve_type(lhs_type_id.ctype).kind;
        let rhs_type = &self.ast_context.resolve_type(rhs_type_id.ctype).kind;

        if let &CTypeKind::Pointer(pointee) = lhs_type {
            let mul = self.compute_size_of_expr(pointee.ctype);
            Ok(WithStmts::new_unsafe_val(pointer_offset(
                lhs, rhs, mul, false, false,
            )))
        } else if let &CTypeKind::Pointer(pointee) = rhs_type {
            let mul = self.compute_size_of_expr(pointee.ctype);
            Ok(WithStmts::new_unsafe_val(pointer_offset(
                rhs, lhs, mul, false, false,
            )))
        } else if lhs_type.is_unsigned_integral_type() {
            Ok(WithStmts::new_val(mk().method_call_expr(
                lhs,
                "wrapping_add",
                vec![rhs],
            )))
        } else {
            Ok(WithStmts::new_val(mk().binary_expr(
                BinOp::Add(Default::default()),
                lhs,
                rhs,
            )))
        }
    }

    fn convert_subtraction(
        &self,
        ty: Box<Type>,
        lhs_type_id: CQualTypeId,
        rhs_type_id: CQualTypeId,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let lhs_type = &self.ast_context.resolve_type(lhs_type_id.ctype).kind;
        let rhs_type = &self.ast_context.resolve_type(rhs_type_id.ctype).kind;

        if let &CTypeKind::Pointer(pointee) = rhs_type {
            let mut offset = mk().method_call_expr(lhs, "offset_from", vec![rhs]);

            if let Some(sz) = self.compute_size_of_expr(pointee.ctype) {
                let div = cast_int(sz, "isize", false);
                offset = mk().binary_expr(BinOp::Div(Default::default()), offset, div);
            }

            Ok(WithStmts::new_unsafe_val(mk().cast_expr(offset, ty)))
        } else if let &CTypeKind::Pointer(pointee) = lhs_type {
            let mul = self.compute_size_of_expr(pointee.ctype);
            Ok(WithStmts::new_unsafe_val(pointer_offset(
                lhs, rhs, mul, true, false,
            )))
        } else if lhs_type.is_unsigned_integral_type() {
            Ok(WithStmts::new_val(mk().method_call_expr(
                lhs,
                "wrapping_sub",
                vec![rhs],
            )))
        } else {
            Ok(WithStmts::new_val(mk().binary_expr(
                BinOp::Sub(Default::default()),
                lhs,
                rhs,
            )))
        }
    }

    fn convert_pre_increment(
        &self,
        ctx: ExprContext,
        ty: CQualTypeId,
        up: bool,
        arg: CExprId,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let op = if up {
            c_ast::BinOp::AssignAdd
        } else {
            c_ast::BinOp::AssignSubtract
        };
        let one = match self.ast_context.resolve_type(ty.ctype).kind {
            // TODO: If rust gets f16 support:
            // CTypeKind::Half |
            CTypeKind::Float | CTypeKind::Double => mk().lit_expr(mk().float_unsuffixed_lit("1.")),
            CTypeKind::LongDouble => {
                self.use_crate(ExternCrate::F128);

                let fn_path = mk().path_expr(vec!["f128", "f128", "new"]);
                let args = vec![mk().lit_expr(mk().float_unsuffixed_lit("1."))];

                mk().call_expr(fn_path, args)
            }
            _ => mk().lit_expr(mk().int_unsuffixed_lit(1)),
        };
        let arg_type = self.ast_context[arg]
            .kind
            .get_qual_type()
            .ok_or_else(|| format_err!("bad arg type"))?;
        self.convert_assignment_operator_with_rhs(
            ctx.used(),
            op,
            arg_type,
            arg,
            ty,
            WithStmts::new_val(one),
            Some(arg_type),
            Some(arg_type),
        )
    }

    fn convert_post_increment(
        &self,
        ctx: ExprContext,
        ty: CQualTypeId,
        up: bool,
        arg: CExprId,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        // If we aren't going to be using the result, may as well do a simple pre-increment
        if ctx.is_unused() {
            return self.convert_pre_increment(ctx, ty, up, arg);
        }

        let ty = self
            .ast_context
            .index(arg)
            .kind
            .get_qual_type()
            .ok_or_else(|| format_err!("bad post inc type"))?;

        self.name_reference_write_read(ctx, arg)?.and_then(
            |NamedReference {
                 lvalue: write,
                 rvalue: read,
             }| {
                let val_name = self.renamer.borrow_mut().fresh();
                let save_old_val = mk().local_stmt(Box::new(mk().local(
                    mk().ident_pat(&val_name),
                    None,
                    Some(read.clone()),
                )));

                let mut one = match self.ast_context[ty.ctype].kind {
                    // TODO: If rust gets f16 support:
                    // CTypeKind::Half |
                    CTypeKind::Float | CTypeKind::Double => {
                        mk().lit_expr(mk().float_unsuffixed_lit("1."))
                    }
                    CTypeKind::LongDouble => {
                        self.use_crate(ExternCrate::F128);

                        let fn_path = mk().path_expr(vec!["f128", "f128", "new"]);
                        let args = vec![mk().lit_expr(mk().float_unsuffixed_lit("1."))];

                        mk().call_expr(fn_path, args)
                    }
                    _ => mk().lit_expr(mk().int_unsuffixed_lit(1)),
                };

                let mut is_unsafe = false; // Track unsafety if we call `pointer::offset`.

                // *p + 1
                let val = if let &CTypeKind::Pointer(pointee) =
                    &self.ast_context.resolve_type(ty.ctype).kind
                {
                    if let Some(n) = self.compute_size_of_expr(pointee.ctype) {
                        one = n
                    }

                    let n = if up {
                        one
                    } else {
                        mk().unary_expr(UnOp::Neg(Default::default()), one)
                    };
                    is_unsafe = true;
                    mk().method_call_expr(read, "offset", vec![n])
                } else if self
                    .ast_context
                    .resolve_type(ty.ctype)
                    .kind
                    .is_unsigned_integral_type()
                {
                    let m = if up { "wrapping_add" } else { "wrapping_sub" };
                    mk().method_call_expr(read, m, vec![one])
                } else {
                    let k = if up {
                        BinOp::Add(Default::default())
                    } else {
                        BinOp::Sub(Default::default())
                    };
                    mk().binary_expr(k, read, one)
                };

                // *p = *p + rhs
                let assign_stmt = if ty.qualifiers.is_volatile {
                    is_unsafe = true;
                    self.volatile_write(write, ty, val)?
                } else {
                    mk().assign_expr(write, val)
                };

                let mut val = WithStmts::new(
                    vec![save_old_val, mk().expr_stmt(assign_stmt)],
                    mk().ident_expr(val_name),
                );
                if is_unsafe {
                    val.set_unsafe();
                }
                Ok(val)
            },
        )
    }

    pub fn convert_unary_operator(
        &self,
        mut ctx: ExprContext,
        name: c_ast::UnOp,
        cqual_type: CQualTypeId,
        arg: CExprId,
        lrvalue: LRValue,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let CQualTypeId { ctype, .. } = cqual_type;
        let ty = self.convert_type(ctype)?;
        let resolved_ctype = self.ast_context.resolve_type(ctype);

        let mut unary = match name {
            c_ast::UnOp::AddressOf => {
                let arg_kind = &self.ast_context[arg].kind;

                match arg_kind {
                    // C99 6.5.3.2 para 4
                    CExprKind::Unary(_, c_ast::UnOp::Deref, target, _) => {
                        return self.convert_expr(ctx, *target, None)
                    }
                    // An AddrOf DeclRef/Member is safe to not decay if the translator isn't already giving a hard
                    // yes to decaying (ie, BitCasts). So we only convert default to no decay.
                    CExprKind::DeclRef(..) | CExprKind::Member(..) => {
                        ctx.decay_ref.set_default_to_no()
                    }
                    _ => (),
                };

                // In this translation, there are only pointers to functions and
                // & becomes a no-op when applied to a function.

                let arg = self.convert_expr(ctx.used().set_needs_address(true), arg, None)?;

                if self.ast_context.is_function_pointer(ctype) {
                    Ok(arg.map(|x| mk().call_expr(mk().ident_expr("Some"), vec![x])))
                } else {
                    let pointee_ty =
                        self.ast_context
                            .get_pointee_qual_type(ctype)
                            .ok_or_else(|| {
                                TranslationError::generic("Address-of should return a pointer")
                            })?;

                    let mutbl = if pointee_ty.qualifiers.is_const {
                        Mutability::Immutable
                    } else {
                        Mutability::Mutable
                    };

                    arg.result_map(|a| {
                        let mut addr_of_arg: Box<Expr>;

                        if ctx.is_static {
                            // static variable initializers aren't able to use &mut,
                            // so we work around that by using & and an extra cast
                            // through & to *const to *mut
                            addr_of_arg = mk().addr_of_expr(a);
                            if let Mutability::Mutable = mutbl {
                                let mut qtype = pointee_ty;
                                qtype.qualifiers.is_const = true;
                                let ty_ = self
                                    .type_converter
                                    .borrow_mut()
                                    .convert_pointer(&self.ast_context, qtype)?;
                                addr_of_arg = mk().cast_expr(addr_of_arg, ty_);
                            }
                        } else {
                            // Normal case is allowed to use &mut if needed
                            addr_of_arg = mk().set_mutbl(mutbl).addr_of_expr(a);

                            // Avoid unnecessary reference to pointer decay in fn call args:
                            if ctx.decay_ref.is_no() {
                                return Ok(addr_of_arg);
                            }
                        }

                        Ok(mk().cast_expr(addr_of_arg, ty))
                    })
                }
            }
            c_ast::UnOp::PreIncrement => self.convert_pre_increment(ctx, cqual_type, true, arg),
            c_ast::UnOp::PreDecrement => self.convert_pre_increment(ctx, cqual_type, false, arg),
            c_ast::UnOp::PostIncrement => self.convert_post_increment(ctx, cqual_type, true, arg),
            c_ast::UnOp::PostDecrement => self.convert_post_increment(ctx, cqual_type, false, arg),
            c_ast::UnOp::Deref => {
                match self.ast_context[arg].kind {
                    CExprKind::Unary(_, c_ast::UnOp::AddressOf, arg_, _) => {
                        self.convert_expr(ctx.used(), arg_, None)
                    }
                    _ => {
                        self.convert_expr(ctx.used(), arg, None)?
                            .result_map(|val: Box<Expr>| {
                                if let CTypeKind::Function(..) =
                                    self.ast_context.resolve_type(ctype).kind
                                {
                                    Ok(unwrap_function_pointer(val))
                                } else if let Some(_vla) = self.compute_size_of_expr(ctype) {
                                    Ok(val)
                                } else {
                                    let mut val =
                                        mk().unary_expr(UnOp::Deref(Default::default()), val);

                                    // If the type on the other side of the pointer we are dereferencing is volatile and
                                    // this whole expression is not an LValue, we should make this a volatile read
                                    if lrvalue.is_rvalue() && cqual_type.qualifiers.is_volatile {
                                        val = self.volatile_read(val, cqual_type)?
                                    }
                                    Ok(val)
                                }
                            })
                    }
                }
            }
            c_ast::UnOp::Plus => self.convert_expr(ctx.used(), arg, Some(cqual_type)), // promotion is explicit in the clang AST

            c_ast::UnOp::Negate => {
                let val = self.convert_expr(ctx.used(), arg, Some(cqual_type))?;

                if resolved_ctype.kind.is_unsigned_integral_type() {
                    Ok(val.map(wrapping_neg_expr))
                } else {
                    Ok(val.map(neg_expr))
                }
            }
            c_ast::UnOp::Complement => Ok(self
                .convert_expr(ctx.used(), arg, Some(cqual_type))?
                .map(|a| mk().unary_expr(UnOp::Not(Default::default()), a))),

            c_ast::UnOp::Not => {
                let val = self.convert_condition(ctx, false, arg)?;
                Ok(val.map(|x| mk().cast_expr(x, mk().path_ty(vec!["std", "ffi", "c_int"]))))
            }
            c_ast::UnOp::Extension => {
                let arg = self.convert_expr(ctx, arg, Some(cqual_type))?;
                Ok(arg)
            }
            c_ast::UnOp::Real | c_ast::UnOp::Imag | c_ast::UnOp::Coawait => {
                panic!("Unsupported extension operator")
            }
        }?;

        // Some unused unary operators (`-foo()`) may have side effects, so we need
        // to add them to stmts when name is not increment/decrement operator.
        if ctx.is_unused()
            && !matches!(
                name,
                c_ast::UnOp::PreDecrement
                    | c_ast::UnOp::PreIncrement
                    | c_ast::UnOp::PostDecrement
                    | c_ast::UnOp::PostIncrement
            )
        {
            let v = unary.clone().into_value();
            unary.add_stmt(mk().semi_stmt(v));
        }
        Ok(unary)
    }
}
