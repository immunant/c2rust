//! This module provides translations of unary and binary operator expressions.

use super::*;

fn neg_expr(arg: P<Expr>) -> P<Expr> {
    mk().unary_expr(ast::UnOp::Neg, arg)
}

fn wrapping_neg_expr(arg: P<Expr>) -> P<Expr> {
    mk().method_call_expr(arg, "wrapping_neg", vec![] as Vec<P<Expr>>)
}

impl<'c> Translation<'c> {
    pub fn convert_binary_expr(
        &self,
        mut ctx: ExprContext,
        type_id: CQualTypeId,
        op: c_ast::BinOp,
        lhs: CExprId,
        rhs: CExprId,
        opt_lhs_type_id: Option<CQualTypeId>,
        opt_res_type_id: Option<CQualTypeId>,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        // If we're not making an assignment, a binop will require parens
        // applied to ternary conditionals
        if !op.is_assignment() {
            ctx.ternary_needs_parens = true;
        }

        match op {
            c_ast::BinOp::Comma => {
                // The value of the LHS of a comma expression is always discarded
                let lhs = self.convert_expr(ctx.unused(), lhs)?;
                let rhs = self.convert_expr(ctx, rhs)?;

                Ok(WithStmts {
                    stmts: lhs.stmts.into_iter().chain(rhs.stmts).collect(),
                    val: rhs.val,
                })
            }

            c_ast::BinOp::And => {
                let lhs = self.convert_condition(ctx, true, lhs)?;
                let rhs = self.convert_condition(ctx, true, rhs)?;
                let mut out =
                    lhs.map(|x| bool_to_int(mk().binary_expr(BinOpKind::And, x, rhs.to_expr())));

                if ctx.is_unused() {
                    let out_val = mem::replace(
                        &mut out.val,
                        self.panic_or_err("Binary expression is not supposed to be used"),
                    );
                    out.stmts.push(mk().semi_stmt(out_val));
                }

                Ok(out)
            }

            c_ast::BinOp::Or => {
                let lhs = self.convert_condition(ctx, true, lhs)?;
                let rhs = self.convert_condition(ctx, true, rhs)?;
                let mut out =
                    lhs.map(|x| bool_to_int(mk().binary_expr(BinOpKind::Or, x, rhs.to_expr())));

                if ctx.is_unused() {
                    let out_val = mem::replace(
                        &mut out.val,
                        self.panic_or_err("Binary expression is not supposed to be used"),
                    );
                    out.stmts.push(mk().semi_stmt(out_val));
                }

                Ok(out)
            }

            // No sequence-point cases
            c_ast::BinOp::AssignAdd
            | c_ast::BinOp::AssignSubtract
            | c_ast::BinOp::AssignMultiply
            | c_ast::BinOp::AssignDivide
            | c_ast::BinOp::AssignModulus
            | c_ast::BinOp::AssignBitXor
            | c_ast::BinOp::AssignShiftLeft
            | c_ast::BinOp::AssignShiftRight
            | c_ast::BinOp::AssignBitOr
            | c_ast::BinOp::AssignBitAnd
            | c_ast::BinOp::Assign => self.convert_assignment_operator(
                ctx,
                op,
                type_id,
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

                let ty = self.convert_type(type_id.ctype)?;

                let lhs_type = self
                    .ast_context
                    .index(lhs)
                    .kind
                    .get_qual_type()
                    .ok_or_else(|| format_err!("bad lhs type"))?;
                let rhs_type = self
                    .ast_context
                    .index(rhs)
                    .kind
                    .get_qual_type()
                    .ok_or_else(|| format_err!("bad rhs type"))?;

                let mut stmts = vec![];

                if ctx.is_unused() {
                    stmts.extend(self.convert_expr(ctx, lhs)?.stmts);
                    stmts.extend(self.convert_expr(ctx, rhs)?.stmts);

                    Ok(WithStmts {
                        stmts,
                        val: self.panic_or_err("Binary expression is not supposed to be used"),
                    })
                } else {
                    let WithStmts {
                        val: lhs_val,
                        stmts: lhs_stmts,
                    } = self.convert_expr(ctx, lhs)?;
                    let WithStmts {
                        val: rhs_val,
                        stmts: rhs_stmts,
                    } = self.convert_expr(ctx, rhs)?;

                    stmts.extend(lhs_stmts);
                    stmts.extend(rhs_stmts);
                    let expr_ids = Some((lhs, rhs));
                    let val = self.convert_binary_operator(
                        op,
                        ty,
                        type_id.ctype,
                        lhs_type,
                        rhs_type,
                        lhs_val,
                        rhs_val,
                        expr_ids,
                    );

                    Ok(WithStmts { stmts, val })
                }
            }
        }
    }

    fn covert_assignment_operator_aux(
        &self,
        bin_op_kind: BinOpKind,
        bin_op: c_ast::BinOp,
        read: P<Expr>,
        write: P<Expr>,
        rhs: P<Expr>,
        compute_lhs_ty: Option<CQualTypeId>,
        compute_res_ty: Option<CQualTypeId>,
        lhs_ty: CQualTypeId,
        rhs_ty: CQualTypeId,
    ) -> Result<P<Expr>, TranslationError> {
        let compute_lhs_ty = compute_lhs_ty.unwrap();
        let compute_res_ty = compute_res_ty.unwrap();

        if self.ast_context.resolve_type_id(compute_lhs_ty.ctype)
            == self.ast_context.resolve_type_id(lhs_ty.ctype)
        {
            Ok(mk().assign_op_expr(bin_op_kind, write, rhs))
        } else {
            let lhs_type = self.convert_type(compute_lhs_ty.ctype)?;
            let lhs = mk().cast_expr(read, lhs_type.clone());
            let ty = self.convert_type(compute_res_ty.ctype)?;
            let val = self.convert_binary_operator(
                bin_op,
                ty,
                compute_res_ty.ctype,
                compute_lhs_ty,
                rhs_ty,
                lhs,
                rhs,
                None,
            );

            let is_enum_result = self.ast_context[self.ast_context.resolve_type_id(lhs_ty.ctype)]
                .kind
                .is_enum();
            let result_type = self.convert_type(lhs_ty.ctype)?;
            let val = if is_enum_result {
                transmute_expr(lhs_type, result_type, val, self.tcfg.emit_no_std)
            } else {
                mk().cast_expr(val, result_type)
            };
            Ok(mk().assign_expr(write.clone(), val))
        }
    }

    fn convert_assignment_operator(
        &self,
        ctx: ExprContext,
        op: c_ast::BinOp,
        qtype: CQualTypeId,
        lhs: CExprId,
        rhs: CExprId,
        compute_type: Option<CQualTypeId>,
        result_type: Option<CQualTypeId>,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        let rhs_type_id = self
            .ast_context
            .index(rhs)
            .kind
            .get_qual_type()
            .ok_or_else(|| format_err!("bad assignment rhs type"))?;
        let rhs_translation = self.convert_expr(ctx.used(), rhs)?;
        self.convert_assignment_operator_with_rhs(
            ctx,
            op,
            qtype,
            lhs,
            rhs_type_id,
            rhs_translation,
            compute_type,
            result_type,
        )
    }

    /// Translate an assignment binary operator
    fn convert_assignment_operator_with_rhs(
        &self,
        ctx: ExprContext,
        op: c_ast::BinOp,
        qtype: CQualTypeId,
        lhs: CExprId,
        rhs_type_id: CQualTypeId,
        rhs_translation: WithStmts<P<Expr>>,
        compute_type: Option<CQualTypeId>,
        result_type: Option<CQualTypeId>,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        let ty = self.convert_type(qtype.ctype)?;

        let result_type_id = result_type.unwrap_or(qtype);
        let compute_lhs_type_id = compute_type.unwrap_or(qtype);
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
            let rhs_expr = if compute_lhs_type_id.ctype == initial_lhs_type_id.ctype {
                rhs_translation.to_expr()
            } else {
                mk().cast_expr(rhs_translation.to_expr(), ty)
            };

            return self.convert_bitfield_assignment_op_with_rhs(ctx, op, lhs, rhs_expr, *field_id);
        }

        let is_volatile = initial_lhs_type_id.qualifiers.is_volatile;
        let is_volatile_compound_assign = op.underlying_assignment().is_some() && is_volatile;

        let qtype_kind = &self.ast_context.resolve_type(qtype.ctype).kind;
        let compute_type_kind = &self
            .ast_context
            .resolve_type(compute_lhs_type_id.ctype)
            .kind;

        let pointer_lhs = match qtype_kind {
            &CTypeKind::Pointer(pointee) => Some(pointee),
            _ => None,
        };

        let is_unsigned_arith = match op {
            c_ast::BinOp::AssignAdd
            | c_ast::BinOp::AssignSubtract
            | c_ast::BinOp::AssignMultiply
            | c_ast::BinOp::AssignDivide
            | c_ast::BinOp::AssignModulus => compute_type_kind.is_unsigned_integral_type(),
            _ => false,
        };

        let (write, read, lhs_stmts) = if initial_lhs_type_id.ctype != compute_lhs_type_id.ctype
            || ctx.is_used()
            || pointer_lhs.is_some()
            || is_volatile_compound_assign
            || is_unsigned_arith
        {
            let WithStmts {
                val: (write, read),
                stmts: lhs_stmts,
            } = self.name_reference_write_read(ctx, lhs)?;
            (write, read, lhs_stmts)
        } else {
            let WithStmts {
                val: write,
                stmts: lhs_stmts,
            } = self.name_reference_write(ctx, lhs)?;
            (
                write,
                self.panic_or_err("Volatile value is not supposed to be read"),
                lhs_stmts,
            )
        };

        let WithStmts {
            val: rhs,
            stmts: rhs_stmts,
        } = rhs_translation;

        // Side effects to accumulate
        let mut stmts = vec![];
        stmts.extend(lhs_stmts);
        stmts.extend(rhs_stmts);

        // Assignment expression itself
        let assign_stmt = match op {
            // Regular (possibly volatile) assignment
            c_ast::BinOp::Assign if !is_volatile => mk().assign_expr(&write, rhs),
            c_ast::BinOp::Assign => self.volatile_write(&write, initial_lhs_type_id, rhs)?,

            // Anything volatile needs to be desugared into explicit reads and writes
            op if is_volatile || is_unsigned_arith => {
                let op = op
                    .underlying_assignment()
                    .expect("Cannot convert non-assignment operator");

                let val = if compute_lhs_type_id.ctype == initial_lhs_type_id.ctype {
                    self.convert_binary_operator(
                        op,
                        ty,
                        qtype.ctype,
                        initial_lhs_type_id,
                        rhs_type_id,
                        read.clone(),
                        rhs,
                        None,
                    )
                } else {
                    let lhs_type = self.convert_type(compute_type.unwrap().ctype)?;
                    let write_type = self.convert_type(qtype.ctype)?;
                    let lhs = mk().cast_expr(read.clone(), lhs_type.clone());
                    let ty = self.convert_type(result_type_id.ctype)?;
                    let val = self.convert_binary_operator(
                        op,
                        ty,
                        result_type_id.ctype,
                        compute_lhs_type_id,
                        rhs_type_id,
                        lhs,
                        rhs,
                        None,
                    );

                    let is_enum_result = self.ast_context
                        [self.ast_context.resolve_type_id(qtype.ctype)]
                    .kind
                    .is_enum();
                    let result_type = self.convert_type(qtype.ctype)?;
                    let val = if is_enum_result {
                        transmute_expr(lhs_type, result_type, val, self.tcfg.emit_no_std)
                    } else {
                        mk().cast_expr(val, result_type)
                    };
                    mk().cast_expr(val, write_type)
                };

                if is_volatile {
                    self.volatile_write(&write, initial_lhs_type_id, val)?
                } else {
                    mk().assign_expr(write, val)
                }
            }

            // Everything else
            c_ast::BinOp::AssignAdd if pointer_lhs.is_some() => {
                let ptr = match self.compute_size_of_expr(pointer_lhs.unwrap().ctype) {
                    Some(sz) => {
                        let offset = mk().binary_expr(
                            BinOpKind::Mul,
                            cast_int(rhs, "isize"),
                            cast_int(sz, "isize"),
                        );
                        pointer_offset_isize(write.clone(), offset)
                    }
                    None => pointer_offset(write.clone(), rhs),
                };
                mk().assign_expr(&write, ptr)
            }
            c_ast::BinOp::AssignSubtract if pointer_lhs.is_some() => {
                let ptr = match self.compute_size_of_expr(pointer_lhs.unwrap().ctype) {
                    Some(sz) => pointer_neg_offset_isize(
                        write.clone(),
                        mk().binary_expr(
                            BinOpKind::Mul,
                            cast_int(rhs, "isize"),
                            cast_int(sz, "isize"),
                        ),
                    ),
                    None => pointer_neg_offset(write.clone(), rhs),
                };
                mk().assign_expr(&write, ptr)
            }

            c_ast::BinOp::AssignAdd => self.covert_assignment_operator_aux(
                BinOpKind::Add,
                c_ast::BinOp::Add,
                read.clone(),
                write,
                rhs,
                compute_type,
                result_type,
                qtype,
                rhs_type_id,
            )?,
            c_ast::BinOp::AssignSubtract => self.covert_assignment_operator_aux(
                BinOpKind::Sub,
                c_ast::BinOp::Subtract,
                read.clone(),
                write,
                rhs,
                compute_type,
                result_type,
                qtype,
                rhs_type_id,
            )?,
            c_ast::BinOp::AssignMultiply => self.covert_assignment_operator_aux(
                BinOpKind::Mul,
                c_ast::BinOp::Multiply,
                read.clone(),
                write,
                rhs,
                compute_type,
                result_type,
                qtype,
                rhs_type_id,
            )?,
            c_ast::BinOp::AssignDivide => self.covert_assignment_operator_aux(
                BinOpKind::Div,
                c_ast::BinOp::Divide,
                read.clone(),
                write,
                rhs,
                compute_type,
                result_type,
                qtype,
                rhs_type_id,
            )?,
            c_ast::BinOp::AssignModulus => self.covert_assignment_operator_aux(
                BinOpKind::Rem,
                c_ast::BinOp::Modulus,
                read.clone(),
                write,
                rhs,
                compute_type,
                result_type,
                qtype,
                rhs_type_id,
            )?,
            c_ast::BinOp::AssignBitXor => self.covert_assignment_operator_aux(
                BinOpKind::BitXor,
                c_ast::BinOp::BitXor,
                read.clone(),
                write,
                rhs,
                compute_type,
                result_type,
                qtype,
                rhs_type_id,
            )?,
            c_ast::BinOp::AssignShiftLeft => self.covert_assignment_operator_aux(
                BinOpKind::Shl,
                c_ast::BinOp::ShiftLeft,
                read.clone(),
                write,
                rhs,
                compute_type,
                result_type,
                qtype,
                rhs_type_id,
            )?,
            c_ast::BinOp::AssignShiftRight => self.covert_assignment_operator_aux(
                BinOpKind::Shr,
                c_ast::BinOp::ShiftRight,
                read.clone(),
                write,
                rhs,
                compute_type,
                result_type,
                qtype,
                rhs_type_id,
            )?,
            c_ast::BinOp::AssignBitOr => self.covert_assignment_operator_aux(
                BinOpKind::BitOr,
                c_ast::BinOp::BitOr,
                read.clone(),
                write,
                rhs,
                compute_type,
                result_type,
                qtype,
                rhs_type_id,
            )?,
            c_ast::BinOp::AssignBitAnd => self.covert_assignment_operator_aux(
                BinOpKind::BitAnd,
                c_ast::BinOp::BitAnd,
                read.clone(),
                write,
                rhs,
                compute_type,
                result_type,
                qtype,
                rhs_type_id,
            )?,

            _ => panic!("Cannot convert non-assignment operator"),
        };

        stmts.push(mk().expr_stmt(assign_stmt));

        Ok(WithStmts { stmts, val: read })
    }

    /// Translate a non-assignment binary operator. It is expected that the `lhs` and `rhs`
    /// arguments be usable as rvalues.
    fn convert_binary_operator(
        &self,
        op: c_ast::BinOp,
        ty: P<Ty>,
        ctype: CTypeId,
        lhs_type: CQualTypeId,
        rhs_type: CQualTypeId,
        lhs: P<Expr>,
        rhs: P<Expr>,
        lhs_rhs_ids: Option<(CExprId, CExprId)>,
    ) -> P<Expr> {
        let is_unsigned_integral_type = self
            .ast_context
            .index(ctype)
            .kind
            .is_unsigned_integral_type();

        match op {
            c_ast::BinOp::Add => self.convert_addition(lhs_type, rhs_type, lhs, rhs),
            c_ast::BinOp::Subtract => self.convert_subtraction(ty, lhs_type, rhs_type, lhs, rhs),

            c_ast::BinOp::Multiply if is_unsigned_integral_type => {
                mk().method_call_expr(lhs, mk().path_segment("wrapping_mul"), vec![rhs])
            }
            c_ast::BinOp::Multiply => mk().binary_expr(BinOpKind::Mul, lhs, rhs),

            c_ast::BinOp::Divide if is_unsigned_integral_type => {
                mk().method_call_expr(lhs, mk().path_segment("wrapping_div"), vec![rhs])
            }
            c_ast::BinOp::Divide => mk().binary_expr(BinOpKind::Div, lhs, rhs),

            c_ast::BinOp::Modulus if is_unsigned_integral_type => {
                mk().method_call_expr(lhs, mk().path_segment("wrapping_rem"), vec![rhs])
            }
            c_ast::BinOp::Modulus => mk().binary_expr(BinOpKind::Rem, lhs, rhs),

            c_ast::BinOp::BitXor => mk().binary_expr(BinOpKind::BitXor, lhs, rhs),

            c_ast::BinOp::ShiftRight => mk().binary_expr(BinOpKind::Shr, lhs, rhs),
            c_ast::BinOp::ShiftLeft => mk().binary_expr(BinOpKind::Shl, lhs, rhs),

            c_ast::BinOp::EqualEqual => {
                // Using is_none method for null comparison means we don't have to
                // rely on the PartialEq trait as much and is also more idiomatic
                let expr = if let Some((lhs_expr_id, rhs_expr_id)) = lhs_rhs_ids {
                    let fn_eq_null = self.ast_context.is_function_pointer(lhs_type.ctype)
                        && self.ast_context.is_null_expr(rhs_expr_id);
                    let null_eq_fn = self.ast_context.is_function_pointer(rhs_type.ctype)
                        && self.ast_context.is_null_expr(lhs_expr_id);

                    if fn_eq_null {
                        mk().method_call_expr(lhs, "is_none", vec![] as Vec<P<Expr>>)
                    } else if null_eq_fn {
                        mk().method_call_expr(rhs, "is_none", vec![] as Vec<P<Expr>>)
                    } else {
                        mk().binary_expr(BinOpKind::Eq, lhs, rhs)
                    }
                } else {
                    mk().binary_expr(BinOpKind::Eq, lhs, rhs)
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
                        mk().method_call_expr(lhs, "is_some", vec![] as Vec<P<Expr>>)
                    } else if null_eq_fn {
                        mk().method_call_expr(rhs, "is_some", vec![] as Vec<P<Expr>>)
                    } else {
                        mk().binary_expr(BinOpKind::Ne, lhs, rhs)
                    }
                } else {
                    mk().binary_expr(BinOpKind::Ne, lhs, rhs)
                };

                bool_to_int(expr)
            }
            c_ast::BinOp::Less => bool_to_int(mk().binary_expr(BinOpKind::Lt, lhs, rhs)),
            c_ast::BinOp::Greater => bool_to_int(mk().binary_expr(BinOpKind::Gt, lhs, rhs)),
            c_ast::BinOp::GreaterEqual => bool_to_int(mk().binary_expr(BinOpKind::Ge, lhs, rhs)),
            c_ast::BinOp::LessEqual => bool_to_int(mk().binary_expr(BinOpKind::Le, lhs, rhs)),

            c_ast::BinOp::BitAnd => mk().binary_expr(BinOpKind::BitAnd, lhs, rhs),
            c_ast::BinOp::BitOr => mk().binary_expr(BinOpKind::BitOr, lhs, rhs),

            op => unimplemented!("Translation of binary operator {:?}", op),
        }
    }

    fn convert_addition(
        &self,
        lhs_type_id: CQualTypeId,
        rhs_type_id: CQualTypeId,
        lhs: P<Expr>,
        rhs: P<Expr>,
    ) -> P<Expr> {
        let lhs_type = &self.ast_context.resolve_type(lhs_type_id.ctype).kind;
        let rhs_type = &self.ast_context.resolve_type(rhs_type_id.ctype).kind;

        if let &CTypeKind::Pointer(pointee) = lhs_type {
            match self.compute_size_of_expr(pointee.ctype) {
                Some(sz) => {
                    let rhs = mk().binary_expr(
                        BinOpKind::Mul,
                        cast_int(rhs, "isize"),
                        cast_int(sz, "isize"),
                    );
                    pointer_offset_isize(lhs, rhs)
                }
                None => pointer_offset(lhs, rhs),
            }
        } else if let &CTypeKind::Pointer(pointee) = rhs_type {
            match self.compute_size_of_expr(pointee.ctype) {
                Some(sz) => {
                    let lhs = mk().binary_expr(
                        BinOpKind::Mul,
                        cast_int(lhs, "isize"),
                        cast_int(sz, "isize"),
                    );
                    pointer_offset_isize(rhs, lhs)
                }
                None => pointer_offset(rhs, lhs),
            }
        } else if lhs_type.is_unsigned_integral_type() {
            mk().method_call_expr(lhs, mk().path_segment("wrapping_add"), vec![rhs])
        } else {
            mk().binary_expr(BinOpKind::Add, lhs, rhs)
        }
    }

    fn convert_subtraction(
        &self,
        ty: P<Ty>,
        lhs_type_id: CQualTypeId,
        rhs_type_id: CQualTypeId,
        lhs: P<Expr>,
        rhs: P<Expr>,
    ) -> P<Expr> {
        let lhs_type = &self.ast_context.resolve_type(lhs_type_id.ctype).kind;
        let rhs_type = &self.ast_context.resolve_type(rhs_type_id.ctype).kind;

        if let &CTypeKind::Pointer(pointee) = rhs_type {
            // The wrapping_offset_from method is locked behind a feature gate
            // and replaces the now deprecated offset_to (opposite argument order)
            // wrapping_offset_from panics when the pointee is a ZST
            self.use_feature("ptr_wrapping_offset_from");

            let mut offset = mk().method_call_expr(lhs, "wrapping_offset_from", vec![rhs]);

            if let Some(sz) = self.compute_size_of_expr(pointee.ctype) {
                offset = mk().binary_expr(BinOpKind::Div, offset, cast_int(sz, "isize"))
            }

            mk().cast_expr(offset, ty)
        } else if let &CTypeKind::Pointer(pointee) = lhs_type {
            match self.compute_size_of_expr(pointee.ctype) {
                None => pointer_neg_offset(lhs, rhs),
                Some(sz) => pointer_neg_offset_isize(
                    lhs,
                    mk().binary_expr(
                        BinOpKind::Mul,
                        cast_int(rhs, "isize"),
                        cast_int(sz, "isize"),
                    ),
                ),
            }
        } else if lhs_type.is_unsigned_integral_type() {
            mk().method_call_expr(lhs, mk().path_segment("wrapping_sub"), vec![rhs])
        } else {
            mk().binary_expr(BinOpKind::Sub, lhs, rhs)
        }
    }

    fn convert_pre_increment(
        &self,
        ctx: ExprContext,
        ty: CQualTypeId,
        up: bool,
        arg: CExprId,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
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
                self.extern_crates.borrow_mut().insert("f128");

                let fn_path = mk().path_expr(vec!["f128", "f128", "new"]);
                let args = vec![mk().ident_expr("1.")];

                mk().call_expr(fn_path, args)
            }
            _ => mk().lit_expr(mk().int_lit(1, LitIntType::Unsuffixed)),
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
            WithStmts::new(one),
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
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
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

        let WithStmts {
            val: (write, read),
            stmts: mut lhs_stmts,
        } = self.name_reference_write_read(ctx, arg)?;

        let val_name = self.renamer.borrow_mut().fresh();
        let save_old_val = mk().local_stmt(P(mk().local(
            mk().ident_pat(&val_name),
            None as Option<P<Ty>>,
            Some(read.clone()),
        )));

        let mut one = match self.ast_context[ty.ctype].kind {
            // TODO: If rust gets f16 support:
            // CTypeKind::Half |
            CTypeKind::Float | CTypeKind::Double => mk().lit_expr(mk().float_unsuffixed_lit("1.")),
            CTypeKind::LongDouble => {
                self.extern_crates.borrow_mut().insert("f128");

                let fn_path = mk().path_expr(vec!["f128", "f128", "new"]);
                let args = vec![mk().ident_expr("1.")];

                mk().call_expr(fn_path, args)
            }
            _ => mk().lit_expr(mk().int_lit(1, LitIntType::Unsuffixed)),
        };

        // *p + 1
        let val =
            if let &CTypeKind::Pointer(pointee) = &self.ast_context.resolve_type(ty.ctype).kind {
                if let Some(n) = self.compute_size_of_expr(pointee.ctype) {
                    one = n
                }

                let n = if up {
                    one
                } else {
                    mk().unary_expr(ast::UnOp::Neg, one)
                };
                mk().method_call_expr(read.clone(), "offset", vec![n])
            } else {
                if self
                    .ast_context
                    .resolve_type(ty.ctype)
                    .kind
                    .is_unsigned_integral_type()
                {
                    let m = if up { "wrapping_add" } else { "wrapping_sub" };
                    mk().method_call_expr(read.clone(), m, vec![one])
                } else {
                    let k = if up { BinOpKind::Add } else { BinOpKind::Sub };
                    mk().binary_expr(k, read.clone(), one)
                }
            };

        // *p = *p + rhs
        let assign_stmt = if ty.qualifiers.is_volatile {
            self.volatile_write(&write, ty, val)?
        } else {
            mk().assign_expr(&write, val)
        };

        lhs_stmts.push(save_old_val);
        lhs_stmts.push(mk().expr_stmt(assign_stmt));

        Ok(WithStmts {
            stmts: lhs_stmts,
            val: mk().ident_expr(val_name),
        })
    }

    pub fn convert_unary_operator(
        &self,
        mut ctx: ExprContext,
        name: c_ast::UnOp,
        cqual_type: CQualTypeId,
        arg: CExprId,
        lrvalue: LRValue,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        let CQualTypeId { ctype, .. } = cqual_type;
        let ty = self.convert_type(ctype)?;
        let resolved_ctype = self.ast_context.resolve_type(ctype);

        match name {
            c_ast::UnOp::AddressOf => {
                let arg_kind = &self.ast_context[arg].kind;

                match arg_kind {
                    // C99 6.5.3.2 para 4
                    CExprKind::Unary(_, c_ast::UnOp::Deref, target, _) => {
                        return self.convert_expr(ctx, *target)
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

                let arg = self.convert_expr(ctx.used().set_needs_address(true), arg)?;

                if self.ast_context.is_function_pointer(ctype) {
                    Ok(arg.map(|x| mk().call_expr(mk().ident_expr("Some"), vec![x])))
                } else {
                    let pointee = match resolved_ctype.kind {
                        CTypeKind::Pointer(pointee) => pointee,
                        _ => {
                            return Err(TranslationError::generic(
                                "Address-of should return a pointer",
                            ))
                        }
                    };

                    let mutbl = if pointee.qualifiers.is_const {
                        Mutability::Immutable
                    } else {
                        Mutability::Mutable
                    };

                    arg.result_map(|a| {
                        let mut addr_of_arg: P<Expr>;

                        if ctx.is_static {
                            // static variable initializers aren't able to use &mut,
                            // so we work around that by using & and an extra cast
                            // through & to *const to *mut
                            addr_of_arg = mk().addr_of_expr(a);
                            if mutbl == Mutability::Mutable {
                                let mut qtype = pointee;
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
                        self.convert_expr(ctx.used(), arg_)
                    }
                    _ => {
                        self.convert_expr(ctx.used(), arg)?
                            .result_map(|val: P<Expr>| {
                                if let CTypeKind::Function(..) =
                                    self.ast_context.resolve_type(ctype).kind
                                {
                                    Ok(unwrap_function_pointer(val))
                                } else if let Some(_vla) = self.compute_size_of_expr(ctype) {
                                    Ok(val)
                                } else {
                                    let mut val = mk().unary_expr(ast::UnOp::Deref, val);

                                    // If the type on the other side of the pointer we are dereferencing is volatile and
                                    // this whole expression is not an LValue, we should make this a volatile read
                                    if lrvalue.is_rvalue() && cqual_type.qualifiers.is_volatile {
                                        val = self.volatile_read(&val, cqual_type)?
                                    }
                                    Ok(val)
                                }
                            })
                    }
                }
            }
            c_ast::UnOp::Plus => self.convert_expr(ctx.used(), arg), // promotion is explicit in the clang AST

            c_ast::UnOp::Negate => {
                let val = self.convert_expr(ctx.used(), arg)?;

                if resolved_ctype.kind.is_unsigned_integral_type() {
                    Ok(val.map(wrapping_neg_expr))
                } else {
                    Ok(val.map(neg_expr))
                }
            }
            c_ast::UnOp::Complement => Ok(self
                .convert_expr(ctx.used(), arg)?
                .map(|a| mk().unary_expr(ast::UnOp::Not, a))),

            c_ast::UnOp::Not => {
                let val = self.convert_condition(ctx, false, arg)?;
                Ok(val.map(|x| mk().cast_expr(x, mk().path_ty(vec!["libc", "c_int"]))))
            }
            c_ast::UnOp::Extension => {
                let arg = self.convert_expr(ctx, arg)?;
                Ok(arg)
            }
            c_ast::UnOp::Real | c_ast::UnOp::Imag | c_ast::UnOp::Coawait => {
                panic!("Unsupported extension operator")
            }
        }
    }
}
