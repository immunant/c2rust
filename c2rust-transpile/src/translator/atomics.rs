use std::sync::atomic::Ordering;
use super::*;

impl<'c> Translation<'c> {
    fn convert_constant_bool(&self, expr: CExprId) -> Option<bool> {
        let val = self.ast_context.resolve_expr_value(expr);
        match val {
            CExprKind::Literal(_, CLiteral::Integer(0, _)) => Some(false),
            CExprKind::Literal(_, CLiteral::Integer(_, _)) => Some(true),
            _ => None,
        }
    }

    fn convert_memordering(&self, expr: CExprId) -> Option<Ordering> {
        let memorder = &self.ast_context[expr];
        match memorder.kind {
            CExprKind::Literal(_, CLiteral::Integer(i, _)) => {
                match i {
                    0 => Some(Ordering::Relaxed),
                    1 => Some(Ordering::Acquire),
                    2 => Some(Ordering::Acquire),
                    3 => Some(Ordering::Release),
                    4 => Some(Ordering::AcqRel),
                    5 => Some(Ordering::SeqCst),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub fn convert_atomic(
        &self,
        ctx: ExprContext,
        name: &str,
        ptr_id: CExprId,
        order_id: CExprId,
        val1_id: Option<CExprId>,
        order_fail_id: Option<CExprId>,
        val2_id: Option<CExprId>,
        weak_id: Option<CExprId>,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        let std_or_core = if self.tcfg.emit_no_std { "core" } else { "std" };
        let ptr = self.convert_expr(ctx.used(), ptr_id)?;
        let order = self.convert_memordering(order_id);
        let val1 = val1_id.map(|x| self.convert_expr(ctx.used(), x)).transpose()?;
        let order_fail = order_fail_id.and_then(|x| self.convert_memordering(x));
        let val2 = val2_id.map(|x| self.convert_expr(ctx.used(), x)).transpose()?;
        let weak = weak_id.and_then(|x| self.convert_constant_bool(x));

        match name {
            "__atomic_load" | "__atomic_load_n" => {
                ptr.and_then(|ptr| {
                    let intrinsic_name = match order {
                        None => unimplemented!("Dynamic memory consistency arguments are not yet supported"),
                        Some(Ordering::SeqCst) => Some("atomic_load"),
                        Some(Ordering::AcqRel) => None,
                        Some(Ordering::Acquire) => Some("atomic_load_acq"),
                        Some(Ordering::Release) => None,
                        Some(Ordering::Relaxed) => Some("atomic_load_relaxed"),
                        Some(_) => unreachable!("Did we not handle a case above??"),
                    }.ok_or_else(|| format_translation_err!(
                        self.ast_context.display_loc(&self.ast_context[order_id].loc),
                        "Invalid memory ordering for __atomic_load",
                    ))?;

                    self.use_feature("core_intrinsics");

                    let atomic_load =
                        mk().path_expr(vec!["", std_or_core, "intrinsics", intrinsic_name]);
                    let call = mk().call_expr(atomic_load, vec![ptr]);
                    if name == "__atomic_load" {
                        let ret = val1.expect("__atomic_load should have a ret argument");
                        ret.and_then(|ret| {
                            let assignment = mk().assign_expr(
                                mk().unary_expr(ast::UnOp::Deref, ret),
                                call,
                            );
                            self.convert_side_effects_expr(
                                ctx,
                                WithStmts::new_val(assignment),
                                "Builtin is not supposed to be used",
                            )
                        })
                    } else {
                        self.convert_side_effects_expr(
                            ctx,
                            WithStmts::new_val(call),
                            "Builtin is not supposed to be used",
                        )
                    }
                })
            }

            "__atomic_store" | "__atomic_store_n" => {
                let val = val1.expect("__atomic_store must have a val argument");
                ptr.and_then(|ptr| {
                    val.and_then(|val| {
                        let intrinsic_name = match order {
                            None => unimplemented!("Dynamic memory consistency arguments are not yet supported"),
                            Some(Ordering::SeqCst) => Some("atomic_store"),
                            Some(Ordering::AcqRel) => None,
                            Some(Ordering::Acquire) => None,
                            Some(Ordering::Release) => Some("atomic_store_rel"),
                            Some(Ordering::Relaxed) => Some("atomic_store_relaxed"),
                            Some(_) => unreachable!("Did we not handle a case above??"),
                        }.ok_or_else(|| format_translation_err!(
                            self.ast_context.display_loc(&self.ast_context[order_id].loc),
                            "Invalid memory ordering for __atomic_store",
                        ))?;

                        self.use_feature("core_intrinsics");

                        let atomic_store =
                            mk().path_expr(vec!["", std_or_core, "intrinsics", intrinsic_name]);
                        let val = if name == "__atomic_store" {
                            mk().unary_expr(ast::UnOp::Deref, val)
                        } else {
                            val
                        };
                        let call = mk().call_expr(atomic_store, vec![ptr, val]);
                        self.convert_side_effects_expr(
                            ctx,
                            WithStmts::new_val(call),
                            "Builtin is not supposed to be used",
                        )
                    })
                })
            }

            "__atomic_exchange" | "__atomic_exchange_n" => {
                let val = val1.expect("__atomic_store must have a val argument");
                ptr.and_then(|ptr| {
                    val.and_then(|val| {
                        let intrinsic_name = match order {
                            None => unimplemented!("Dynamic memory consistency arguments are not yet supported"),
                            Some(Ordering::SeqCst) => Some("atomic_xchg"),
                            Some(Ordering::AcqRel) => Some("atomic_xchg_acqrel"),
                            Some(Ordering::Acquire) => Some("atomic_xchg_acq"),
                            Some(Ordering::Release) => Some("atomic_xchg_rel"),
                            Some(Ordering::Relaxed) => Some("atomic_xchg_relaxed"),
                            Some(_) => unreachable!("Did we not handle a case above??"),
                        }.ok_or_else(|| format_translation_err!(
                            self.ast_context.display_loc(&self.ast_context[order_id].loc),
                            "Invalid memory ordering for __atomic_exchange",
                        ))?;

                        self.use_feature("core_intrinsics");

                        let fn_path =
                            mk().path_expr(vec!["", std_or_core, "intrinsics", intrinsic_name]);
                        let val = if name == "__atomic_exchange" {
                            mk().unary_expr(ast::UnOp::Deref, val)
                        } else {
                            val
                        };
                        let call = mk().call_expr(fn_path, vec![ptr, val]);
                        if name == "__atomic_exchange" {
                            // LLVM stores the ret pointer in the order_fail slot
                            order_fail_id
                                .map(|x| self.convert_expr(ctx.used(), x))
                                .transpose()?
                                .expect("__atomic_exchange must have a ret pointer argument")
                                .and_then(|ret| {
                                    let assignment = mk().assign_expr(
                                        mk().unary_expr(ast::UnOp::Deref, ret),
                                        call,
                                    );
                                    self.convert_side_effects_expr(
                                        ctx,
                                        WithStmts::new_val(assignment),
                                        "Builtin is not supposed to be used",
                                    )
                                })
                        } else {
                            self.convert_side_effects_expr(
                                ctx,
                                WithStmts::new_val(call),
                                "Builtin is not supposed to be used",
                            )
                        }
                    })
                })
            }

            "__atomic_compare_exchange" | "__atomic_compare_exchange_n" => {
                let expected = val1.expect("__atomic_compare_exchange must have a expected argument");
                let desired = val2.expect("__atomic_compare_exchange must have a desired argument");
                ptr.and_then(|ptr| {
                    expected.and_then(|expected| {
                        desired.and_then(|desired| {
                            let intrinsic_name = match (weak, order, order_fail) {
                                (None, _, _) | (_, None, _) | (_, _, None) => {
                                    // We have to select which intrinsic to use at runtime
                                    unimplemented!("Dynamic memory consistency arguments are not yet supported");
                                }
                                (_, _, Some(Ordering::Release)) | (_, _, Some(Ordering::AcqRel)) =>
                                    None,

                                (Some(false), Some(Ordering::SeqCst), Some(Ordering::SeqCst)) =>
                                    Some("atomic_cxchg"),
                                (Some(false), Some(Ordering::SeqCst), Some(Ordering::Acquire)) =>
                                    Some("atomic_cxchg_failacq"),
                                (Some(false), Some(Ordering::SeqCst), Some(Ordering::Relaxed)) =>
                                    Some("atomic_cxchg_failrelaxed"),
                                (Some(false), Some(Ordering::SeqCst), Some(_)) =>
                                    None,
                                (Some(false), Some(Ordering::AcqRel), Some(Ordering::Acquire)) =>
                                    Some("atomic_cxchg_acqrel"),
                                (Some(false), Some(Ordering::AcqRel), Some(Ordering::Relaxed)) =>
                                    Some("atomic_cxchg_acqrel_failrelaxed"),
                                (Some(false), Some(Ordering::AcqRel), Some(_)) =>
                                    None,
                                (Some(false), Some(Ordering::Release), Some(Ordering::Relaxed)) =>
                                    Some("atomic_cxchg_rel"),
                                (Some(false), Some(Ordering::Release), Some(_)) =>
                                    None,
                                (Some(false), Some(Ordering::Acquire), Some(Ordering::Acquire)) =>
                                    Some("atomic_cxchg_acq"),
                                (Some(false), Some(Ordering::Acquire), Some(Ordering::Relaxed)) =>
                                    Some("atomic_cxchg_acq_failrelaxed"),
                                (Some(false), Some(Ordering::Acquire), Some(_)) =>
                                    None,
                                (Some(false), Some(Ordering::Relaxed), Some(Ordering::Relaxed)) =>
                                    Some("atomic_cxchg_relaxed"),
                                (Some(false), Some(Ordering::Relaxed), Some(_)) =>
                                    None,

                                (Some(true), Some(Ordering::SeqCst), Some(Ordering::SeqCst)) =>
                                    Some("atomic_cxchgweak"),
                                (Some(true), Some(Ordering::SeqCst), Some(Ordering::Acquire)) =>
                                    Some("atomic_cxchgweak_failacq"),
                                (Some(true), Some(Ordering::SeqCst), Some(Ordering::Relaxed)) =>
                                    Some("atomic_cxchgweak_failrelaxed"),
                                (Some(true), Some(Ordering::SeqCst), Some(_)) =>
                                    None,
                                (Some(true), Some(Ordering::AcqRel), Some(Ordering::Acquire)) =>
                                    Some("atomic_cxchgweak_acqrel"),
                                (Some(true), Some(Ordering::AcqRel), Some(Ordering::Relaxed)) =>
                                    Some("atomic_cxchgweak_acqrel_failrelaxed"),
                                (Some(true), Some(Ordering::AcqRel), Some(_)) =>
                                    None,
                                (Some(true), Some(Ordering::Release), Some(Ordering::Relaxed)) =>
                                    Some("atomic_cxchgweak_rel"),
                                (Some(true), Some(Ordering::Release), Some(_)) =>
                                    None,
                                (Some(true), Some(Ordering::Acquire), Some(Ordering::Acquire)) =>
                                    Some("atomic_cxchgweak_acq"),
                                (Some(true), Some(Ordering::Acquire), Some(Ordering::Relaxed)) =>
                                    Some("atomic_cxchgweak_acq_failrelaxed"),
                                (Some(true), Some(Ordering::Acquire), Some(_)) =>
                                    None,
                                (Some(true), Some(Ordering::Relaxed), Some(Ordering::Relaxed)) =>
                                    Some("atomic_cxchgweak_relaxed"),
                                (Some(true), Some(Ordering::Relaxed), Some(_)) =>
                                    None,

                                (Some(_), Some(_), Some(_)) => unreachable!("Did we not handle a case above??"),
                            }.ok_or_else(|| format_translation_err!(
                                self.ast_context.display_loc(&self.ast_context[order_fail_id.unwrap()].loc),
                                "Invalid failure memory ordering",
                            ))?;

                            self.use_feature("core_intrinsics");
                            let expected = mk().unary_expr(ast::UnOp::Deref, expected);
                            let desired = if name == "__atomic_compare_exchange_n" {
                                desired
                            } else {
                                mk().unary_expr(ast::UnOp::Deref, desired)
                            };

                            let atomic_cxchg =
                                mk().path_expr(vec!["", std_or_core, "intrinsics", intrinsic_name]);
                            let call = mk().call_expr(atomic_cxchg, vec![ptr, expected.clone(), desired]);
                            let res_name = self.renamer.borrow_mut().fresh();
                            let res_let = mk().local_stmt(P(mk().local(
                                mk().ident_pat(&res_name),
                                None as Option<P<Ty>>,
                                Some(call),
                            )));
                            let assignment = mk().semi_stmt(mk().assign_expr(
                                expected,
                                mk().field_expr(mk().ident_expr(&res_name), "0"),
                            ));
                            let return_value = mk().field_expr(mk().ident_expr(&res_name), "1");
                            self.convert_side_effects_expr(
                                ctx,
                                WithStmts::new(vec![res_let, assignment], return_value),
                                "Builtin is not supposed to be used",
                            )
                        })
                    })
                })
            }

            "__atomic_add_fetch"
            | "__atomic_sub_fetch"
            | "__atomic_and_fetch"
            | "__atomic_xor_fetch"
            | "__atomic_or_fetch"
            | "__atomic_nand_fetch"
            | "__atomic_fetch_add"
            | "__atomic_fetch_sub"
            | "__atomic_fetch_and"
            | "__atomic_fetch_xor"
            | "__atomic_fetch_or"
            | "__atomic_fetch_nand" => {
                let intrinsic_name = if name.contains("_add") {
                    "atomic_xadd"
                } else if name.contains("_sub") {
                    "atomic_xsub"
                } else if name.contains("_or") {
                    "atomic_or"
                } else if name.contains("_xor") {
                    "atomic_xor"
                } else if name.contains("_nand") {
                    "atomic_nand"
                } else {
                    "atomic_and"
                };

                let intrinsic_name = match order {
                    None => {
                        unimplemented!("Dynamic memory consistency arguments are not yet supported");
                    }

                    Some(Ordering::SeqCst) => format!("{}", intrinsic_name),
                    Some(Ordering::AcqRel) => format!("{}_acqrel", intrinsic_name),
                    Some(Ordering::Acquire) => format!("{}_acq", intrinsic_name),
                    Some(Ordering::Release) => format!("{}_rel", intrinsic_name),
                    Some(Ordering::Relaxed) => format!("{}_relaxed", intrinsic_name),

                    _ => unreachable!("Unknown memory ordering"),
                };

                let fetch_first = name.starts_with("__atomic_fetch");
                let val = val1.expect("__atomic arithmetic operations must have a val argument");
                ptr.and_then(|ptr| {
                    val.and_then(|val| {
                        self.convert_atomic_op(
                            ctx,
                            &intrinsic_name,
                            ptr,
                            val,
                            fetch_first,
                        )
                    })
                })
            }

            _ => unimplemented!("atomic not implemented"),
        }
    }

    pub(crate) fn convert_atomic_cxchg(
        &self,
        ctx: ExprContext,
        intrinsic_name: &str,
        dst: P<Expr>,
        old_val: P<Expr>,
        src_val: P<Expr>,
        returns_val: bool,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        self.use_feature("core_intrinsics");
        let std_or_core = if self.tcfg.emit_no_std { "core" } else { "std" };

        // Emit `atomic_cxchg(a0, a1, a2).idx`
        let atomic_cxchg =
            mk().path_expr(vec!["", std_or_core, "intrinsics", intrinsic_name]);
        let call = mk().call_expr(atomic_cxchg, vec![dst, old_val, src_val]);
        let field_idx = if returns_val { "0" } else { "1" };
        let call_expr = mk().field_expr(call, field_idx);
        self.convert_side_effects_expr(
            ctx,
            WithStmts::new_val(call_expr),
            "Builtin is not supposed to be used",
        )
    }

    pub(crate) fn convert_atomic_op(
        &self,
        ctx: ExprContext,
        func_name: &str,
        dst: P<Expr>,
        src: P<Expr>,
        fetch_first: bool,
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        self.use_feature("core_intrinsics");
        let std_or_core = if self.tcfg.emit_no_std { "core" } else { "std" };

        // Emit `atomic_func(a0, a1) (op a1)?`
        let atomic_func = mk().path_expr(vec!["", std_or_core, "intrinsics", func_name]);

        if fetch_first {
            let call_expr = mk().call_expr(atomic_func, vec![dst, src]);
            self.convert_side_effects_expr(
                ctx,
                WithStmts::new_val(call_expr),
                "Builtin is not supposed to be used",
            )
        } else {
            let (binary_op, is_nand) = if func_name.starts_with("atomic_xadd") {
                (BinOpKind::Add, false)
            } else if func_name.starts_with("atomic_xsub") {
                (BinOpKind::Sub, false)
            } else if func_name.starts_with("atomic_or") {
                (BinOpKind::BitOr, false)
            } else if func_name.starts_with("atomic_xor") {
                (BinOpKind::BitXor, false)
            } else if func_name.starts_with("atomic_nand") {
                (BinOpKind::BitAnd, true)
            } else if func_name.starts_with("atomic_and") {
                (BinOpKind::BitAnd, false)
            } else {
                panic!("Unexpected atomic intrinsic name: {}", func_name)
            };

            // Since the value of `arg1` is used twice, we need to copy
            // it into a local temporary so we don't duplicate any side-effects
            // To preserve ordering of side-effects, we also do this for arg0
            let arg0_name = self.renamer.borrow_mut().fresh();
            let arg0_let = mk().local_stmt(P(mk().local(
                mk().ident_pat(&arg0_name),
                None as Option<P<Ty>>,
                Some(dst),
            )));

            let arg1_name = self.renamer.borrow_mut().fresh();
            let arg1_let = mk().local_stmt(P(mk().local(
                mk().ident_pat(&arg1_name),
                None as Option<P<Ty>>,
                Some(src),
            )));

            let call = mk().call_expr(
                atomic_func,
                vec![mk().ident_expr(&arg0_name), mk().ident_expr(&arg1_name)],
            );
            let val = mk().binary_expr(binary_op, call, mk().ident_expr(arg1_name));
            let val = if is_nand {
                // For nand, return `!(atomic_nand(arg0, arg1) & arg1)`
                mk().unary_expr(UnOp::Not, val)
            } else {
                val
            };
            self.convert_side_effects_expr(
                ctx,
                WithStmts::new(
                    vec![arg0_let, arg1_let],
                    val,
                ),
                "Builtin is not supposed to be used",
            )
        }
    }

}
