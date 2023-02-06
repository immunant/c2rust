use crate::format_translation_err;

use super::*;
use std::sync::atomic::Ordering;

impl<'c> Translation<'c> {
    fn convert_constant_bool(&self, expr: CExprId) -> Option<bool> {
        let val = self.ast_context.resolve_expr(expr).1;
        match val {
            &CExprKind::Literal(_, CLiteral::Integer(i, _)) => Some(i != 0),
            _ => None,
        }
    }

    fn convert_memordering(&self, expr: CExprId) -> Option<Ordering> {
        let memorder = &self.ast_context[expr];
        let i = match memorder.kind {
            CExprKind::Literal(_, CLiteral::Integer(i, _)) => Some(i),
            CExprKind::DeclRef(_, decl_id, LRValue::RValue) => {
                let decl = self.ast_context.get_decl(&decl_id).unwrap();
                match decl.kind {
                    CDeclKind::EnumConstant { name: _, value: v } => match v {
                        ConstIntExpr::I(i) => {
                            assert!(0 <= i);
                            Some(i as u64)
                        }
                        ConstIntExpr::U(u) => Some(u),
                    },
                    _ => unimplemented!(),
                }
            }
            _ => None,
        }?;
        use Ordering::*;
        let ordering = match i {
            0 => Relaxed,
            1 => Acquire,
            2 => Acquire,
            3 => Release,
            4 => AcqRel,
            5 => SeqCst,
            _ => return None,
        };
        Some(ordering)
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
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let ptr = self.convert_expr(ctx.used(), ptr_id)?;
        let order = self.convert_memordering(order_id);
        let val1 = val1_id
            .map(|x| self.convert_expr(ctx.used(), x))
            .transpose()?;
        let order_fail = order_fail_id.and_then(|x| self.convert_memordering(x));
        let val2 = val2_id
            .map(|x| self.convert_expr(ctx.used(), x))
            .transpose()?;
        let weak = weak_id.and_then(|x| self.convert_constant_bool(x));

        fn static_order<T>(order: Option<T>) -> T {
            order.unwrap_or_else(|| {
                // We have to select which intrinsic to use at runtime
                unimplemented!("Dynamic memory consistency arguments are not yet supported");
            })
        }

        fn order_name(order: Ordering) -> &'static str {
            use Ordering::*;
            match order {
                SeqCst => "seqcst",
                AcqRel => "acqrel",
                Acquire => "acquire",
                Release => "release",
                Relaxed => "relaxed",
                _ => unreachable!(
                    "new variants added to `{}`",
                    std::any::type_name::<Ordering>()
                ),
            }
        }

        match name {
            "__atomic_load" | "__atomic_load_n" | "__c11_atomic_load" => ptr.and_then(|ptr| {
                let intrinsic_name = format!("atomic_load_{}", order_name(static_order(order)));

                self.use_feature("core_intrinsics");

                let atomic_load = mk().abs_path_expr(vec!["core", "intrinsics", &intrinsic_name]);
                let call = mk().call_expr(atomic_load, vec![ptr]);
                if name == "__atomic_load" {
                    let ret = val1.expect("__atomic_load should have a ret argument");
                    ret.and_then(|ret| {
                        let assignment = mk().assign_expr(
                            mk().unary_expr(UnOp::Deref(Default::default()), ret),
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
            }),

            "__atomic_store" | "__atomic_store_n" | "__c11_atomic_store" => {
                let val = val1.expect("__atomic_store must have a val argument");
                ptr.and_then(|ptr| {
                    val.and_then(|val| {
                        let intrinsic_name =
                            format!("atomic_store_{}", order_name(static_order(order)));

                        self.use_feature("core_intrinsics");

                        let atomic_store =
                            mk().abs_path_expr(vec!["core", "intrinsics", &intrinsic_name]);
                        let val = if name == "__atomic_store" {
                            mk().unary_expr(UnOp::Deref(Default::default()), val)
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

            // NOTE: there is no corresponding __atomic_init builtin in clang
            "__c11_atomic_init" => {
                let val = val1.expect(&format!("__atomic_init must have a val argument"));
                ptr.and_then(|ptr| {
                    val.and_then(|val| {
                        let assignment = mk().assign_expr(
                            mk().unary_expr(UnOp::Deref(Default::default()), ptr),
                            val,
                        );
                        self.convert_side_effects_expr(
                            ctx,
                            WithStmts::new_val(assignment),
                            "Builtin is not supposed to be used",
                        )
                    })
                })
            }

            "__atomic_exchange" | "__atomic_exchange_n" | "__c11_atomic_exchange" => {
                let val = val1.expect("__atomic_store must have a val argument");
                ptr.and_then(|ptr| {
                    val.and_then(|val| {
                        let intrinsic_name =
                            format!("atomic_xchg_{}", order_name(static_order(order)));

                        self.use_feature("core_intrinsics");

                        let fn_path =
                            mk().abs_path_expr(vec!["core", "intrinsics", &intrinsic_name]);
                        let val = if name == "__atomic_exchange" {
                            mk().unary_expr(UnOp::Deref(Default::default()), val)
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
                                        mk().unary_expr(UnOp::Deref(Default::default()), ret),
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

            "__atomic_compare_exchange"
            | "__atomic_compare_exchange_n"
            | "__c11_atomic_compare_exchange_strong" => {
                // TODO(perl): __c11_atomic_compare_exchange_strong does not
                // seem to produce correct code. It produces a deref operation
                // on the `src` argument to atomic_cxchg_seqcst_seqcst.
                let expected =
                    val1.expect("__atomic_compare_exchange must have a expected argument");
                let desired = val2.expect("__atomic_compare_exchange must have a desired argument");
                // Some C11 atomic operations encode the weak property in the name
                let weak = match (name, weak) {
                    ("__c11_atomic_compare_exchange_strong", None) => Some(false),
                    ("__c11_atomic_compare_exchange_weak", None) => Some(true),
                    _ => weak,
                };

                ptr.and_then(|ptr| {
                    expected.and_then(|expected| {
                        desired.and_then(|desired| {
                            let weak = static_order(weak);
                            let order = static_order(order);
                            let order_fail = static_order(order_fail);
                            use Ordering::*;
                            let intrinsic_name = match (order, order_fail) {
                                (_, Release | AcqRel) => None,
                                (SeqCst, SeqCst | Acquire | Relaxed)
                                | (AcqRel, Acquire | Relaxed)
                                | (Release, Relaxed)
                                | (Acquire | Relaxed, Acquire | Relaxed) => {
                                    Some((order, order_fail))
                                }
                                (SeqCst | AcqRel | Release | Acquire | Relaxed, _) => None,

                                (_, _) => unreachable!("Did we not handle a case above??"),
                            }
                            .map(|(order, order_fail)| {
                                let weak = if weak { "weak" } else { "" };
                                let order = order_name(order);
                                let order_fail = order_name(order_fail);
                                format!("atomic_cxchg{weak}_{order}_{order_fail}")
                            })
                            .ok_or_else(|| {
                                format_translation_err!(
                                    self.ast_context
                                        .display_loc(&self.ast_context[order_fail_id.unwrap()].loc),
                                    "Invalid failure memory ordering",
                                )
                            })?;

                            self.use_feature("core_intrinsics");
                            let expected =
                                mk().unary_expr(UnOp::Deref(Default::default()), expected);
                            let desired = if name == "__atomic_compare_exchange_n" {
                                desired
                            } else {
                                mk().unary_expr(UnOp::Deref(Default::default()), desired)
                            };

                            let atomic_cxchg =
                                mk().abs_path_expr(vec!["core", "intrinsics", &intrinsic_name]);
                            let call =
                                mk().call_expr(atomic_cxchg, vec![ptr, expected.clone(), desired]);
                            let res_name = self.renamer.borrow_mut().fresh();
                            let res_let = mk().local_stmt(Box::new(mk().local(
                                mk().ident_pat(&res_name),
                                None,
                                Some(call),
                            )));
                            let assignment = mk().semi_stmt(mk().assign_expr(
                                expected,
                                mk().anon_field_expr(mk().ident_expr(&res_name), 0),
                            ));
                            let return_value = mk().anon_field_expr(mk().ident_expr(&res_name), 1);
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
            | "__atomic_fetch_nand"
            | "__c11_atomic_fetch_add"
            | "__c11_atomic_fetch_sub"
            | "__c11_atomic_fetch_and"
            | "__c11_atomic_fetch_xor"
            | "__c11_atomic_fetch_or"
            | "__c11_atomic_fetch_nand" => {
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

                let intrinsic_suffix = order_name(static_order(order));
                let intrinsic_name = format!("{intrinsic_name}_{intrinsic_suffix}");

                let fetch_first = name.starts_with("__atomic_fetch") || name.starts_with("__c11_atomic_fetch");
                let val = val1.expect("__atomic arithmetic operations must have a val argument");
                ptr.and_then(|ptr| {
                    val.and_then(|val| {
                        self.convert_atomic_op(ctx, &intrinsic_name, ptr, val, fetch_first)
                    })
                })
            }

            _ => unimplemented!("atomic not implemented: {}", name),
        }
    }

    pub(crate) fn convert_atomic_cxchg(
        &self,
        ctx: ExprContext,
        intrinsic_name: &str,
        dst: Box<Expr>,
        old_val: Box<Expr>,
        src_val: Box<Expr>,
        returns_val: bool,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        self.use_feature("core_intrinsics");

        // Emit `atomic_cxchg(a0, a1, a2).idx`
        let atomic_cxchg = mk().abs_path_expr(vec!["core", "intrinsics", intrinsic_name]);
        let call = mk().call_expr(atomic_cxchg, vec![dst, old_val, src_val]);
        let field_idx = if returns_val { 0 } else { 1 };
        let call_expr = mk().anon_field_expr(call, field_idx);
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
        dst: Box<Expr>,
        src: Box<Expr>,
        fetch_first: bool,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        self.use_feature("core_intrinsics");

        // Emit `atomic_func(a0, a1) (op a1)?`
        let atomic_func = mk().abs_path_expr(vec!["core", "intrinsics", func_name]);

        if fetch_first {
            let call_expr = mk().call_expr(atomic_func, vec![dst, src]);
            self.convert_side_effects_expr(
                ctx,
                WithStmts::new_val(call_expr),
                "Builtin is not supposed to be used",
            )
        } else {
            let (binary_op, is_nand) = if func_name.starts_with("atomic_xadd") {
                (BinOp::Add(Default::default()), false)
            } else if func_name.starts_with("atomic_xsub") {
                (BinOp::Sub(Default::default()), false)
            } else if func_name.starts_with("atomic_or") {
                (BinOp::BitOr(Default::default()), false)
            } else if func_name.starts_with("atomic_xor") {
                (BinOp::BitXor(Default::default()), false)
            } else if func_name.starts_with("atomic_nand") {
                (BinOp::BitAnd(Default::default()), true)
            } else if func_name.starts_with("atomic_and") {
                (BinOp::BitAnd(Default::default()), false)
            } else {
                panic!("Unexpected atomic intrinsic name: {}", func_name)
            };

            // Since the value of `arg1` is used twice, we need to copy
            // it into a local temporary so we don't duplicate any side-effects
            // To preserve ordering of side-effects, we also do this for arg0
            let arg0_name = self.renamer.borrow_mut().fresh();
            let arg0_let = mk().local_stmt(Box::new(mk().local(
                mk().ident_pat(&arg0_name),
                None,
                Some(dst),
            )));

            let arg1_name = self.renamer.borrow_mut().fresh();
            let arg1_let = mk().local_stmt(Box::new(mk().local(
                mk().ident_pat(&arg1_name),
                None,
                Some(src),
            )));

            let call = mk().call_expr(
                atomic_func,
                vec![mk().ident_expr(&arg0_name), mk().ident_expr(&arg1_name)],
            );
            let val = mk().binary_expr(binary_op, call, mk().ident_expr(arg1_name));
            let val = if is_nand {
                // For nand, return `!(atomic_nand(arg0, arg1) & arg1)`
                mk().unary_expr(UnOp::Not(Default::default()), val)
            } else {
                val
            };
            self.convert_side_effects_expr(
                ctx,
                WithStmts::new(vec![arg0_let, arg1_let], val),
                "Builtin is not supposed to be used",
            )
        }
    }
}
