use crate::format_translation_err;
use itertools::repeat_n;
use std::sync::atomic::Ordering;

use super::*;

fn order_suffix(order: Ordering) -> &'static str {
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

fn order_ty_name(order: Ordering) -> &'static str {
    use Ordering::*;
    match order {
        SeqCst => "SeqCst",
        AcqRel => "AcqRel",
        Acquire => "Acquire",
        Release => "Release",
        Relaxed => "Relaxed",
        _ => unreachable!(
            "new variants added to `{}`",
            std::any::type_name::<Ordering>()
        ),
    }
}

impl<'c> Translation<'c> {
    fn atomic_intrinsic_expr_edition_2021(
        &self,
        base_name: &str,
        orders: &[Ordering],
    ) -> Box<Expr> {
        let prefix = ["atomic", base_name];
        let suffix = orders.iter().map(|&order| order_suffix(order));
        let intrinsic_name = prefix.iter().copied().chain(suffix).join("_");
        mk().abs_path_expr(vec!["core", "intrinsics", &intrinsic_name])
    }

    fn atomic_intrinsic_expr_edition_2024(
        &self,
        base_name: &str,
        orders: &[Ordering],
    ) -> Box<Expr> {
        let num_ty_params = match base_name {
            "fence" => 0,
            "load" | "store" | "xchg" | "cxchg" | "cxchgweak" => 1,
            "xadd" | "xsub" | "or" | "xor" | "nand" | "and" => 2,
            _ => unimplemented!("unknown atomic intrinsic: {base_name}"),
        };

        let args = repeat_n("_".to_owned(), num_ty_params)
            .chain(orders.iter().map(|&order| {
                let order = order_ty_name(order);
                format!("{{ ::core::intrinsics::AtomicOrdering::{order} }}")
            }))
            .join(", ");
        let path = format!("::core::intrinsics::atomic_{base_name}::<{args}>");
        // `mk()`/`Builder` doesn't seem to support const generic expressions,
        // so re-parsing with `syn` is simpler.
        let expr = syn::parse_str::<Expr>(&path)
            .unwrap_or_else(|_| panic!("failed to parse intrinsic path: {path}"));
        Box::new(expr)
    }

    pub fn atomic_intrinsic_expr(&self, base_name: &str, orders: &[Ordering]) -> Box<Expr> {
        assert!(matches!(
            orders,
            &[_ /* order */] | &[_ /* order_succ */, _ /* order_fail */]
        ));

        self.use_feature("core_intrinsics");
        if self.tcfg.edition < Edition2024 {
            self.atomic_intrinsic_expr_edition_2021(base_name, orders)
        } else {
            self.atomic_intrinsic_expr_edition_2024(base_name, orders)
        }
    }

    fn atomic_intrinsic_cxchg_expr(
        &self,
        weak: bool,
        order_succ: Ordering,
        order_fail: Ordering,
    ) -> Box<Expr> {
        let base = if weak { "cxchgweak" } else { "cxchg" };
        self.atomic_intrinsic_expr(base, &[order_succ, order_fail])
    }

    fn convert_constant_bool(&self, expr: CExprId) -> Option<bool> {
        let val = self.ast_context.unwrap_cast_expr(expr);
        match self.ast_context.index(val).kind {
            CExprKind::Literal(_, CLiteral::Integer(i, _)) => Some(i != 0),
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
                        ConstIntExpr::I(i) => Some(i.try_into().unwrap()),
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
        let ptr = self.convert_expr(ctx.used(), ptr_id, None)?;
        let order = self.convert_memordering(order_id);
        let val1 = val1_id
            .map(|x| self.convert_expr(ctx.used(), x, None))
            .transpose()?;
        let order_fail = order_fail_id.and_then(|x| self.convert_memordering(x));
        let val2 = val2_id
            .map(|x| self.convert_expr(ctx.used(), x, None))
            .transpose()?;
        let weak = weak_id.and_then(|x| self.convert_constant_bool(x));

        fn static_order<T>(order: Option<T>) -> T {
            order.unwrap_or_else(|| {
                // We have to select which intrinsic to use at runtime
                unimplemented!("Dynamic memory consistency arguments are not yet supported");
            })
        }

        match name {
            "__atomic_load" | "__atomic_load_n" | "__c11_atomic_load" => ptr.and_then(|ptr| {
                let order = static_order(order);

                let atomic_load = self.atomic_intrinsic_expr("load", &[order]);
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
                let order = static_order(order);
                let val = val1.expect("__atomic_store must have a val argument");
                ptr.and_then(|ptr| {
                    val.and_then(|val| {
                        let atomic_store = self.atomic_intrinsic_expr("store", &[order]);
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
                let val = val1.expect("__atomic_init must have a val argument");
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
                let order = static_order(order);
                let val = val1.expect("__atomic_store must have a val argument");
                ptr.and_then(|ptr| {
                    val.and_then(|val| {
                        let fn_path = self.atomic_intrinsic_expr("xchg", &[order]);
                        let val = if name == "__atomic_exchange" {
                            mk().unary_expr(UnOp::Deref(Default::default()), val)
                        } else {
                            val
                        };
                        let call = mk().call_expr(fn_path, vec![ptr, val]);
                        if name == "__atomic_exchange" {
                            // LLVM stores the ret pointer in the order_fail slot
                            order_fail_id
                                .map(|x| self.convert_expr(ctx.used(), x, None))
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
            | "__c11_atomic_compare_exchange_strong"
            | "__c11_atomic_compare_exchange_weak" => {
                let expected =
                    val1.expect("__atomic_compare_exchange must have a expected argument");
                let desired = val2.expect("__atomic_compare_exchange must have a desired argument");
                // Some C11 atomic operations encode the weak property in the name
                let weak = match (name, weak) {
                    ("__c11_atomic_compare_exchange_strong", None) => Some(false),
                    ("__c11_atomic_compare_exchange_weak", None) => Some(true),
                    _ => weak,
                };

                let order = static_order(order);
                let order_fail = static_order(order_fail);
                let weak = static_order(weak);

                ptr.and_then(|ptr| {
                    expected.and_then(|expected| {
                        desired.and_then(|desired| {
                            use Ordering::*;
                            let (order, order_fail) = match (order, order_fail) {
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
                            .ok_or_else(|| {
                                format_translation_err!(
                                    self.ast_context
                                        .display_loc(&self.ast_context[order_fail_id.unwrap()].loc),
                                    "Invalid failure memory ordering",
                                )
                            })?;

                            let expected =
                                mk().unary_expr(UnOp::Deref(Default::default()), expected);
                            let desired = match name {
                                "__atomic_compare_exchange_n"
                                | "__c11_atomic_compare_exchange_strong"
                                | "__c11_atomic_compare_exchange_weak" => desired,
                                _ => mk().unary_expr(UnOp::Deref(Default::default()), desired),
                            };

                            let atomic_cxchg =
                                self.atomic_intrinsic_cxchg_expr(weak, order, order_fail);
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

            _ => {
                if let Some(atomic_op) = CAtomicBinOp::from_atomic_fn(name) {
                    let order = static_order(order);
                    let val =
                        val1.expect("__atomic arithmetic operations must have a val argument");
                    ptr.and_then(|ptr| {
                        val.and_then(|val| self.convert_atomic_op(ctx, atomic_op, order, ptr, val))
                    })
                } else {
                    unimplemented!("atomic not implemented: {}", name)
                }
            }
        }
    }

    pub(crate) fn convert_atomic_cxchg(
        &self,
        ctx: ExprContext,
        weak: bool,
        order_succ: Ordering,
        order_fail: Ordering,
        dst: Box<Expr>,
        old_val: Box<Expr>,
        src_val: Box<Expr>,
        returns_val: bool,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        // Emit `atomic_cxchg(a0, a1, a2).idx`
        let atomic_cxchg = self.atomic_intrinsic_cxchg_expr(weak, order_succ, order_fail);
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
        atomic_op: CAtomicBinOp,
        order: Ordering,
        dst: Box<Expr>,
        src: Box<Expr>,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        // Emit `atomic_func(a0, a1) (op a1)?`
        let atomic_func =
            self.atomic_intrinsic_expr(atomic_op.rust_intrinsic_base_name(), &[order]);

        if atomic_op.fetches_first() {
            let call_expr = mk().call_expr(atomic_func, vec![dst, src]);
            self.convert_side_effects_expr(
                ctx,
                WithStmts::new_val(call_expr),
                "Builtin is not supposed to be used",
            )
        } else {
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
            let val = mk().binary_expr(atomic_op.rust_bin_op(), call, mk().ident_expr(arg1_name));
            let val = if atomic_op.is_nand() {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CAtomicBinOp {
    FetchAdd,
    FetchSub,
    FetchOr,
    FetchAnd,
    FetchXor,
    FetchNand,

    AddFetch,
    SubFetch,
    OrFetch,
    AndFetch,
    XorFetch,
    NandFetch,
}

impl CAtomicBinOp {
    pub fn from_atomic_fn(name: &str) -> Option<Self> {
        Some(match name {
            "__atomic_fetch_add" | "__c11_atomic_fetch_add" => Self::FetchAdd,
            "__atomic_fetch_sub" | "__c11_atomic_fetch_sub" => Self::FetchSub,
            "__atomic_fetch_or" | "__c11_atomic_fetch_or" => Self::FetchOr,
            "__atomic_fetch_and" | "__c11_atomic_fetch_and" => Self::FetchAnd,
            "__atomic_fetch_xor" | "__c11_atomic_fetch_xor" => Self::FetchXor,
            "__atomic_fetch_nand" | "__c11_atomic_fetch_nand" => Self::FetchNand,

            "__atomic_add_fetch" => Self::AddFetch,
            "__atomic_sub_fetch" => Self::SubFetch,
            "__atomic_or_fetch" => Self::OrFetch,
            "__atomic_and_fetch" => Self::AndFetch,
            "__atomic_xor_fetch" => Self::XorFetch,
            "__atomic_nand_fetch" => Self::NandFetch,

            _ => return None,
        })
    }

    pub fn from_sync_builtin_fn(name: &str) -> Option<Self> {
        Some(match name {
            "__sync_add_and_fetch_1"
            | "__sync_add_and_fetch_2"
            | "__sync_add_and_fetch_4"
            | "__sync_add_and_fetch_8"
            | "__sync_add_and_fetch_16" => Self::AddFetch,

            "__sync_sub_and_fetch_1"
            | "__sync_sub_and_fetch_2"
            | "__sync_sub_and_fetch_4"
            | "__sync_sub_and_fetch_8"
            | "__sync_sub_and_fetch_16" => Self::SubFetch,

            "__sync_or_and_fetch_1"
            | "__sync_or_and_fetch_2"
            | "__sync_or_and_fetch_4"
            | "__sync_or_and_fetch_8"
            | "__sync_or_and_fetch_16" => Self::OrFetch,

            "__sync_and_and_fetch_1"
            | "__sync_and_and_fetch_2"
            | "__sync_and_and_fetch_4"
            | "__sync_and_and_fetch_8"
            | "__sync_and_and_fetch_16" => Self::AndFetch,

            "__sync_xor_and_fetch_1"
            | "__sync_xor_and_fetch_2"
            | "__sync_xor_and_fetch_4"
            | "__sync_xor_and_fetch_8"
            | "__sync_xor_and_fetch_16" => Self::XorFetch,

            "__sync_nand_and_fetch_1"
            | "__sync_nand_and_fetch_2"
            | "__sync_nand_and_fetch_4"
            | "__sync_nand_and_fetch_8"
            | "__sync_nand_and_fetch_16" => Self::NandFetch,

            "__sync_fetch_and_add_1"
            | "__sync_fetch_and_add_2"
            | "__sync_fetch_and_add_4"
            | "__sync_fetch_and_add_8"
            | "__sync_fetch_and_add_16" => Self::FetchAdd,

            "__sync_fetch_and_sub_1"
            | "__sync_fetch_and_sub_2"
            | "__sync_fetch_and_sub_4"
            | "__sync_fetch_and_sub_8"
            | "__sync_fetch_and_sub_16" => Self::FetchSub,

            "__sync_fetch_and_or_1"
            | "__sync_fetch_and_or_2"
            | "__sync_fetch_and_or_4"
            | "__sync_fetch_and_or_8"
            | "__sync_fetch_and_or_16" => Self::FetchOr,

            "__sync_fetch_and_and_1"
            | "__sync_fetch_and_and_2"
            | "__sync_fetch_and_and_4"
            | "__sync_fetch_and_and_8"
            | "__sync_fetch_and_and_16" => Self::FetchAnd,

            "__sync_fetch_and_xor_1"
            | "__sync_fetch_and_xor_2"
            | "__sync_fetch_and_xor_4"
            | "__sync_fetch_and_xor_8"
            | "__sync_fetch_and_xor_16" => Self::FetchXor,

            "__sync_fetch_and_nand_1"
            | "__sync_fetch_and_nand_2"
            | "__sync_fetch_and_nand_4"
            | "__sync_fetch_and_nand_8"
            | "__sync_fetch_and_nand_16" => Self::FetchNand,

            _ => return None,
        })
    }

    pub fn fetches_first(self) -> bool {
        matches!(
            self,
            Self::FetchAdd
                | Self::FetchSub
                | Self::FetchOr
                | Self::FetchAnd
                | Self::FetchXor
                | Self::FetchNand
        )
    }

    pub fn is_nand(self) -> bool {
        matches!(self, Self::FetchNand | Self::NandFetch)
    }

    pub fn rust_intrinsic_base_name(self) -> &'static str {
        match self {
            Self::FetchAdd | Self::AddFetch => "xadd",
            Self::FetchSub | Self::SubFetch => "xsub",
            Self::FetchOr | Self::OrFetch => "or",
            Self::FetchAnd | Self::AndFetch => "and",
            Self::FetchXor | Self::XorFetch => "xor",
            Self::FetchNand | Self::NandFetch => "nand",
        }
    }

    pub fn rust_bin_op(self) -> BinOp {
        match self {
            Self::FetchAdd | Self::AddFetch => BinOp::Add(Default::default()),
            Self::FetchSub | Self::SubFetch => BinOp::Sub(Default::default()),
            Self::FetchOr | Self::OrFetch => BinOp::BitOr(Default::default()),
            Self::FetchAnd | Self::AndFetch => BinOp::BitAnd(Default::default()),
            Self::FetchXor | Self::XorFetch => BinOp::BitXor(Default::default()),
            // Gets translated to `!(atomic_nand(arg0, arg1) & arg1)`.
            // See uses of `Self::is_nand`.
            Self::FetchNand | Self::NandFetch => BinOp::BitAnd(Default::default()),
        }
    }
}
