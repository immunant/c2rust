#![deny(missing_docs)]
//! Implementations of clang's builtin functions

use super::*;

impl<'c> Translation<'c> {
    /// Convert a call to a builtin function to a Rust expression
    pub fn convert_builtin(
        &self,
        ctx: ExprContext,
        fexp: CExprId,
        args: &[CExprId],
    ) -> Result<WithStmts<P<Expr>>, String> {
        let decl_id = match self.ast_context[fexp].kind {
            CExprKind::DeclRef(_, decl_id, _) => decl_id,
            _ => return Err(format!("Expected declref when processing builtin")),
        };

        let builtin_name: &str = match self.ast_context[decl_id].kind {
            CDeclKind::Function { ref name, .. } => name,
            _ => return Err(format!("Expected function when processing builtin")),
        };
        let std_or_core = if self.tcfg.emit_no_std {
            "core"
        } else {
            "std"
        };

        match builtin_name {
            "__builtin_huge_valf" => Ok(WithStmts::new(
                mk().path_expr(vec!["", std_or_core, "f32", "INFINITY"]),
            )),
            "__builtin_huge_val" | "__builtin_huge_vall" => Ok(WithStmts::new(
                mk().path_expr(vec!["", std_or_core, "f64", "INFINITY"]),
            )),
            "__builtin_inff" => Ok(WithStmts::new(
                mk().path_expr(vec!["", std_or_core, "f32", "INFINITY"]),
            )),
            "__builtin_inf" | "__builtin_infl" => Ok(WithStmts::new(
                mk().path_expr(vec!["", std_or_core, "f64", "INFINITY"]),
            )),
            "__builtin_nanf" => Ok(WithStmts::new(
                mk().path_expr(vec!["", std_or_core, "f32", "NAN"]),
            )),
            "__builtin_nan" => Ok(WithStmts::new(
                mk().path_expr(vec!["", std_or_core, "f64", "NAN"]),
            )),
            "__builtin_clz" | "__builtin_clzl" | "__builtin_clzll" => {
                let val = self.convert_expr(ctx.used(), args[0])?;
                Ok(val.map(|x| {
                    let zeros = mk().method_call_expr(x, "leading_zeros", vec![] as Vec<P<Expr>>);
                    mk().cast_expr(zeros, mk().path_ty(vec!["i32"]))
                }))
            },
            "__builtin_ctz" | "__builtin_ctzl" | "__builtin_ctzll" => {
                let val = self.convert_expr(ctx.used(), args[0])?;
                Ok(val.map(|x| {
                    let zeros = mk().method_call_expr(x, "trailing_zeros", vec![] as Vec<P<Expr>>);
                    mk().cast_expr(zeros, mk().path_ty(vec!["i32"]))
                }))
            },
            "__builtin_bswap16" | "__builtin_bswap32" | "__builtin_bswap64" => {
                let val = self.convert_expr(ctx.used(), args[0])?;
                Ok(val.map(|x| mk().method_call_expr(x, "swap_bytes", vec![] as Vec<P<Expr>>)))
            },
            "__builtin_fabs" | "__builtin_fabsf" | "__builtin_fabsl" => {
                let val = self.convert_expr(ctx.used(), args[0])?;
                Ok(val.map(|x| mk().method_call_expr(x, "abs", vec![] as Vec<P<Expr>>)))
            },
            "__builtin_expect" => self.convert_expr(ctx.used(), args[0]),

            "__builtin_popcount" | "__builtin_popcountl" | "__builtin_popcountll" => {
                let val = self.convert_expr(ctx.used(), args[0])?;
                Ok(val.map(|x| {
                    let zeros = mk().method_call_expr(x, "count_ones", vec![] as Vec<P<Expr>>);
                    mk().cast_expr(zeros, mk().path_ty(vec!["i32"]))
                }))
            },
            "__builtin_bzero" => {
                let ptr_stmts = self.convert_expr(ctx.used(), args[0])?;
                let n_stmts = self.convert_expr(ctx.used(), args[1])?;
                let write_bytes = mk().path_expr(vec!["", "std", "ptr", "write_bytes"]);
                let zero = mk().lit_expr(mk().int_lit(0, "u8"));
                Ok(ptr_stmts.and_then(|ptr| {
                    n_stmts.map(|n| mk().call_expr(write_bytes, vec![ptr, zero, n]))
                }))
            },

            // If the target does not support data prefetch, the address expression is evaluated if
            // it includes side effects but no other code is generated and GCC does not issue a warning.
            // void __builtin_prefetch (const void *addr, ...);
            "__builtin_prefetch" => self.convert_expr(ctx.unused(), args[0]),

            "__builtin_memcpy" => self.convert_memcpy(ctx, args),

            "__builtin_add_overflow"
            | "__builtin_sadd_overflow"
            | "__builtin_saddl_overflow"
            | "__builtin_saddll_overflow"
            | "__builtin_uadd_overflow"
            | "__builtin_uaddl_overflow"
            | "__builtin_uaddll_overflow" => {
                self.convert_overflow_arith(ctx, "overflowing_add", args)
            },

            "__builtin_sub_overflow"
            | "__builtin_ssub_overflow"
            | "__builtin_ssubl_overflow"
            | "__builtin_ssubll_overflow"
            | "__builtin_usub_overflow"
            | "__builtin_usubl_overflow"
            | "__builtin_usubll_overflow" => {
                self.convert_overflow_arith(ctx, "overflowing_sub", args)
            },

            "__builtin_mul_overflow"
            | "__builtin_smul_overflow"
            | "__builtin_smull_overflow"
            | "__builtin_smulll_overflow"
            | "__builtin_umul_overflow"
            | "__builtin_umull_overflow"
            | "__builtin_umulll_overflow" => {
                self.convert_overflow_arith(ctx, "overflowing_mul", args)
            },

            // Should be safe to always return 0 here.  "A return of 0 does not indicate that the
            // value is *not* a constant, but merely that GCC cannot prove it is a constant with
            // the specified value of the -O option. "
            "__builtin_constant_p" => Ok(WithStmts::new(mk().lit_expr(mk().int_lit(0, "")))),

            "__builtin_va_start" => {
                if ctx.is_unused() && args.len() == 2 {
                    if let Some(va_id) = self.match_vastart(args[0]) {
                        if ctx.is_va_decl(va_id) {
                            return Ok(WithStmts::new(self.panic("va_start stub")))
                        }
                    }
                }
                Err(format!("Unsupported va_start"))
            },
            "__builtin_va_copy" => Err(format!(
                "va_copy not supported"
            )),
            "__builtin_va_end" => {
                if ctx.is_unused() && args.len() == 1 {
                    if let Some(va_id) = self.match_vaend(args[0]) {
                        if ctx.is_va_decl(va_id) {
                            return Ok(WithStmts::new(self.panic("va_end stub")))
                        }
                    }
                }
                Err(format!("Unsupported va_end"))
            },

            "__builtin_alloca" => {
                let count = self.convert_expr(ctx.used(), args[0])?;
                let mut stmts = count.stmts;

                let alloca_name = self.renamer.borrow_mut().fresh();
                let zero_elem = mk().lit_expr(mk().int_lit(0, LitIntType::Unsuffixed));
                stmts.push(mk().local_stmt(P(mk().local(
                    mk().mutbl().ident_pat(&alloca_name),
                    None as Option<P<Ty>>,
                    Some(vec_expr(zero_elem, cast_int(count.val, "usize"))),
                ))));
                Ok(WithStmts {
                    stmts,
                    val: mk().method_call_expr(
                        mk().ident_expr(&alloca_name),
                        "as_mut_ptr",
                        vec![] as Vec<P<Expr>>,
                    ),
                })
            },

            // In clang 6 this first one is the only true SIMD builtin, clang 7 converted a bunch more after it:
            "__builtin_ia32_pshufw" =>
                self.convert_simd_builtin(ctx, "_mm_shuffle_pi16", args),
            "__builtin_ia32_shufps" =>
                self.convert_simd_builtin(ctx, "_mm_shuffle_ps", args),
            "__builtin_ia32_shufpd" =>
                self.convert_simd_builtin(ctx, "_mm_shuffle_pd", args),
            "__builtin_ia32_shufps256" =>
                self.convert_simd_builtin(ctx, "_mm256_shuffle_ps", args),
            "__builtin_ia32_shufpd256" =>
                self.convert_simd_builtin(ctx, "_mm256_shuffle_pd", args),
            "__builtin_ia32_pshufd" =>
                self.convert_simd_builtin(ctx, "_mm_shuffle_epi32", args),
            "__builtin_ia32_pshufhw" =>
                self.convert_simd_builtin(ctx, "_mm_shufflehi_epi16", args),
            "__builtin_ia32_pshuflw" =>
                self.convert_simd_builtin(ctx, "_mm_shufflelo_epi16", args),
            "__builtin_ia32_pslldqi128_byteshift" =>
                self.convert_simd_builtin(ctx, "_mm_slli_si128", args),
            "__builtin_ia32_pshufd256" =>
                self.convert_simd_builtin(ctx, "_mm256_shuffle_epi32", args),
            "__builtin_ia32_pshufhw256" =>
                self.convert_simd_builtin(ctx, "_mm256_shufflehi_epi16", args),
            "__builtin_ia32_pshuflw256" =>
                self.convert_simd_builtin(ctx, "_mm256_shufflelo_epi16", args),

            "__sync_val_compare_and_swap_1" |
            "__sync_val_compare_and_swap_2" |
            "__sync_val_compare_and_swap_4" |
            "__sync_val_compare_and_swap_8" |
            "__sync_val_compare_and_swap_16" |
            "__sync_bool_compare_and_swap_1" |
            "__sync_bool_compare_and_swap_2" |
            "__sync_bool_compare_and_swap_4" |
            "__sync_bool_compare_and_swap_8" |
            "__sync_bool_compare_and_swap_16" => {
                self.extern_crates.borrow_mut().insert("core");
                self.use_feature("core_intrinsics");

                // Emit `atomic_cxchg(a0, a1, a2).idx`
                let atomic_cxchg = mk().path_expr(vec!["", "core", "intrinsics", "atomic_cxchg"]);
                let arg0 = self.convert_expr(ctx.used(), args[0])?;
                let arg1 = self.convert_expr(ctx.used(), args[1])?;
                let arg2 = self.convert_expr(ctx.used(), args[2])?;
                Ok(arg0.and_then(|arg0| arg1.and_then(|arg1| arg2.and_then(|arg2| {
                    let call = mk().call_expr(atomic_cxchg, vec![arg0, arg1, arg2]);
                    let field_idx = if builtin_name.starts_with("__sync_val") {
                        "0"
                    } else {
                        "1"
                    };
                    let call_expr = mk().field_expr(call, field_idx);
                    self.convert_side_effects_expr(ctx, vec![], call_expr,
                                                   "Builtin is not supposed to be used")
                }))))
            },

            "__sync_fetch_and_add_1" |
            "__sync_fetch_and_add_2" |
            "__sync_fetch_and_add_4" |
            "__sync_fetch_and_add_8" |
            "__sync_fetch_and_add_16" |
            "__sync_fetch_and_sub_1" |
            "__sync_fetch_and_sub_2" |
            "__sync_fetch_and_sub_4" |
            "__sync_fetch_and_sub_8" |
            "__sync_fetch_and_sub_16" |
            "__sync_fetch_and_or_1" |
            "__sync_fetch_and_or_2" |
            "__sync_fetch_and_or_4" |
            "__sync_fetch_and_or_8" |
            "__sync_fetch_and_or_16" |
            "__sync_fetch_and_and_1" |
            "__sync_fetch_and_and_2" |
            "__sync_fetch_and_and_4" |
            "__sync_fetch_and_and_8" |
            "__sync_fetch_and_and_16" |
            "__sync_fetch_and_xor_1" |
            "__sync_fetch_and_xor_2" |
            "__sync_fetch_and_xor_4" |
            "__sync_fetch_and_xor_8" |
            "__sync_fetch_and_xor_16" |
            "__sync_fetch_and_nand_1" |
            "__sync_fetch_and_nand_2" |
            "__sync_fetch_and_nand_4" |
            "__sync_fetch_and_nand_8" |
            "__sync_fetch_and_nand_16" |
            "__sync_add_and_fetch_1" |
            "__sync_add_and_fetch_2" |
            "__sync_add_and_fetch_4" |
            "__sync_add_and_fetch_8" |
            "__sync_add_and_fetch_16" |
            "__sync_sub_and_fetch_1" |
            "__sync_sub_and_fetch_2" |
            "__sync_sub_and_fetch_4" |
            "__sync_sub_and_fetch_8" |
            "__sync_sub_and_fetch_16" |
            "__sync_or_and_fetch_1" |
            "__sync_or_and_fetch_2" |
            "__sync_or_and_fetch_4" |
            "__sync_or_and_fetch_8" |
            "__sync_or_and_fetch_16" |
            "__sync_and_and_fetch_1" |
            "__sync_and_and_fetch_2" |
            "__sync_and_and_fetch_4" |
            "__sync_and_and_fetch_8" |
            "__sync_and_and_fetch_16" |
            "__sync_xor_and_fetch_1" |
            "__sync_xor_and_fetch_2" |
            "__sync_xor_and_fetch_4" |
            "__sync_xor_and_fetch_8" |
            "__sync_xor_and_fetch_16" |
            "__sync_nand_and_fetch_1" |
            "__sync_nand_and_fetch_2" |
            "__sync_nand_and_fetch_4" |
            "__sync_nand_and_fetch_8" |
            "__sync_nand_and_fetch_16" => {
                self.extern_crates.borrow_mut().insert("core");
                self.use_feature("core_intrinsics");

                let (func_name, binary_op, is_nand) = if builtin_name.contains("_add_") {
                    ("atomic_xadd", BinOpKind::Add, false)
                } else if builtin_name.contains("_sub_") {
                    ("atomic_xsub", BinOpKind::Sub, false)
                } else if builtin_name.contains("_or_") {
                    ("atomic_or", BinOpKind::BitOr, false)
                } else if builtin_name.contains("_xor_") {
                    ("atomic_xor", BinOpKind::BitXor, false)
                } else if builtin_name.contains("_nand_") {
                    ("atomic_nand", BinOpKind::BitAnd, true)
                } else {
                    // We can't explicitly check for "_and_" since they all contain it
                    ("atomic_and", BinOpKind::BitAnd, false)
                };

                // Emit `atomic_func(a0, a1) (op a1)?`
                let atomic_func = mk().path_expr(vec!["", "core", "intrinsics", func_name]);
                let arg0 = self.convert_expr(ctx.used(), args[0])?;
                let arg1 = self.convert_expr(ctx.used(), args[1])?;
                if builtin_name.starts_with("__sync_fetch") {
                    // __sync_fetch_and_XXX
                    Ok(arg0.and_then(|arg0| arg1.and_then(|arg1| {
                        let call_expr = mk().call_expr(atomic_func, vec![arg0, arg1]);
                        self.convert_side_effects_expr(ctx, vec![], call_expr,
                                                       "Builtin is not supposed to be used")
                    })))
                } else {
                    // __sync_XXX_and_fetch
                    Ok(arg0.and_then(|arg0| arg1.and_then(|arg1| {
                        // Since the value of `arg1` is used twice, we need to copy
                        // it into a local temporary so we don't duplicate any side-effects
                        // To preserve ordering of side-effects, we also do this for arg0
                        let arg0_name = self.renamer.borrow_mut().fresh();
                        let arg0_let = mk().local_stmt(P(mk().local(
                            mk().ident_pat(&arg0_name),
                            None as Option<P<Ty>>,
                            Some(arg0))));

                        let arg1_name = self.renamer.borrow_mut().fresh();
                        let arg1_let = mk().local_stmt(P(mk().local(
                            mk().ident_pat(&arg1_name),
                            None as Option<P<Ty>>,
                            Some(arg1))));

                        let call = mk().call_expr(atomic_func,
                                                  vec![mk().ident_expr(&arg0_name),
                                                       mk().ident_expr(&arg1_name)]);
                        let val = mk().binary_expr(binary_op, call, mk().ident_expr(arg1_name));
                        let val = if is_nand {
                            // For nand, return `!(atomic_nand(arg0, arg1) & arg1)`
                            mk().unary_expr(UnOp::Not, val)
                        } else {
                            val
                        };
                        self.convert_side_effects_expr(ctx, vec![arg0_let, arg1_let], val,
                                                       "Builtin is not supposed to be used")
                    })))
                }
            },

            "__sync_synchronize" => {
                self.extern_crates.borrow_mut().insert("core");
                self.use_feature("core_intrinsics");

                let atomic_func = mk().path_expr(vec!["", "core", "intrinsics", "atomic_fence"]);
                let call_expr = mk().call_expr(atomic_func, vec![] as Vec<P<Expr>>);
                Ok(self.convert_side_effects_expr(ctx, vec![], call_expr,
                                                  "Builtin is not supposed to be used"))
            },

            "__sync_lock_test_and_set_1" |
            "__sync_lock_test_and_set_2" |
            "__sync_lock_test_and_set_4" |
            "__sync_lock_test_and_set_8" |
            "__sync_lock_test_and_set_16" => {
                self.extern_crates.borrow_mut().insert("core");
                self.use_feature("core_intrinsics");

                // Emit `atomic_xchg_acq(arg0, arg1)`
                let atomic_func = mk().path_expr(vec!["", "core", "intrinsics", "atomic_xchg_acq"]);
                let arg0 = self.convert_expr(ctx.used(), args[0])?;
                let arg1 = self.convert_expr(ctx.used(), args[1])?;
                Ok(arg0.and_then(|arg0| arg1.and_then(|arg1| {
                    let call_expr = mk().call_expr(atomic_func, vec![arg0, arg1]);
                    self.convert_side_effects_expr(ctx, vec![], call_expr,
                                                   "Builtin is not supposed to be used")
                })))
            },

            "__sync_lock_release_1" |
            "__sync_lock_release_2" |
            "__sync_lock_release_4" |
            "__sync_lock_release_8" |
            "__sync_lock_release_16" => {
                self.extern_crates.borrow_mut().insert("core");
                self.use_feature("core_intrinsics");

                // Emit `atomic_store_rel(arg0, 0)`
                let atomic_func = mk().path_expr(vec!["", "core", "intrinsics", "atomic_store_rel"]);
                let arg0 = self.convert_expr(ctx.used(), args[0])?;
                Ok(arg0.and_then(|arg0| {
                    let zero = mk().lit_expr(mk().int_lit(0, ""));
                    let call_expr = mk().call_expr(atomic_func, vec![arg0, zero]);
                    self.convert_side_effects_expr(ctx, vec![], call_expr,
                                                   "Builtin is not supposed to be used")
                }))
            },

            _ => Err(format!("Unimplemented builtin: {}", builtin_name)),
        }
    }

    // This translation logic handles converting code that uses
    // https://gcc.gnu.org/onlinedocs/gcc/Integer-Overflow-Builtins.html
    fn convert_overflow_arith(
        &self,
        ctx: ExprContext,
        method_name: &str,
        args: &[CExprId],
    ) -> Result<WithStmts<P<Expr>>, String> {
        let a = self.convert_expr(ctx.used(), args[0])?;
        let mut b = self.convert_expr(ctx.used(), args[1])?;
        let mut c = self.convert_expr(ctx.used(), args[2])?;

        let overflowing = mk().method_call_expr(a.val, method_name, vec![b.val]);
        let sum_name = self.renamer.borrow_mut().fresh();
        let over_name = self.renamer.borrow_mut().fresh();
        let overflow_let = mk().local_stmt(P(mk().local(
            mk().tuple_pat(vec![
                mk().ident_pat(&sum_name),
                mk().ident_pat(over_name.clone()),
            ]),
            None as Option<P<Ty>>,
            Some(overflowing),
        )));

        let out_assign = mk().assign_expr(
            mk().unary_expr(ast::UnOp::Deref, c.val),
            mk().ident_expr(&sum_name),
        );

        let mut stmts = a.stmts;
        stmts.append(&mut b.stmts);
        stmts.append(&mut c.stmts);
        stmts.push(overflow_let);
        stmts.push(mk().expr_stmt(out_assign));

        Ok(WithStmts {
            stmts,
            val: mk().ident_expr(over_name),
        })
    }

    /// Convert a builtin_memcpy use by calling into libc's memcpy directly.
    fn convert_memcpy(
        &self,
        ctx: ExprContext,
        args: &[CExprId],
    ) -> Result<WithStmts<P<Expr>>, String> {
        let memcpy = mk().path_expr(vec!["", "libc", "memcpy"]);
        let dst = self.convert_expr(ctx.used(), args[0])?;
        let mut src = self.convert_expr(ctx.used(), args[1])?;
        let mut len = self.convert_expr(ctx.used(), args[2])?;
        let size_t = mk().path_ty(vec!["libc", "size_t"]);
        let len1 = mk().cast_expr(len.val, size_t);
        let memcpy_expr = mk().call_expr(memcpy, vec![dst.val, src.val, len1]);

        let mut stmts = dst.stmts;
        stmts.append(&mut src.stmts);
        stmts.append(&mut len.stmts);

        let val =
            if ctx.is_used() {
                memcpy_expr
            } else {
                stmts.push(mk().semi_stmt(memcpy_expr));
                self.panic("__builtin_memcpy not used")
            };

        Ok(WithStmts { stmts, val })
    }
}
