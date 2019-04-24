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
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        let expr = &self.ast_context[fexp];
        let src_loc = &expr.loc;
        let decl_id = match expr.kind {
            CExprKind::DeclRef(_, decl_id, _) => decl_id,
            _ => {
                return Err(TranslationError::generic(
                    "Expected declref when processing builtin",
                ))
            }
        };

        let builtin_name: &str = match self.ast_context[decl_id].kind {
            CDeclKind::Function { ref name, .. } => name,
            _ => {
                return Err(TranslationError::generic(
                    "Expected function when processing builtin",
                ))
            }
        };
        let std_or_core = if self.tcfg.emit_no_std { "core" } else { "std" };

        match builtin_name {
            "__builtin_huge_valf" => Ok(WithStmts::new(mk().path_expr(vec![
                "",
                std_or_core,
                "f32",
                "INFINITY",
            ]))),
            "__builtin_huge_val" | "__builtin_huge_vall" => {
                Ok(WithStmts::new(mk().path_expr(vec![
                    "",
                    std_or_core,
                    "f64",
                    "INFINITY",
                ])))
            }
            "__builtin_inff" => Ok(WithStmts::new(mk().path_expr(vec![
                "",
                std_or_core,
                "f32",
                "INFINITY",
            ]))),
            "__builtin_inf" | "__builtin_infl" => Ok(WithStmts::new(mk().path_expr(vec![
                "",
                std_or_core,
                "f64",
                "INFINITY",
            ]))),
            "__builtin_nanf" => Ok(WithStmts::new(mk().path_expr(vec![
                "",
                std_or_core,
                "f32",
                "NAN",
            ]))),
            "__builtin_nan" => Ok(WithStmts::new(mk().path_expr(vec![
                "",
                std_or_core,
                "f64",
                "NAN",
            ]))),
            "__builtin_clz" | "__builtin_clzl" | "__builtin_clzll" => {
                let val = self.convert_expr(ctx.used(), args[0])?;
                Ok(val.map(|x| {
                    let zeros = mk().method_call_expr(x, "leading_zeros", vec![] as Vec<P<Expr>>);
                    mk().cast_expr(zeros, mk().path_ty(vec!["i32"]))
                }))
            }
            "__builtin_ctz" | "__builtin_ctzl" | "__builtin_ctzll" => {
                let val = self.convert_expr(ctx.used(), args[0])?;
                Ok(val.map(|x| {
                    let zeros = mk().method_call_expr(x, "trailing_zeros", vec![] as Vec<P<Expr>>);
                    mk().cast_expr(zeros, mk().path_ty(vec!["i32"]))
                }))
            }
            "__builtin_bswap16" | "__builtin_bswap32" | "__builtin_bswap64" => {
                let val = self.convert_expr(ctx.used(), args[0])?;
                Ok(val.map(|x| mk().method_call_expr(x, "swap_bytes", vec![] as Vec<P<Expr>>)))
            }
            "__builtin_fabs" | "__builtin_fabsf" | "__builtin_fabsl" => {
                let val = self.convert_expr(ctx.used(), args[0])?;
                Ok(val.map(|x| mk().method_call_expr(x, "abs", vec![] as Vec<P<Expr>>)))
            }
            "__builtin_flt_rounds" => {
                // LLVM simply lowers this to the constant one which means
                // that floats are rounded to the nearest number.
                // https://github.com/llvm-mirror/llvm/blob/master/lib/CodeGen/IntrinsicLowering.cpp#L470
                Ok(WithStmts::new(mk().lit_expr(mk().int_lit(1, "i32"))))
            }
            "__builtin_expect" => self.convert_expr(ctx.used(), args[0]),

            "__builtin_popcount" | "__builtin_popcountl" | "__builtin_popcountll" => {
                let val = self.convert_expr(ctx.used(), args[0])?;
                Ok(val.map(|x| {
                    let zeros = mk().method_call_expr(x, "count_ones", vec![] as Vec<P<Expr>>);
                    mk().cast_expr(zeros, mk().path_ty(vec!["i32"]))
                }))
            }
            "__builtin_bzero" => {
                let ptr_stmts = self.convert_expr(ctx.used(), args[0])?;
                let n_stmts = self.convert_expr(ctx.used(), args[1])?;
                let write_bytes = mk().path_expr(vec!["", "std", "ptr", "write_bytes"]);
                let zero = mk().lit_expr(mk().int_lit(0, "u8"));
                Ok(ptr_stmts.and_then(|ptr| {
                    n_stmts.map(|n| mk().call_expr(write_bytes, vec![ptr, zero, n]))
                }))
            }

            // If the target does not support data prefetch, the address expression is evaluated if
            // it includes side effects but no other code is generated and GCC does not issue a warning.
            // void __builtin_prefetch (const void *addr, ...);
            "__builtin_prefetch" => self.convert_expr(ctx.unused(), args[0]),

            "__builtin_memcpy"
            | "__builtin_memchr"
            | "__builtin_memcmp"
            | "__builtin_memmove"
            | "__builtin_memset" => self.convert_mem_fns(builtin_name, ctx, args),

            "__builtin_add_overflow"
            | "__builtin_sadd_overflow"
            | "__builtin_saddl_overflow"
            | "__builtin_saddll_overflow"
            | "__builtin_uadd_overflow"
            | "__builtin_uaddl_overflow"
            | "__builtin_uaddll_overflow" => {
                self.convert_overflow_arith(ctx, "overflowing_add", args)
            }

            "__builtin_sub_overflow"
            | "__builtin_ssub_overflow"
            | "__builtin_ssubl_overflow"
            | "__builtin_ssubll_overflow"
            | "__builtin_usub_overflow"
            | "__builtin_usubl_overflow"
            | "__builtin_usubll_overflow" => {
                self.convert_overflow_arith(ctx, "overflowing_sub", args)
            }

            "__builtin_mul_overflow"
            | "__builtin_smul_overflow"
            | "__builtin_smull_overflow"
            | "__builtin_smulll_overflow"
            | "__builtin_umul_overflow"
            | "__builtin_umull_overflow"
            | "__builtin_umulll_overflow" => {
                self.convert_overflow_arith(ctx, "overflowing_mul", args)
            }

            // Should be safe to always return 0 here.  "A return of 0 does not indicate that the
            // value is *not* a constant, but merely that GCC cannot prove it is a constant with
            // the specified value of the -O option. "
            "__builtin_constant_p" => Ok(WithStmts::new(mk().lit_expr(mk().int_lit(0, "")))),

            "__builtin_object_size" => {
                // We can't convert this to Rust, but it should be safe to always return -1/0
                // (depending on the value of `type`), so we emit the following:
                // `if (type & 2) == 0 { -1 } else { 0 }`
                let ptr_arg = self.convert_expr(ctx.unused(), args[0])?;
                let type_arg = self.convert_expr(ctx.used(), args[1])?;
                Ok(ptr_arg.and_then(|_| {
                    type_arg.map(|type_arg| {
                        let type_and_2 = mk().binary_expr(BinOpKind::BitAnd, type_arg,
                                                          mk().lit_expr(mk().int_lit(2, "")));
                        let if_cond = mk().binary_expr(BinOpKind::Eq, type_and_2,
                                                       mk().lit_expr(mk().int_lit(0, "")));
                        let minus_one = mk().unary_expr(UnOp::Neg, mk().lit_expr(mk().int_lit(1, "")));
                        mk().ifte_expr(if_cond,
                                       mk().block(vec![mk().expr_stmt(minus_one)]),
                                       Some(mk().lit_expr(mk().int_lit(0, ""))))
                    })
                }))
            }

            "__builtin_va_start" => {
                if ctx.is_unused() && args.len() == 2 {
                    if let Some(va_id) = self.match_vastart(args[0]) {
                        if self.is_promoted_va_decl(va_id) {
                            // `va_start` is automatically called for the promoted decl.
                            return Ok(WithStmts::new(self.panic_or_err("va_start stub")));
                        }
                    }
                }
                Err(TranslationError::generic("Unsupported va_start"))
            }
            "__builtin_va_copy" => {
                // We are waiting on raw va_copy support to land in rustc:
                // https://github.com/rust-lang/rust/pull/59625
                Err(TranslationError::new(
                    &expr.loc,
                    Context::new(TranslationErrorKind::VaCopyNotImplemented),
                ))
                // if ctx.is_unused() && args.len() == 2 {
                //     if let Some((_dst_va_id, _src_va_id)) = self.match_vacopy(args[0], args[1]) {
                //         let dst = self.convert_expr(ctx.used(), args[0])?;
                //         let src = self.convert_expr(ctx.used(), args[1])?;

                //         let path = {
                //             let std_or_core = if self.tcfg.emit_no_std { "core" } else { "std" };
                //             let path = vec!["", std_or_core, "intrinsics", "va_copy"];
                //             mk().path_expr(path)
                //         };
                //         let mut_ref_src = mk().mutbl().addr_of_expr(src.val);
                //         let call_expr = mk().call_expr(path, vec![mut_ref_src] as Vec<P<Expr>>);
                //         let assign_expr = mk().assign_expr(dst.val, call_expr);
                //         let stmt = mk().semi_stmt(assign_expr);

                //         let mut res = WithStmts::new(self.panic_or_err("va_copy stub"));
                //         res.stmts.push(stmt);
                //         return Ok(res);
                //     }
                // }
                // Err(TranslationError::generic("Unsupported va_copy"))
            }
            "__builtin_va_end" => {
                if ctx.is_unused() && args.len() == 1 {
                    if let Some(va_id) = self.match_vaend(args[0]) {
                        if self.is_promoted_va_decl(va_id) {
                            // no need to call end on `va_end` on `va_list` promoted to arg
                            return Ok(WithStmts::new(self.panic_or_err("va_end stub")));
                        } else if self.is_copied_va_decl(va_id) {
                            // call to `va_end` on non-promoted `va_list`

                            let val = self.convert_expr(ctx.used(), args[0])?;

                            let path = {
                                let std_or_core =
                                    if self.tcfg.emit_no_std { "core" } else { "std" };
                                let path = vec!["", std_or_core, "intrinsics", "va_end"];
                                mk().path_expr(path)
                            };
                            let ref_val = mk().mutbl().addr_of_expr(val.val);
                            let call_expr = mk().call_expr(path, vec![ref_val] as Vec<P<Expr>>);

                            let stmt = mk().semi_stmt(call_expr);

                            let mut res = WithStmts::new(self.panic_or_err("va_end stub"));
                            res.stmts.push(stmt);
                            return Ok(res);

                            // return Ok(WithStmts::new(self.panic("va_end stub")))
                        }
                    }
                }
                Err(TranslationError::generic("Unsupported va_end"))
            }

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
            }

            // SIMD builtins:
            "__builtin_ia32_pshufw" => self.convert_simd_builtin(ctx, "_mm_shuffle_pi16", args),
            "__builtin_ia32_shufps" => self.convert_simd_builtin(ctx, "_mm_shuffle_ps", args),
            "__builtin_ia32_shufpd" => self.convert_simd_builtin(ctx, "_mm_shuffle_pd", args),
            "__builtin_ia32_shufps256" => self.convert_simd_builtin(ctx, "_mm256_shuffle_ps", args),
            "__builtin_ia32_shufpd256" => self.convert_simd_builtin(ctx, "_mm256_shuffle_pd", args),
            "__builtin_ia32_pshufd" => self.convert_simd_builtin(ctx, "_mm_shuffle_epi32", args),
            "__builtin_ia32_pshufhw" => self.convert_simd_builtin(ctx, "_mm_shufflehi_epi16", args),
            "__builtin_ia32_pshuflw" => self.convert_simd_builtin(ctx, "_mm_shufflelo_epi16", args),
            "__builtin_ia32_pslldqi128_byteshift" => {
                self.convert_simd_builtin(ctx, "_mm_slli_si128", args)
            }
            "__builtin_ia32_pshufd256" => {
                self.convert_simd_builtin(ctx, "_mm256_shuffle_epi32", args)
            }
            "__builtin_ia32_pshufhw256" => {
                self.convert_simd_builtin(ctx, "_mm256_shufflehi_epi16", args)
            }
            "__builtin_ia32_pshuflw256" => {
                self.convert_simd_builtin(ctx, "_mm256_shufflelo_epi16", args)
            }
            "__builtin_ia32_vec_ext_v4si" => {
                self.convert_simd_builtin(ctx, "_mm_extract_epi32", args)
            }
            "__builtin_ia32_vec_ext_v16qi" => {
                self.convert_simd_builtin(ctx, "_mm_extract_epi8", args)
            }
            "__builtin_ia32_vec_ext_v2di" => {
                self.convert_simd_builtin(ctx, "_mm_extract_epi64", args)
            }
            "__builtin_ia32_roundps" => self.convert_simd_builtin(ctx, "_mm_round_ps", args),
            "__builtin_ia32_roundss" => self.convert_simd_builtin(ctx, "_mm_round_ss", args),
            "__builtin_ia32_roundpd" => self.convert_simd_builtin(ctx, "_mm_round_pd", args),
            "__builtin_ia32_roundsd" => self.convert_simd_builtin(ctx, "_mm_round_sd", args),
            "__builtin_ia32_blendpd" => self.convert_simd_builtin(ctx, "_mm_blend_pd", args),
            "__builtin_ia32_blendps" => self.convert_simd_builtin(ctx, "_mm_blend_ps", args),
            "__builtin_ia32_pblendw128" => self.convert_simd_builtin(ctx, "_mm_blend_epi16", args),
            "__builtin_ia32_dpps" => self.convert_simd_builtin(ctx, "_mm_dp_ps", args),
            "__builtin_ia32_dppd" => self.convert_simd_builtin(ctx, "_mm_dp_pd", args),
            "__builtin_ia32_insertps128" => self.convert_simd_builtin(ctx, "_mm_insert_ps", args),
            "__builtin_ia32_vec_ext_v4sf" => self.convert_simd_builtin(ctx, "_mm_extract_ps", args),
            "__builtin_ia32_vec_set_v16qi" => {
                self.convert_simd_builtin(ctx, "_mm_insert_epi8", args)
            }
            "__builtin_ia32_vec_set_v2di" => {
                self.convert_simd_builtin(ctx, "_mm_insert_epi64", args)
            }
            "__builtin_ia32_mpsadbw128" => self.convert_simd_builtin(ctx, "_mm_mpsadbw_epu8", args),
            "__builtin_ia32_pcmpistrm128" => self.convert_simd_builtin(ctx, "_mm_cmpistrm", args),
            "__builtin_ia32_pcmpistri128" => self.convert_simd_builtin(ctx, "_mm_cmpistri", args),
            "__builtin_ia32_pcmpestrm128" => self.convert_simd_builtin(ctx, "_mm_cmpestrm", args),
            "__builtin_ia32_pcmpistria128" => self.convert_simd_builtin(ctx, "_mm_cmpistra", args),
            "__builtin_ia32_pcmpistric128" => self.convert_simd_builtin(ctx, "_mm_cmpistrc", args),
            "__builtin_ia32_pcmpistrio128" => self.convert_simd_builtin(ctx, "_mm_cmpistro", args),
            "__builtin_ia32_pcmpistris128" => self.convert_simd_builtin(ctx, "_mm_cmpistrs", args),
            "__builtin_ia32_pcmpistriz128" => self.convert_simd_builtin(ctx, "_mm_cmpistrz", args),
            "__builtin_ia32_pcmpestria128" => self.convert_simd_builtin(ctx, "_mm_cmpestra", args),
            "__builtin_ia32_pcmpestric128" => self.convert_simd_builtin(ctx, "_mm_cmpestrc", args),
            "__builtin_ia32_pcmpestrio128" => self.convert_simd_builtin(ctx, "_mm_cmpestro", args),
            "__builtin_ia32_pcmpestris128" => self.convert_simd_builtin(ctx, "_mm_cmpestrs", args),
            "__builtin_ia32_pcmpestriz128" => self.convert_simd_builtin(ctx, "_mm_cmpestrz", args),

            "__sync_val_compare_and_swap_1"
            | "__sync_val_compare_and_swap_2"
            | "__sync_val_compare_and_swap_4"
            | "__sync_val_compare_and_swap_8"
            | "__sync_val_compare_and_swap_16"
            | "__sync_bool_compare_and_swap_1"
            | "__sync_bool_compare_and_swap_2"
            | "__sync_bool_compare_and_swap_4"
            | "__sync_bool_compare_and_swap_8"
            | "__sync_bool_compare_and_swap_16" => {
                self.use_feature("core_intrinsics");

                // Emit `atomic_cxchg(a0, a1, a2).idx`
                let atomic_cxchg =
                    mk().path_expr(vec!["", std_or_core, "intrinsics", "atomic_cxchg"]);
                let arg0 = self.convert_expr(ctx.used(), args[0])?;
                let arg1 = self.convert_expr(ctx.used(), args[1])?;
                let arg2 = self.convert_expr(ctx.used(), args[2])?;
                Ok(arg0.and_then(|arg0| {
                    arg1.and_then(|arg1| {
                        arg2.and_then(|arg2| {
                            let call = mk().call_expr(atomic_cxchg, vec![arg0, arg1, arg2]);
                            let field_idx = if builtin_name.starts_with("__sync_val") {
                                "0"
                            } else {
                                "1"
                            };
                            let call_expr = mk().field_expr(call, field_idx);
                            self.convert_side_effects_expr(
                                ctx,
                                vec![],
                                call_expr,
                                "Builtin is not supposed to be used",
                            )
                        })
                    })
                }))
            }

            "__sync_fetch_and_add_1"
            | "__sync_fetch_and_add_2"
            | "__sync_fetch_and_add_4"
            | "__sync_fetch_and_add_8"
            | "__sync_fetch_and_add_16"
            | "__sync_fetch_and_sub_1"
            | "__sync_fetch_and_sub_2"
            | "__sync_fetch_and_sub_4"
            | "__sync_fetch_and_sub_8"
            | "__sync_fetch_and_sub_16"
            | "__sync_fetch_and_or_1"
            | "__sync_fetch_and_or_2"
            | "__sync_fetch_and_or_4"
            | "__sync_fetch_and_or_8"
            | "__sync_fetch_and_or_16"
            | "__sync_fetch_and_and_1"
            | "__sync_fetch_and_and_2"
            | "__sync_fetch_and_and_4"
            | "__sync_fetch_and_and_8"
            | "__sync_fetch_and_and_16"
            | "__sync_fetch_and_xor_1"
            | "__sync_fetch_and_xor_2"
            | "__sync_fetch_and_xor_4"
            | "__sync_fetch_and_xor_8"
            | "__sync_fetch_and_xor_16"
            | "__sync_fetch_and_nand_1"
            | "__sync_fetch_and_nand_2"
            | "__sync_fetch_and_nand_4"
            | "__sync_fetch_and_nand_8"
            | "__sync_fetch_and_nand_16"
            | "__sync_add_and_fetch_1"
            | "__sync_add_and_fetch_2"
            | "__sync_add_and_fetch_4"
            | "__sync_add_and_fetch_8"
            | "__sync_add_and_fetch_16"
            | "__sync_sub_and_fetch_1"
            | "__sync_sub_and_fetch_2"
            | "__sync_sub_and_fetch_4"
            | "__sync_sub_and_fetch_8"
            | "__sync_sub_and_fetch_16"
            | "__sync_or_and_fetch_1"
            | "__sync_or_and_fetch_2"
            | "__sync_or_and_fetch_4"
            | "__sync_or_and_fetch_8"
            | "__sync_or_and_fetch_16"
            | "__sync_and_and_fetch_1"
            | "__sync_and_and_fetch_2"
            | "__sync_and_and_fetch_4"
            | "__sync_and_and_fetch_8"
            | "__sync_and_and_fetch_16"
            | "__sync_xor_and_fetch_1"
            | "__sync_xor_and_fetch_2"
            | "__sync_xor_and_fetch_4"
            | "__sync_xor_and_fetch_8"
            | "__sync_xor_and_fetch_16"
            | "__sync_nand_and_fetch_1"
            | "__sync_nand_and_fetch_2"
            | "__sync_nand_and_fetch_4"
            | "__sync_nand_and_fetch_8"
            | "__sync_nand_and_fetch_16" => {
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
                let atomic_func = mk().path_expr(vec!["", std_or_core, "intrinsics", func_name]);
                let arg0 = self.convert_expr(ctx.used(), args[0])?;
                let arg1 = self.convert_expr(ctx.used(), args[1])?;
                if builtin_name.starts_with("__sync_fetch") {
                    // __sync_fetch_and_XXX
                    Ok(arg0.and_then(|arg0| {
                        arg1.and_then(|arg1| {
                            let call_expr = mk().call_expr(atomic_func, vec![arg0, arg1]);
                            self.convert_side_effects_expr(
                                ctx,
                                vec![],
                                call_expr,
                                "Builtin is not supposed to be used",
                            )
                        })
                    }))
                } else {
                    // __sync_XXX_and_fetch
                    Ok(arg0.and_then(|arg0| {
                        arg1.and_then(|arg1| {
                            // Since the value of `arg1` is used twice, we need to copy
                            // it into a local temporary so we don't duplicate any side-effects
                            // To preserve ordering of side-effects, we also do this for arg0
                            let arg0_name = self.renamer.borrow_mut().fresh();
                            let arg0_let = mk().local_stmt(P(mk().local(
                                mk().ident_pat(&arg0_name),
                                None as Option<P<Ty>>,
                                Some(arg0),
                            )));

                            let arg1_name = self.renamer.borrow_mut().fresh();
                            let arg1_let = mk().local_stmt(P(mk().local(
                                mk().ident_pat(&arg1_name),
                                None as Option<P<Ty>>,
                                Some(arg1),
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
                                vec![arg0_let, arg1_let],
                                val,
                                "Builtin is not supposed to be used",
                            )
                        })
                    }))
                }
            }

            "__sync_synchronize" => {
                self.use_feature("core_intrinsics");

                let atomic_func =
                    mk().path_expr(vec!["", std_or_core, "intrinsics", "atomic_fence"]);
                let call_expr = mk().call_expr(atomic_func, vec![] as Vec<P<Expr>>);
                Ok(self.convert_side_effects_expr(
                    ctx,
                    vec![],
                    call_expr,
                    "Builtin is not supposed to be used",
                ))
            }

            "__sync_lock_test_and_set_1"
            | "__sync_lock_test_and_set_2"
            | "__sync_lock_test_and_set_4"
            | "__sync_lock_test_and_set_8"
            | "__sync_lock_test_and_set_16" => {
                self.use_feature("core_intrinsics");

                // Emit `atomic_xchg_acq(arg0, arg1)`
                let atomic_func =
                    mk().path_expr(vec!["", std_or_core, "intrinsics", "atomic_xchg_acq"]);
                let arg0 = self.convert_expr(ctx.used(), args[0])?;
                let arg1 = self.convert_expr(ctx.used(), args[1])?;
                Ok(arg0.and_then(|arg0| {
                    arg1.and_then(|arg1| {
                        let call_expr = mk().call_expr(atomic_func, vec![arg0, arg1]);
                        self.convert_side_effects_expr(
                            ctx,
                            vec![],
                            call_expr,
                            "Builtin is not supposed to be used",
                        )
                    })
                }))
            }

            "__sync_lock_release_1"
            | "__sync_lock_release_2"
            | "__sync_lock_release_4"
            | "__sync_lock_release_8"
            | "__sync_lock_release_16" => {
                self.use_feature("core_intrinsics");

                // Emit `atomic_store_rel(arg0, 0)`
                let atomic_func =
                    mk().path_expr(vec!["", std_or_core, "intrinsics", "atomic_store_rel"]);
                let arg0 = self.convert_expr(ctx.used(), args[0])?;
                Ok(arg0.and_then(|arg0| {
                    let zero = mk().lit_expr(mk().int_lit(0, ""));
                    let call_expr = mk().call_expr(atomic_func, vec![arg0, zero]);
                    self.convert_side_effects_expr(
                        ctx,
                        vec![],
                        call_expr,
                        "Builtin is not supposed to be used",
                    )
                }))
            }

            "__builtin_unreachable" => {
                let mut res = WithStmts::new(self.panic_or_err("unreachable stub"));
                res.stmts.push(mk().semi_stmt(mk().mac_expr(mk().mac(
                    vec!["unreachable"],
                    vec![],
                    MacDelimiter::Parenthesis,
                ))));
                Ok(res)
            }

            _ => Err(format_translation_err!(src_loc, "Unimplemented builtin {}", builtin_name)),
        }
    }

    // This translation logic handles converting code that uses
    // https://gcc.gnu.org/onlinedocs/gcc/Integer-Overflow-Builtins.html
    fn convert_overflow_arith(
        &self,
        ctx: ExprContext,
        method_name: &str,
        args: &[CExprId],
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
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

    /// Converts a __buitlin_mem* use by calling into libc's mem* directly.
    fn convert_mem_fns(
        &self,
        builtin_name: &str,
        ctx: ExprContext,
        args: &[CExprId],
    ) -> Result<WithStmts<P<Expr>>, TranslationError> {
        let name = &builtin_name[10..];
        let mem = mk().path_expr(vec!["libc", name]);
        let dst = self.convert_expr(ctx.used(), args[0])?;
        let mut c = self.convert_expr(ctx.used(), args[1])?;
        let mut len = self.convert_expr(ctx.used(), args[2])?;
        let size_t = mk().path_ty(vec!["libc", "size_t"]);
        let len1 = mk().cast_expr(len.val, size_t);
        let mem_expr = mk().call_expr(mem, vec![dst.val, c.val, len1]);

        let mut stmts = dst.stmts;
        stmts.append(&mut c.stmts);
        stmts.append(&mut len.stmts);

        let val = if ctx.is_used() {
            mem_expr
        } else {
            stmts.push(mk().semi_stmt(mem_expr));
            self.panic_or_err(&format!("__builtin_{} not used", name))
        };

        Ok(WithStmts { stmts, val })
    }
}
