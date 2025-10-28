#![deny(missing_docs)]
//! Implementations of clang's builtin functions

use crate::format_translation_err;

use super::*;

/// The argument type for a libc builtin function
#[derive(Copy, Clone, PartialEq)]
enum LibcFnArgType {
    /// char* or void*
    Mem,
    /// size_t
    Size,
    /// int or char
    Int,
}

impl<'c> Translation<'c> {
    /// Convert a call to a rotate builtin.
    fn convert_builtin_rotate(
        &self,
        ctx: ExprContext,
        args: &[CExprId],
        rotate_method_name: &'static str,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        // Emit `arg0.{method_name}(arg1)`
        let arg0 = self.convert_expr(ctx.used(), args[0], None)?;
        let arg1 = self.convert_expr(ctx.used(), args[1], None)?;
        arg0.and_then(|arg0| {
            arg1.and_then(|arg1| {
                let arg1 = mk().cast_expr(arg1, mk().path_ty(vec!["u32"]));
                let method_call_expr = mk().method_call_expr(arg0, rotate_method_name, vec![arg1]);
                self.convert_side_effects_expr(
                    ctx,
                    WithStmts::new_val(method_call_expr),
                    "Builtin is not supposed to be used",
                )
            })
        })
    }

    /// Convert a call to a builtin function to a Rust expression
    pub fn convert_builtin(
        &self,
        ctx: ExprContext,
        fexp: CExprId,
        args: &[CExprId],
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
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

        match builtin_name {
            "__builtin_huge_valf" => Ok(WithStmts::new_val(
                mk().abs_path_expr(vec!["core", "f32", "INFINITY"]),
            )),
            "__builtin_huge_val" | "__builtin_huge_vall" => Ok(WithStmts::new_val(
                mk().abs_path_expr(vec!["core", "f64", "INFINITY"]),
            )),
            "__builtin_inff" => Ok(WithStmts::new_val(
                mk().abs_path_expr(vec!["core", "f32", "INFINITY"]),
            )),
            "__builtin_inf" | "__builtin_infl" => Ok(WithStmts::new_val(
                mk().abs_path_expr(vec!["core", "f64", "INFINITY"]),
            )),
            "__builtin_nanf" => Ok(WithStmts::new_val(
                mk().abs_path_expr(vec!["core", "f32", "NAN"]),
            )),
            "__builtin_nan" => Ok(WithStmts::new_val(
                mk().abs_path_expr(vec!["core", "f64", "NAN"]),
            )),
            "__builtin_nanl" => {
                self.use_crate(ExternCrate::F128);

                Ok(WithStmts::new_val(
                    mk().abs_path_expr(vec!["f128", "f128", "NAN"]),
                ))
            }
            "__builtin_signbit" | "__builtin_signbitf" | "__builtin_signbitl" => {
                // Long doubles require the Float trait from num_traits to call this method
                if builtin_name == "__builtin_signbitl" {
                    self.with_cur_file_item_store(|item_store| {
                        item_store.add_use(true, vec!["num_traits".into()], "Float");
                    });
                }

                let val = self.convert_expr(ctx.used(), args[0], None)?;

                Ok(val.map(|v| {
                    let val = mk().method_call_expr(v, "is_sign_negative", vec![]);

                    mk().cast_expr(val, mk().abs_path_ty(vec!["core", "ffi", "c_int"]))
                }))
            }
            "__builtin_ffs" | "__builtin_ffsl" | "__builtin_ffsll" => {
                let val = self.convert_expr(ctx.used(), args[0], None)?;

                Ok(val.map(|x| {
                    let add = BinOp::Add(Default::default());
                    let zero = mk().lit_expr(mk().int_lit(0, ""));
                    let one = mk().lit_expr(mk().int_lit(1, ""));
                    let cmp = BinOp::Eq(Default::default());
                    let zeros = mk().method_call_expr(x.clone(), "trailing_zeros", vec![]);
                    let zeros_cast = mk().cast_expr(zeros, mk().path_ty(vec!["i32"]));
                    let zeros_plus1 = mk().binary_expr(add, zeros_cast, one);
                    let block = mk().block(vec![mk().expr_stmt(zero.clone())]);
                    let cond = mk().binary_expr(cmp, x, zero);

                    mk().ifte_expr(cond, block, Some(zeros_plus1))
                }))
            }
            "__builtin_clz" | "__builtin_clzl" | "__builtin_clzll" => {
                let val = self.convert_expr(ctx.used(), args[0], None)?;
                Ok(val.map(|x| {
                    let zeros = mk().method_call_expr(x, "leading_zeros", vec![]);
                    mk().cast_expr(zeros, mk().path_ty(vec!["i32"]))
                }))
            }
            "__builtin_ctz" | "__builtin_ctzl" | "__builtin_ctzll" => {
                let val = self.convert_expr(ctx.used(), args[0], None)?;
                Ok(val.map(|x| {
                    let zeros = mk().method_call_expr(x, "trailing_zeros", vec![]);
                    mk().cast_expr(zeros, mk().path_ty(vec!["i32"]))
                }))
            }
            "__builtin_bswap16" | "__builtin_bswap32" | "__builtin_bswap64" => {
                let val = self.convert_expr(ctx.used(), args[0], None)?;
                Ok(val.map(|x| mk().method_call_expr(x, "swap_bytes", vec![])))
            }
            "__builtin_fabs" | "__builtin_fabsf" | "__builtin_fabsl" => {
                let val = self.convert_expr(ctx.used(), args[0], None)?;
                Ok(val.map(|x| mk().method_call_expr(x, "abs", vec![])))
            }
            "__builtin_isfinite" | "__builtin_isnan" => {
                let val = self.convert_expr(ctx.used(), args[0], None)?;

                let seg = match builtin_name {
                    "__builtin_isfinite" => "is_finite",
                    "__builtin_isnan" => "is_nan",
                    _ => panic!(),
                };
                Ok(val.map(|x| {
                    let call = mk().method_call_expr(x, seg, vec![]);
                    mk().cast_expr(call, mk().path_ty(vec!["i32"]))
                }))
            }
            "__builtin_isinf_sign" => {
                // isinf_sign(x) -> fabs(x) == infinity ? (signbit(x) ? -1 : 1) : 0
                let val = self.convert_expr(ctx.used(), args[0], None)?;
                Ok(val.map(|x| {
                    let inner_cond = mk().method_call_expr(x.clone(), "is_sign_positive", vec![]);
                    let one = mk().lit_expr(mk().int_lit(1, ""));
                    let minus_one = mk().unary_expr(
                        UnOp::Neg(Default::default()),
                        mk().lit_expr(mk().int_lit(1, "")),
                    );
                    let one_block = mk().block(vec![mk().expr_stmt(one)]);
                    let inner_ifte = mk().ifte_expr(inner_cond, one_block, Some(minus_one));
                    let zero = mk().lit_expr(mk().int_lit(0, ""));
                    let outer_cond = mk().method_call_expr(x, "is_infinite", vec![]);
                    let inner_ifte_block = mk().block(vec![mk().expr_stmt(inner_ifte)]);
                    mk().ifte_expr(outer_cond, inner_ifte_block, Some(zero))
                }))
            }
            "__builtin_flt_rounds" => {
                // LLVM simply lowers this to the constant one which means
                // that floats are rounded to the nearest number.
                // https://github.com/llvm-mirror/llvm/blob/master/lib/CodeGen/IntrinsicLowering.cpp#L470
                Ok(WithStmts::new_val(mk().lit_expr(mk().int_lit(1, "i32"))))
            }
            "__builtin_expect" => self.convert_expr(ctx.used(), args[0], None),

            "__builtin_popcount" | "__builtin_popcountl" | "__builtin_popcountll" => {
                let val = self.convert_expr(ctx.used(), args[0], None)?;
                Ok(val.map(|x| {
                    let zeros = mk().method_call_expr(x, "count_ones", vec![]);
                    mk().cast_expr(zeros, mk().path_ty(vec!["i32"]))
                }))
            }
            "__builtin_bzero" => {
                let ptr_stmts = self.convert_expr(ctx.used(), args[0], None)?;
                let n_stmts = self.convert_expr(ctx.used(), args[1], None)?;
                let write_bytes = mk().abs_path_expr(vec!["core", "ptr", "write_bytes"]);
                let zero = mk().lit_expr(mk().int_lit(0, "u8"));
                ptr_stmts.and_then(|ptr| {
                    Ok(n_stmts.map(|n| mk().call_expr(write_bytes, vec![ptr, zero, n])))
                })
            }

            // If the target does not support data prefetch, the address expression is evaluated if
            // it includes side effects but no other code is generated and GCC does not issue a warning.
            // void __builtin_prefetch (const void *addr, ...);
            "__builtin_prefetch" => self.convert_expr(ctx.unused(), args[0], None),

            "__builtin_memcpy" | "__builtin_memcmp" | "__builtin_memmove" | "__builtin_strncmp"
            | "__builtin_strncpy" | "__builtin_strncat" => self.convert_libc_fns(
                builtin_name,
                ctx,
                args,
                &[LibcFnArgType::Mem, LibcFnArgType::Mem, LibcFnArgType::Size],
            ),
            "__builtin_memchr" | "__builtin_memset" => self.convert_libc_fns(
                builtin_name,
                ctx,
                args,
                &[LibcFnArgType::Mem, LibcFnArgType::Int, LibcFnArgType::Size],
            ),
            "__builtin_strchr" | "__builtin_strrchr" => self.convert_libc_fns(
                builtin_name,
                ctx,
                args,
                &[LibcFnArgType::Mem, LibcFnArgType::Int],
            ),
            "__builtin_strndup" | "__builtin_strnlen" => self.convert_libc_fns(
                builtin_name,
                ctx,
                args,
                &[LibcFnArgType::Mem, LibcFnArgType::Size],
            ),
            "__builtin_strdup" | "__builtin_strlen" => {
                self.convert_libc_fns(builtin_name, ctx, args, &[LibcFnArgType::Mem])
            }
            "__builtin_strcmp" | "__builtin_strcat" | "__builtin_strcpy" | "__builtin_strcspn"
            | "__builtin_strpbrk" | "__builtin_strspn" | "__builtin_strstr" => self
                .convert_libc_fns(
                    builtin_name,
                    ctx,
                    args,
                    &[LibcFnArgType::Mem, LibcFnArgType::Mem],
                ),

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
            "__builtin_constant_p" => Ok(WithStmts::new_val(mk().lit_expr(mk().int_lit(0, "")))),

            "__builtin_object_size" => {
                // We can't convert this to Rust, but it should be safe to always return -1/0
                // (depending on the value of `type`), so we emit the following:
                // `(if (type & 2) == 0 { -1isize } else { 0isize }) as libc::size_t`
                let ptr_arg = self.convert_expr(ctx.unused(), args[0], None)?;
                let type_arg = self.convert_expr(ctx.used(), args[1], None)?;
                ptr_arg.and_then(|_| {
                    Ok(type_arg.map(|type_arg| {
                        let type_and_2 = mk().binary_expr(
                            BinOp::BitAnd(Default::default()),
                            type_arg,
                            mk().lit_expr(mk().int_lit(2, "")),
                        );
                        let if_cond = mk().binary_expr(
                            BinOp::Eq(Default::default()),
                            type_and_2,
                            mk().lit_expr(mk().int_lit(0, "")),
                        );
                        let minus_one = mk().unary_expr(
                            UnOp::Neg(Default::default()),
                            mk().lit_expr(mk().int_lit(1, "isize")),
                        );
                        let if_expr = mk().ifte_expr(
                            if_cond,
                            mk().block(vec![mk().expr_stmt(minus_one)]),
                            Some(mk().lit_expr(mk().int_lit(0, "isize"))),
                        );
                        self.use_crate(ExternCrate::Libc);
                        let size_t = mk().abs_path_ty(vec!["libc", "size_t"]);
                        mk().cast_expr(if_expr, size_t)
                    }))
                })
            }

            "__builtin_va_start" => {
                if ctx.is_unused() && args.len() == 2 {
                    if let Some(va_id) = self.match_vastart(args[0]) {
                        if self.ast_context.get_decl(&va_id).is_some() {
                            let dst =
                                self.convert_expr(ctx.expect_valistimpl().used(), args[0], None)?;
                            let fn_ctx = self.function_context.borrow();
                            let src = fn_ctx.get_va_list_arg_name();

                            let call_expr =
                                mk().method_call_expr(mk().ident_expr(src), "clone", vec![]);
                            let assign_expr = mk().assign_expr(dst.to_expr(), call_expr);
                            let stmt = mk().semi_stmt(assign_expr);

                            return Ok(WithStmts::new(
                                vec![stmt],
                                self.panic_or_err("va_start stub"),
                            ));
                        }
                    }
                }
                Err(TranslationError::generic("Unsupported va_start"))
            }
            "__builtin_va_copy" => {
                if ctx.is_unused() && args.len() == 2 {
                    if let Some((_dst_va_id, _src_va_id)) = self.match_vacopy(args[0], args[1]) {
                        let dst =
                            self.convert_expr(ctx.expect_valistimpl().used(), args[0], None)?;
                        let src =
                            self.convert_expr(ctx.expect_valistimpl().used(), args[1], None)?;

                        let call_expr = mk().method_call_expr(src.to_expr(), "clone", vec![]);
                        let assign_expr = mk().assign_expr(dst.to_expr(), call_expr);
                        let stmt = mk().semi_stmt(assign_expr);

                        return Ok(WithStmts::new(
                            vec![stmt],
                            self.panic_or_err("va_copy stub"),
                        ));
                    }
                }
                Err(TranslationError::generic("Unsupported va_copy"))
            }
            "__builtin_va_end" => {
                if ctx.is_unused() && args.len() == 1 {
                    if let Some(_va_id) = self.match_vaend(args[0]) {
                        // nothing to do since `VaListImpl`s get `Drop`'ed.
                        return Ok(WithStmts::new_val(self.panic("va_end stub")));
                    }
                }
                Err(TranslationError::generic("Unsupported va_end"))
            }

            "__builtin_alloca" => {
                let count = self.convert_expr(ctx.used(), args[0], None)?;
                count.and_then(|count| {
                    // Get `alloca` allocation storage.
                    let mut fn_ctx = self.function_context.borrow_mut();
                    let alloca_allocations_name =
                        &*fn_ctx.alloca_allocations_name.get_or_insert_with(|| {
                            self.renamer.borrow_mut().pick_name("alloca_allocations")
                        });

                    // alloca_allocations.push(std::vec::from_elem(0, count));
                    let init_expr = vec_expr(
                        mk().lit_expr(mk().int_unsuffixed_lit(0)),
                        cast_int(count, "usize", false),
                    );
                    let push_stmt = mk().semi_stmt(mk().method_call_expr(
                        mk().ident_expr(alloca_allocations_name),
                        "push",
                        vec![init_expr],
                    ));

                    // alloca_allocations.last_mut().unwrap().as_mut_ptr()
                    let last_mut_expr = mk().method_call_expr(
                        mk().ident_expr(alloca_allocations_name),
                        "last_mut",
                        vec![],
                    );
                    let unwrap_expr = mk().method_call_expr(last_mut_expr, "unwrap", vec![]);
                    let as_mut_ptr_expr = mk().method_call_expr(unwrap_expr, "as_mut_ptr", vec![]);

                    Ok(WithStmts::new(vec![push_stmt], as_mut_ptr_expr))
                })
            }

            "__builtin_ia32_pause" => {
                let fn_name = "_mm_pause";
                self.import_simd_function(fn_name)?;
                let ident = mk().ident_expr(fn_name);
                let call = mk().call_expr(ident, vec![]);
                self.convert_side_effects_expr(
                    ctx,
                    WithStmts::new_val(call),
                    "Builtin is not supposed to be used",
                )
            }

            "__builtin_arm_yield" => {
                let fn_name = "__yield";
                self.use_feature("stdsimd");
                // TODO See #1298.
                // In Rust 1.7, `#![feature(stdsimd)]` was removed and split into (at least):
                // `#![feature("stdarch_arm_hints")]` and
                // `#![cfg_attr(target_arch = "arm", feature(stdarch_arm_neon_intrinsics))]`.
                // self.use_feature("stdarch_arm_hints");
                // self.use_feature("stdarch_arm_neon_intrinsics"); // TODO need to add `cfg_attr` support.
                self.import_arch_function("arm", fn_name);
                self.import_arch_function("aarch64", fn_name);
                let ident = mk().ident_expr(fn_name);
                let call = mk().call_expr(ident, vec![]);
                self.convert_side_effects_expr(
                    ctx,
                    WithStmts::new_val(call),
                    "Builtin is not supposed to be used",
                )
            }

            // SIMD builtins:
            "__builtin_ia32_aeskeygenassist128" => {
                self.convert_simd_builtin(ctx, "_mm_aeskeygenassist_si128", args)
            }
            "__builtin_ia32_aesimc128" => self.convert_simd_builtin(ctx, "_mm_aesimc_si128", args),
            "__builtin_ia32_aesenc128" => self.convert_simd_builtin(ctx, "_mm_aesenc_si128", args),
            "__builtin_ia32_aesenclast128" => {
                self.convert_simd_builtin(ctx, "_mm_aesenclast_si128", args)
            }
            "__builtin_ia32_aesdec128" => self.convert_simd_builtin(ctx, "_mm_aesdec_si128", args),
            "__builtin_ia32_aesdeclast128" => {
                self.convert_simd_builtin(ctx, "_mm_aesdeclast_si128", args)
            }
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
            "__builtin_ia32_palignr128" => self.convert_simd_builtin(ctx, "_mm_alignr_epi8", args),
            "__builtin_ia32_palignr256" => {
                self.convert_simd_builtin(ctx, "_mm256_alignr_epi8", args)
            }
            "__builtin_ia32_permti256" => {
                self.convert_simd_builtin(ctx, "_mm256_permute2x128_si256", args)
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
            "__builtin_ia32_vperm2f128_pd256" => {
                self.convert_simd_builtin(ctx, "_mm256_permute2f128_pd", args)
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
            "__builtin_ia32_vec_ext_v8si" => {
                self.convert_simd_builtin(ctx, "_mm256_extract_epi32", args)
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
                let arg0 = self.convert_expr(ctx.used(), args[0], None)?;
                let arg1 = self.convert_expr(ctx.used(), args[1], None)?;
                let arg2 = self.convert_expr(ctx.used(), args[2], None)?;
                arg0.and_then(|arg0| {
                    arg1.and_then(|arg1| {
                        arg2.and_then(|arg2| {
                            let returns_val = builtin_name.starts_with("__sync_val");
                            self.convert_atomic_cxchg(
                                ctx,
                                "atomic_cxchg_seqcst_seqcst",
                                arg0,
                                arg1,
                                arg2,
                                returns_val,
                            )
                        })
                    })
                })
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
                let func_name = if builtin_name.contains("_add_") {
                    "atomic_xadd_seqcst"
                } else if builtin_name.contains("_sub_") {
                    "atomic_xsub_seqcst"
                } else if builtin_name.contains("_or_") {
                    "atomic_or_seqcst"
                } else if builtin_name.contains("_xor_") {
                    "atomic_xor_seqcst"
                } else if builtin_name.contains("_nand_") {
                    "atomic_nand_seqcst"
                } else {
                    // We can't explicitly check for "_and_" since they all contain it
                    "atomic_and_seqcst"
                };

                let arg0 = self.convert_expr(ctx.used(), args[0], None)?;
                let arg1 = self.convert_expr(ctx.used(), args[1], None)?;
                let fetch_first = builtin_name.starts_with("__sync_fetch");
                arg0.and_then(|arg0| {
                    arg1.and_then(|arg1| {
                        self.convert_atomic_op(ctx, func_name, arg0, arg1, fetch_first)
                    })
                })
            }

            "__sync_synchronize" => {
                self.use_feature("core_intrinsics");

                let atomic_func =
                    mk().abs_path_expr(vec!["core", "intrinsics", "atomic_fence_seqcst"]);
                let call_expr = mk().call_expr(atomic_func, vec![]);
                self.convert_side_effects_expr(
                    ctx,
                    WithStmts::new_val(call_expr),
                    "Builtin is not supposed to be used",
                )
            }

            "__sync_lock_test_and_set_1"
            | "__sync_lock_test_and_set_2"
            | "__sync_lock_test_and_set_4"
            | "__sync_lock_test_and_set_8"
            | "__sync_lock_test_and_set_16" => {
                self.use_feature("core_intrinsics");

                // Emit `atomic_xchg_acquire(arg0, arg1)`
                let atomic_func =
                    mk().abs_path_expr(vec!["core", "intrinsics", "atomic_xchg_acquire"]);
                let arg0 = self.convert_expr(ctx.used(), args[0], None)?;
                let arg1 = self.convert_expr(ctx.used(), args[1], None)?;
                arg0.and_then(|arg0| {
                    arg1.and_then(|arg1| {
                        let call_expr = mk().call_expr(atomic_func, vec![arg0, arg1]);
                        self.convert_side_effects_expr(
                            ctx,
                            WithStmts::new_val(call_expr),
                            "Builtin is not supposed to be used",
                        )
                    })
                })
            }

            "__sync_lock_release_1"
            | "__sync_lock_release_2"
            | "__sync_lock_release_4"
            | "__sync_lock_release_8"
            | "__sync_lock_release_16" => {
                self.use_feature("core_intrinsics");

                // Emit `atomic_store_release(arg0, 0)`
                let atomic_func =
                    mk().abs_path_expr(vec!["core", "intrinsics", "atomic_store_release"]);
                let arg0 = self.convert_expr(ctx.used(), args[0], None)?;
                arg0.and_then(|arg0| {
                    let zero = mk().lit_expr(mk().int_lit(0, ""));
                    let call_expr = mk().call_expr(atomic_func, vec![arg0, zero]);
                    self.convert_side_effects_expr(
                        ctx,
                        WithStmts::new_val(call_expr),
                        "Builtin is not supposed to be used",
                    )
                })
            }

            // TODO: provide proper implementation, this is just to get stuff building
            "__builtin_assume" =>  Ok(self.convert_expr(ctx.used(), args[0])?),
            // There's currently no way to replicate this functionality in Rust, so we just
            // pass the ptr input param in its place.
            "__builtin_assume_aligned" => Ok(self.convert_expr(ctx.used(), args[0], None)?),
            // Skip over, there's no way to implement it in Rust
            "__builtin_unwind_init" => Ok(WithStmts::new_val(self.panic_or_err("no value"))),
            "__builtin_unreachable" => Ok(WithStmts::new(
                vec![mk().semi_stmt(mk().mac_expr(mk().mac::<Vec<TokenTree>>(
                    mk().path(vec!["unreachable"]),
                    vec![],
                    MacroDelimiter::Paren(Default::default()),
                )))],
                self.panic_or_err("unreachable stub"),
            )),

            "__builtin_rotateleft8"
            | "__builtin_rotateleft16"
            | "__builtin_rotateleft32"
            | "__builtin_rotateleft64" => self.convert_builtin_rotate(ctx, args, "rotate_left"),

            "__builtin_rotateright8"
            | "__builtin_rotateright16"
            | "__builtin_rotateright32"
            | "__builtin_rotateright64" => self.convert_builtin_rotate(ctx, args, "rotate_right"),

            _ => Err(format_translation_err!(
                self.ast_context.display_loc(src_loc),
                "Unimplemented builtin {}",
                builtin_name
            )),
        }
    }

    // This translation logic handles converting code that uses
    // https://gcc.gnu.org/onlinedocs/gcc/Integer-Overflow-Builtins.html
    fn convert_overflow_arith(
        &self,
        ctx: ExprContext,
        method_name: &str,
        args: &[CExprId],
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let args = self.convert_exprs(ctx.used(), args, None)?;
        args.and_then(|args| {
            let [a, b, c]: [_; 3] = args
                .try_into()
                .map_err(|_| "`convert_overflow_arith` must have exactly 3 arguments")?;
            let overflowing = mk().method_call_expr(a, method_name, vec![b]);
            let sum_name = self.renamer.borrow_mut().fresh();
            let over_name = self.renamer.borrow_mut().fresh();
            let overflow_let = mk().local_stmt(Box::new(mk().local(
                mk().tuple_pat(vec![
                    mk().ident_pat(&sum_name),
                    mk().ident_pat(over_name.clone()),
                ]),
                None,
                Some(overflowing),
            )));

            let out_assign = mk().assign_expr(
                mk().unary_expr(UnOp::Deref(Default::default()), c),
                mk().ident_expr(&sum_name),
            );

            Ok(WithStmts::new(
                vec![overflow_let, mk().expr_stmt(out_assign)],
                mk().ident_expr(over_name),
            ))
        })
    }

    /// Converts a `__builtin_{mem|str}*` use by calling the equivalent libc fn.
    fn convert_libc_fns(
        &self,
        builtin_name: &str,
        ctx: ExprContext,
        args: &[CExprId],
        arg_types: &[LibcFnArgType],
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let name = &builtin_name[10..];
        self.use_crate(ExternCrate::Libc);
        let mem = mk().abs_path_expr(vec!["libc", name]);
        let args = self.convert_exprs(ctx.used(), args, None)?;
        args.and_then(|args| {
            if args.len() != arg_types.len() {
                // This should not generally happen, as the C frontend checks these first
                Err(err_msg(format!(
                    "wrong number of arguments for {}: expected {}, found {}",
                    builtin_name,
                    arg_types.len(),
                    args.len()
                ))
                .context(TranslationErrorKind::Generic))?
            }
            let args_casted = args
                .into_iter()
                .zip(arg_types)
                .map(|(arg, &ty)| {
                    if ty == LibcFnArgType::Size {
                        self.use_crate(ExternCrate::Libc);
                        mk().cast_expr(arg, mk().abs_path_ty(vec!["libc", "size_t"]))
                    } else {
                        arg
                    }
                })
                .collect::<Vec<_>>();
            let mem_expr = mk().call_expr(mem, args_casted);

            if ctx.is_used() {
                Ok(WithStmts::new_val(mem_expr))
            } else {
                Ok(WithStmts::new(
                    vec![mk().semi_stmt(mem_expr)],
                    self.panic_or_err(&format!("__builtin_{} not used", name)),
                ))
            }
        })
    }
}
