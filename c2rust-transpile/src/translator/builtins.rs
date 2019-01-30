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
            "__builtin_prefetch" => {
                self.convert_expr(ctx.unused(), args[0])
            }

            "__builtin_memcpy" => self.convert_memcpy(ctx, args),

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

            "__builtin_va_start" => {
                if ctx.is_unused() && args.len() == 2 {
                    if let Some(va_id) = self.match_vastart(args[0]) {
                        if ctx.is_va_decl(va_id) {
                            return Ok(WithStmts::new(self.panic("va_start stub")))
                        }
                    }
                }
                Err(format!("Unsupported va_start"))
            }
            ,
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
