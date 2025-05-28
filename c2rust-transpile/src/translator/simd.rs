#![deny(missing_docs)]
//! This module provides translation for SIMD operations and expressions.

use super::*;

use crate::c_ast::BinOp::{Add, BitAnd, ShiftRight};
use crate::c_ast::CExprKind::{Binary, Call, Conditional, ExplicitCast, ImplicitCast, Literal};
use crate::c_ast::CLiteral::Integer;
use crate::c_ast::CTypeKind::{Char, Double, Float, Int, LongLong, Short};
use crate::c_ast::CastKind::{BitCast, IntegralCast};

/// As of rustc 1.29, rust is known to be missing some SIMD functions.
/// See <https://github.com/rust-lang-nursery/stdsimd/issues/579>
static MISSING_SIMD_FUNCTIONS: &[&str] = &[
    "_mm_and_si64",
    "_mm_andnot_si64",
    "_mm_cmpeq_pi16",
    "_mm_cmpeq_pi32",
    "_mm_cmpeq_pi8",
    "_mm_cvtm64_si64",
    "_mm_cvtsi32_si64",
    "_mm_cvtsi64_m64",
    "_mm_cvtsi64_si32",
    "_mm_empty",
    "_mm_free",
    "_mm_madd_pi16",
    "_mm_malloc",
    "_mm_mulhi_pi16",
    "_mm_mulhrs_pi16",
    "_mm_or_si64",
    "_mm_packs_pu16",
    "_mm_sll_pi16",
    "_mm_sll_pi32",
    "_mm_sll_si64",
    "_mm_slli_pi16",
    "_mm_slli_pi32",
    "_mm_slli_si64",
    "_mm_sra_pi16",
    "_mm_sra_pi32",
    "_mm_srai_pi16",
    "_mm_srai_pi32",
    "_mm_srl_pi16",
    "_mm_srl_pi32",
    "_mm_srl_si64",
    "_mm_srli_pi16",
    "_mm_srli_pi32",
    "_mm_srli_si64",
    "_mm_xor_si64",
];

static SIMD_X86_64_ONLY: &[&str] = &[
    "_mm_crc32_u64",
    "_mm_cvti64_sd",
    "_mm_cvti64_ss",
    "_mm_cvt_roundi64_sd",
    "_mm_cvt_roundi64_ss",
    "_mm_cvt_roundsd_i64",
    "_mm_cvt_roundsd_si64",
    "_mm_cvt_roundsd_u64",
    "_mm_cvt_roundsi64_sd",
    "_mm_cvt_roundsi64_ss",
    "_mm_cvt_roundss_i64",
    "_mm_cvt_roundss_si64",
    "_mm_cvt_roundss_u64",
    "_mm_cvt_roundu64_sd",
    "_mm_cvt_roundu64_ss",
    "_mm_cvtsd_i64",
    "_mm_cvtsd_si64",
    "_mm_cvtsd_si64x",
    "_mm_cvtsd_u64",
    "_mm_cvtsi128_si64",
    "_mm_cvtsi128_si64x",
    "_mm_cvtsi64_sd",
    "_mm_cvtsi64_si128",
    "_mm_cvtsi64_ss",
    "_mm_cvtsi64x_sd",
    "_mm_cvtsi64x_si128",
    "_mm_cvtss_i64",
    "_mm_cvtss_si64",
    "_mm_cvtss_u64",
    "_mm_cvtt_roundsd_i64",
    "_mm_cvtt_roundsd_si64",
    "_mm_cvtt_roundsd_u64",
    "_mm_cvtt_roundss_i64",
    "_mm_cvtt_roundss_si64",
    "_mm_cvtt_roundss_u64",
    "_mm_cvttsd_i64",
    "_mm_cvttsd_si64",
    "_mm_cvttsd_si64x",
    "_mm_cvttsd_u64",
    "_mm_cvttss_i64",
    "_mm_cvttss_si64",
    "_mm_cvttss_u64",
    "_mm_cvtu64_sd",
    "_mm_cvtu64_ss",
    "_mm_extract_epi64",
    "_mm_insert_epi64",
    "_mm_stream_si64",
    "_mm_tzcnt_64",
];

fn add_arch_use(store: &mut ItemStore, arch_name: &str, item_name: &str) {
    store.add_use_with_attr(
        vec!["core".into(), "arch".into(), arch_name.into()],
        item_name,
        mk().meta_item_attr(
            AttrStyle::Outer,
            mk().meta_list(
                "cfg",
                vec![NestedMeta::Meta(
                    mk().meta_namevalue("target_arch", arch_name),
                )],
            ),
        )
        .pub_(),
    );
}

fn add_arch_use_rename(store: &mut ItemStore, arch_name: &str, item_name: &str, rename: &str) {
    store.add_use_with_attr_rename(
        vec!["core".into(), "arch".into(), arch_name.into()],
        item_name,
        mk().meta_item_attr(
            AttrStyle::Outer,
            mk().meta_list(
                "cfg",
                vec![NestedMeta::Meta(
                    mk().meta_namevalue("target_arch", arch_name),
                )],
            ),
        )
        .pub_(),
        rename,
    );
}

impl<'c> Translation<'c> {
    /// Given the name of a typedef check if its one of the SIMD types.
    /// This function returns `true` when the name of the type is one that
    /// it knows how to implement and no further translation should be done.
    pub fn import_simd_typedef(&self, name: &str) -> TranslationResult<bool> {
        Ok(match name {
            // Public API SIMD typedefs:
            "__m128i" | "__m128" | "__m128d" | "__m64" | "__m256" | "__m256d" | "__m256i" => {
                // __m64 and MMX support were removed from upstream Rust.
                // See https://github.com/immunant/c2rust/issues/369
                if name == "__m64" {
                    return Err(format_err!(
                        "__m64 and MMX are no longer supported, due to removed upstream support. See https://github.com/immunant/c2rust/issues/369"
                    ).into());
                }

                self.with_cur_file_item_store(|item_store| {
                    add_arch_use(item_store, "x86", name);
                    add_arch_use(item_store, "x86_64", name);
                });

                true
            }
            "__m128_u" | "__m128i_u" | "__m128d_u" | "__m256_u" | "__m256i_u" | "__m256d_u" => {
                // Rust doesn't have unaligned SIMD types, but it's not incorrect to use an unaligned
                // type instead, it's just slightly less efficient. We'll just use the aligned type
                // and rename it to the unaligned type.
                self.with_cur_file_item_store(|item_store| {
                    add_arch_use_rename(item_store, "x86", &name.replace("_u", ""), name);
                    add_arch_use_rename(item_store, "x86_64", &name.replace("_u", ""), name);
                });

                self.with_cur_file_item_store(|item_store| {
                    add_arch_use(item_store, "x86", &name.replace("_u", ""));
                    add_arch_use(item_store, "x86_64", &name.replace("_u", ""));
                });

                true
            }
            // These seem to be C internal types only, and shouldn't need any explicit support.
            // See https://internals.rust-lang.org/t/getting-explicit-simd-on-stable-rust/4380/115
            "__v1di"
            | "__v2si"
            | "__v4hi"
            | "__v8qi"
            | "__v4si"
            | "__v4sf"
            | "__v4su"
            | "__v2df"
            | "__v2di"
            | "__v8hi"
            | "__v16qi"
            | "__v2du"
            | "__v8hu"
            | "__v16qu"
            | "__v32qu"
            | "__v4df"
            | "__v8sf"
            | "__v4di"
            | "__v8si"
            | "__v16hi"
            | "__v32qi"
            | "__v4du"
            | "__v8di_aligned"
            | "__v8df_aligned"
            | "__v16sf_aligned"
            | "__v8sf_aligned"
            | "__v4df_aligned"
            | "__v4di_aligned"
            | "__v16qs"
            | "__v32qs"
            | "__v8su"
            | "__v16hu"
            | "__mm_loadh_pi_v2f32"
            | "__mm_loadl_pi_v2f32" => self.generate_simd_type(name)?,
            _ => false,
        })
    }

    /// Given the name of a SIMD typedef that is valid but not a built in core Rust type, attempt
    /// to generate a Rust type for it.
    /// https://internals.rust-lang.org/t/getting-explicit-simd-on-stable-rust/4380?page=6
    pub fn generate_simd_type(&self, name: &str) -> TranslationResult<bool> {
        let prefix = name
            .chars()
            .take_while(|c| !c.is_numeric())
            .collect::<String>();
        let width = name
            .split_at(prefix.len())
            .1
            .chars()
            .take_while(|c| c.is_numeric())
            .collect::<String>()
            .parse::<usize>()
            .unwrap();
        let elem_ty = name.split_at(prefix.len() + width.to_string().len()).1;
        // Prefixes: q (8), h (16), s (32), d (64)
        // Signedness: i (signed), u (unsigned), f (float)
        let elem_width = match elem_ty {
            "qi" | "qu" => 8,
            "hi" | "hu" => 16,
            "si" | "su" | "sf" => 32,
            "di" | "du" | "df" => 64,
            _ => return Err(format_err!("Unknown SIMD type: {}", name).into()),
        };

        // Suffix is either 'd' (for 64-bit fp), 'i' (for integral types) or '' (for 32-bit fp)
        let suffix = match elem_ty {
            "df" => "d",
            "sf" => "",
            _ => "i",
        };

        let conversion_ty_name = format!("__m{}{}", width * elem_width, suffix,);

        self.with_cur_file_item_store(|item_store| {
            add_arch_use_rename(item_store, "x86", &conversion_ty_name, name);
            add_arch_use_rename(item_store, "x86_64", &conversion_ty_name, name);
            add_arch_use(item_store, "x86", &conversion_ty_name);
            add_arch_use(item_store, "x86_64", &conversion_ty_name);
        });

        Ok(true)
    }

    /// Determine if a particular function name is an SIMD primitive. If so an appropriate
    /// use statement is generated, `true` is returned, and no further processing will need to be done.
    pub fn import_simd_function(&self, name: &str) -> TranslationResult<bool> {
        if name.starts_with("_mm") {
            // REVIEW: This will do a linear lookup against all SIMD fns. Could use a lazy static hashset
            if MISSING_SIMD_FUNCTIONS.contains(&name) {
                return Err(format_err!(
                    "SIMD function {} doesn't currently have a rust counterpart",
                    name
                )
                .into());
            }

            // The majority of x86/64 SIMD is stable, however there are still some
            // bits that are behind a feature gate.
            self.use_feature("stdsimd");

            self.with_cur_file_item_store(|item_store| {
                // REVIEW: Also a linear lookup
                if !SIMD_X86_64_ONLY.contains(&name) {
                    add_arch_use(item_store, "x86", name);
                }

                add_arch_use(item_store, "x86_64", name);
            });

            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// This function will strip either an implicitly casted int or explicitly casted
    /// vector as both casts are unnecessary (and problematic) for our purposes
    fn clean_int_or_vector_param(&self, expr_id: CExprId) -> CExprId {
        match self.ast_context[expr_id].kind {
            // For some reason there seems to be an incorrect implicit cast here to char
            // it's possible the builtin takes a char even though the function takes an int
            ImplicitCast(_, _, IntegralCast, _, _) => expr_id,
            // (internal)(external)(vector input)
            ExplicitCast(qty, _, BitCast, _, _) => {
                if let CTypeKind::Vector(..) = self.ast_context.resolve_type(qty.ctype).kind {
                    let (_, stripped_expr_id, _) = self.strip_vector_explicit_cast(expr_id);

                    stripped_expr_id
                } else {
                    expr_id
                }
            }
            _ => expr_id,
        }
    }

    /// Generate a call to a rust SIMD function based on a builtin function. Clang 6 only supports one of these
    /// but clang 7 converts a bunch more from "super builtins"
    pub fn convert_simd_builtin(
        &self,
        ctx: ExprContext,
        fn_name: &str,
        args: &[CExprId],
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        self.import_simd_function(fn_name)?;

        let mut processed_args = vec![];
        let (_, first_expr_id, _) = self.strip_vector_explicit_cast(args[0]);
        processed_args.push(first_expr_id);
        processed_args.extend(
            args[1..]
                .iter()
                .map(|arg| self.clean_int_or_vector_param(*arg)),
        );

        let param_translation = self.convert_exprs(ctx.used(), &processed_args)?;
        param_translation.and_then(|call_params| {
            let call = mk().call_expr(mk().ident_expr(fn_name), call_params);

            if ctx.is_used() {
                // Get the ty of the return value of the call
                Ok(WithStmts::new_val(call))
            } else {
                Ok(WithStmts::new(
                    vec![mk().semi_stmt(call)],
                    self.panic_or_err("No value for unused shuffle vector return"),
                ))
            }
        })
    }

    /// Generate a zero value to be used for initialization of a given vector type. The type
    /// is specified with the underlying element type and the number of elements in the vector.
    pub fn implicit_vector_default(
        &self,
        ctype: CTypeId,
        len: usize,
        is_static: bool,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        // NOTE: This is only for x86/_64, and so support for other architectures
        // might need some sort of disambiguation to be exported
        let (fn_name, bytes) = match (&self.ast_context[ctype].kind, len) {
            (Float, 4) => ("_mm_setzero_ps", 16),
            (Float, 8) => ("_mm256_setzero_ps", 32),
            (Double, 2) => ("_mm_setzero_pd", 16),
            (Double, 4) => ("_mm256_setzero_pd", 32),
            (Char, 16) | (Short, 8) | (Int, 4) | (LongLong, 2) => ("_mm_setzero_si128", 16),
            (Char, 32) | (Short, 16) | (Int, 8) | (LongLong, 4) => ("_mm256_setzero_si256", 32),
            (Char, 8) | (Short, 4) | (Int, 2) | (LongLong, 1) => {
                // __m64 is still unstable as of rust 1.29
                self.use_feature("stdsimd");

                ("_mm_setzero_si64", 8)
            }
            (kind, len) => {
                return Err(format_err!(
                    "Unsupported vector default initializer: {:?} x {}",
                    kind,
                    len
                )
                .into())
            }
        };

        if is_static {
            let zero_expr = mk().lit_expr(mk().int_lit(0, "u8"));
            let n_bytes_expr = mk().lit_expr(mk().int_lit(bytes, ""));
            let expr = mk().repeat_expr(zero_expr, n_bytes_expr);

            Ok(WithStmts::new_unsafe_val(transmute_expr(
                mk().infer_ty(),
                mk().infer_ty(),
                expr,
            )))
        } else {
            self.import_simd_function(fn_name)
                .expect("None of these fns should be unsupported in rust");

            Ok(WithStmts::new_val(
                mk().call_expr(mk().ident_expr(fn_name), Vec::new()),
            ))
        }
    }

    /// Translate a list initializer corresponding to a vector type.
    pub fn vector_list_initializer(
        &self,
        ctx: ExprContext,
        ids: &[CExprId],
        ctype: CTypeId,
        len: usize,
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        let param_translation = self.convert_exprs(ctx, ids)?;
        param_translation.and_then(|mut params| {
            // When used in a static, we cannot call the standard functions since they
            // are not const and so we are forced to transmute
            let call = if ctx.is_static {
                let tuple = mk().tuple_expr(params);

                transmute_expr(mk().infer_ty(), mk().infer_ty(), tuple)
            } else {
                let fn_call_name = match (&self.ast_context[ctype].kind, len) {
                    (Float, 4) => "_mm_setr_ps",
                    (Float, 8) => "_mm256_setr_ps",
                    (Double, 2) => "_mm_setr_pd",
                    (Double, 4) => "_mm256_setr_pd",
                    (LongLong, 2) => "_mm_set_epi64x",
                    (LongLong, 4) => "_mm256_setr_epi64x",
                    (Char, 8) => "_mm_setr_pi8",
                    (Char, 16) => "_mm_setr_epi8",
                    (Char, 32) => "_mm256_setr_epi8",
                    (Int, 2) => "_mm_setr_pi32",
                    (Int, 4) => "_mm_setr_epi32",
                    (Int, 8) => "_mm256_setr_epi32",
                    (Short, 4) => "_mm_setr_pi16",
                    (Short, 8) => "_mm_setr_epi16",
                    (Short, 16) => "_mm256_setr_epi16",
                    e => return Err(format_err!("Unknown vector init list: {:?}", e).into()),
                };

                self.import_simd_function(fn_call_name)?;

                // rust is missing support for _mm_setr_epi64x, so we have to use
                // the reverse arguments for _mm_set_epi64x
                if fn_call_name == "_mm_set_epi64x" {
                    params.reverse();
                }

                mk().call_expr(mk().ident_expr(fn_call_name), params)
            };

            if ctx.is_used() {
                Ok(WithStmts::new_val(call))
            } else {
                Ok(WithStmts::new(
                    vec![mk().expr_stmt(call)],
                    self.panic_or_err("No value for unused shuffle vector return"),
                ))
            }
        })
    }

    /// Convert a shuffle operation into the equivalent Rust SIMD library calls.
    ///
    /// Because clang implements some shuffle operations as macros around intrinsic
    /// shuffle functions, this translation works to find the high-level shuffle
    /// call corresponding to the low-level one found in the C AST.
    pub fn convert_shuffle_vector(
        &self,
        ctx: ExprContext,
        child_expr_ids: &[CExprId],
    ) -> TranslationResult<WithStmts<Box<Expr>>> {
        // There are three shuffle vector functions which are actually functions, not superbuiltins/macros,
        // which do not need to be handled here: _mm_shuffle_pi8, _mm_shuffle_epi8, _mm256_shuffle_epi8

        let input_params = [4, 6, 10, 18];
        if !input_params.contains(&child_expr_ids.len()) {
            return Err(format_err!(
                "Unsupported shuffle vector without input params: found {}, expected one of {:?}",
                child_expr_ids.len(),
                input_params,
            )
            .into());
        };

        // There is some internal explicit casting which is okay for us to strip off
        let (first_vec, first_expr_id, first_vec_len) =
            self.strip_vector_explicit_cast(child_expr_ids[0]);
        let (second_vec, second_expr_id, second_vec_len) =
            self.strip_vector_explicit_cast(child_expr_ids[1]);

        if first_vec != second_vec {
            return Err("Unsupported shuffle vector with different vector kinds".into());
        }
        if first_vec_len != second_vec_len {
            return Err("Unsupported shuffle vector with different vector lengths".into());
        }

        let mask_expr_id = self.get_shuffle_vector_mask(&child_expr_ids[2..])?;
        let param_translation =
            self.convert_exprs(ctx.used(), &[first_expr_id, second_expr_id, mask_expr_id])?;
        param_translation.and_then(|params| {
            let [first, second, third]: [_; 3] = params
                .try_into()
                .map_err(|_| "`convert_shuffle_vector` must have exactly 3 parameters")?;
            let mut new_params = vec![first];

            // Some don't take a second param, but the expr is still there for some reason
            match (child_expr_ids.len(), &first_vec, first_vec_len) {
                // _mm256_shuffle_epi32
                (10, Int, 8) |
                // _mm_shuffle_epi32
                (6, Int, 4) |
                // _mm_shufflehi_epi16, _mm_shufflelo_epi16
                (10, Short, 8) |
                // _mm256_shufflehi_epi16, _mm256_shufflelo_epi16
                (18, Short, 16) => {},
                // _mm_slli_si128
                (18, Char, 16) => {
                    new_params.pop();
                    new_params.push(second);
                },
                _ => new_params.push(second),
            }

            let shuffle_fn_name = match (first_vec, first_vec_len) {
                (Float, 4) => "_mm_shuffle_ps",
                (Float, 8) => "_mm256_shuffle_ps",
                (Double, 2) => "_mm_shuffle_pd",
                (Double, 4) => "_mm256_shuffle_pd",
                (Int, 4) => "_mm_shuffle_epi32",
                (Int, 8) => "_mm256_shuffle_epi32",
                (Char, 16) => "_mm_slli_si128",
                (Short, 8) => {
                    // _mm_shufflehi_epi16 mask params start with const int,
                    // _mm_shufflelo_epi16 does not
                    let expr_id = &child_expr_ids[2];
                    if let Literal(_, Integer(0, IntBase::Dec)) = self.ast_context[*expr_id].kind {
                        "_mm_shufflehi_epi16"
                    } else {
                        "_mm_shufflelo_epi16"
                    }
                }
                (Short, 16) => {
                    // _mm256_shufflehi_epi16 mask params start with const int,
                    // _mm256_shufflelo_epi16 does not
                    let expr_id = &child_expr_ids[2];
                    if let Literal(_, Integer(0, IntBase::Dec)) = self.ast_context[*expr_id].kind {
                        "_mm256_shufflehi_epi16"
                    } else {
                        "_mm256_shufflelo_epi16"
                    }
                }
                e => return Err(format_err!("Unknown shuffle vector signature: {:?}", e).into()),
            };

            new_params.push(third);

            self.import_simd_function(shuffle_fn_name)?;

            let call = mk().call_expr(mk().ident_expr(shuffle_fn_name), new_params);

            if ctx.is_used() {
                Ok(WithStmts::new_val(call))
            } else {
                Ok(WithStmts::new(
                    vec![mk().expr_stmt(call)],
                    self.panic_or_err("No value for unused shuffle vector return"),
                ))
            }
        })
    }

    /// Vectors tend to have casts to and from internal types. This is problematic for shuffle vectors
    /// in particular which are usually macros ontop of a builtin call. Although one of these casts
    /// is likely redundant (external type), the other is not (internal type). We remove both of the
    /// casts for simplicity and readability
    fn strip_vector_explicit_cast(&self, expr_id: CExprId) -> (&CTypeKind, CExprId, usize) {
        match self.ast_context[expr_id].kind {
            ExplicitCast(CQualTypeId { ctype, .. }, expr_id, _, _, _) => {
                let expr_id = match &self.ast_context[expr_id].kind {
                    ExplicitCast(_, expr_id, _, _, _) => *expr_id,
                    // The expr_id wont be used in this case (the function only has one
                    // vector param, not two, despite the following type match), so it's
                    // okay to provide a dummy here
                    Call(..) => expr_id,
                    ImplicitCast(_, expr_id, _, _, _) => *expr_id,
                    e => unreachable!("Found unexpected AST element: {:?}", e),
                };

                match &self.ast_context.resolve_type(ctype).kind {
                    CTypeKind::Vector(CQualTypeId { ctype, .. }, len) => {
                        (&self.ast_context[*ctype].kind, expr_id, *len)
                    }
                    _ => unreachable!("Found type other than vector"),
                }
            }
            // _mm_insert_ps seems to be the exception to the rule as it has an implicit cast rather
            // than an explicit one
            ImplicitCast(CQualTypeId { ctype, .. }, expr_id, _, _, _) => {
                match &self.ast_context.resolve_type(ctype).kind {
                    CTypeKind::Vector(CQualTypeId { ctype, .. }, len) => {
                        (&self.ast_context[*ctype].kind, expr_id, *len)
                    }
                    _ => unreachable!("Found type other than vector"),
                }
            }
            ref e => unreachable!("Found something other than a cast cast: {:?}", e),
        }
    }

    /// This function takes the expr ids belonging to a shuffle vector "super builtin" call,
    /// excluding the first two (which are always vector exprs). These exprs contain mathematical
    /// offsets applied to a mask expr (or are otherwise a numeric constant) which we'd like to extract.
    fn get_shuffle_vector_mask(&self, expr_ids: &[CExprId]) -> TranslationResult<CExprId> {
        fn unknown_mask_format(e: &CExprKind) -> Result<CExprId, TranslationError> {
            Err(format_err!("Found unknown mask format: {:?}", e).into())
        }
        match self.ast_context[expr_ids[0]].kind {
            // Need to unmask which looks like this most of the time: X + (((mask) >> Y) & Z):
            Binary(_, Add, _, rhs_expr_id, None, None) => {
                self.get_shuffle_vector_mask(&[rhs_expr_id])
            }
            // Sometimes there is a mask like this: ((mask) >> X) & Y:
            Binary(_, BitAnd, lhs_expr_id, _, None, None) => {
                match self.ast_context[lhs_expr_id].kind {
                    Binary(_, ShiftRight, lhs_expr_id, _, None, None) => Ok(lhs_expr_id),
                    ref e => unknown_mask_format(e),
                }
            }
            // Sometimes you find a constant and the mask is used further down the expr list
            Literal(_, Integer(0, IntBase::Dec)) => self.get_shuffle_vector_mask(&[expr_ids[4]]),
            // format: ((char)(mask) & A) ?  B : C - (char)(mask)
            Conditional(_, lhs_expr_id, _, _) => match self.ast_context[lhs_expr_id].kind {
                Binary(_, BitAnd, lhs_expr_id, _, None, None) => {
                    match self.ast_context[lhs_expr_id].kind {
                        ImplicitCast(_, expr_id, IntegralCast, _, _) => {
                            match self.ast_context[expr_id].kind {
                                ExplicitCast(_, expr_id, IntegralCast, _, _) => Ok(expr_id),
                                ref e => unknown_mask_format(e),
                            }
                        }
                        ref e => unknown_mask_format(e),
                    }
                }
                ref e => unknown_mask_format(e),
            },
            ref e => unknown_mask_format(e),
        }
    }

    /// Determine whether or not the expr in question is a SIMD call value being casted,
    /// as the builtin definition will add a superfluous cast for our purposes
    pub fn casting_simd_builtin_call(
        &self,
        expr_id: CExprId,
        is_explicit: bool,
        kind: CastKind,
    ) -> bool {
        use self::CastKind::BuiltinFnToFnPtr;

        match self.ast_context[expr_id].kind {
            CExprKind::ShuffleVector(..) => is_explicit && kind == CastKind::BitCast,
            CExprKind::Call(_, fn_id, _) => {
                let fn_expr = &self.ast_context[fn_id].kind;

                if let CExprKind::ImplicitCast(_, expr_id, BuiltinFnToFnPtr, _, _) = fn_expr {
                    let expr = &self.ast_context[*expr_id].kind;

                    if let CExprKind::DeclRef(_, decl_id, _) = expr {
                        let decl = &self.ast_context[*decl_id].kind;

                        if let CDeclKind::Function { ref name, .. } = decl {
                            return name.starts_with("__builtin_ia32_");
                        }
                    }
                }

                false
            }
            _ => false,
        }
    }
}
