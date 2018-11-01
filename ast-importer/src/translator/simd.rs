/// This module provides translation for SIMD 

use super::*;


/// As of rustc 1.29, rust is known to be missing some SIMD functions.
/// See https://github.com/rust-lang-nursery/stdsimd/issues/579
static MISSING_SIMD_FUNCTIONS: [&str; 36] = [
    "_mm_and_si64",
    "_mm_andnot_si64",
    "_mm_cmpeq_pi16",
    "_mm_cmpeq_pi32",
    "_mm_cmpeq_pi8",
    "_mm_cvtm64_si64",
    "_mm_cvtph_ps",
    "_mm_cvtsi32_si64",
    "_mm_cvtsi64_m64",
    "_mm_cvtsi64_si32",
    "_mm_empty",
    "_mm_free",
    "_mm_loadu_si64",
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

static SIMD_X86_64_ONLY: [&str; 11] = [
    "_mm_cvtsd_si64",
    "_mm_cvtsi128_si64",
    "_mm_cvtsi128_si64x",
    "_mm_cvtsi64_sd",
    "_mm_cvtsi64_si128",
    "_mm_cvtsi64_ss",
    "_mm_cvtss_si64",
    "_mm_cvttsd_si64",
    "_mm_cvttsd_si64x",
    "_mm_cvttss_si64",
    "_mm_stream_si64",
];

impl Translation {

    pub 
    fn import_simd_typedef(&self, name: &str) -> bool {
        match name {
            // Public API SIMD typedefs:
            "__m128i" | "__m128" | "__m128d" | "__m64" | "__m256" | "__m256d" | "__m256i" => {
                // __m64 is still behind a feature gate
                if name == "__m64" {
                    self.features.borrow_mut().insert("stdsimd");
                }

                let mut item_store = self.item_store.borrow_mut();

                let x86_attr = mk().call_attr("cfg", vec!["target_arch = \"x86\""]).pub_();
                let x86_64_attr = mk().call_attr("cfg", vec!["target_arch = \"x86_64\""]).pub_();

                item_store.uses
                    .get_mut(vec!["std".into(), "arch".into(), "x86".into()])
                    .insert_with_attr(name, x86_attr);
                item_store.uses
                    .get_mut(vec!["std".into(), "arch".into(), "x86_64".into()])
                    .insert_with_attr(name, x86_64_attr);

                true
            },
            // These seem to be C internal types only, and shouldn't need any explicit support.
            // See https://internals.rust-lang.org/t/getting-explicit-simd-on-stable-rust/4380/115
            "__v1di" | "__v2si" | "__v4hi" | "__v8qi" | "__v4si" | "__v4sf" | "__v4su" |
            "__v2df" | "__v2di" | "__v8hi" | "__v16qi" | "__v2du" | "__v8hu" | "__v16qu" |
            "__v4df" | "__v8sf" | "__v4di" | "__v8si" | "__v16hi" | "__v32qi" | "__v4du" |
            "__v8di_aligned" | "__v8df_aligned" | "__v16sf_aligned" | "__v8sf_aligned" |
            "__v4df_aligned" | "__v4di_aligned" |
            "__v16qs" | "__v8su" | "__v16hu" | "__mm_loadh_pi_v2f32" | "__mm_loadl_pi_v2f32" => true,
            _ => false,
        }
    }

    pub fn import_simd_function(&self, name: &str) -> Result<bool, String> {
        if name.starts_with("_mm") {
            // REVIEW: This will do a linear lookup against all SIMD fns. Could use a lazy static hashset
            if MISSING_SIMD_FUNCTIONS.contains(&name) {
                return Err(format!("SIMD function {} doesn't currently have a rust counterpart", name));
            }

            // The majority of x86/64 SIMD is stable, however there are still some
            // bits that are behind a feature gate.
            self.features.borrow_mut().insert("stdsimd");

            let mut item_store = self.item_store.borrow_mut();

            // REVIEW: Also a linear lookup
            if !SIMD_X86_64_ONLY.contains(&name) {
                let x86_attr = mk().call_attr("cfg", vec!["target_arch = \"x86\""]).pub_();

                item_store.uses
                    .get_mut(vec!["std".into(), "arch".into(), "x86".into()])
                    .insert_with_attr(name, x86_attr);
            }

            let x86_64_attr = mk().call_attr("cfg", vec!["target_arch = \"x86_64\""]).pub_();

            item_store.uses
                .get_mut(vec!["std".into(), "arch".into(), "x86_64".into()])
                .insert_with_attr(name, x86_64_attr);

            return Ok(true);
        }

        Ok(false)
    }

    pub fn convert_builtin_ia32_pshufw(
        &self,
        use_: ExprUse,
        is_static: bool,
        decay_ref: DecayRef,
        args: &[CExprId],
    ) -> Result<WithStmts<P<Expr>>, String> {
        self.import_simd_function("_mm_shuffle_pi16")?;

        let (_, first_expr_id, _) = self.strip_vector_explicit_cast(args[0]);
        let first_param = self.convert_expr(ExprUse::Used, first_expr_id, is_static, decay_ref)?;
        let second_expr_id = match self.ast_context.c_exprs[&args[1]].kind {
            // For some reason there seems to be an incorrect implicit cast here to char
            // it's possible the builtin takes a char even though the function takes an int
            CExprKind::ImplicitCast(_, expr_id, CastKind::IntegralCast, _, _) => expr_id,
            _ => args[1],
        };
        let second_param =
            self.convert_expr(ExprUse::Used, second_expr_id, is_static, decay_ref)?;
        let call = mk().call_expr(
            mk().ident_expr("_mm_shuffle_pi16"),
            vec![first_param.val, second_param.val],
        );

        if use_ == ExprUse::Used {
            Ok(WithStmts {
                stmts: Vec::new(),
                val: call,
            })
        } else {
            Ok(WithStmts {
                stmts: vec![mk().expr_stmt(call)],
                val: self.panic("No value for unused shuffle vector return"),
            })
        }
    }

    pub fn implicit_vector_default(&self, ctype: CTypeId, len: usize) -> Result<P<Expr>, String> {
        // NOTE: This is only for x86/_64, and so support for other architectures
        // might need some sort of disambiguation to be exported
        let fn_name = match (&self.ast_context[ctype].kind, len) {
            (CTypeKind::Float, 4) => "_mm_setzero_ps",
            (CTypeKind::Float, 8) => "_mm256_setzero_ps",
            (CTypeKind::Double, 2) => "_mm_setzero_pd",
            (CTypeKind::Double, 4) => "_mm256_setzero_pd",
            (CTypeKind::LongLong, 2) => "_mm_setzero_si128",
            (CTypeKind::LongLong, 4) => "_mm256_setzero_si256",
            (CTypeKind::LongLong, 1) => {
                // __m64 is still unstable as of rust 1.29
                self.features.borrow_mut().insert("stdsimd");

                "_mm_setzero_si64"
            }
            (kind, len) => {
                return Err(format!(
                    "Unsupported vector default initializer: {:?} x {}",
                    kind, len
                ))
            }
        };

        self.import_simd_function(fn_name)
            .expect("None of these fns should be unsupported in rust");

        Ok(mk().call_expr(mk().ident_expr(fn_name), Vec::new() as Vec<P<Expr>>))
    }

    pub fn vector_list_initializer(
        &self,
        use_: ExprUse,
        is_static: bool,
        decay_ref: DecayRef,
        ids: &[CExprId],
        ctype: CTypeId,
        len: usize,
    ) -> Result<WithStmts<P<Expr>>, String> {
        let fn_call_name = match (&self.ast_context.c_types[&ctype].kind, len) {
            (CTypeKind::Float, 4) => "_mm_setr_ps",
            (CTypeKind::Float, 8) => "_mm256_setr_ps",
            (CTypeKind::Double, 2) => "_mm_setr_pd",
            (CTypeKind::Double, 4) => "_mm256_setr_pd",
            (CTypeKind::LongLong, 2) => "_mm_set_epi64x",
            (CTypeKind::LongLong, 4) => "_mm256_setr_epi64x",
            (CTypeKind::Char, 8) => "_mm_setr_pi8",
            (CTypeKind::Char, 16) => "_mm_setr_epi8",
            (CTypeKind::Char, 32) => "_mm256_setr_epi8",
            (CTypeKind::Int, 2) => "_mm_setr_pi32",
            (CTypeKind::Int, 4) => "_mm_setr_epi32",
            (CTypeKind::Int, 8) => "_mm256_setr_epi32",
            (CTypeKind::Short, 4) => "_mm_setr_pi16",
            (CTypeKind::Short, 8) => "_mm_setr_epi16",
            (CTypeKind::Short, 16) => "_mm256_setr_epi16",
            ref e => return Err(format!("Unknown vector init list: {:?}", e)),
        };

        self.import_simd_function(fn_call_name)?;

        let mut params: Vec<P<Expr>> = vec![];

        for param_id in ids {
            params.push(
                self.convert_expr(use_, *param_id, is_static, decay_ref)?
                    .val,
            );
        }

        // rust is missing support for _mm_setr_epi64x, so we have to use
        // the reverse arguments for _mm_set_epi64x
        if fn_call_name == "_mm_set_epi64x" {
            params.reverse();
        }

        let call = mk().call_expr(mk().ident_expr(fn_call_name), params);

        if use_ == ExprUse::Used {
            Ok(WithStmts {
                stmts: Vec::new(),
                val: call,
            })
        } else {
            Ok(WithStmts {
                stmts: vec![mk().expr_stmt(call)],
                val: self.panic("No value for unused shuffle vector return"),
            })
        }
    }

    /// Convert a shuffle operation into the equivalent Rust SIMD library calls.
    /// 
    /// Because clang implements some shuffle operations as macros around intrinsic
    /// shuffle functions, this translation works to find the high-level shuffle
    /// call corresponding to the low-level one found in the C AST.
    pub fn convert_shuffle_vector(
        &self,
        use_: ExprUse,
        is_static: bool,
        decay_ref: DecayRef,
        child_expr_ids: &[CExprId],
    ) -> Result<WithStmts<P<Expr>>, String> {
        use c_ast::CExprKind::Literal;
        use c_ast::CLiteral::Integer;
        use c_ast::CTypeKind::{Double, Float, Int, Short};

        // There are three shuffle vector functions which are actually functions, not superbuiltins/macros,
        // which do not need to be handled here: _mm_shuffle_pi8, _mm_shuffle_epi8, _mm256_shuffle_epi8

        if ![4, 6, 10, 18].contains(&child_expr_ids.len()) {
            return Err(format!(
                "Unsupported shuffle vector without 4, 6, 10, or 18 input params: {}",
                child_expr_ids.len()
            ));
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

        let mask_expr_id = self.get_shuffle_vector_mask(&child_expr_ids[2..]);
        let first_param = self.convert_expr(ExprUse::Used, first_expr_id, is_static, decay_ref)?;
        let second_param =
            self.convert_expr(ExprUse::Used, second_expr_id, is_static, decay_ref)?;
        let third_param = self.convert_expr(ExprUse::Used, mask_expr_id, is_static, decay_ref)?;
        let mut params = vec![first_param.val];

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
                    _ => params.push(second_param.val),
                }

        params.push(third_param.val);

        let shuffle_fn_name = match (&first_vec, first_vec_len) {
            (Float, 4) => "_mm_shuffle_ps",
            (Float, 8) => "_mm256_shuffle_ps",
            (Double, 2) => "_mm_shuffle_pd",
            (Double, 4) => "_mm256_shuffle_pd",
            (Int, 4) => "_mm_shuffle_epi32",
            (Int, 8) => "_mm256_shuffle_epi32",
            (Short, 8) => {
                // _mm_shufflehi_epi16 mask params start with const int,
                // _mm_shufflelo_epi16 does not
                let expr_id = &child_expr_ids[2];
                if let Literal(_, Integer(0, IntBase::Dec)) = self.ast_context.c_exprs[expr_id].kind
                {
                    "_mm_shufflehi_epi16"
                } else {
                    "_mm_shufflelo_epi16"
                }
            }
            (Short, 16) => {
                // _mm256_shufflehi_epi16 mask params start with const int,
                // _mm256_shufflelo_epi16 does not
                let expr_id = &child_expr_ids[2];
                if let Literal(_, Integer(0, IntBase::Dec)) = self.ast_context.c_exprs[expr_id].kind
                {
                    "_mm256_shufflehi_epi16"
                } else {
                    "_mm256_shufflelo_epi16"
                }
            }
            e => return Err(format!("Unknown shuffle vector signature: {:?}", e)),
        };

        self.import_simd_function(shuffle_fn_name)?;

        let call = mk().call_expr(mk().ident_expr(shuffle_fn_name), params);

        if use_ == ExprUse::Used {
            Ok(WithStmts {
                stmts: Vec::new(),
                val: call,
            })
        } else {
            Ok(WithStmts {
                stmts: vec![mk().expr_stmt(call)],
                val: self.panic("No value for unused shuffle vector return"),
            })
        }
    }

    /// Vectors tend to have casts to and from internal types. This is problematic for shuffle vectors
    /// in particular which are usually macros ontop of a builtin call. Although one of these casts
    /// is likely redundant (external type), the other is not (internal type). We remove both of the
    /// casts for simplicity and readability
    fn strip_vector_explicit_cast(&self, expr_id: CExprId) -> (&CTypeKind, CExprId, usize) {
        match self.ast_context.c_exprs[&expr_id].kind {
            CExprKind::ExplicitCast(CQualTypeId { ctype, .. }, expr_id, _, _, _) => {
                let expr_id = match &self.ast_context.c_exprs[&expr_id].kind {
                    CExprKind::ExplicitCast(_, expr_id, _, _, _) => *expr_id,
                    // The expr_id wont be used in this case (the function only has one
                    // vector param, not two, despite the following type match), so it's
                    // okay to provide a dummy here
                    CExprKind::Call(..) => expr_id,
                    _ => unreachable!("Found cast other than explicit cast"),
                };

                match &self.ast_context.resolve_type(ctype).kind {
                    CTypeKind::Vector(CQualTypeId { ctype, .. }, len) => {
                        (&self.ast_context.c_types[ctype].kind, expr_id, *len)
                    }
                    _ => unreachable!("Found type other than vector"),
                }
            }
            _ => unreachable!("Found cast other than explicit cast"),
        }
    }

    /// This function takes the expr ids belonging to a shuffle vector "super builtin" call,
    /// excluding the first two (which are always vector exprs). These exprs contain mathematical
    /// offsets applied to a mask expr (or are otherwise a numeric constant) which we'd like to extract.
    fn get_shuffle_vector_mask(&self, expr_ids: &[CExprId]) -> CExprId {
        use c_ast::BinOp::{Add, BitAnd, ShiftRight};
        use c_ast::CExprKind::{Binary, Literal};
        use c_ast::CLiteral::Integer;

        match self.ast_context.c_exprs[&expr_ids[0]].kind {
            // Need to unmask which looks like this most of the time: X + (((mask) >> Y) & Z):
            Binary(_, Add, _, rhs_expr_id, None, None) => {
                self.get_shuffle_vector_mask(&[rhs_expr_id])
            }
            // Sometimes there is a mask like this: ((mask) >> X) & Y:
            Binary(_, BitAnd, lhs_expr_id, _, None, None) => {
                match self.ast_context.c_exprs[&lhs_expr_id].kind {
                    Binary(_, ShiftRight, lhs_expr_id, _, None, None) => lhs_expr_id,
                    _ => unreachable!("Found unknown mask format"),
                }
            }
            // Sometimes you find a constant and the mask is used further down the expr list
            Literal(_, Integer(0, IntBase::Dec)) => self.get_shuffle_vector_mask(&[expr_ids[4]]),
            ref e => unreachable!("Found unknown mask format: {:?}", e),
        }
    }
}
