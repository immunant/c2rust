use crate::panic_detail;
use crate::rewrite::expr::distribute::DistRewrite;
use crate::rewrite::expr::mir_op::{self, ZeroizeType};
use crate::rewrite::expr::unlower::MirOriginDesc;
use crate::rewrite::{LifetimeName, Rewrite};
use assert_matches::assert_matches;
use log::*;
use rustc_hir as hir;
use rustc_hir::def::Namespace;
use rustc_hir::intravisit::{self, Visitor};
use rustc_hir::{ExprKind, HirId};
use rustc_middle::hir::nested_filter;
use rustc_middle::ty::adjustment::{Adjust, Adjustment, AutoBorrow, PointerCast};
use rustc_middle::ty::print::{FmtPrinter, Print};
use rustc_middle::ty::{Ty, TyCtxt, TyKind, TypeckResults};
use rustc_span::Span;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt::Write as _;

macro_rules! format_rewrite {
    ($($args:tt)*) => {
        Rewrite::Text(format!($($args)*))
    };
}

struct ConvertVisitor<'tcx> {
    tcx: TyCtxt<'tcx>,
    typeck_results: &'tcx TypeckResults<'tcx>,
    mir_rewrites: HashMap<HirId, Vec<DistRewrite>>,
    rewrites: HashMap<HirId, (Span, Rewrite)>,
    subsumed_child_rewrites: RefCell<HashSet<HirId>>,
    /// When `true`, any `Expr` where rustc added an implicit adjustment will be rewritten to make
    /// that adjustment explicit.  Any node that emits a non-adjustment rewrite sets this flag when
    /// visiting its children.  This is important to ensure that implicit ref/deref operations are
    /// not simply discarded by our rewrites.
    ///
    /// For example, suppose we'd like to remove the `as_ptr` call from `arr.as_ptr()` to produce a
    /// safe reference `&[T]` instead of a raw pointer `*const T`.  Simply eliminating the call,
    /// leaving `arr`, is incorrect if `arr` has type `[T; 10]`.  In this case, rustc was adding an
    /// implicit `Ref` adjustment, as if the programmer had written `(&arr).as_ptr()`.  The correct
    /// rewriting of this code is therefore `&arr`, not `arr`.
    ///
    /// To get the right result, we rewrite in two steps.  First, we materialize the implicit `Ref`
    /// adjustment that rustc applies to `arr`, producing the expression `(&arr).as_ptr()`.
    /// Second, we remove the `as_ptr` call, leaving only `&arr`.
    ///
    /// However, we don't want to apply this `x.f()` to `(&x).f()` step on code that's already
    /// safe, since it's unnecessary there and makes the code harder to read.  Our solution is to
    /// only materialize adjustments within the children (and further descendants) of nodes that
    /// are already being rewritten for some other reason.
    materialize_adjustments: bool,
}

impl<'tcx> ConvertVisitor<'tcx> {
    /// If `set`, set `self.materialize_adjustments` to `true` while running the closure.  If `set`
    /// is `false`, `self.materialize_adjustments` is left unchanged (inherited from the parent).
    fn with_materialize_adjustments<R>(&mut self, set: bool, f: impl FnOnce(&mut Self) -> R) -> R {
        let old = self.materialize_adjustments;
        self.materialize_adjustments |= set;
        let r = f(self);
        self.materialize_adjustments = old;
        r
    }

    /// Get subexpression `idx` of `ex`.  Panics if the index is out of range for `ex`.  The
    /// precise meaning of the index depends on the expression kind.
    fn get_subexpr(&self, ex: &'tcx hir::Expr<'tcx>, idx: usize) -> Rewrite {
        use hir::ExprKind::*;
        let sub_ex = match (&ex.kind, idx) {
            (&Box(e), 0) => e,
            (&Array(es), i) => &es[i],
            (&Call(_, args), i) => &args[i],
            (&MethodCall(_, args, _), i) => &args[i],
            (&Tup(es), i) => &es[i],
            (&Binary(_, x, _), 0) => x,
            (&Binary(_, _, y), 1) => y,
            (&Unary(_, x), 0) => x,
            (&Cast(e, _), 0) => e,
            (&Type(e, _), 0) => e,
            (&DropTemps(e), 0) => e,
            (&If(cond, _, _), 0) => cond,
            (&If(_, then, _), 1) => then,
            (&If(_, _, Some(else_)), 2) => else_,
            (&Match(e, _, _), 0) => e,
            (&Assign(l, _, _), 0) => l,
            (&Assign(_, r, _), 1) => r,
            (&AssignOp(_, l, _), 0) => l,
            (&AssignOp(_, _, r), 1) => r,
            (&Field(e, _), 0) => e,
            (&Index(arr, _), 0) => arr,
            (&Index(_, e_idx), 1) => e_idx,
            (&AddrOf(_, _, e), 0) => e,
            (&Break(_, Some(e)), 0) => e,
            (&Ret(Some(e)), 0) => e,
            (&Struct(_, flds, base), i) => {
                if i == flds.len() {
                    base.unwrap()
                } else {
                    flds[i].expr
                }
            }
            (&Repeat(e, _), 0) => e,
            (&Yield(e, _), 0) => e,
            _ => panic!("bad subexpression index {} for {:?}", idx, ex),
        };
        let rw_sub = Rewrite::Sub(idx, sub_ex.span);
        if let Some(child_span_rw) = self.rewrites.get(&sub_ex.hir_id) {
            let child_rw = &child_span_rw.1;
            if let Some(subst_rw) = child_rw.try_subst(&rw_sub) {
                debug!(
                    "get_subexpr: substituted {rw_sub:?} into {child_rw:?}, producing {subst_rw:?}"
                );
                self.subsumed_child_rewrites
                    .borrow_mut()
                    .insert(sub_ex.hir_id);
                return subst_rw;
            }
        }
        rw_sub
    }

    fn rewrite_from_mir_rw(
        &self,
        ex: Option<&'tcx hir::Expr<'tcx>>,
        rw: &mir_op::RewriteKind,
        hir_rw: Rewrite,
    ) -> Rewrite {
        if ex.is_none() {
            return convert_cast_rewrite(rw, hir_rw);
        }
        let ex = ex.unwrap();

        // Cases that extract a subexpression are handled here; cases that only wrap the
        // top-level expression (and thus can handle a non-`Identity` `hir_rw`) are handled by
        // `convert_cast_rewrite`.
        match *rw {
            mir_op::RewriteKind::OffsetSlice { mutbl } => {
                // `p.offset(i)` -> `&p[i as usize ..]`
                assert!(matches!(hir_rw, Rewrite::Identity));
                let arr = self.get_subexpr(ex, 0);
                let idx = Rewrite::Cast(
                    Box::new(self.get_subexpr(ex, 1)),
                    Box::new(Rewrite::Print("usize".to_owned())),
                );
                let elem = Rewrite::SliceRange(Box::new(arr), Some(Box::new(idx)), None);
                Rewrite::Ref(Box::new(elem), mutbl_from_bool(mutbl))
            }

            mir_op::RewriteKind::OptionMapOffsetSlice { mutbl } => {
                // `p.offset(i)` -> `p.as_ref().map(|p| &p[i as usize ..])`
                assert!(matches!(hir_rw, Rewrite::Identity));

                // Build let binding
                let arr = self.get_subexpr(ex, 0);
                let idx = Rewrite::Cast(
                    Box::new(self.get_subexpr(ex, 1)),
                    Box::new(Rewrite::Print("usize".to_owned())),
                );
                let rw_let = Rewrite::Let(vec![("arr".into(), arr), ("idx".into(), idx)]);
                let arr = Rewrite::Text("arr".into());
                let idx = Rewrite::Text("idx".into());

                // Build  closure
                let elem = Rewrite::SliceRange(Box::new(arr.clone()), Some(Box::new(idx)), None);
                let ref_elem = Rewrite::Ref(Box::new(elem), mutbl_from_bool(mutbl));
                let closure = Rewrite::Closure1("arr".into(), Box::new(ref_elem));

                // Call `Option::map`
                let call = Rewrite::MethodCall("map".into(), Box::new(arr), vec![closure]);
                Rewrite::Block(vec![rw_let], Some(Box::new(call)))
            }

            mir_op::RewriteKind::RemoveAsPtr => {
                // `slice.as_ptr()` -> `slice`
                assert!(matches!(hir_rw, Rewrite::Identity));
                self.get_subexpr(ex, 0)
            }

            mir_op::RewriteKind::RemoveCast => {
                // `x as T` -> `x`
                match hir_rw {
                    Rewrite::Identity => {
                        assert!(matches!(hir_rw, Rewrite::Identity));
                        self.get_subexpr(ex, 0)
                    }
                    // Can happen when attempting to delete a cast adjustment.
                    Rewrite::Cast(rw, _) => *rw,
                    Rewrite::RemovedCast(rw) => *rw,
                    _ => panic!("unexpected hir_rw {hir_rw:?} for RawToRef"),
                }
            }

            mir_op::RewriteKind::RawToRef { mutbl } => {
                // &raw _ to &_ or &raw mut _ to &mut _
                match hir_rw {
                    Rewrite::Identity => {
                        Rewrite::Ref(Box::new(self.get_subexpr(ex, 0)), mutbl_from_bool(mutbl))
                    }
                    Rewrite::AddrOf(rw, mutbl) => Rewrite::Ref(rw, mutbl),
                    _ => panic!("unexpected hir_rw {hir_rw:?} for RawToRef"),
                }
            }

            mir_op::RewriteKind::IsNullToIsNone => {
                // `p.is_null()` -> `p.is_none()`
                assert!(matches!(hir_rw, Rewrite::Identity));
                Rewrite::MethodCall("is_none".into(), Box::new(self.get_subexpr(ex, 0)), vec![])
            }
            mir_op::RewriteKind::IsNullToConstFalse => {
                // `p.is_null()` -> `false`
                assert!(matches!(hir_rw, Rewrite::Identity));
                Rewrite::Text("false".into())
            }
            mir_op::RewriteKind::PtrNullToNone => {
                // `ptr::null()` -> `None`
                assert!(matches!(hir_rw, Rewrite::Identity));
                Rewrite::Text("None".into())
            }
            mir_op::RewriteKind::ZeroAsPtrToNone => {
                // `0 as *const T` -> `None`
                assert!(matches!(hir_rw, Rewrite::Identity));
                Rewrite::Text("None".into())
            }

            mir_op::RewriteKind::MemcpySafe {
                ref elem_ty,
                dest_single,
                dest_option,
                src_single,
                src_option,
            } => {
                // `memcpy(dest, src, byte_len)` to a `copy_from_slice` call
                assert!(matches!(hir_rw, Rewrite::Identity));
                let mut stmts = Vec::with_capacity(6);

                stmts.push(Rewrite::Let(vec![
                    ("dest".into(), self.get_subexpr(ex, 0)),
                    ("src".into(), self.get_subexpr(ex, 1)),
                    ("byte_len".into(), self.get_subexpr(ex, 2)),
                ]));
                // Best-effort check to detect size mismatches.  This can happen if we infer the
                // wrong pointee type, or if the C code used a hardcoded size for `elem_ty` but we
                // changed its size during rewriting, or possibly other cases.  These errors could
                // potentially cause too few items to be copied, introducing a subtle logic error;
                // this assertion tries to catch this situation early so it's easier to diagnose.
                stmts.push(format_rewrite!(
                    "assert_eq!(byte_len as usize % std::mem::size_of::<{elem_ty}>(), 0)"
                ));
                stmts.push(Rewrite::Let(vec![(
                    "n".into(),
                    format_rewrite!("byte_len as usize / std::mem::size_of::<{elem_ty}>()"),
                )]));
                let mut convert = |var: &str, is_mut, is_single, is_option| {
                    let single_to_slice = if is_single {
                        if is_mut {
                            format_rewrite!("std::slice::from_mut({var})")
                        } else {
                            format_rewrite!("std::slice::from_ref({var})")
                        }
                    } else {
                        Rewrite::Text(var.into())
                    };
                    let rhs = if is_option {
                        // ```
                        // match var {
                        //     Some(x) => x,  // or slice::from_ref(x), etc
                        //     None => { assert_eq!(n, 0); &[] },
                        // }
                        // ```
                        let empty_slice = if is_mut {
                            format_rewrite!("&mut []")
                        } else {
                            format_rewrite!("&[]")
                        };
                        Rewrite::Match(
                            Box::new(Rewrite::Text(var.into())),
                            vec![
                                (format!("Some({var})"), single_to_slice),
                                (
                                    "None".into(),
                                    Rewrite::Block(
                                        vec![format_rewrite!("assert_eq!(n, 0)")],
                                        Some(Box::new(empty_slice)),
                                    ),
                                ),
                            ],
                        )
                    } else {
                        single_to_slice
                    };
                    stmts.push(Rewrite::Let1(var.into(), Box::new(rhs)));
                };
                convert("dest", true, dest_single, dest_option);
                convert("src", false, src_single, src_option);
                // `dest[..n].copy_from_slice(&src[..n]);`
                stmts.push(Rewrite::MethodCall(
                    "copy_from_slice".into(),
                    Box::new(format_rewrite!("dest[..n]")),
                    vec![format_rewrite!("&src[..n]")],
                ));

                // TODO: `memcpy` cases that actually use the return value are only partially
                // supported.  Currently we always return `&mut [T]`, which may not match the
                // permissions on the output.  Doing this correctly would require saving the
                // original `dest` and then applying `slice::from_mut`, `OptionDowngrade`, and/or
                // `DynOwnedDowngrade` to get `&mut [T]` for the call to `copy_from_slice`.  This
                // would allow ownership to flow from `p` to `q` in `q = memcpy(p, ...)`, for
                // example.  Fortunately, most code just uses `memcpy` for its side effect and
                // ignores the return value.
                Rewrite::Block(stmts, Some(Box::new(format_rewrite!("dest"))))
            }

            mir_op::RewriteKind::MemsetZeroize {
                ref zero_ty,
                ref elem_ty,
                dest_single,
            } => {
                // `memset(dest, 0, n)` to assignments that zero out each field of `*dest`
                assert!(matches!(hir_rw, Rewrite::Identity));
                let zeroize_body = if dest_single {
                    Rewrite::Text(generate_zeroize_code(zero_ty, "(*dest)"))
                } else {
                    format_rewrite!(
                        "for i in 0..n {{\n    {};\n}}",
                        generate_zeroize_code(zero_ty, "(*dest)[i]")
                    )
                };
                Rewrite::Block(
                    vec![
                        Rewrite::Let(vec![
                            ("dest".into(), self.get_subexpr(ex, 0)),
                            ("val".into(), self.get_subexpr(ex, 1)),
                            ("byte_len".into(), self.get_subexpr(ex, 2)),
                        ]),
                        // Best-effort check to detect size mismatches, as in `MemcpySafe`.
                        format_rewrite!(
                            "assert_eq!(byte_len as usize % std::mem::size_of::<{elem_ty}>(), 0)"
                        ),
                        Rewrite::Let(vec![(
                            "n".into(),
                            format_rewrite!("byte_len as usize / std::mem::size_of::<{elem_ty}>()"),
                        )]),
                        format_rewrite!("assert_eq!(val, 0, \"non-zero memset NYI\")"),
                        zeroize_body,
                    ],
                    Some(Box::new(format_rewrite!("dest"))),
                )
            }

            mir_op::RewriteKind::MallocSafe {
                ref zero_ty,
                ref elem_ty,
                single,
            }
            | mir_op::RewriteKind::CallocSafe {
                ref zero_ty,
                ref elem_ty,
                single,
            } => {
                // `malloc(n)` -> `Box::new(z)` or similar
                assert!(matches!(hir_rw, Rewrite::Identity));
                let zeroize_expr = generate_zeroize_expr(zero_ty);
                let mut stmts = match *rw {
                    mir_op::RewriteKind::MallocSafe { .. } => vec![
                        Rewrite::Let(vec![("byte_len".into(), self.get_subexpr(ex, 0))]),
                        // Best-effort check to detect size mismatches, as in `MemcpySafe`.
                        format_rewrite!(
                            "assert_eq!(byte_len as usize % std::mem::size_of::<{elem_ty}>(), 0)"
                        ),
                        Rewrite::Let1(
                            "n".into(),
                            Box::new(format_rewrite!(
                                "byte_len as usize / std::mem::size_of::<{elem_ty}>()"
                            )),
                        ),
                    ],
                    mir_op::RewriteKind::CallocSafe { .. } => vec![
                        Rewrite::Let(vec![
                            ("count".into(), self.get_subexpr(ex, 0)),
                            ("size".into(), self.get_subexpr(ex, 1)),
                        ]),
                        format_rewrite!(
                            "assert_eq!(size as usize, std::mem::size_of::<{elem_ty}>())"
                        ),
                        Rewrite::Let1("n".into(), Box::new(format_rewrite!("count as usize"))),
                    ],
                    _ => unreachable!(),
                };
                let expr = if single {
                    stmts.push(Rewrite::Text("assert_eq!(n, 1)".into()));
                    format_rewrite!("Box::new({})", zeroize_expr)
                } else {
                    stmts.push(Rewrite::Let1(
                        "mut v".into(),
                        Box::new(Rewrite::Text("Vec::with_capacity(n)".into())),
                    ));
                    stmts.push(format_rewrite!(
                        "for i in 0..n {{\n    v.push({});\n}}",
                        zeroize_expr,
                    ));
                    Rewrite::Text("v.into_boxed_slice()".into())
                };
                Rewrite::Block(stmts, Some(Box::new(expr)))
            }

            mir_op::RewriteKind::FreeSafe { single: _ } => {
                // `free(p)` -> `drop(p)`
                assert!(matches!(hir_rw, Rewrite::Identity));
                Rewrite::Call("std::mem::drop".to_string(), vec![self.get_subexpr(ex, 0)])
            }

            mir_op::RewriteKind::ReallocSafe {
                ref zero_ty,
                ref elem_ty,
                src_single,
                dest_single,
                option,
            } => {
                // `realloc(p, n)` -> `Box::new(...)`
                assert!(matches!(hir_rw, Rewrite::Identity));
                let zeroize_expr = generate_zeroize_expr(zero_ty);
                let mut stmts = vec![
                    Rewrite::Let(vec![
                        ("src_ptr".into(), self.get_subexpr(ex, 0)),
                        ("dest_byte_len".into(), self.get_subexpr(ex, 1)),
                    ]),
                    // Best-effort check to detect size mismatches, as in `MemcpySafe`.
                    format_rewrite!(
                        "assert_eq!(dest_byte_len as usize % std::mem::size_of::<{elem_ty}>(), 0)"
                    ),
                    Rewrite::Let1(
                        "dest_n".into(),
                        Box::new(format_rewrite!(
                            "dest_byte_len as usize / std::mem::size_of::<{elem_ty}>()"
                        )),
                    ),
                ];
                if dest_single {
                    stmts.push(Rewrite::Text("assert_eq!(dest_n, 1)".into()));
                }
                let expr = match (src_single, dest_single) {
                    (false, false) => {
                        let src = if option {
                            "src_ptr.unwrap_or(Box::new([]))"
                        } else {
                            "src_ptr"
                        };
                        stmts.push(Rewrite::Let1(
                            "mut dest_ptr".into(),
                            Box::new(format_rewrite!("Vec::from({src})")),
                        ));
                        stmts.push(format_rewrite!(
                            "dest_ptr.resize_with(dest_n, || {})",
                            zeroize_expr,
                        ));
                        Rewrite::Text("dest_ptr.into_boxed_slice()".into())
                    }
                    (false, true) => {
                        let opt_flatten = if option { ".flatten()" } else { "" };
                        format_rewrite!(
                            "src_ptr.into_iter(){opt_flatten}.next().unwrap_or_else(|| {})",
                            zeroize_expr
                        )
                    }
                    (true, false) => {
                        stmts.push(Rewrite::Let1(
                            "mut dest_ptr".into(),
                            Box::new(Rewrite::Text("Vec::with_capacity(dest_n)".into())),
                        ));
                        if option {
                            stmts.push(Rewrite::Text(
                                "if dest_n >= 1 { if let Some(src) = src_ptr { dest_ptr.push(*src); } }".into(),
                            ));
                        } else {
                            stmts.push(Rewrite::Text(
                                "if dest_n >= 1 { dest_ptr.push(*src_ptr); }".into(),
                            ));
                        }
                        stmts.push(format_rewrite!(
                            "dest_ptr.resize_with(dest_n, || {})",
                            zeroize_expr,
                        ));
                        Rewrite::Text("dest_ptr.into_boxed_slice()".into())
                    }
                    (true, true) => {
                        if option {
                            format_rewrite!("src_ptr.unwrap_or_else(|| Box::new({}))", zeroize_expr)
                        } else {
                            Rewrite::Text("src_ptr".into())
                        }
                    }
                };
                Rewrite::Block(stmts, Some(Box::new(expr)))
            }

            mir_op::RewriteKind::CellGet => {
                // `*x` to `Cell::get(x)`
                assert!(matches!(hir_rw, Rewrite::Identity));
                Rewrite::MethodCall("get".to_string(), Box::new(self.get_subexpr(ex, 0)), vec![])
            }

            mir_op::RewriteKind::CellSet => {
                // `*x` to `Cell::set(x)`
                assert!(matches!(hir_rw, Rewrite::Identity));
                let deref_lhs = assert_matches!(ex.kind, ExprKind::Assign(lhs, ..) => lhs);
                let lhs = self.get_subexpr(deref_lhs, 0);
                let rhs = self.get_subexpr(ex, 1);
                Rewrite::MethodCall("set".to_string(), Box::new(lhs), vec![rhs])
            }

            _ => convert_cast_rewrite(rw, hir_rw),
        }
    }

    /// Generate an `Option::map` call from the rewrites in `mir_rws`.  After seeing an
    /// `OptionMapBegin` in a list of MIR rewrites, pass the remaining rewrites to this method.  If
    /// it returns `Ok((new_hir_rw, remaining_mir_rws))`, then the `OptionMapBegin` and some
    /// additional rewrites have been processed, and only `remaining_mir_rws` are left.  Otherwise,
    /// if it failed to process any rewrites, it returns `Err(original_hir_rw)`.
    fn try_rewrite_option_map<'a>(
        &self,
        mut mir_rws: &'a [DistRewrite],
        mut hir_rw: Rewrite,
    ) -> Result<(Rewrite, &'a [DistRewrite]), Rewrite> {
        /// Find the next `OptionMapEnd` and return the parts of `mir_rws` that come strictly
        /// before it and strictly after it.  Returns `None` if there's an `OptionMapBegin` before
        /// the next `OptionMapEnd`, or if there's no `OptionMapEnd` found in `mir_rws`.
        fn split_option_map_rewrites(
            mir_rws: &[DistRewrite],
        ) -> Option<(&[DistRewrite], &[DistRewrite])> {
            for (i, mir_rw) in mir_rws.iter().enumerate() {
                match mir_rw.rw {
                    // Bail out if we see nested delimiters.  This prevents the special
                    // `Option::map` rewrite from applying, so the caller will fall back on the
                    // `unwrap()` + `Some(_)` rewrites for `OptionMapBegin/End` that are
                    // implemented in `convert_cast_rewrite`.
                    mir_op::RewriteKind::OptionMapBegin => return None,
                    mir_op::RewriteKind::OptionMapEnd => {
                        let (a, b) = mir_rws.split_at(i);
                        return Some((a, &b[1..]));
                    }
                    _ => {}
                }
            }
            None
        }

        // Build up the body of the closure.  We expect `mir_rws` to start just after an
        // `OptionMapBegin` delimiter.  If we find a matching `OptionMapEnd` (with no intervening
        // `OptionMapBegin`; nesting is unsupported), then we add all the rewrites between the
        // delimiters to the `Option::map` body.  Furthermore, as long as the `OptionMapEnd` is
        // followed by another `OptionMapBegin/End` delimited range, we add those rewrites to the
        // body as well.
        let mut opt_body_hir_rw = None;
        // Did we find a matching delimiter for the implicit `OptionMapBegin` that precedes
        // `mir_rws`?
        let mut found_matching_delim = false;
        while let Some((body_rws, other_rws)) = split_option_map_rewrites(mir_rws) {
            found_matching_delim = true;
            mir_rws = other_rws;
            if body_rws.is_empty() {
                // Don't initialize `opt_body_hir_rw` until we've seen at least one actual rewrite.
                // This lets us omit empty `Option::map` calls.
                continue;
            }

            let mut body_hir_rw = opt_body_hir_rw.unwrap_or_else(|| Rewrite::Text("__ptr".into()));
            body_hir_rw = self.rewrite_from_mir_rws(None, body_rws, body_hir_rw);

            opt_body_hir_rw = Some(body_hir_rw);
        }

        if !found_matching_delim {
            return Err(hir_rw);
        }

        // If some actual rewrites were collected, generate the `map` call.
        if let Some(body) = opt_body_hir_rw {
            let closure = Rewrite::Closure1("__ptr".into(), Box::new(body));
            hir_rw = Rewrite::MethodCall("map".into(), Box::new(hir_rw), vec![closure]);
        }
        Ok((hir_rw, mir_rws))
    }

    fn rewrite_from_mir_rws(
        &self,
        ex: Option<&'tcx hir::Expr<'tcx>>,
        mir_rws: &[DistRewrite],
        mut hir_rw: Rewrite,
    ) -> Rewrite {
        let mut iter = mir_rws.iter();
        while let Some(mir_rw) = iter.next() {
            if let mir_op::RewriteKind::OptionMapBegin = mir_rw.rw {
                match self.try_rewrite_option_map(iter.as_slice(), hir_rw) {
                    Ok((new_hir_rw, remaining_mir_rws)) => {
                        hir_rw = new_hir_rw;
                        iter = remaining_mir_rws.iter();
                        continue;
                    }
                    Err(orig_hir_rw) => {
                        hir_rw = orig_hir_rw;
                    }
                }
            }

            hir_rw = self.rewrite_from_mir_rw(ex, &mir_rw.rw, hir_rw);
        }
        hir_rw
    }
}

impl<'tcx> Visitor<'tcx> for ConvertVisitor<'tcx> {
    type NestedFilter = nested_filter::OnlyBodies;

    fn nested_visit_map(&mut self) -> Self::Map {
        self.tcx.hir()
    }

    fn visit_expr(&mut self, ex: &'tcx hir::Expr<'tcx>) {
        let _g = panic_detail::set_current_span(ex.span);
        let mut hir_rw = Rewrite::Identity;

        // This span will be used to apply the actual rewrite.
        // To prevent attempts to rewrite macros in their definition
        // location (e.g. `std::ptr::addr_of`) we instead specify that
        // the rewrite should occur at the callsite
        let callsite_span = ex.span.source_callsite();

        let mir_rws = self.mir_rewrites.remove(&ex.hir_id).unwrap_or_default();
        let mut mir_rws = &mir_rws as &[_];

        // Emit rewrites on subexpressions up front so we can access them in `get_subexpr`.
        self.with_materialize_adjustments(!mir_rws.is_empty(), |this| {
            intravisit::walk_expr(this, ex);
        });

        // Apply rewrites on the expression itself.  These will be the first rewrites in the sorted
        // list produced by `distribute`.
        let expr_rws = take_prefix_while(&mut mir_rws, |x: &DistRewrite| {
            matches!(x.desc, MirOriginDesc::Expr)
        });
        hir_rw = self.rewrite_from_mir_rws(Some(ex), expr_rws, hir_rw);

        // Materialize adjustments if requested by an ancestor or required locally.
        let has_adjustment_rewrites = mir_rws
            .iter()
            .any(|x| matches!(x.desc, MirOriginDesc::Adjustment(_)));
        if self.materialize_adjustments || has_adjustment_rewrites {
            let adjusts = self.typeck_results.expr_adjustments(ex);
            hir_rw =
                materialize_adjustments(self.tcx, adjusts, hir_rw, |step, hir_rw| match step {
                    AdjustmentStep::Before(i) => {
                        let load_rws = take_prefix_while(&mut mir_rws, |x| {
                            x.desc == MirOriginDesc::LoadFromTempForAdjustment(i)
                        });
                        self.rewrite_from_mir_rws(Some(ex), load_rws, hir_rw)
                    }
                    AdjustmentStep::After(i) => {
                        let adj_rws = take_prefix_while(&mut mir_rws, |x| {
                            x.desc == MirOriginDesc::Adjustment(i)
                        });
                        self.rewrite_from_mir_rws(Some(ex), adj_rws, hir_rw)
                    }
                });
        }

        // Apply late rewrites.
        for mir_rw in mir_rws {
            assert!(
                matches!(
                    mir_rw.desc,
                    MirOriginDesc::StoreIntoLocal | MirOriginDesc::LoadFromTemp
                ),
                "bad desc {:?} for late rewrite: {mir_rw:?}",
                mir_rw.desc,
            );
        }
        hir_rw = self.rewrite_from_mir_rws(Some(ex), mir_rws, hir_rw);

        if !matches!(hir_rw, Rewrite::Identity) {
            debug!(
                "rewrite {:?} at {:?} (materialize? {})",
                hir_rw, callsite_span, self.materialize_adjustments
            );
            self.rewrites.insert(ex.hir_id, (callsite_span, hir_rw));
        }
    }
}

fn mutbl_from_bool(m: bool) -> hir::Mutability {
    if m {
        hir::Mutability::Mut
    } else {
        hir::Mutability::Not
    }
}

fn apply_adjustment<'tcx>(
    tcx: TyCtxt<'tcx>,
    adjustment: &Adjustment<'tcx>,
    rw: Rewrite,
) -> Rewrite {
    match adjustment.kind {
        Adjust::NeverToAny => rw,
        Adjust::Deref(_) => Rewrite::Deref(Box::new(rw)),
        Adjust::Borrow(AutoBorrow::Ref(_, mutbl)) => Rewrite::Ref(Box::new(rw), mutbl.into()),
        Adjust::Borrow(AutoBorrow::RawPtr(mutbl)) => Rewrite::AddrOf(Box::new(rw), mutbl),
        Adjust::Pointer(PointerCast::Unsize) | Adjust::Pointer(PointerCast::MutToConstPointer) => {
            let target_ty = adjustment.target;
            let print_ty = |ty: Ty<'tcx>| -> String {
                let printer = FmtPrinter::new(tcx, Namespace::TypeNS);
                ty.print(printer).unwrap().into_buffer()
            };
            // Use structured rewrites where possible.
            // TODO: this should operate recursively and handle more cases
            let ty_rw = match *target_ty.kind() {
                TyKind::RawPtr(tm) => {
                    Rewrite::TyPtr(Box::new(Rewrite::Print(print_ty(tm.ty))), tm.mutbl)
                }
                TyKind::Ref(_, ty, mutbl) => Rewrite::TyRef(
                    LifetimeName::Elided,
                    Box::new(Rewrite::Print(print_ty(ty))),
                    mutbl,
                ),
                _ => Rewrite::Print(print_ty(target_ty)),
            };
            Rewrite::Cast(Box::new(rw), Box::new(ty_rw))
        }
        Adjust::Pointer(cast) => todo!("Adjust::Pointer({:?})", cast),
    }
}

enum AdjustmentStep {
    Before(usize),
    After(usize),
}

fn materialize_adjustments<'tcx>(
    tcx: TyCtxt<'tcx>,
    adjustments: &[Adjustment<'tcx>],
    hir_rw: Rewrite,
    mut callback: impl FnMut(AdjustmentStep, Rewrite) -> Rewrite,
) -> Rewrite {
    let adj_kinds: Vec<&_> = adjustments.iter().map(|a| &a.kind).collect();
    match (hir_rw, &adj_kinds[..]) {
        // The mut-to-const cast should be unneeded once the inner rewrite switches to a safe
        // reference type appropriate for the pointer's uses.  However, we still want to give
        // `callback` a chance to remove the cast itself so that if there's a `RemoveCast` rewrite
        // on this adjustment, we don't get an error about it failing to apply.
        (mut hir_rw, &[Adjust::Pointer(PointerCast::MutToConstPointer)]) => {
            hir_rw = callback(AdjustmentStep::Before(0), hir_rw);
            hir_rw = Rewrite::RemovedCast(Box::new(hir_rw));
            hir_rw = callback(AdjustmentStep::After(0), hir_rw);
            match hir_rw {
                Rewrite::RemovedCast(rw) => *rw,
                rw => rw,
            }
        }
        (mut hir_rw, _) => {
            for (i, adj) in adjustments.iter().enumerate() {
                hir_rw = callback(AdjustmentStep::Before(i), hir_rw);
                hir_rw = apply_adjustment(tcx, adj, hir_rw);
                hir_rw = callback(AdjustmentStep::After(i), hir_rw);
            }
            hir_rw
        }
    }
}

/// Generate code to zeroize an instance of `zero_ty` at place `lv`.  Returns an expression of type
/// `()`, which can be used as a statement by appending a semicolon.
fn generate_zeroize_code(zero_ty: &ZeroizeType, lv: &str) -> String {
    match *zero_ty {
        ZeroizeType::Int => format!("{lv} = 0"),
        ZeroizeType::Bool => format!("{lv} = false"),
        ZeroizeType::Option => format!("{lv} = None"),
        ZeroizeType::Array(ref elem_zero_ty) => format!(
            "
            {{
                for elem in {lv}.iter_mut() {{
                    {};
                }}
            }}
        ",
            generate_zeroize_code(elem_zero_ty, "(*elem)")
        ),
        ZeroizeType::Struct(_, ref fields) => {
            debug!("zeroize: {} fields on {lv}: {fields:?}", fields.len());
            let mut s = String::new();
            writeln!(s, "{{").unwrap();
            for (name, field_zero_ty) in fields {
                writeln!(
                    s,
                    "{};",
                    generate_zeroize_code(field_zero_ty, &format!("{lv}.{name}"))
                )
                .unwrap();
            }
            writeln!(s, "}}").unwrap();
            s
        }
    }
}

/// Generate an expression to produce a zeroized version of a value.
fn generate_zeroize_expr(zero_ty: &ZeroizeType) -> String {
    match *zero_ty {
        ZeroizeType::Int => format!("0"),
        ZeroizeType::Bool => format!("false"),
        ZeroizeType::Option => format!("None"),
        ZeroizeType::Array(ref elem_zero_ty) => format!(
            "std::array::from_fn(|| {})",
            generate_zeroize_expr(elem_zero_ty)
        ),
        ZeroizeType::Struct(ref name, ref fields) => {
            let mut s = String::new();
            write!(s, "{} {{\n", name).unwrap();
            for (name, field_zero_ty) in fields {
                write!(s, "{}: {},\n", name, generate_zeroize_expr(field_zero_ty),).unwrap();
            }
            write!(s, "}}\n").unwrap();
            s
        }
    }
}

fn take_prefix_while<'a, T>(slice: &mut &'a [T], mut pred: impl FnMut(&'a T) -> bool) -> &'a [T] {
    let i = slice.iter().position(|x| !pred(x)).unwrap_or(slice.len());
    let (a, b) = slice.split_at(i);
    *slice = b;
    a
}

/// Convert a single `RewriteKind` representing a cast into a `Span`-based `Rewrite`.  This panics
/// on rewrites that modify the original expression; only rewrites that wrap the expression in some
/// kind of cast or conversion are supported.
pub fn convert_cast_rewrite(kind: &mir_op::RewriteKind, hir_rw: Rewrite) -> Rewrite {
    match *kind {
        mir_op::RewriteKind::SliceFirst { mutbl } => {
            // `p` -> `&p[0]`
            let arr = hir_rw;
            let idx = Rewrite::LitZero;
            let elem = Rewrite::Index(Box::new(arr), Box::new(idx));
            Rewrite::Ref(Box::new(elem), mutbl_from_bool(mutbl))
        }

        mir_op::RewriteKind::Reborrow { mutbl } => {
            // `p` -> `&*p` / `&mut *p`
            let hir_rw = match fold_mut_to_imm(hir_rw) {
                Ok(folded_rw) => return folded_rw,
                Err(rw) => rw,
            };
            let place = Rewrite::Deref(Box::new(hir_rw));
            Rewrite::Ref(Box::new(place), mutbl_from_bool(mutbl))
        }

        mir_op::RewriteKind::Deref => {
            // `p` -> `*p`
            Rewrite::Deref(Box::new(hir_rw))
        }

        mir_op::RewriteKind::OptionUnwrap => {
            // `p` -> `p.unwrap()`
            Rewrite::MethodCall("unwrap".to_string(), Box::new(hir_rw), vec![])
        }
        mir_op::RewriteKind::OptionSome => {
            // `p` -> `Some(p)`
            Rewrite::Call("std::option::Option::Some".to_string(), vec![hir_rw])
        }

        mir_op::RewriteKind::OptionMapBegin => {
            // `p` -> `p.unwrap()`
            Rewrite::MethodCall("unwrap /*map_begin*/".to_string(), Box::new(hir_rw), vec![])
        }
        mir_op::RewriteKind::OptionMapEnd => {
            // `p` -> `Some(p)`
            Rewrite::Call(
                "std::option::Option::Some /*map_end*/".to_string(),
                vec![hir_rw],
            )
        }

        mir_op::RewriteKind::OptionDowngrade { mutbl, kind } => {
            // `p` -> `Some(p)`
            match kind {
                mir_op::OptionDowngradeKind::Borrow => {
                    let ref_method = if mutbl {
                        "as_mut".into()
                    } else {
                        "as_ref".into()
                    };
                    Rewrite::MethodCall(ref_method, Box::new(hir_rw), vec![])
                }
                mir_op::OptionDowngradeKind::Deref => {
                    let ref_method = if mutbl {
                        "as_deref_mut".into()
                    } else {
                        "as_deref".into()
                    };
                    Rewrite::MethodCall(ref_method, Box::new(hir_rw), vec![])
                }
                mir_op::OptionDowngradeKind::MoveAndDeref => {
                    let closure = if mutbl {
                        format_rewrite!("|ptr| &mut *ptr")
                    } else {
                        format_rewrite!("|ptr| &*ptr")
                    };
                    Rewrite::MethodCall("map".into(), Box::new(hir_rw), vec![closure])
                }
            }
        }

        mir_op::RewriteKind::DynOwnedUnwrap => {
            Rewrite::MethodCall("unwrap".to_string(), Box::new(hir_rw), vec![])
        }
        mir_op::RewriteKind::DynOwnedTake => {
            // `p` -> `mem::replace(&mut p, Err(()))`
            Rewrite::Call(
                "std::mem::replace".to_string(),
                vec![
                    Rewrite::Ref(Box::new(hir_rw), hir::Mutability::Mut),
                    Rewrite::Text("Err(())".into()),
                ],
            )
        }
        mir_op::RewriteKind::DynOwnedWrap => {
            Rewrite::Call("std::result::Result::<_, ()>::Ok".to_string(), vec![hir_rw])
        }

        mir_op::RewriteKind::DynOwnedDowngrade { mutbl } => {
            let ref_method = if mutbl {
                "as_deref_mut".into()
            } else {
                "as_deref".into()
            };
            let hir_rw = Rewrite::MethodCall(ref_method, Box::new(hir_rw), vec![]);
            Rewrite::MethodCall("unwrap".into(), Box::new(hir_rw), vec![])
        }

        mir_op::RewriteKind::CastRefToRaw { mutbl } => {
            // `addr_of!(*p)` is cleaner than `p as *const _`; we don't know the pointee
            // type here, so we can't emit `p as *const T`.
            let rw_pl = Rewrite::Deref(Box::new(hir_rw));
            Rewrite::AddrOf(Box::new(rw_pl), mutbl_from_bool(mutbl))
        }
        mir_op::RewriteKind::CastRawToRaw { to_mutbl } => {
            let method = if to_mutbl { "cast_mut" } else { "cast_const" };
            Rewrite::MethodCall(method.to_string(), Box::new(hir_rw), vec![])
        }
        mir_op::RewriteKind::UnsafeCastRawToRef { mutbl } => {
            let rw_pl = Rewrite::Deref(Box::new(hir_rw));
            Rewrite::Ref(Box::new(rw_pl), mutbl_from_bool(mutbl))
        }

        mir_op::RewriteKind::CellNew => {
            // `x` to `Cell::new(x)`
            Rewrite::Call("std::cell::Cell::new".to_string(), vec![hir_rw])
        }

        mir_op::RewriteKind::CellFromMut => {
            // `x` to `Cell::from_mut(x)`
            Rewrite::Call("std::cell::Cell::from_mut".to_string(), vec![hir_rw])
        }
        mir_op::RewriteKind::AsPtr => {
            // `x` to `x.as_ptr()`
            Rewrite::MethodCall("as_ptr".to_string(), Box::new(hir_rw), vec![])
        }
        mir_op::RewriteKind::CastRawMutToCellPtr { ref ty } => Rewrite::Cast(
            Box::new(hir_rw),
            Box::new(Rewrite::TyPtr(
                Box::new(Rewrite::TyCtor(
                    "std::cell::Cell".into(),
                    vec![Rewrite::Print(ty.to_string())],
                )),
                hir::Mutability::Not,
            )),
        ),

        _ => panic!(
            "rewrite {:?} is not supported by convert_cast_rewrite",
            kind
        ),
    }
}

/// Try to change `&mut e -> &e` or `&mut e as &mut T -> &e as &T`.  Returns `Ok` on success, or
/// `Err` with the original rewrite on failure.
fn fold_mut_to_imm(rw: Rewrite) -> Result<Rewrite, Rewrite> {
    // TODO: would be nice to make this work recursively through nested deref/ref chains
    fn check(rw: &Rewrite) -> bool {
        match *rw {
            Rewrite::Cast(ref ref_expr, ref ref_ty) => {
                matches!(**ref_expr, Rewrite::Ref(_, hir::Mutability::Mut))
                    && matches!(**ref_ty, Rewrite::TyRef(_, _, hir::Mutability::Mut))
            }
            Rewrite::Ref(_, hir::Mutability::Mut) => true,
            _ => false,
        }
    }
    if !check(&rw) {
        return Err(rw);
    }

    Ok(match rw {
        Rewrite::Cast(ref_expr, ref_ty) => {
            let expr = assert_matches!(*ref_expr, Rewrite::Ref(expr, _) => expr);
            let (lt, ty) = assert_matches!(*ref_ty, Rewrite::TyRef(lt, ty, _) => (lt, ty));
            Rewrite::Cast(
                Box::new(Rewrite::Ref(expr, hir::Mutability::Not)),
                Box::new(Rewrite::TyRef(lt, ty, hir::Mutability::Not)),
            )
        }
        Rewrite::Ref(expr, hir::Mutability::Mut) => Rewrite::Ref(expr, hir::Mutability::Not),
        _ => unreachable!(),
    })
}

/// Convert the MIR rewrites attached to each HIR node into `Span`-based `rewrite::Rewrite`s.
pub fn convert_rewrites(
    tcx: TyCtxt,
    hir_body_id: hir::BodyId,
    mir_rewrites: HashMap<HirId, Vec<DistRewrite>>,
) -> Vec<(Span, Rewrite)> {
    // Run the visitor.
    let typeck_results = tcx.typeck_body(hir_body_id);
    let hir = tcx.hir().body(hir_body_id);

    let mut v = ConvertVisitor {
        tcx,
        typeck_results,
        mir_rewrites,
        rewrites: HashMap::new(),
        subsumed_child_rewrites: RefCell::new(HashSet::new()),
        materialize_adjustments: false,
    };
    v.visit_body(hir);

    if !v.mir_rewrites.is_empty() {
        info!("leftover rewrites:");
        let count = v.mir_rewrites.len();
        for (hir_id, rws) in v.mir_rewrites {
            let ex = tcx.hir().expect_expr(hir_id);
            info!("  {:?}: {:?}, expr = {:?}", ex.span, rws, ex);
        }
        error!("found {} leftover rewrites", count);
    }

    let subsumed = v.subsumed_child_rewrites.into_inner();
    v.rewrites
        .into_iter()
        .filter(|&(hir_id, _)| !subsumed.contains(&hir_id))
        .map(|(_, (span, rw))| (span, rw))
        .collect()
}
