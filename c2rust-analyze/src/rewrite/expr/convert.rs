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
                eprintln!(
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
        self.with_materialize_adjustments(mir_rws.len() > 0, |this| {
            intravisit::walk_expr(this, ex);
        });

        let rewrite_from_mir_rws = |rw: &mir_op::RewriteKind, hir_rw: Rewrite| -> Rewrite {
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

                mir_op::RewriteKind::MemcpySafe {
                    elem_size,
                    dest_single,
                    src_single,
                } => {
                    // `memcpy(dest, src, n)` to a `copy_from_slice` call
                    assert!(matches!(hir_rw, Rewrite::Identity));
                    assert!(!dest_single, "&T -> &[T] conversion for memcpy dest NYI");
                    assert!(!src_single, "&T -> &[T] conversion for memcpy src NYI");
                    Rewrite::Block(
                        vec![
                            Rewrite::Let(vec![
                                ("dest".into(), self.get_subexpr(ex, 0)),
                                ("src".into(), self.get_subexpr(ex, 1)),
                                ("byte_len".into(), self.get_subexpr(ex, 2)),
                            ]),
                            Rewrite::Let(vec![(
                                "n".into(),
                                format_rewrite!("byte_len as usize / {elem_size}"),
                            )]),
                            Rewrite::MethodCall(
                                "copy_from_slice".into(),
                                Box::new(format_rewrite!("dest[..n]")),
                                vec![format_rewrite!("&src[..n]")],
                            ),
                        ],
                        Some(Box::new(format_rewrite!("dest"))),
                    )
                }

                mir_op::RewriteKind::MemsetZeroize {
                    ref zero_ty,
                    elem_size,
                    dest_single,
                } => {
                    // `memcpy(dest, src, n)` to a `copy_from_slice` call
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
                            Rewrite::Let(vec![(
                                "n".into(),
                                format_rewrite!("byte_len as usize / {elem_size}"),
                            )]),
                            format_rewrite!("assert_eq!(val, 0, \"non-zero memset NYI\")"),
                            zeroize_body,
                        ],
                        Some(Box::new(format_rewrite!("dest"))),
                    )
                }

                mir_op::RewriteKind::CellGet => {
                    // `*x` to `Cell::get(x)`
                    assert!(matches!(hir_rw, Rewrite::Identity));
                    Rewrite::MethodCall(
                        "get".to_string(),
                        Box::new(self.get_subexpr(ex, 0)),
                        vec![],
                    )
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
        };

        // Apply rewrites on the expression itself.  These will be the first rewrites in the sorted
        // list produced by `distribute`.
        let expr_rws = take_prefix_while(&mut mir_rws, |x: &DistRewrite| {
            matches!(x.desc, MirOriginDesc::Expr)
        });
        for mir_rw in expr_rws {
            hir_rw = rewrite_from_mir_rws(&mir_rw.rw, hir_rw);
        }

        // Materialize adjustments if requested by an ancestor or required locally.
        let has_adjustment_rewrites = mir_rws
            .iter()
            .any(|x| matches!(x.desc, MirOriginDesc::Adjustment(_)));
        if self.materialize_adjustments || has_adjustment_rewrites {
            let adjusts = self.typeck_results.expr_adjustments(ex);
            hir_rw = materialize_adjustments(self.tcx, adjusts, hir_rw, |i, mut hir_rw| {
                let adj_rws =
                    take_prefix_while(&mut mir_rws, |x| x.desc == MirOriginDesc::Adjustment(i));
                for mir_rw in adj_rws {
                    eprintln!("would apply {mir_rw:?} for adjustment #{i}, over {hir_rw:?}");
                    hir_rw = rewrite_from_mir_rws(&mir_rw.rw, hir_rw);
                }
                hir_rw
            });
        }

        // Apply late rewrites.
        for mir_rw in mir_rws {
            assert!(matches!(
                mir_rw.desc,
                MirOriginDesc::StoreIntoLocal | MirOriginDesc::LoadFromTemp
            ));
            hir_rw = rewrite_from_mir_rws(&mir_rw.rw, hir_rw);
        }

        if !matches!(hir_rw, Rewrite::Identity) {
            eprintln!(
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

fn apply_identity_adjustment<'tcx>(
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

fn materialize_adjustments<'tcx>(
    tcx: TyCtxt<'tcx>,
    adjustments: &[Adjustment<'tcx>],
    hir_rw: Rewrite,
    mut callback: impl FnMut(usize, Rewrite) -> Rewrite,
) -> Rewrite {
    let adj_kinds: Vec<&_> = adjustments.iter().map(|a| &a.kind).collect();
    match (hir_rw, &adj_kinds[..]) {
        (Rewrite::Identity, []) => Rewrite::Identity,
        (Rewrite::Identity, _) => {
            let mut hir_rw = Rewrite::Identity;
            for (i, adj) in adjustments.iter().enumerate() {
                hir_rw = apply_identity_adjustment(tcx, adj, hir_rw);
                hir_rw = callback(i, hir_rw);
            }
            hir_rw
        }
        // TODO: ideally we should always materialize all adjustments (removing these special
        // cases), and use `MirRewrite`s to eliminate any adjustments we no longer need.
        (rw @ Rewrite::Ref(..), &[Adjust::Deref(..), Adjust::Borrow(..)]) => rw,
        (rw @ Rewrite::MethodCall(..), &[Adjust::Deref(..), Adjust::Borrow(..)]) => rw,
        // The mut-to-const cast should be unneeded once the inner rewrite switches to a safe
        // reference type appropriate for the pointer's uses.  However, we still want to give
        // `callback` a chance to remove the cast itself so that if there's a `RemoveCast` rewrite
        // on this adjustment, we don't get an error about it failing to apply.
        (rw, &[Adjust::Pointer(PointerCast::MutToConstPointer)]) => {
            let mut hir_rw = Rewrite::RemovedCast(Box::new(rw));
            hir_rw = callback(0, hir_rw);
            match hir_rw {
                Rewrite::RemovedCast(rw) => *rw,
                rw => rw,
            }
        }
        (rw, &[]) => rw,
        (rw, adjs) => panic!("rewrite {rw:?} and materializations {adjs:?} NYI"),
    }
}

/// Generate code to zeroize an instance of `zero_ty` at place `lv`.  Returns an expression of type
/// `()`, which can be used as a statement by appending a semicolon.
fn generate_zeroize_code(zero_ty: &ZeroizeType, lv: &str) -> String {
    match *zero_ty {
        ZeroizeType::Int => format!("{lv} = 0"),
        ZeroizeType::Bool => format!("{lv} = false"),
        ZeroizeType::Iterable(ref elem_zero_ty) => format!(
            "
            {{
                for elem in {lv}.iter_mut() {{
                    {};
                }}
            }}
        ",
            generate_zeroize_code(elem_zero_ty, "(*elem)")
        ),
        ZeroizeType::Struct(ref fields) => {
            eprintln!("zeroize: {} fields on {lv}: {fields:?}", fields.len());
            let mut s = String::new();
            write!(s, "{{\n").unwrap();
            for (name, field_zero_ty) in fields {
                write!(
                    s,
                    "{};\n",
                    generate_zeroize_code(field_zero_ty, &format!("{lv}.{name}"))
                )
                .unwrap();
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

        mir_op::RewriteKind::MutToImm => {
            // `p` -> `&*p`
            let hir_rw = match fold_mut_to_imm(hir_rw) {
                Ok(folded_rw) => return folded_rw,
                Err(rw) => rw,
            };
            let place = Rewrite::Deref(Box::new(hir_rw));
            Rewrite::Ref(Box::new(place), hir::Mutability::Not)
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
                    vec![Rewrite::Print(format!("{}", ty))],
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
