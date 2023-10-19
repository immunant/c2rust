use crate::panic_detail;
use crate::rewrite::expr::distribute::DistRewrite;
use crate::rewrite::expr::mir_op;
use crate::rewrite::expr::unlower::MirOriginDesc;
use crate::rewrite::Rewrite;
use assert_matches::assert_matches;
use log::*;
use rustc_hir as hir;
use rustc_hir::def::Namespace;
use rustc_hir::intravisit::{self, Visitor};
use rustc_hir::{ExprKind, HirId};
use rustc_middle::hir::nested_filter;
use rustc_middle::ty::adjustment::{Adjust, Adjustment, AutoBorrow, PointerCast};
use rustc_middle::ty::print::{FmtPrinter, Print};
use rustc_middle::ty::{TyCtxt, TypeckResults};
use rustc_span::Span;
use std::collections::HashMap;

struct ConvertVisitor<'tcx> {
    tcx: TyCtxt<'tcx>,
    typeck_results: &'tcx TypeckResults<'tcx>,
    mir_rewrites: HashMap<HirId, Vec<DistRewrite>>,
    rewrites: Vec<(Span, Rewrite)>,
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
        Rewrite::Sub(idx, sub_ex.span)
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

        let rewrite_from_mir_rws = |rw: &mir_op::RewriteKind, hir_rw: Rewrite| -> Rewrite {
            // Cases that extract a subexpression are handled here; cases that only wrap the
            // top-level expression (and thus can handle a non-`Identity` `hir_rw`) are handled by
            // `convert_cast_rewrite`.
            match rw {
                mir_op::RewriteKind::OffsetSlice { mutbl } => {
                    // `p.offset(i)` -> `&p[i as usize ..]`
                    assert!(matches!(hir_rw, Rewrite::Identity));
                    let arr = self.get_subexpr(ex, 0);
                    let idx = Rewrite::Cast(Box::new(self.get_subexpr(ex, 1)), "usize".to_owned());
                    let elem = Rewrite::SliceRange(Box::new(arr), Some(Box::new(idx)), None);
                    Rewrite::Ref(Box::new(elem), mutbl_from_bool(*mutbl))
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
                        _ => panic!("unexpected hir_rw {hir_rw:?} for RawToRef"),
                    }
                }

                mir_op::RewriteKind::RawToRef { mutbl } => {
                    // &raw _ to &_ or &raw mut _ to &mut _
                    match hir_rw {
                        Rewrite::Identity => {
                            Rewrite::Ref(Box::new(self.get_subexpr(ex, 0)), mutbl_from_bool(*mutbl))
                        }
                        Rewrite::AddrOf(rw, mutbl) => Rewrite::Ref(rw, mutbl),
                        _ => panic!("unexpected hir_rw {hir_rw:?} for RawToRef"),
                    }
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

        // Emit rewrites on subexpressions first, then emit the rewrite on the expression itself,
        // if it's nontrivial.
        let applied_mir_rewrite = !matches!(hir_rw, Rewrite::Identity);
        self.with_materialize_adjustments(applied_mir_rewrite, |this| {
            intravisit::walk_expr(this, ex);
        });

        if !matches!(hir_rw, Rewrite::Identity) {
            eprintln!(
                "rewrite {:?} at {:?} (materialize? {})",
                hir_rw, callsite_span, self.materialize_adjustments
            );
            self.rewrites.push((callsite_span, hir_rw));
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
        Adjust::Pointer(PointerCast::Unsize) |
        Adjust::Pointer(PointerCast::MutToConstPointer) => {
            let ty = adjustment.target;
            let printer = FmtPrinter::new(tcx, Namespace::TypeNS);
            let s = ty.print(printer).unwrap().into_buffer();
            Rewrite::Cast(Box::new(rw), s)
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
        (rw @ Rewrite::Ref(..), &[Adjust::Deref(..), Adjust::Borrow(..)]) => rw,
        (rw @ Rewrite::MethodCall(..), &[Adjust::Deref(..), Adjust::Borrow(..)]) => rw,
        (rw, &[]) => rw,
        (rw, adjs) => panic!("rewrite {rw:?} and materializations {adjs:?} NYI"),
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
        mir_op::RewriteKind::CastRawMutToCellPtr { ref ty } => {
            Rewrite::Cast(Box::new(hir_rw), format!("*const std::cell::Cell<{}>", ty))
        }

        _ => panic!(
            "rewrite {:?} is not supported by convert_cast_rewrite",
            kind
        ),
    }
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
        rewrites: Vec::new(),
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

    v.rewrites
}
