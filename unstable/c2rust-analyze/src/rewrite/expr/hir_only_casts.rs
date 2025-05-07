//! Special handling for casts that exist only in the HIR, for which no MIR is emitted by rustc.
//!
//! One example is the "two-part address-of" operation, which looks like `&x as *const T` or `&mut
//! x as *mut T`.  The lowering to MIR omits the cast and instead adds a `Borrow(RawPtr)`
//! adjustment on `&x`.  This module looks for occurrences of this pattern and generates `Rewrite`s
//! to remove the cast if the `&x` part is rewritten.
//!
//! This logic is separate from the normal `unlower`/`distribute`/`convert` machinery because it
//! involves generating HIR rewrites on code with no corresponding MIR, whereas the normal code
//! path starts with rewrites on MIR and lifts them up to the HIR level.

use crate::rewrite::Rewrite;
use log::debug;
use rustc_hir as hir;
use rustc_hir::intravisit::{self, Visitor};
use rustc_middle::hir::nested_filter;
use rustc_middle::ty::adjustment::{Adjust, AutoBorrow};
use rustc_middle::ty::{TyCtxt, TypeckResults};
use rustc_span::Span;

struct HirOnlyCastVisitor<'tcx, F> {
    tcx: TyCtxt<'tcx>,
    typeck_results: &'tcx TypeckResults<'tcx>,
    filter: F,
    rewrites: Vec<(Span, Rewrite)>,
}

/// If `ex` is an expression of the form `&x as *const T` or `&mut x as *mut T`, return `&x` /
/// `&mut x` and the `Mutability` (for convenience).
fn match_two_part_address_of<'tcx>(
    ex: &'tcx hir::Expr<'tcx>,
) -> Option<(&'tcx hir::Expr<'tcx>, hir::Mutability)> {
    let (cast_expr, cast_ty) = match ex.kind {
        hir::ExprKind::Cast(e, t) => (e, t),
        _ => return None,
    };
    let expr_mutbl = match cast_expr.kind {
        hir::ExprKind::AddrOf(hir::BorrowKind::Ref, mutbl, _) => mutbl,
        _ => return None,
    };
    let ty_mutbl = match cast_ty.kind {
        hir::TyKind::Ptr(hir::MutTy { ty: _, mutbl }) => mutbl,
        _ => return None,
    };
    if expr_mutbl != ty_mutbl {
        return None;
    }
    Some((cast_expr, expr_mutbl))
}

impl<'tcx, F> HirOnlyCastVisitor<'tcx, F> {
    fn expr_has_address_of_adjustments(
        &self,
        ex: &'tcx hir::Expr<'tcx>,
        mutbl: hir::Mutability,
    ) -> bool {
        let adjusts = self.typeck_results.expr_adjustments(ex);
        // Rustc implements the ref-to-rawptr coercion by adding `Deref` and `Borrow(RawPtr)`
        // adjustments at the end of `expr_adjustments`.  Check if those adjustments are present.
        let (kind1, kind2) = match *adjusts {
            [.., ref x, ref y] => (&x.kind, &y.kind),
            _ => return false,
        };

        match *kind1 {
            Adjust::Deref(None) => {}
            _ => return false,
        }

        match *kind2 {
            Adjust::Borrow(AutoBorrow::RawPtr(adj_mutbl)) if adj_mutbl == mutbl => {}
            _ => return false,
        }

        true
    }
}

impl<'tcx, F> Visitor<'tcx> for HirOnlyCastVisitor<'tcx, F>
where
    F: FnMut(&'tcx hir::Expr<'tcx>) -> bool,
{
    type NestedFilter = nested_filter::OnlyBodies;

    fn nested_visit_map(&mut self) -> Self::Map {
        self.tcx.hir()
    }

    fn visit_expr(&mut self, ex: &'tcx hir::Expr<'tcx>) {
        // Check for the syntactic pattern of a two-part address-of.
        if let Some((ref_expr, mutbl)) = match_two_part_address_of(ex) {
            debug!(
                "found two-part address-of pattern at {:?}, {:?}, {:?}",
                ex.span, ref_expr.span, mutbl
            );
            // Check whether the `&x` / `&mut x` expr got the expected adjustments.
            if self.expr_has_address_of_adjustments(ref_expr, mutbl) {
                if (self.filter)(ref_expr) {
                    // Emit a rewrite to remove the cast, leaving only the inner `&x`.
                    debug!("  emit rewrite for expr at {:?}", ref_expr.span);
                    self.rewrites
                        .push((ex.span, Rewrite::Sub(0, ref_expr.span)));
                } else {
                    debug!("  filter rejected expr at {:?}", ref_expr.span);
                }
            } else {
                debug!("  missing adjustments for expr at {:?}", ref_expr.span);
            }
        }

        // Check for a cast that's made redundant by an adjustment on the inner expression.
        if let hir::ExprKind::Cast(src_expr, _dest_ty) = ex.kind {
            let src_adjusts = self.typeck_results.expr_adjustments(src_expr);
            if let Some(last_adjust) = src_adjusts.last() {
                if matches!(last_adjust.kind, Adjust::Pointer(_))
                    && last_adjust.target == self.typeck_results.expr_ty(ex)
                {
                    // `ex` has the form `x as T`, where `x` has a pointer adjustment and its
                    // final adjusted type is identical to `T`.  In this case, rustc skips
                    // generating MIR for this cast.
                    if (self.filter)(src_expr) {
                        self.rewrites
                            .push((ex.span, Rewrite::Sub(0, src_expr.span)));
                    }
                }
            }
        }

        intravisit::walk_expr(self, ex);
    }
}

/// Generate rewrites that remove HIR-only casts where the `filter` indicates (by returning `true`)
/// that the inner expression has rewrites.
pub fn remove_hir_only_casts<'tcx>(
    tcx: TyCtxt<'tcx>,
    hir_body_id: hir::BodyId,
    filter: impl FnMut(&'tcx hir::Expr<'tcx>) -> bool,
) -> Vec<(Span, Rewrite)> {
    let hir = tcx.hir().body(hir_body_id);
    let typeck_results = tcx.typeck_body(hir_body_id);
    let mut visitor = HirOnlyCastVisitor {
        tcx,
        typeck_results,
        filter,
        rewrites: Vec::new(),
    };
    visitor.visit_body(hir);
    visitor.rewrites
}
