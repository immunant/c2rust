use crate::context::{FlagSet, GlobalAnalysisCtxt, GlobalAssignment};
use crate::rewrite::Rewrite;
use rustc_hir::def::{DefKind, Res};
use rustc_hir::def_id::DefId;
use rustc_hir::intravisit;
use rustc_hir::{Expr, ExprKind};
use rustc_middle::hir::nested_filter;
use rustc_middle::ty::{DefIdTree, TypeckResults};
use rustc_span::Span;
use std::collections::HashSet;
use std::iter;

struct ShimCallVisitor<'a, 'tcx> {
    gacx: &'a GlobalAnalysisCtxt<'tcx>,
    gasn: &'a GlobalAssignment,
    typeck_results: &'tcx TypeckResults<'tcx>,
    rewrites: Vec<(Span, Rewrite)>,
    mentioned_fns: HashSet<DefId>,
}

impl<'a, 'tcx> ShimCallVisitor<'a, 'tcx> {
    fn handle_def_mention(&mut self, def_id: DefId, span: Span) {
        let tcx = self.gacx.tcx;

        // We only care about mentions of local functions, not including trait methods.
        if !def_id.is_local() {
            return;
        }

        match tcx.def_kind(def_id) {
            DefKind::Fn => {}
            DefKind::AssocFn => {
                let parent_def_id = tcx.parent(def_id);
                if tcx.def_kind(parent_def_id) == DefKind::Trait {
                    return;
                }
                if tcx.impl_trait_ref(parent_def_id).is_some() {
                    // Ignore calls to trait methods.
                    return;
                }
            }
            _ => return,
        }

        // Only functions whose signatures might change are relevant here.  Check that the function
        // has at least one non-`FIXED` pointer in its signature.
        let lsig = match self.gacx.fn_sigs.get(&def_id) {
            Some(x) => x,
            None => return,
        };
        let has_non_fixed_ptr = lsig
            .inputs
            .iter()
            .cloned()
            .chain(iter::once(lsig.output))
            .flat_map(|lty| lty.iter())
            .any(|lty| {
                let ptr = lty.label;
                !ptr.is_none() && !self.gasn.flags[ptr].contains(FlagSet::FIXED)
            });
        if !has_non_fixed_ptr {
            return;
        }

        // Change this mention to refer to the function's unsafe shim instead.
        let insert_span = span.shrink_to_hi();
        self.rewrites
            .push((insert_span, Rewrite::Text("_shim".to_owned())));
        self.mentioned_fns.insert(def_id);
    }
}

impl<'a, 'tcx> intravisit::Visitor<'tcx> for ShimCallVisitor<'a, 'tcx> {
    type NestedFilter = nested_filter::OnlyBodies;

    fn nested_visit_map(&mut self) -> Self::Map {
        self.gacx.tcx.hir()
    }

    fn visit_expr(&mut self, ex: &'tcx Expr<'tcx>) {
        match ex.kind {
            ExprKind::Path(ref qp) => {
                let res = self.typeck_results.qpath_res(qp, ex.hir_id);
                match res {
                    Res::Def(DefKind::Fn, def_id) | Res::Def(DefKind::AssocFn, def_id) => {
                        let ident_span = qp.last_segment_span();
                        self.handle_def_mention(def_id, ident_span);
                    }
                    _ => {}
                }
            }

            ExprKind::MethodCall(ps, _, _) => {
                if let Some(def_id) = self.typeck_results.type_dependent_def_id(ex.hir_id) {
                    self.handle_def_mention(def_id, ps.ident.span);
                }
            }

            _ => {}
        }

        intravisit::walk_expr(self, ex);
    }
}

/// For each failed function that calls or mentions a non-failed function meeting certain criteria,
/// generate rewrites to change calls to `foo` into calls to `foo_shim`.  Also produces a set of
/// callee `DefId`s for the calls that were rewritten this way.
///
/// The criteria we look for are:
/// * The callee must be a local function.
/// * The callee must have at least one non-`FIXED` pointer type in its signature.
/// * The callee must not be a trait method.  Adding shims for trait methods is more complex than
///   handling free functions or inherent methods.
pub fn gen_shim_call_rewrites<'tcx>(
    gacx: &GlobalAnalysisCtxt<'tcx>,
    gasn: &GlobalAssignment,
) -> (Vec<(Span, Rewrite)>, HashSet<DefId>) {
    let tcx = gacx.tcx;

    let mut rewrites = Vec::new();
    let mut mentioned_fns = HashSet::new();

    for &failed_def_id in gacx.fns_failed.keys() {
        let failed_def_id = match failed_def_id.as_local() {
            Some(x) => x,
            None => continue,
        };
        let hir_body_id = tcx.hir().body_owned_by(failed_def_id);
        let hir = tcx.hir().body(hir_body_id);
        let typeck_results = tcx.typeck_body(hir_body_id);
        let mut v = ShimCallVisitor {
            gacx,
            gasn,
            typeck_results,
            rewrites,
            mentioned_fns,
        };
        intravisit::Visitor::visit_body(&mut v, hir);

        rewrites = v.rewrites;
        mentioned_fns = v.mentioned_fns;
    }

    (rewrites, mentioned_fns)
}
