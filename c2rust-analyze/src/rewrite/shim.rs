use crate::context::{FlagSet, GlobalAnalysisCtxt, GlobalAssignment};
use crate::rewrite::expr::{self, CastBuilder};
use crate::rewrite::Rewrite;
use crate::type_desc::{self, TypeDesc};
use crate::LTy;
use rustc_hir::def::{DefKind, Res};
use rustc_hir::def_id::DefId;
use rustc_hir::intravisit::{self, Visitor};
use rustc_hir::{Expr, ExprKind, FnRetTy};
use rustc_middle::hir::nested_filter;
use rustc_middle::ty::{DefIdTree, TyCtxt, TypeckResults};
use rustc_span::Span;
use std::collections::HashSet;
use std::mem;

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
            .inputs_and_output()
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

impl<'a, 'tcx> Visitor<'tcx> for ShimCallVisitor<'a, 'tcx> {
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
        v.visit_body(hir);

        rewrites = v.rewrites;
        mentioned_fns = v.mentioned_fns;
    }

    (rewrites, mentioned_fns)
}

/// Convert an `LTy` to a pair of `TypeDesc`s, one computed normally and one with `FIXED` added.
/// Returns `None` if the input `LTy` already has `FIXED` set.
fn lty_to_desc_pair<'tcx>(
    tcx: TyCtxt<'tcx>,
    gasn: &GlobalAssignment,
    lty: LTy<'tcx>,
) -> Option<(TypeDesc<'tcx>, TypeDesc<'tcx>)> {
    let ptr = lty.label;
    if ptr.is_none() || gasn.flags[ptr].contains(FlagSet::FIXED) {
        return None;
    }

    let desc = type_desc::perms_to_desc(lty.ty, gasn.perms[ptr], gasn.flags[ptr]);
    let fixed_desc = type_desc::perms_to_desc_with_pointee(
        tcx,
        desc.pointee_ty,
        lty.ty,
        gasn.perms[ptr],
        gasn.flags[ptr] | FlagSet::FIXED,
    );
    Some((desc, fixed_desc))
}

pub fn gen_shim_definition_rewrite<'tcx>(
    gacx: &GlobalAnalysisCtxt<'tcx>,
    gasn: &GlobalAssignment,
    def_id: DefId,
) -> (Span, Rewrite) {
    let tcx = gacx.tcx;

    let owner_node = tcx.hir().expect_owner(def_id.as_local().unwrap());
    let insert_span = owner_node.span().shrink_to_hi();

    let fn_decl = owner_node.fn_decl().unwrap();
    let arg_tys = fn_decl
        .inputs
        .iter()
        .map(|ty| Rewrite::Extract(ty.span))
        .collect::<Vec<_>>();
    let return_ty = match fn_decl.output {
        FnRetTy::DefaultReturn(..) => None,
        FnRetTy::Return(ty) => Some(Box::new(Rewrite::Extract(ty.span))),
    };

    // `def_id` should always refer to a rewritten function, and all rewritten functions have
    // valid `fn_sigs` entries.
    let lsig = gacx.fn_sigs[&def_id];

    let mut arg_exprs = Vec::with_capacity(arg_tys.len());
    for (i, arg_lty) in lsig.inputs.iter().enumerate() {
        let mut hir_rw = Rewrite::FnArg(i);

        let (arg_desc, fixed_desc) = match lty_to_desc_pair(tcx, gasn, arg_lty) {
            Some(x) => x,
            None => {
                // `arg_lty` is a FIXED pointer, which doesn't need a cast; the shim argument
                // type is the same as the argument type of the wrapped function.
                arg_exprs.push(hir_rw);
                continue;
            }
        };

        let mut cast_builder = CastBuilder::new(tcx, &gasn.perms, &gasn.flags, |rk| {
            hir_rw = expr::convert_cast_rewrite(&rk, mem::take(&mut hir_rw));
        });
        cast_builder.build_cast_desc_desc(fixed_desc, arg_desc);
        arg_exprs.push(hir_rw);
    }

    let mut body_rw = Rewrite::Call(owner_node.ident().unwrap().as_str().to_owned(), arg_exprs);

    if let Some((return_desc, fixed_desc)) = lty_to_desc_pair(tcx, gasn, lsig.output) {
        let mut cast_builder = CastBuilder::new(tcx, &gasn.perms, &gasn.flags, |rk| {
            body_rw = expr::convert_cast_rewrite(&rk, mem::take(&mut body_rw));
        });
        cast_builder.build_cast_desc_desc(return_desc, fixed_desc);
    }

    let rw = Rewrite::DefineFn {
        name: format!("{}_shim", owner_node.ident().unwrap().as_str()),
        arg_tys,
        return_ty,
        body: Box::new(body_rw),
    };
    (insert_span, rw)
}
