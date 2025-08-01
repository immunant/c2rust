use crate::context::LTy;
use crate::context::{Assignment, FlagSet, GlobalAnalysisCtxt};
use crate::rewrite::expr::{self, CastBuilder};
use crate::rewrite::ty;
use crate::rewrite::Rewrite;
use crate::type_desc::{self, TypeDesc};
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
    asn: &'a Assignment,
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
                !ptr.is_none() && !self.asn.flags[ptr].contains(FlagSet::FIXED)
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

/// For each non-rewritable function that calls or mentions a rewritable function meeting certain
/// criteria, generate rewrites to change calls to `foo` into calls to `foo_shim`.  Also produces a
/// set of callee `DefId`s for the calls that were rewritten this way.
///
/// The criteria we look for are:
/// * The callee must be a local function.
/// * The callee must have at least one non-`FIXED` pointer type in its signature.
/// * The callee must not be a trait method.  Adding shims for trait methods is more complex than
///   handling free functions or inherent methods.
pub fn gen_shim_call_rewrites<'tcx>(
    gacx: &GlobalAnalysisCtxt<'tcx>,
    asn: &Assignment,
) -> (Vec<(Span, Rewrite)>, HashSet<DefId>) {
    let tcx = gacx.tcx;

    let mut rewrites = Vec::new();
    let mut mentioned_fns = HashSet::new();

    for skip_def_id in gacx.iter_fns_skip_rewrite() {
        let skip_def_id = match skip_def_id.as_local() {
            Some(x) => x,
            None => continue,
        };
        // When using --rewrite-paths, fns in extern blocks may show up here.  We can't do anything
        // with these, since they don't have a HIR body, so skip them.
        let hir_body_id = match tcx.hir().maybe_body_owned_by(skip_def_id) {
            Some(x) => x,
            None => continue,
        };
        let hir = tcx.hir().body(hir_body_id);
        let typeck_results = tcx.typeck_body(hir_body_id);
        let mut v = ShimCallVisitor {
            gacx,
            asn,
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
    asn: &Assignment,
    lty: LTy<'tcx>,
) -> Option<(TypeDesc<'tcx>, TypeDesc<'tcx>)> {
    let ptr = lty.label;
    if ptr.is_none() || asn.flags[ptr].contains(FlagSet::FIXED) {
        return None;
    }

    let desc = type_desc::perms_to_desc(lty.ty, asn.perms[ptr], asn.flags[ptr]);
    let fixed_desc = type_desc::perms_to_desc_with_pointee(
        tcx,
        desc.pointee_ty,
        lty.ty,
        asn.perms[ptr],
        asn.flags[ptr] | FlagSet::FIXED,
    );
    Some((desc, fixed_desc))
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum ManualShimCasts {
    No,
    /// Emit `todo!("cast X from T1 to T2")` instead of panicking when a cast can't be generated.
    /// The user can then implement the necessary casts manually.
    Yes,
}

pub fn gen_shim_definition_rewrite<'tcx>(
    gacx: &GlobalAnalysisCtxt<'tcx>,
    asn: &Assignment,
    def_id: DefId,
    manual_casts: ManualShimCasts,
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

    // 1 cast per arg, 1 call, 1 cast for the result.  The final result is returned using the
    // trailing expression of the block.
    let mut stmts = Vec::with_capacity(arg_tys.len() + 2);

    // Generate `let safe_arg0 = arg0 as ...;` for each argument.
    let mut arg_exprs = Vec::with_capacity(arg_tys.len());
    for (i, arg_lty) in lsig.inputs.iter().enumerate() {
        let mut hir_rw = Rewrite::FnArg(i);

        if let Some((arg_desc, fixed_desc)) = lty_to_desc_pair(tcx, asn, arg_lty) {
            let mut cast_builder = CastBuilder::new(tcx, &asn.perms, &asn.flags, |rk| {
                hir_rw = expr::convert_cast_rewrite(&rk, mem::take(&mut hir_rw));
            });
            match cast_builder.try_build_cast_desc_desc(fixed_desc, arg_desc) {
                Ok(()) => {}
                Err(e) => {
                    if manual_casts == ManualShimCasts::Yes {
                        hir_rw = Rewrite::Print(format!(
                            r#"todo!("cast arg{i} from {} to {}")"#,
                            ty::desc_to_ty(tcx, fixed_desc),
                            ty::desc_to_ty(tcx, arg_desc),
                        ));
                    } else {
                        panic!("error generating cast for {:?} arg{}: {}", def_id, i, e);
                    }
                }
            }
        } else {
            // No-op.  `arg_lty` is a FIXED pointer, which doesn't need a cast; the shim argument
            // type is the same as the argument type of the wrapped function.
        }

        let safe_name = format!("safe_arg{}", i);
        stmts.push(Rewrite::Let1(safe_name.clone(), Box::new(hir_rw)));
        arg_exprs.push(Rewrite::Print(safe_name));
    }

    // Generate the call: `let safe_result = f(safe_arg0, safe_arg1);`
    let call_rw = Rewrite::Call(owner_node.ident().unwrap().as_str().to_owned(), arg_exprs);
    stmts.push(Rewrite::Let1("safe_result".into(), Box::new(call_rw)));

    // Generate `let result = safe_result as ...;`
    let mut result_rw = Rewrite::Print("safe_result".into());
    if let Some((return_desc, fixed_desc)) = lty_to_desc_pair(tcx, asn, lsig.output) {
        let mut cast_builder = CastBuilder::new(tcx, &asn.perms, &asn.flags, |rk| {
            result_rw = expr::convert_cast_rewrite(&rk, mem::take(&mut result_rw));
        });
        match cast_builder.try_build_cast_desc_desc(return_desc, fixed_desc) {
            Ok(()) => {}
            Err(e) => {
                if manual_casts == ManualShimCasts::Yes {
                    result_rw = Rewrite::Print(format!(
                        r#"todo!("cast safe_result from {} to {}")"#,
                        ty::desc_to_ty(tcx, return_desc),
                        ty::desc_to_ty(tcx, fixed_desc),
                    ));
                } else {
                    panic!("error generating cast for {:?} result: {}", def_id, e);
                }
            }
        }
    }
    stmts.push(Rewrite::Let1("result".into(), Box::new(result_rw)));

    // Build the function body.
    let body_rw = Rewrite::Block(stmts, Some(Box::new(Rewrite::Print("result".into()))));

    let rw = Rewrite::DefineFn {
        name: format!("{}_shim", owner_node.ident().unwrap().as_str()),
        arg_tys,
        return_ty,
        body: Box::new(body_rw),
    };
    (insert_span, rw)
}
