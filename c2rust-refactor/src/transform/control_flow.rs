use log::debug;
use rustc_hir::HirId;
use rustc_infer::infer::TyCtxtInferExt;
use rustc_middle::hir::place::PlaceWithHirId;
use rustc_middle::mir::FakeReadCause;
use rustc_middle::ty::{self, ParamEnv};
use rustc_typeck::expr_use_visitor::*;
use rustc_ast::{Crate, Expr, ExprKind, Lit, LitKind, Stmt, StmtKind};
use rustc_ast::ptr::P;

use crate::command::{CommandState, Registry};
use crate::context::HirMap;
use crate::driver::Phase;
use crate::match_or;
use crate::matcher::{MatchCtxt, Subst, replace_expr, mut_visit_match_with, find_first};
use crate::transform::Transform;
use crate::RefactorCtxt;
use crate::ast_builder::mk;


/// # `reconstruct_while` Command
/// 
/// Obsolete - the translator now does this automatically.
/// 
/// Usage: `reconstruct_while`
/// 
/// Replaces all instances of `loop { if !cond { break; } ... }` with `while` loops.
pub struct ReconstructWhile;

impl Transform for ReconstructWhile {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        let krate = replace_expr(
            st, cx, krate,
            r#"
                $'label:?Ident: loop {
                    if !($cond:Expr) {
                        break;
                    }
                    $body:MultiStmt;
                }
            "#,
            r#"
                $'label: while $cond {
                    $body:MultiStmt;
                }
            "#);
        krate
    }
}


/// # `reconstruct_for_range` Command
/// 
/// Usage: `reconstruct_for_range`
/// 
/// Replaces `i = start; while i < end { ...; i += step; }` with
/// `for i in (start .. end).step_by(step) { ...; }`.
///
/// This takes a pretty conservative approach: the command only replaces the loop
/// if the induction variable is written exactly once inside the loop (by the
/// increment statement) and never read outside the loop.
pub struct ReconstructForRange;

impl Transform for ReconstructForRange {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        let mut mcx = MatchCtxt::new(st, cx);
        let pat_str = r#"
            $i:Expr = $start:Expr;
            $'label:?Ident: while $cond:Expr {
                $body:MultiStmt;
                $incr:Stmt;
            }"#;
        let pat = mcx.parse_stmts(&pat_str);

        let lt_cond = mcx.parse_expr("$i < $end:Expr");
        let le_cond = mcx.parse_expr("$i <= $end:Expr");

        let i_plus_eq = mcx.parse_expr("$i += $step:Expr");
        let i_eq_plus = mcx.parse_expr("$i = $i + $step:Expr");

        let range_one_excl = mcx.parse_stmts("$'label: for $ipat:Pat in $start .. $end { $body; }");
        let range_one_incl = mcx.parse_stmts("$'label: for $ipat:Pat in $start ..= $end { $body; }");
        let range_step_excl = mcx.parse_stmts("$'label: for $ipat:Pat in ($start .. $end).step_by($step as usize) { $body; }");
        let range_step_incl = mcx.parse_stmts("$'label: for $ipat:Pat in ($start ..= $end).step_by($step as usize) { $body; }");

        mut_visit_match_with(mcx, pat, krate, |orig, mut mcx| {
            let cond = mcx.bindings.get::<_, P<Expr>>("$cond").unwrap().clone();
            let range_excl = if mcx.try_match(&*lt_cond, &cond).is_ok() {
                true
            } else if mcx.try_match(&*le_cond, &cond).is_ok() {
                false
            } else {
                return;
            };

            let incr = match mcx.bindings.get::<_, Stmt>("$incr").unwrap().kind {
                StmtKind::Semi(ref e) |
                StmtKind::Expr(ref e) => e.clone(),
                _ => { return; }
            };
            if !mcx.try_match(&*i_plus_eq, &incr).is_ok() &&
               !mcx.try_match(&*i_eq_plus, &incr).is_ok() {
                return;
            }

            let hir_map = cx.hir_map();
			let while_hir_id = hir_map.node_to_hir_id(orig[1].id);
			let parent_hir_id = hir_map.get_parent_node(while_hir_id);
            let var_expr = mcx.bindings.get::<_, P<Expr>>("$i")
                .unwrap().clone();
            let var_hir_id = match_or!([cx.try_resolve_expr_hir(&var_expr)]
                                       Some(rustc_hir::def::Res::Local(x)) => x; return);
            let mut delegate = ForRangeDelegate {
                hir_map,
                while_hir_id,
                parent_hir_id,
                var_hir_id,

                writes_inside_loop: 0,
                reads_outside_loop: 0,
            };

            let tcx = cx.ty_ctxt();
            let parent_did = match_or!([hir_map.opt_local_def_id(parent_hir_id)]
                                       Some(x) => x; return);
            let parent_body_id = match_or!([hir_map.maybe_body_owned_by(parent_did)]
                                           Some(x) => x; return);
            let parent_body = hir_map.body(parent_body_id);
            let tables = tcx.typeck_body(parent_body_id);
            tcx.infer_ctxt().enter(|infcx| {
                ExprUseVisitor::new(&mut delegate, &infcx, parent_did,
                                    ParamEnv::empty(), tables)
                    .consume_body(&parent_body);
            });
            assert!(delegate.writes_inside_loop > 0);
            debug!("Loop variable '{:?}' writes:{} reads:{}",
                   var_expr,
                   delegate.writes_inside_loop,
                   delegate.reads_outside_loop);
            if delegate.writes_inside_loop > 1 || delegate.reads_outside_loop > 0 {
                return;
            }

            if let ExprKind::Path(ref qself, ref path) = var_expr.kind {
                let var_pat = if qself.is_none() &&
                    path.segments.len() == 1 &&
                    path.segments[0].args.is_none()
                {
                    // If this path is a single-segment identifier,
                    // we need to emit it as a `PatKind::Ident`
                    mk()
                        .span(var_expr.span)
                        .ident_pat(path.segments[0].ident)
                } else {
                    mk()
                        .span(var_expr.span)
                        .qpath_pat(qself.clone(), path.clone())
                };
                mcx.bindings.add("$ipat", var_pat);
            } else {
                return;
            };

            let step = mcx.bindings.get::<_, P<Expr>>("$step").unwrap();
            let repl_step = match (is_one_expr(&*step), range_excl) {
                (true, true) => range_one_excl.clone(),
                (true, false) => range_one_incl.clone(),
                (false, true) => range_step_excl.clone(),
                (false, false) => range_step_incl.clone(),
            };
            *orig = repl_step.subst(st, cx, &mcx.bindings);
        });
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}

fn is_one_expr(e: &Expr) -> bool {
    match e.kind {
        ExprKind::Lit(ref l) => is_one_lit(l),
        _ => false,
    }
}

fn is_one_lit(l: &Lit) -> bool {
    match l.kind {
        LitKind::Int(1, _) => true,
        _ => false,
    }
}

struct ForRangeDelegate<'a, 'hir> {
    hir_map: &'a HirMap<'hir>,
    while_hir_id: HirId,
    parent_hir_id: HirId,
    var_hir_id: HirId,

    writes_inside_loop: usize,
    reads_outside_loop: usize,
}

impl<'a, 'hir> ForRangeDelegate<'a, 'hir> {
    fn node_inside_loop(&self, id: HirId) -> bool {
        let mut cur_id = id;
        loop {
            if cur_id == self.while_hir_id {
                return true;
            }
            if cur_id == self.parent_hir_id {
                return false;
            }

            let parent_id = self.hir_map.get_parent_node(cur_id);
            if parent_id == cur_id {
                panic!("expected node {} inside parent item {}",
                       id, self.parent_hir_id);
            }
            cur_id = parent_id;
        }
    }
}

impl<'a, 'hir, 'tcx> Delegate<'tcx> for ForRangeDelegate<'a, 'hir> {
    fn consume(&mut self, cmt: &PlaceWithHirId<'tcx>, _diag_expr_id: HirId) {
        match cmt.place.base {
            PlaceBase::Local(hir_id) if hir_id == self.var_hir_id => {},
            _ => return
        }

        if !self.node_inside_loop(cmt.hir_id) {
            self.reads_outside_loop += 1;
        }
    }

    fn borrow(&mut self, cmt: &PlaceWithHirId<'tcx>, _diag_expr_id: HirId, bk: ty::BorrowKind) {
        match cmt.place.base {
            PlaceBase::Local(hir_id) if hir_id == self.var_hir_id => {},
            _ => return
        }

        if bk == ty::BorrowKind::MutBorrow {
            if !self.node_inside_loop(cmt.hir_id) {
                // Be conservative here and assume that a
                // mutable borrow outside the loop implies a read
                self.reads_outside_loop += 1;
            } else {
                self.writes_inside_loop += 1;
            }
        } else {
            if !self.node_inside_loop(cmt.hir_id) {
                self.reads_outside_loop += 1;
            }
        }
    }

    fn mutate(&mut self, cmt: &PlaceWithHirId<'tcx>, _diag_expr_id: HirId) {
        match cmt.place.base {
            PlaceBase::Local(hir_id) if hir_id == self.var_hir_id => {},
            _ => return
        }

        if self.node_inside_loop(cmt.hir_id) {
            self.writes_inside_loop += 1;
        }
    }

    fn fake_read(
        &mut self,
        _cmt: &PlaceWithHirId<'tcx>,
        _cause: FakeReadCause,
        _diag_expr_id: HirId,
    ) {}
}

/// # `remove_unused_labels` Command
/// 
/// Usage: `remove_unused_labels`
/// 
/// Removes loop labels that are not used in a named `break` or `continue`.
pub struct RemoveUnusedLabels;

fn remove_unused_labels_from_loop_kind(krate: &mut Crate,
                                       st: &CommandState,
                                       cx: &RefactorCtxt,
                                       pat: &str,
                                       repl: &str) {
    let mut mcx = MatchCtxt::new(st, cx);
    let pat = mcx.parse_expr(pat);
    let repl = mcx.parse_expr(repl);

    let find_continue = mcx.parse_expr("continue $'label");
    let find_break = mcx.parse_expr("break $'label");
    let find_break_expr = mcx.parse_expr("break $'label $bv:Expr");

    mut_visit_match_with(mcx, pat, krate, |orig, mcx| {
        let body = mcx.bindings.get::<_, Vec<Stmt>>("$body").unwrap();
        // TODO: Would be nice to get rid of the clones of body.  Might require making
        // `find_first` use a visitor instead of a `fold`, which means duplicating a lot of the
        // `PatternFolder` definitions in matcher.rs to make `PatternVisitor` variants.
        if find_first(st, cx, find_continue.clone().subst(st, cx, &mcx.bindings), &mut body.clone()).is_none() &&
            find_first(st, cx, find_break.clone().subst(st, cx, &mcx.bindings), &mut body.clone()).is_none() &&
            find_first(st, cx, find_break_expr.clone().subst(st, cx, &mcx.bindings), &mut body.clone()).is_none()
        {
            *orig = repl.clone().subst(st, cx, &mcx.bindings);
        }
    });
}

impl Transform for RemoveUnusedLabels {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        remove_unused_labels_from_loop_kind(krate, st, cx,
                "$'label:Ident: loop { $body:MultiStmt; }",
                "loop { $body; }");
        remove_unused_labels_from_loop_kind(krate, st, cx,
                "$'label:Ident: while $cond:Expr { $body:MultiStmt; }",
                "while $cond { $body; }");
        remove_unused_labels_from_loop_kind(krate, st, cx,
                "$'label:Ident: while let $pat:Pat = $init:Expr { $body:MultiStmt; }",
                "while let $pat = $init { $body; }");
        remove_unused_labels_from_loop_kind(krate, st, cx,
                "$'label:Ident: for $pat:Pat in $iter { $body:MultiStmt; }",
                "for $pat in $iter { $body; }");
    }
}


pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("reconstruct_while", |_args| mk(ReconstructWhile));
    reg.register("reconstruct_for_range", |_args| mk(ReconstructForRange));
    reg.register("remove_unused_labels", |_args| mk(RemoveUnusedLabels));
}
