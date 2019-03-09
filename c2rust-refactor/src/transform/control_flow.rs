use syntax::ast::{Crate, Expr, ExprKind, Lit, LitKind, Stmt, StmtKind};
use syntax::ptr::P;

use crate::command::{CommandState, Registry};
use crate::matcher::{MatchCtxt, Subst, replace_expr, fold_match_with, find_first};
use crate::transform::Transform;
use crate::RefactorCtxt;


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
pub struct ReconstructForRange;

impl Transform for ReconstructForRange {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        let mut mcx = MatchCtxt::new(st, cx);
        let pat_str = r#"
            $i:Ident = $start:Expr;
            $'label:?Ident: while $cond:Expr {
                $body:MultiStmt;
                $incr:Stmt;
            }"#;
        let pat = mcx.parse_stmts(&pat_str);

        let lt_cond = mcx.parse_expr("$i < $end:Expr");
        let le_cond = mcx.parse_expr("$i <= $end:Expr");

        let i_plus_eq = mcx.parse_expr("$i += $step:Expr");
        let i_eq_plus = mcx.parse_expr("$i = $i + $step:Expr");

        let range_one_excl = mcx.parse_stmts("$'label: for $i in $start .. $end { $body; }");
        let range_one_incl = mcx.parse_stmts("$'label: for $i in $start ..= $end { $body; }");
        let range_step_excl = mcx.parse_stmts("$'label: for $i in ($start .. $end).step_by($step) { $body; }");
        let range_step_incl = mcx.parse_stmts("$'label: for $i in ($start ..= $end).step_by($step) { $body; }");

        fold_match_with(mcx, pat, krate, |orig, mut mcx| {
            let cond = mcx.bindings.get::<_, P<Expr>>("$cond").unwrap().clone();
            let range_excl = if mcx.try_match(&*lt_cond, &cond).is_ok() {
                true
            } else if mcx.try_match(&*le_cond, &cond).is_ok() {
                false
            } else {
                return orig;
            };

            let incr = match mcx.bindings.get::<_, Stmt>("$incr").unwrap().node {
                StmtKind::Semi(ref e) |
                StmtKind::Expr(ref e) => e.clone(),
                _ => { return orig; }
            };
            if !mcx.try_match(&*i_plus_eq, &incr).is_ok() &&
               !mcx.try_match(&*i_eq_plus, &incr).is_ok() {
                return orig;
            }

            let step = mcx.bindings.get::<_, P<Expr>>("$step").unwrap();
            let repl_step = match (is_one_expr(&*step), range_excl) {
                (true, true) => range_one_excl.clone(),
                (true, false) => range_one_incl.clone(),
                (false, true) => range_step_excl.clone(),
                (false, false) => range_step_incl.clone(),
            };
            repl_step.subst(st, cx, &mcx.bindings)
        })
    }
}

fn is_one_expr(e: &Expr) -> bool {
    match e.node {
        ExprKind::Lit(ref l) => is_one_lit(l),
        _ => false,
    }
}

fn is_one_lit(l: &Lit) -> bool {
    match l.node {
        LitKind::Int(1, _) => true,
        _ => false,
    }
}

/// # `remove_unused_labels` Command
/// 
/// Usage: `remove_unused_labels`
/// 
/// Removes loop labels that are not used in a named `break` or `continue`.
pub struct RemoveUnusedLabels;

fn remove_unused_labels_from_loop_kind(krate: Crate,
                                       st: &CommandState,
                                       cx: &RefactorCtxt,
                                       pat: &str,
                                       repl: &str) -> Crate {
    let mut mcx = MatchCtxt::new(st, cx);
    let pat = mcx.parse_expr(pat);
    let repl = mcx.parse_expr(repl);

    let find_continue = mcx.parse_expr("continue $'label");
    let find_break = mcx.parse_expr("break $'label");
    let find_break_expr = mcx.parse_expr("break $'label $bv:Expr");

    fold_match_with(mcx, pat, krate, |orig, mcx| {
        let body = mcx.bindings.get::<_, Vec<Stmt>>("$body").unwrap();
        // TODO: Would be nice to get rid of the clones of body.  Might require making
        // `find_first` use a visitor instead of a `fold`, which means duplicating a lot of the
        // `PatternFolder` definitions in matcher.rs to make `PatternVisitor` variants.
        if find_first(st, cx, find_continue.clone().subst(st, cx, &mcx.bindings), body.clone()).is_none() &&
           find_first(st, cx, find_break.clone().subst(st, cx, &mcx.bindings), body.clone()).is_none() &&
           find_first(st, cx, find_break_expr.clone().subst(st, cx, &mcx.bindings), body.clone()).is_none() {
            repl.clone().subst(st, cx, &mcx.bindings)
        } else {
            orig
        }
    })
}

impl Transform for RemoveUnusedLabels {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        let krate = remove_unused_labels_from_loop_kind(krate, st, cx,
                "$'label:Ident: loop { $body:MultiStmt; }",
                "loop { $body; }");
        let krate = remove_unused_labels_from_loop_kind(krate, st, cx,
                "$'label:Ident: while $cond:Expr { $body:MultiStmt; }",
                "while $cond { $body; }");
        let krate = remove_unused_labels_from_loop_kind(krate, st, cx,
                "$'label:Ident: while let $pat:Pat = $init:Expr { $body:MultiStmt; }",
                "while let $pat = $init { $body; }");
        let krate = remove_unused_labels_from_loop_kind(krate, st, cx,
                "$'label:Ident: for $pat:Pat in $iter { $body:MultiStmt; }",
                "for $pat in $iter { $body; }");
        krate
    }
}


pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("reconstruct_while", |_args| mk(ReconstructWhile));
    reg.register("reconstruct_for_range", |_args| mk(ReconstructForRange));
    reg.register("remove_unused_labels", |_args| mk(RemoveUnusedLabels));
}
