use syntax::ast::{Crate, Expr, ExprKind, Lit, LitKind, StmtKind};

use crate::api::*;
use crate::command::{CommandState, Registry};
use crate::driver;
use crate::transform::Transform;


/// # `reconstruct_while` Command
/// 
/// Obsolete - the translator now does this automatically.
/// 
/// Usage: `reconstruct_while`
/// 
/// Replaces all instances of `loop { if !cond { break; } ... }` with `while` loops.
pub struct ReconstructWhile;

impl Transform for ReconstructWhile {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let krate = replace_expr(
            st, cx, krate,
            r#"
                $'label:opt_label: loop {
                    if !($cond:expr) {
                        break;
                    }
                    $body:multi_stmt;
                }
            "#,
            r#"
                $'label: while $cond {
                    $body:multi_stmt;
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
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let mut mcx = MatchCtxt::new(st, cx);
        let pat_str = r#"
            $i:ident = $start:expr;
            $'label:opt_label: while $cond:expr {
                $body:multi_stmt;
                $incr:stmt;
            }"#;
        let (pat, pat_bt) = parse_free_stmts(cx.session(), &pat_str);
        mcx.merge_binding_types(pat_bt);

        let (lt_cond, bt) = parse_free_expr(cx.session(), "$i < $end:expr");
        mcx.merge_binding_types(bt);
        let (le_cond, bt) = parse_free_expr(cx.session(), "$i <= $end:expr");
        mcx.merge_binding_types(bt);

        let (i_plus_eq, bt) = parse_free_expr(cx.session(), "$i += $step:expr");
        mcx.merge_binding_types(bt);
        let (i_eq_plus, bt) = parse_free_expr(cx.session(), "$i = $i + $step:expr");
        mcx.merge_binding_types(bt);

        let (range_one_excl, _) = parse_free_stmts(cx.session(),
            "$'label: for $i in $start .. $end { $body; }");
        let (range_one_incl, _) = parse_free_stmts(cx.session(),
            "$'label: for $i in $start ..= $end { $body; }");
        let (range_step_excl, _) = parse_free_stmts(cx.session(),
            "$'label: for $i in ($start .. $end).step_by($step) { $body; }");
        let (range_step_incl, _) = parse_free_stmts(cx.session(),
            "$'label: for $i in ($start ..= $end).step_by($step) { $body; }");

        fold_match_with(mcx, pat, krate, |orig, mut mcx| {
            let cond = mcx.bindings.expr("$cond").clone();
            let range_excl = if mcx.try_match(&*lt_cond, &cond).is_ok() {
                true
            } else if mcx.try_match(&*le_cond, &cond).is_ok() {
                false
            } else {
                return orig;
            };

            let incr = match mcx.bindings.stmt("$incr").node {
                StmtKind::Semi(ref e) |
                StmtKind::Expr(ref e) => e.clone(),
                _ => { return orig; }
            };
            if !mcx.try_match(&*i_plus_eq, &incr).is_ok() &&
               !mcx.try_match(&*i_eq_plus, &incr).is_ok() {
                return orig;
            }

            let step = mcx.bindings.expr("$step");
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
                                       cx: &driver::Ctxt,
                                       pat: &str,
                                       repl: &str) -> Crate {
    let mut mcx = MatchCtxt::new(st, cx);
    let (pat, pat_bt) = parse_free_expr(cx.session(), pat);
    mcx.merge_binding_types(pat_bt);
    let (repl, repl_bt) = parse_free_expr(cx.session(), repl);
    mcx.merge_binding_types(repl_bt);

    let (find_continue, _) = parse_free_expr(cx.session(), "continue $'label");
    let (find_break, _) = parse_free_expr(cx.session(), "break $'label");
    let (find_break_expr, _) = parse_free_expr(cx.session(), "break $'label $bv:expr");

    fold_match_with(mcx, pat, krate, |orig, mcx| {
        let body = mcx.bindings.multi_stmt("$body");
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
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let krate = remove_unused_labels_from_loop_kind(krate, st, cx,
                "$'label:ident: loop { $body:multi_stmt; }",
                "loop { $body; }");
        let krate = remove_unused_labels_from_loop_kind(krate, st, cx,
                "$'label:ident: while $cond:expr { $body:multi_stmt; }",
                "while $cond { $body; }");
        let krate = remove_unused_labels_from_loop_kind(krate, st, cx,
                "$'label:ident: while let $pat:pat = $init:expr { $body:multi_stmt; }",
                "while let $pat = $init { $body; }");
        let krate = remove_unused_labels_from_loop_kind(krate, st, cx,
                "$'label:ident: for $pat:pat in $iter { $body:multi_stmt; }",
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
