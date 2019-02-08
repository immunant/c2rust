use syntax::ast::Crate;

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
    fn transform(&self, mut krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let cmps = &[("<", ".."), ("<=", "..=")];
        let incrs = &["+=", "= $i +"];
        let steps = &[("1", "", ""), ("$step:expr", "(", ").step_by($step)")];
        let scs = &["", ";"];
        for ((cmp_from, cmp_to), incr, (step_from, step_to_prefix, step_to), sc) in
            iproduct!(cmps, incrs, steps, scs) {
            let mut mcx = MatchCtxt::new(st, cx);
            let pat_str = format!(r#"
                $i:ident = $start:expr;
                $'label:opt_label: while $i {} $end:expr {{
                    $body:multi_stmt;
                    $i {} {} {}
                }}
            "#, cmp_from, incr, step_from, sc);
            let (pat, pat_bt) = parse_free_stmts(cx.session(), &pat_str);
            mcx.merge_binding_types(pat_bt);

            let repl_str = format!(r#"
                $'label: for $i in {}$start {} $end{} {{
                    $body;
                    {}
                }}
            "#, step_to_prefix, cmp_to, step_to, sc);
            let (repl_step, repl_bt) = parse_free_stmts(cx.session(), &repl_str);
            mcx.merge_binding_types(repl_bt);

            krate = fold_match_with(mcx, pat, krate, |_, bnd| {
                repl_step.clone().subst(st, cx, &bnd)
            });
        }
        krate
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
    let (find_break_expr, _) = parse_free_expr(cx.session(), "break $'label __expr");

    fold_match_with(mcx, pat, krate, |orig, bnd| {
        let body = bnd.multi_stmt("$body");
        // TODO: Would be nice to get rid of the clones of body.  Might require making
        // `find_first` use a visitor instead of a `fold`, which means duplicating a lot of the
        // `PatternFolder` definitions in matcher.rs to make `PatternVisitor` variants.
        if find_first(st, cx, find_continue.clone().subst(st, cx, &bnd), body.clone()).is_none() &&
           find_first(st, cx, find_break.clone().subst(st, cx, &bnd), body.clone()).is_none() &&
           find_first(st, cx, find_break_expr.clone().subst(st, cx, &bnd), body.clone()).is_none() {
            repl.clone().subst(st, cx, &bnd)
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
