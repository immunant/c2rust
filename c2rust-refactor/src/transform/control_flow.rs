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
                $'label: loop {
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
        let labels = &["", "$'label:"];
        let cmps = &[("<", ".."), ("<=", "..=")];
        let incrs = &["+=", "= $i +"];
        let steps = &[("1", ""), ("$step:expr", ".step_by($step)")];
        let scs = &["", ";"];
        for (label, (cmp_from, cmp_to), incr, (step_from, step_to), sc) in
            iproduct!(labels, cmps, incrs, steps, scs) {
            let mut mcx = MatchCtxt::new(st, cx);
            let pat_str = format!(r#"
                $i:ident = $start:expr;
                {} while $i {} $end:expr {{
                    $body:multi_stmt;
                    $i {} {} {}
                }}
            "#, label, cmp_from, incr, step_from, sc);
            let (pat, pat_bt) = parse_free_stmts(cx.session(), &pat_str);
            mcx.merge_binding_types(pat_bt);

            let repl_str = format!(r#"
                {} for $i in $start {} $end{} {{
                    $body;
                    {}
                }}
            "#, label, cmp_to, step_to, sc);
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
    let pat = parse_expr(cx.session(), pat);
    let repl = parse_expr(cx.session(), repl);

    let find_continue = parse_expr(cx.session(), "continue '__label");
    let find_break = parse_expr(cx.session(), "break '__label");
    let find_break_expr = parse_expr(cx.session(), "break '__label __expr");

    fold_match(st, cx, pat, krate, |orig, bnd| {
        let body = bnd.multi_stmt("__m_body");
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
                "'__label: loop { __m_body; }",
                "loop { __m_body; }");
        let krate = remove_unused_labels_from_loop_kind(krate, st, cx,
                "'__label: while __cond { __m_body; }",
                "while __cond { __m_body; }");
        let krate = remove_unused_labels_from_loop_kind(krate, st, cx,
                "'__label: while let __pat = __expr { __m_body; }",
                "while let __pat = __expr { __m_body; }");
        let krate = remove_unused_labels_from_loop_kind(krate, st, cx,
                "'__label: for __pat in __iter { __m_body; }",
                "for __pat in __iter { __m_body; }");
        krate
    }
}


pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("reconstruct_while", |_args| mk(ReconstructWhile));
    reg.register("reconstruct_for_range", |_args| mk(ReconstructForRange));
    reg.register("remove_unused_labels", |_args| mk(RemoveUnusedLabels));
}
