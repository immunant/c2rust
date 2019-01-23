use syntax::ast::{Crate, Expr, ExprKind, Lit, LitKind};

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
                '__label: loop {
                    if !(__cond) {
                        break;
                    }
                    __m_body;
                }
            "#,
            r#"
                '__label: while __cond {
                    __m_body;
                }
            "#);
        krate
    }
}


/// # `reconstruct_for_loops` Command
/// 
/// Usage: `reconstruct_for_range`
/// 
/// Replaces `i = start; while i < end { ...; i += step; }` with
/// `for i in (start .. end).step_by(step) { ...; }`.
pub struct ReconstructForRange;

impl Transform for ReconstructForRange {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let pat = parse_stmts(cx.session(), r#"
            __i = __start;
            '__label: while __i < __end {
                __m_body;
                __i = __i + __step;
            }
        "#);

        let repl_step_one = parse_stmts(cx.session(), r#"
            '__label: for __i in __start .. __end {
                __m_body;
            }
        "#);

        let repl_step_more = parse_stmts(cx.session(), r#"
            '__label: for __i in (__start .. __end).step_by(__step) {
                __m_body;
            }
        "#);

        let mut mcx = MatchCtxt::new(st, cx);
        mcx.set_type("__i", BindingType::Ident);
        mcx.set_type("__step", BindingType::Expr);

        fold_match_with(mcx, pat, krate, |_, bnd| {
            if is_one_expr(bnd.expr("__step")) {
                repl_step_one.clone().subst(st, cx, &bnd)
            } else {
                repl_step_more.clone().subst(st, cx, &bnd)
            }
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
