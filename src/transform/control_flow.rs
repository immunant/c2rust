use rustc::session::Session;
use syntax::ast::{Crate, Expr, ExprKind, Lit, LitKind};

use api::*;
use transform::Transform;


pub struct ReconstructWhile;

impl Transform for ReconstructWhile {
    fn transform(&self, krate: Crate, sess: &Session) -> Crate {
        let krate = replace_expr(
            sess, krate,
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


pub struct ReconstructForRange;

impl Transform for ReconstructForRange {
    fn transform(&self, krate: Crate, sess: &Session) -> Crate {
        let pat = parse_stmts(sess, r#"
            __i = __start;
            '__label: while __i < __end {
                __m_body;
                __i = __i + __step;
            }
        "#).unwrap();

        let repl_step_one = parse_stmts(sess, r#"
            '__label: for __i in __start .. __end {
                __m_body;
            }
        "#).unwrap();

        let repl_step_more = parse_stmts(sess, r#"
            '__label: for __i in (__start .. __end).step_by(__step) {
                __m_body;
            }
        "#).unwrap();

        let mut mcx = MatchCtxt::new();
        mcx.set_type("__i", BindingType::Ident);
        mcx.set_type("__step", BindingType::Expr);

        fold_match_with(mcx, pat, krate, |_, bnd| {
            if is_one_expr(bnd.expr("__step")) {
                repl_step_one.clone().subst(&bnd)
            } else {
                repl_step_more.clone().subst(&bnd)
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
