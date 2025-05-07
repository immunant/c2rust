use rustc_ast::*;
use rustc_ast::ptr::P;

use crate::ast_builder::mk;
use crate::command::{CommandState, Registry};
use crate::RefactorCtxt;
use crate::driver::{self, Phase};
use crate::matcher::{Bindings, BindingType, MatchCtxt, Subst, mut_visit_match_with};
use crate::transform::Transform;

/// # `char_literals` Command
/// 
/// Obsolete - the translator now does this automatically.
/// 
/// Usage: `char_literals`
/// 
/// Replace integer literals cast to `libc::c_char` with actual char literals.
/// For example, replaces `65 as libc::c_char` with `'A' as libc::c_char`.
struct CharLits {
}

impl Transform for CharLits {
    fn min_phase(&self) -> Phase { Phase::Phase2 }
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {

        let pattern = driver::parse_expr(cx.session(), "__number as libc::c_char");
        let mut mcx = MatchCtxt::new(st, cx);
        mcx.set_type("__number", BindingType::Expr);

        mut_visit_match_with(mcx, pattern.clone(), krate, |e, mcx| {
            let field: &P<Expr> = mcx.bindings.get::<_, P<Expr>>("__number").unwrap();
            if let ExprKind::Lit(ref l) = field.kind {
                if let LitKind::Int(i, _) = l.kind {
                    if i < 256 {
                        let mut bnd = Bindings::new();
                        bnd.add("__number", mk().lit_expr(i as u8 as char));
                        *e = pattern.clone().subst(st, cx, &bnd);
                    }
                }
            }
        });
    }
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("char_literals", |_args| mk(CharLits{}))
}
