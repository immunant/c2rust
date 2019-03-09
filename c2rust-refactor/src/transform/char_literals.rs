use syntax::ast::*;
use syntax::ptr::P;

use c2rust_ast_builder::mk;
use crate::command::{CommandState, Registry};
use crate::RefactorCtxt;
use crate::driver::{self, Phase};
use crate::matcher::{Bindings, BindingType, MatchCtxt, Subst, fold_match_with};
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
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &driver::Ctxt) {

        let pattern = driver::parse_expr(cx.session(), "__number as libc::c_char");
        let mut mcx = MatchCtxt::new(st, cx);
        mcx.set_type("__number", BindingType::Expr);

        let krate = fold_match_with(mcx, pattern.clone(), krate, |e, mcx| {
            let field: &P<Expr> = mcx.bindings.get::<_, P<Expr>>("__number").unwrap();
            if let ExprKind::Lit(ref l) = field.node {
                if let LitKind::Int(i, _) = l.node {
                    if i < 256 {
                        let mut bnd = Bindings::new();
                        bnd.add("__number", mk().lit_expr(mk().char_lit(i as u8 as char)));
                        return pattern.clone().subst(st, cx, &bnd)
                    }
                }
            }
            e
        });

        krate
    }
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("char_literals", |_args| mk(CharLits{}))
}
