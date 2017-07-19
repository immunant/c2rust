//! Transformation passes used for testing parts of the system.

use syntax::ast::Crate;

use api::*;
use command::{CommandState, Registry};
use driver;
use transform::Transform;


/// `2 -> 1 + 1`.  Useful for testing the rewriter's handling of operator precedence.  The `1 + 1`
/// may or may not need enclosing parens, depending on the context.
pub struct OnePlusOne;

impl Transform for OnePlusOne {
    fn transform(&self, krate: Crate, _st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let krate = replace_expr(cx.session(), krate, "2", "1 + 1");
        krate
    }
}


/// `f(x) -> x + 1`.  Useful for testing the rewriter's handling of operator precedence.  The `x`
/// may or may not need enclosing parens, depending on what type of expression it is.
pub struct FPlusOne;

impl Transform for FPlusOne {
    fn transform(&self, krate: Crate, _st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let krate = replace_expr(cx.session(), krate, "f(__x)", "__x + 1");
        krate
    }
}


pub struct ReplaceStmts(pub String, pub String);

impl Transform for ReplaceStmts {
    fn transform(&self, krate: Crate, _st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let krate = replace_stmts(cx.session(), krate, &self.0, &self.1);
        krate
    }
}


pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("test_one_plus_one", |_args| mk(OnePlusOne));
    reg.register("test_f_plus_one", |_args| mk(FPlusOne));
    reg.register("test_replace_stmts", |args| mk(
            ReplaceStmts(args[0].clone(), args[1].clone())));
}
