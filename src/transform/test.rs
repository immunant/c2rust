//! Transformation passes used for testing parts of the system.

use rustc::session::Session;
use syntax::ast::Crate;

use api::*;
use transform::Transform;


/// `2 -> 1 + 1`.  Useful for testing the rewriter's handling of operator precedence.  The `1 + 1`
/// may or may not need enclosing parens, depending on the context.
pub struct OnePlusOne;

impl Transform for OnePlusOne {
    fn transform(&self, krate: Crate, sess: &Session) -> Crate {
        let krate = replace_expr(sess, krate, "2", "1 + 1");
        krate
    }
}


/// `f(x) -> x + 1`.  Useful for testing the rewriter's handling of operator precedence.  The `x`
/// may or may not need enclosing parens, depending on what type of expression it is.
pub struct FPlusOne;

impl Transform for FPlusOne {
    fn transform(&self, krate: Crate, sess: &Session) -> Crate {
        let krate = replace_expr(sess, krate, "f(__x)", "__x + 1");
        krate
    }
}
