//! Transformation passes used for testing parts of the system.

use rustc::session::Session;
use syntax::ast::Crate;

use api::*;
use transform::Transform;


pub struct OnePlusOne;

impl Transform for OnePlusOne {
    fn transform(&self, krate: Crate, sess: &Session) -> Crate {
        let krate = replace_expr(sess, krate, "2", "1 + 1");
        krate
    }
}
