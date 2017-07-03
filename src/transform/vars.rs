use rustc::session::Session;
use syntax::ast::Crate;

use api::*;
use transform::Transform;


pub struct LetXUninitialized;

impl Transform for LetXUninitialized {
    fn transform(&self, krate: Crate, sess: &Session) -> Crate {
        let krate = replace_stmts(sess, krate,
                                  "let __pat;",
                                  "let __pat = ::std::mem::uninitialized();");
        let krate = replace_stmts(sess, krate,
                                  "let __pat: __ty;",
                                  "let __pat: __ty = ::std::mem::uninitialized();");
        krate
    }
}
