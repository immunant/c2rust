use syntax::ast::Crate;

use api::*;
use driver;
use transform::Transform;


pub struct LetXUninitialized;

impl Transform for LetXUninitialized {
    fn transform(&self, krate: Crate, cx: &driver::Ctxt) -> Crate {
        let krate = replace_stmts(cx.session(), krate,
                                  "let __pat;",
                                  "let __pat = ::std::mem::uninitialized();");
        let krate = replace_stmts(cx.session(), krate,
                                  "let __pat: __ty;",
                                  "let __pat: __ty = ::std::mem::uninitialized();");
        krate
    }
}
