use syntax::ast::Crate;

use api::*;
use driver;
use transform::Transform;


pub struct WrappingToNormal;

impl Transform for WrappingToNormal {
    fn transform(&self, krate: Crate, cx: &driver::Ctxt) -> Crate {
        let krate = replace_expr(cx.session(), krate,
                                 "__x.wrapping_add(__y)",
                                 "__x + __y");
        let krate = replace_expr(cx.session(), krate,
                                 "__x.wrapping_sub(__y)",
                                 "__x - __y");
        let krate = replace_expr(cx.session(), krate,
                                 "__x.wrapping_mul(__y)",
                                 "__x * __y");
        let krate = replace_expr(cx.session(), krate,
                                 "__x.wrapping_div(__y)",
                                 "__x / __y");
        let krate = replace_expr(cx.session(), krate,
                                 "__x.wrapping_rem(__y)",
                                 "__x % __y");
        let krate = replace_expr(cx.session(), krate,
                                 "__x.wrapping_neg()",
                                 "-__x");
        let krate = replace_expr(cx.session(), krate,
                                 "__x.wrapping_shl(__y)",
                                 "__x << __y");
        let krate = replace_expr(cx.session(), krate,
                                 "__x.wrapping_shr(__y)",
                                 "__x >> __y");
        let krate = replace_expr(cx.session(), krate,
                                 "__x.wrapping_abs()",
                                 "__x.abs()");
        krate
    }
}
