use rustc::session::Session;
use syntax::ast::Crate;

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
