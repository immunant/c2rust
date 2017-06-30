use rustc::session::Session;
use syntax::ast::Crate;

pub trait Transform {
    fn transform(&self, krate: Crate, sess: &Session) -> Crate;
}

pub mod wrapping_arith;

pub fn get_transform(name: &str) -> Box<Transform> {
    Box::new(match name {
        "wrapping_arith_to_normal" => wrapping_arith::WrappingToNormal,
        _ => panic!("unknown transform {:?}", name),
    })
}
