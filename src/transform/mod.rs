use rustc::session::Session;
use syntax::ast::Crate;

pub trait Transform {
    fn transform(&self, krate: Crate, sess: &Session) -> Crate;
}

pub mod wrapping_arith;
pub mod test;

pub fn get_transform(name: &str) -> Box<Transform> {
    match name {
        "wrapping_arith_to_normal" => Box::new(wrapping_arith::WrappingToNormal),

        "test_one_plus_one" => Box::new(test::OnePlusOne),

        _ => panic!("unknown transform {:?}", name),
    }
}
