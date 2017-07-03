use rustc::session::Session;
use syntax::ast::Crate;

pub trait Transform {
    fn transform(&self, krate: Crate, sess: &Session) -> Crate;
}

pub mod control_flow;
pub mod test;
pub mod vars;
pub mod wrapping_arith;

pub fn get_transform(name: &str, args: &[String]) -> Box<Transform> {
    match name {
        "reconstruct_while" => Box::new(control_flow::ReconstructWhile),

        "test_one_plus_one" => Box::new(test::OnePlusOne),
        "test_f_plus_one" => Box::new(test::FPlusOne),
        "test_replace_stmts" => Box::new(test::ReplaceStmts(args[0].clone(), args[1].clone())),

        "let_x_uninitialized" => Box::new(vars::LetXUninitialized),

        "wrapping_arith_to_normal" => Box::new(wrapping_arith::WrappingToNormal),

        _ => panic!("unknown transform {:?}", name),
    }
}
