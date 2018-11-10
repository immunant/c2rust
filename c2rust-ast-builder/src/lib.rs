#![feature(rustc_private)]
extern crate syntax;
extern crate rustc;
extern crate syntax_pos;
extern crate rustc_target;

mod builder;
pub use builder::{Builder, Make, mk, mk_with_id};

mod into_symbol;
pub use into_symbol::IntoSymbol;
