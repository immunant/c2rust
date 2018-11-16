#![feature(rustc_private)]
extern crate syntax;
extern crate rustc;
extern crate syntax_pos;
extern crate rustc_target;

mod builder;
pub use builder::{Builder, Make, mk};

mod into_symbol;
pub use into_symbol::IntoSymbol;
