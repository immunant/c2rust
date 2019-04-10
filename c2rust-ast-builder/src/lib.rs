#![feature(rustc_private)]
extern crate rustc;
extern crate rustc_target;
extern crate syntax;
extern crate syntax_pos;

mod builder;
pub use builder::{mk, Builder, Make};

mod into_symbol;
pub use into_symbol::IntoSymbol;
