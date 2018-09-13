#![feature(rustc_private)]
extern crate serde_cbor;
extern crate syntax;
extern crate syntax_pos;
extern crate rustc_target;
extern crate dtoa;
#[macro_use] extern crate indexmap;
extern crate serde;
extern crate serde_json;
extern crate rustc;
extern crate rust_ast_builder;
extern crate libc;

pub mod renamer;
pub mod clang_ast;
pub mod convert_type;
pub mod loops;
pub mod translator;
pub mod c_ast;
pub mod rust_ast;
pub mod cfg;
pub mod with_stmts;
pub mod exporter;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
