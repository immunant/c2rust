#![feature(rustc_private)]
extern crate cbor;
extern crate syntax;
extern crate syntax_pos;
extern crate rustc_target;
extern crate dtoa;

extern crate serde;
extern crate serde_json;

pub mod renamer;
pub mod name_manager;
pub mod clang_ast;
pub mod convert_type;
pub mod loops;
pub mod comment_store;
pub mod translator;
pub mod c_ast;
pub mod rust_ast;
pub mod cfg;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
