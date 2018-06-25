#![feature(rustc_private)]
#![feature(i128_type)]
extern crate cbor;
extern crate syntax;
extern crate syntax_pos;
extern crate idiomize;
extern crate dtoa;

extern crate serde;
extern crate serde_json;

pub mod renamer;
pub mod clang_ast;
pub mod convert_type;
pub mod loops;
pub mod comment_store;
pub mod translator;
pub mod c_ast;
pub mod cfg;
pub mod with_stmts;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
