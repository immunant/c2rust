#![feature(rustc_private)]
extern crate bimap;
extern crate cbor;
extern crate syntax;
extern crate idiomize;

pub mod renamer;
pub mod name_manager;
pub mod clang_ast;
pub mod convert_type;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
