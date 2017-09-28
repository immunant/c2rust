#![feature(rustc_private)]
extern crate bimap;
extern crate cbor;
extern crate syntax;

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
