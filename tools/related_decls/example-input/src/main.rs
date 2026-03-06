#![allow(unused)]

use bytes::TryGetError;
use std::path::PathBuf;

fn main() {
    println!("Hello, world!");
}

/// example module for testing
mod another {
    pub const MAX: usize = usize::MAX;
    pub struct X {
        field1: usize,
        field2: std::path::PathBuf,
    }
    pub enum EnumWithBodiedVariant {
        Variant = isize::MIN,
    }
    pub enum EnumWithFieldedVariant {
        Variant(X),
    }
    pub mod foo {
        pub const CONSTANT: &str = "someconstant";
        pub const OTHER_CONSTANT: &str = "otherconstant";
    }
    pub mod bar {
        pub const CONSTANT: &str = "barconst";
    }
}

#[allow(unused)]
fn returns_pathbuf() -> std::path::PathBuf {
    PathBuf::new() //unimplemented!()
}

#[allow(unused)]
fn synthetic_usages() {
    let _ = main;
    use another::bar;
    eprintln!("{}", another::foo::OTHER_CONSTANT);
    let c = another::foo::CONSTANT;
    let alsomain = crate::main;
    let indirect = bar::CONSTANT;
}

static ERR: std::io::ErrorKind = std::io::ErrorKind::NotFound;
static TGE: TryGetError = TryGetError {
    requested: 0,
    available: 0,
};
