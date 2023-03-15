pub mod common;

use std::path::PathBuf;

use common::Analyze;

fn test(file_name: &str) {
    let analyze = Analyze::resolve();
    let path = ["tests", "analyze", file_name].iter().collect::<PathBuf>();
    analyze.run(&path);
}

macro_rules! define_test {
    ($name:ident) => {
        #[test]
        fn $name() {
            test(concat!(stringify!($name), ".rs"));
        }
    };
}

macro_rules! define_tests {
    ($($name:ident,)*) => {
        $(define_test! { $name })*
    }
}

define_tests! {
    string_literals,
    string_casts,
    macros,
}

#[test]
fn lighttpd_minimal() {
    Analyze::resolve().run("../analysis/tests/lighttpd-minimal/src/main.rs");
}
