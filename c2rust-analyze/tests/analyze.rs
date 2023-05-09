pub mod common;

use crate::common::{check_for_missing_tests_for, test_dir_for, Analyze};

#[test]
fn check_for_missing_tests() {
    check_for_missing_tests_for(file!());
}

fn test(file_name: &str) {
    let analyze = Analyze::resolve();
    let path = test_dir_for(file!(), true).join(file_name);
    analyze.dont_catch_panic().run(&path);
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
    macros,
    ptr_addr_of,
    string_literals,
    string_casts,
}

#[test]
fn lighttpd_minimal() {
    Analyze::resolve().run("../analysis/tests/lighttpd-minimal/src/main.rs");
}
