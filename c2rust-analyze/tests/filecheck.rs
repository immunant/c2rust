pub mod common;

use std::path::PathBuf;

use common::{Analyze, FileCheck};

fn test(file_name: &str) {
    let analyze = Analyze::resolve();
    let file_check = FileCheck::resolve();
    let path = ["tests", "filecheck", file_name]
        .iter()
        .collect::<PathBuf>();
    let output_path = analyze.run(&path);
    file_check.run(&path, &output_path);
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
    aggregate1,
    alias1,
    alias2,
    alias3,
    alloc,
    as_ptr,
    call1,
    cast,
    clone1,
    extern_fn1,
    fields,
    insertion_sort,
    offset1,
    offset2,
    ptrptr1,
    trivial,
}
