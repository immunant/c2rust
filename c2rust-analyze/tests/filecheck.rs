pub mod common;

use crate::common::{check_for_missing_tests_for, test_dir_for, Analyze, FileCheck};

#[test]
fn check_for_missing_tests() {
    check_for_missing_tests_for(file!());
}

fn test(file_name: &str) {
    let analyze = Analyze::resolve();
    let file_check = FileCheck::resolve();
    let path = test_dir_for(file!(), true).join(file_name);
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
    addr_of,
    aggregate1,
    alias1,
    alias2,
    alias3,
    alloc,
    as_ptr,
    call1,
    call_cast,
    cast,
    catch_panic,
    cell,
    clone1,
    extern_fn1,
    fields,
    field_temp,
    fixed,
    insertion_sort,
    insertion_sort_driver,
    insertion_sort_rewrites,
    offset1,
    offset2,
    ptrptr1,
    statics,
    trivial,
    type_annotation_rewrite,
}
