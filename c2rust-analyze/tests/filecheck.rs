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
    adjust_unsize,
    aggregate1,
    algo_md5,
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
    foreign,
    insertion_sort,
    insertion_sort_driver,
    insertion_sort_rewrites,
    known_fn,
    lighttpd_buffer,
    move_mut,
    non_null,
    non_null_force,
    non_null_rewrites,
    offset1,
    offset2,
    offset_rewrites,
    pointee,
    ptrptr1,
    regions_fixed,
    rewrite_nullable_box,
    rewrite_paths,
    rewrite_paths_manual_shim,
    statics,
    test_attrs,
    trivial,
    type_alias,
    type_annotation_rewrite,
    unrewritten_calls,
    unrewritten_calls_shim_fail,
}

#[test]
fn tail_call() {
    let path = test_dir_for(file!(), true).join("tail_call.rs");
    let metadata = std::env::temp_dir().join(format!(
        "c2rust-analyze-tail-call-{}.rmeta",
        std::process::id()
    ));
    // The candidate compiler lowers explicit tail calls but its code generator
    // does not implement them yet. Check that analysis preserves the tail call
    // and fixes its signature, without asking rustc to generate machine code.
    let output = Analyze::resolve().run_with(
        &path,
        |cmd| {
            cmd.arg("--emit=metadata").arg("-o").arg(&metadata);
        },
        None,
    );
    FileCheck::resolve().run(&path, &output);
    std::fs::remove_file(metadata).unwrap();
}
