use crate::cleanup::{
    rust_run_early_return, rust_run_goto, rust_run_multiple, rust_run_nested, rust_run_single,
    rust_run_typedef,
};
use std::ffi::c_int;

#[link(name = "test")]
unsafe extern "C" {
    fn run_single(out: *mut c_int, n: *mut c_int);
    fn run_multiple(out: *mut c_int, n: *mut c_int);
    fn run_early_return(out: *mut c_int, n: *mut c_int);
    fn run_nested(out: *mut c_int, n: *mut c_int);
    fn run_goto(out: *mut c_int, n: *mut c_int);
    fn run_typedef(out: *mut c_int, n: *mut c_int);
}

type EntryFn = unsafe extern "C" fn(out: *mut c_int, n: *mut c_int);

fn collect(entry: EntryFn) -> Vec<c_int> {
    let mut buf = [0i32; 16];
    let mut n: c_int = 0;
    unsafe { entry(buf.as_mut_ptr(), &mut n) };
    buf[..n as usize].to_vec()
}

fn check(c_entry: EntryFn, rust_entry: EntryFn, expected: &[c_int]) {
    let c_trace = collect(c_entry);
    let rust_trace = collect(rust_entry);
    assert_eq!(c_trace, expected, "C side diverged from expected");
    assert_eq!(rust_trace, expected, "Rust side diverged from expected");
}

#[test]
pub fn test_single() {
    check(run_single, rust_run_single, &[5]);
}

#[test]
pub fn test_reverse_declaration_order() {
    check(run_multiple, rust_run_multiple, &[3, 2, 1]);
}

#[test]
pub fn test_fires_through_early_return() {
    check(run_early_return, rust_run_early_return, &[7]);
}

#[test]
pub fn test_nested_block() {
    check(run_nested, rust_run_nested, &[20, 30, 10]);
}

#[test]
pub fn test_goto_with_hoisted_local() {
    check(run_goto, rust_run_goto, &[3]);
}

#[test]
pub fn test_typedef_signature() {
    check(run_typedef, rust_run_typedef, &[42]);
}
