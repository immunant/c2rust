//! Test cases for rewriting of nullable owned pointers.
#![feature(rustc_private)]
#![feature(register_tool)]
#![register_tool(c2rust_analyze_test)]

use std::mem;
extern crate libc;

extern "C" {
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    fn free(__ptr: *mut libc::c_void);
}

#[c2rust_analyze_test::skip_borrowck]
// CHECK-LABEL: fn test_is_null(
pub unsafe fn test_is_null(cond: bool) {
    // CHECK: let mut ptr: core::option::Option<core::result::Result<std::boxed::Box<(i32)>,()>> =
    let mut ptr: *mut i32 = malloc(4) as *mut i32;
    // This assignment exercises the option + dyn_owned case in `visit_place_ref`.
    // CHECK: *(*(ptr).as_mut().unwrap()).as_deref_mut().unwrap() = 1;
    *ptr = 1;
    if cond {
        ptr = 0 as *mut i32;
    }
    // This condition exercises the option + dyn_owned case in `try_build_cast_desc_desc`.
    // CHECK: if !(ptr).as_ref().map(|__ptr| (*__ptr).as_deref().unwrap()).is_none() {
    if !ptr.is_null() {
        // CHECK: *(*(ptr).as_mut().unwrap()).as_deref_mut().unwrap() = 2;
        *ptr = 2;
    }
    // This condition exercises the option + dyn_owned case in the `assignment_transfers_ownership`
    // case of `visit_statement`.
    // CHECK: std::mem::drop(((ptr).as_mut().map(|__ptr| std::mem::replace(&mut *__ptr,Err(()))).map(|__ptr| __ptr.unwrap())));
    free(ptr as *mut libc::c_void);
}
