//! --env RUST_LOG_PANIC=error,c2rust_analyze::dataflow::type_check=off

use std::cell::Cell;
use std::ffi;
use std::ptr::NonNull;

extern "C" {
    fn close(__fd: ffi::c_int) -> ffi::c_int;
}

pub struct Local;

pub fn test_extern_fns() {
    unsafe {
        // CHECK: unsafe extern "C" fn(i32) -> i32 {close} has PtrEffects::Deep
        close(-1);
    }
}

pub fn test_std_unsafe_fn() {
    unsafe {
        // CHECK: unsafe extern "rust-intrinsic" fn(i32) -> u32 {std::intrinsics::transmute::<i32, u32>} has PtrEffects::Deep
        std::mem::transmute::<i32, u32>(0);
    }
}

pub fn test_std_fn_with_non_local_ty() {
    // CHECK: fn(i32) -> std::cell::Cell<i32> {std::cell::Cell::<i32>::new} has PtrEffects::None
    Cell::new(0);
}

pub fn test_std_fn_with_local_ty(local: Local) {
    // CHECK: fn(Local) -> std::cell::Cell<Local> {std::cell::Cell::<Local>::new} has PtrEffects::None
    Cell::new(local);
}

pub fn test_std_fn_with_ptr(local: Local) {
    // CHECK: fn(*const Local) -> std::cell::Cell<*const Local> {std::cell::Cell::<*const Local>::new} has PtrEffects::Shallow
    Cell::new(&local as *const Local);
}

pub fn test_generic_ptr(generic_ptr: Option<*mut u8>) {
    // CHECK: for<'r> fn(&'r std::option::Option<*mut u8>) -> std::option::Option<*mut u8> {<std::option::Option<*mut u8> as std::clone::Clone>::clone} has PtrEffects::None
    let _ = generic_ptr.clone();
}

pub fn test_std_container_with_internal_ptr(containing_ptr: NonNull<u8>) {
    // CHECK: for<'r> fn(&'r std::ptr::NonNull<u8>) -> std::ptr::NonNull<u8> {<std::ptr::NonNull<u8> as std::clone::Clone>::clone} has PtrEffects::None
    let _ = containing_ptr.clone();
}
