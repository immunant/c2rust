use std::cell::{Cell, RefCell};
use std::path::{Path, PathBuf};
use std::ptr::NonNull;
use std::sync::atomic::{AtomicBool, AtomicPtr};

pub fn fn4<A, B, C, D>(_a: A, _b: B, _c: C, _d: D) {}

/// Check a type for triviality.
///
/// There are a lot of restrictions here.
/// Generics are not supported.
/// And more importantly, aggregate initialization is not supported (#736),
/// so we can't create most types, including even simple ones like `None` and `()`.
/// And we need to make a function call to test triviality,
/// so we define a local function with the type already as an argument,
/// and then call itself recursively to create the function call.
/// The function is never actually called, so infinite recursion doesn't happen,
/// though `rustc` still complains, so we put it in a dead branch.
macro_rules! safe_fn {
    ($t:ty) => {{
        #[allow(unused)]
        fn f(t: $t) {
            if false {
                f(t);
            }
        }
    }};
}

/// Same as [`safe_fn`] above, but `unsafe`.
macro_rules! unsafe_fn {
    ($t:ty) => {{
        #[allow(unused)]
        unsafe fn f(t: $t) {
            if false {
                unsafe {
                    f(t);
                }
            }
        }
    }};
}

pub struct Trivial<'a> {
    pub a: u32,
    pub b: &'a str,
    pub c: Result<&'a Path, &'static str>,
}

pub struct NonTrivial {
    pub a: *mut i32,
}

pub struct RecursiveWithPtr {
    pub a: *const RecursiveWithPtr,
}

pub struct RecursiveWithRef {
    pub a: &'static RecursiveWithRef,
}

enum Never {}

pub fn main() {
    {
        // trivial

        fn4(1, "", 1u128, 1.1); // CHECK: fn(i32, &str, u128, f64) {fn4::<i32, &str, u128, f64>} is trivial: true

        safe_fn!(()); // CHECK: fn(()) {main::f} is trivial: true
        safe_fn!(i32); // CHECK: fn(i32) {main::f} is trivial: true
        safe_fn!(usize); // CHECK: fn(usize) {main::f} is trivial: true
        safe_fn!(f64); // CHECK: fn(f64) {main::f} is trivial: true
        safe_fn!(&str); // CHECK: for<'r> fn(&'r str) {main::f} is trivial: true

        safe_fn!((&str, i32)); // CHECK: for<'r> fn((&'r str, i32)) {main::f} is trivial: true
        safe_fn!([usize; 0]); // CHECK: fn([usize; 0]) {main::f} is trivial: true
        safe_fn!([char; 3]); // CHECK: fn([char; 3]) {main::f} is trivial: true
        safe_fn!(&[u8; 3]); // for<'r> fn(&'r [u8; 3]) {main::f} is trivial: true

        safe_fn!(&Path); // CHECK: for<'r> fn(&'r std::path::Path) {main::f} is trivial: true
        safe_fn!(Cell<()>); // CHECK: fn(std::cell::Cell<()>) {main::f} is trivial: true
        safe_fn!(RefCell<()>); // CHECK: fn(std::cell::RefCell<()>) {main::f} is trivial: true
        safe_fn!(AtomicBool); // CHECK: fn(std::sync::atomic::AtomicBool) {main::f} is trivial: true

        safe_fn!(Trivial); // CHECK: for<'r> fn(Trivial<'r>) {main::f} is trivial: true

        // Types with internal pointers are trivial in safe functions with external types.
        safe_fn!(Vec<&str>); // for<'r> fn(std::vec::Vec<&'r str>) {main::f} is trivial: true
        safe_fn!(Box<()>); // CHECK: fn(std::boxed::Box<()>) {main::f} is trivial: true
        safe_fn!(PathBuf); // CHECK: fn(std::path::PathBuf) {main::f} is trivial: true
        safe_fn!(AtomicPtr<()>); // CHECK: fn(std::sync::atomic::AtomicPtr<()>) {main::f} is trivial: true
        safe_fn!(NonNull<u8>); // CHECK: fn(std::ptr::NonNull<u8>) {main::f} is trivial: true

        // TODO(kkysen) Test self-referential/recursive types through references (see #834).
        // Since transpiled types shouldn't have references, only pointers,
        // this shouldn't be an issue for a while until we get to partially-refactored code.
        // safe_fn!(RecursiveWithRef);

        safe_fn!(Never); // CHECK: fn(Never) {main::f} is trivial: true
    }

    {
        // non-trivial

        // Types with internal pointers are non-trivial in unsafe functions.
        unsafe_fn!(Vec<&str>); // for<'r> fn(std::vec::Vec<&'r str>) {main::f} is trivial: false
        unsafe_fn!(Box<()>); // CHECK: fn(std::boxed::Box<()>) {main::f} is trivial: false
        unsafe_fn!(PathBuf); // CHECK: fn(std::path::PathBuf) {main::f} is trivial: false

        unsafe_fn!(AtomicPtr<()>); // CHECK: fn(std::sync::atomic::AtomicPtr<()>) {main::f} is trivial: false

        unsafe_fn!(*const u8); // CHECK: fn(*const u8) {main::f} is trivial: false
        unsafe_fn!(*mut i32); // CHECK: fn(*mut i32) {main::f} is trivial: false

        unsafe_fn!(NonTrivial); // CHECK: fn(NonTrivial) {main::f} is trivial: false
        unsafe_fn!(RecursiveWithPtr); // CHECK: fn(RecursiveWithPtr) {main::f} is trivial: false
        
        // Types with internal pointers are non-trivial in safe functions when the types are local.
        safe_fn!(NonTrivial); // CHECK: fn(NonTrivial) {main::f} is trivial: false
        safe_fn!(RecursiveWithPtr); // CHECK: fn(RecursiveWithPtr) {main::f} is trivial: false

        // Types with directly visible pointers are non-trivial in safe functions even when the types are external.
        safe_fn!(*const u8); // CHECK: fn(*const u8) {main::f} is trivial: false
        safe_fn!(*mut i32); // CHECK: fn(*mut i32) {main::f} is trivial: false

        // Types with directly visible pointers are still non-trivial even in safe functions.
        safe_fn!(Box<*const u8>); // CHECK: fn(std::boxed::Box<*const u8>) {main::f} is trivial: false
        safe_fn!(&[*mut u8]); // CHECK: for<'r> fn(&'r [*mut u8]) {main::f} is trivial: false
        safe_fn!(&*mut u8); // CHECK: for<'r> fn(&'r *mut u8) {main::f} is trivial: false
        safe_fn!(*const *mut u8); // CHECK: fn(*const *mut u8) {main::f} is trivial: false
        safe_fn!(Vec<[&&*const u8; 4]>); // for<'r, 's> fn(std::vec::Vec<[&'r &'s *const u8; 4]>) {main::f} is trivial: false
    }
}
