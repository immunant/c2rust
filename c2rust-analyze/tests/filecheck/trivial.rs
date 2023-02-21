use std::cell::{Cell, RefCell};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicPtr};

fn f4<A, B, C, D>(_a: A, _b: B, _c: C, _d: D) {}

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
macro_rules! f {
    ($t:ty) => {{
        #[allow(unused)]
        fn f(t: $t) {
            if false {
                f(t);
            }
        }
    }};
}

#[allow(dead_code)]
struct Trivial<'a> {
    a: u32,
    b: &'a str,
    c: Result<&'a Path, &'static str>,
}

#[allow(dead_code)]
struct NonTrivial {
    a: *mut i32,
}

enum Never {}

fn main() {
    {
        // trivial

        f4(1, "", 1u128, 1.1); // CHECK: fn(i32, &str, u128, f64) {f4::<i32, &str, u128, f64>} is trivial: true

        f!(()); // CHECK: fn(()) {main::f} is trivial: true
        f!(i32); // CHECK: fn(i32) {main::f} is trivial: true
        f!(usize); // CHECK: fn(usize) {main::f} is trivial: true
        f!(f64); // CHECK: fn(f64) {main::f} is trivial: true
        f!(&str); // CHECK: for<'r> fn(&'r str) {main::f} is trivial: true

        f!((&str, i32)); // CHECK: for<'r> fn((&'r str, i32)) {main::f} is trivial: true
        f!([usize; 0]); // CHECK: fn([usize; 0]) {main::f} is trivial: true
        f!([char; 3]); // CHECK: fn([char; 3]) {main::f} is trivial: true
        f!(&[u8; 3]); // for<'r> fn(&'r [u8; 3]) {main::f} is trivial: true

        f!(&Path); // CHECK: for<'r> fn(&'r std::path::Path) {main::f} is trivial: true
        f!(Cell<()>); // CHECK: fn(std::cell::Cell<()>) {main::f} is trivial: true
        f!(RefCell<()>); // CHECK: fn(std::cell::RefCell<()>) {main::f} is trivial: true
        f!(AtomicBool); // CHECK: fn(std::sync::atomic::AtomicBool) {main::f} is trivial: true

        f!(Trivial); // CHECK: for<'r> fn(Trivial<'r>) {main::f} is trivial: true

        f!(Never); // CHECK: fn(Never) {main::f} is trivial: true
    }

    {
        // non-trivial

        // std smart pointers and containers, while safe, have internal pointers,
        // so currently they're non trivial
        // see #820
        f!(Vec<&str>); // for<'r> fn(std::vec::Vec<&'r str>) {main::f} is trivial: false
        f!(Box<()>); // CHECK: fn(std::boxed::Box<()>) {main::f} is trivial: false
        f!(PathBuf); // CHECK: fn(std::path::PathBuf) {main::f} is trivial: false

        f!(AtomicPtr<()>); // CHECK: fn(std::sync::atomic::AtomicPtr<()>) {main::f} is trivial: false

        f!(*const u8); // CHECK: fn(*const u8) {main::f} is trivial: false
        f!(*mut i32); // CHECK: fn(*mut i32) {main::f} is trivial: false

        f!(NonTrivial); // CHECK: fn(NonTrivial) {main::f} is trivial: false
    }

    // TODO(kkysen) test self-referential types
}
