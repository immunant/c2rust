//! allow_crash

use std::cell::{Cell, RefCell};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicPtr};
use std::sync::{Mutex, RwLock};

fn f<T>(_t: T) {}

fn f4<A, B, C, D>(_a: A, _b: B, _c: C, _d: D) {}

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

fn main() {
    {
        // trivial

        f(()); // CHECK: fn(()) {f::<()>} is trivial: true
        f(1); // CHECK: fn(i32) {f::<i32>} is trivial: true
        f(2usize); // CHECK: fn(usize) {f::<usize>} is trivial: true
        f(3.14); // CHECK: fn(f64) {f::<f64>} is trivial: true
        f(""); // CHECK: fn(&str) {f::<&str>} is trivial: true
        f(("", 1)); // CHECK: fn((&str, i32)) {f::<(&str, i32)>} is trivial: true

        f([0usize; 0]); // CHECK: fn([usize; 0]) {f::<[usize; 0]>} is trivial: true
        f(['a', 'b', 'c']); // CHECK: fn([char; 3]) {f::<[char; 3]>} is trivial: true
        f(b"abc"); // CHECK: fn(&[u8; 3]) {f::<&[u8; 3]>} is trivial: true

        f4((), 1, "", [b""; 4]); // CHECK: fn((), i32, &str, [&[u8; 0]; 4]) {f4::<(), i32, &str, [&[u8; 0]; 4]>} is trivial: true
        f(Some("")); // CHECK: fn(std::option::Option<&str>) {f::<std::option::Option<&str>>} is trivial: true
        Path::new(""); // CHECK: for<'r> fn(&'r str) -> &'r std::path::Path {std::path::Path::new::<str>} is trivial: true
        Cell::new(0); // CHECK: fn(i32) -> std::cell::Cell<i32> {std::cell::Cell::<i32>::new} is trivial: true
        RefCell::new(0); // CHECK: fn(i32) -> std::cell::RefCell<i32> {std::cell::RefCell::<i32>::new} is trivial: true
        let _ = AtomicBool::new(true); // CHECK: fn(bool) -> std::sync::atomic::AtomicBool {std::sync::atomic::AtomicBool::new} is trivial: true
        
        Mutex::new(0); // CHECK: fn(i32) -> std::sync::Mutex<i32> {std::sync::Mutex::<i32>::new} is trivial: true
        RwLock::new(0); // CHECK: fn(i32) -> std::sync::RwLock<i32> {std::sync::RwLock::<i32>::new} is trivial: true

        // custom structs
        // can't use normal initialization b/c we don't have aggregate initialization support yet,
        // but we just need the type, so we can use None::<T>
        f(None::<Trivial>); // CHECK: fn(std::option::Option<Trivial>) {f::<std::option::Option<Trivial>>} is trivial: true
    }

    {
        // non-trivial

        // std smart pointers and containers, while safe, have internal pointers,
        // so currently they're non trivial
        // see #820
        let _ = Vec::<&str>::new(); // CHECK: fn() -> std::vec::Vec<&str> {std::vec::Vec::<&str>::new} is trivial: false
        let _ = Box::new(0); // CHECK: fn(i32) -> std::boxed::Box<i32> {std::boxed::Box::<i32>::new} is trivial: false
        let _ = PathBuf::from(""); // CHECK: fn(&str) -> std::path::PathBuf {<std::path::PathBuf as std::convert::From<&str>>::from} is trivial: false
        f(None::<AtomicPtr<i32>>); // CHECK: fn(std::option::Option<std::sync::atomic::AtomicPtr<i32>>) {f::<std::option::Option<std::sync::atomic::AtomicPtr<i32>>>} is trivial: false

        f(Some(b"" as *const u8)); // CHECK: fn(std::option::Option<*const u8>) {f::<std::option::Option<*const u8>>} is trivial: false

        // custom structs
        // can't use normal initialization b/c we don't have aggregate initialization support yet,
        // but we just need the type, so we can use None::<T>
        f(None::<NonTrivial>); // CHECK: fn(std::option::Option<NonTrivial>) {f::<std::option::Option<NonTrivial>>} is trivial: false
    }

    // TODO test self-referential types

    #[allow(unreachable_code)]
    f(panic!()); // CHECK: fn(&str) -> ! {std::rt::begin_panic::<&str>} is trivial: true
}
