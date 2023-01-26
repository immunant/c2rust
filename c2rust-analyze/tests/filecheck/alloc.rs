#![feature(extern_types)]
#![feature(label_break_value)]
#![feature(rustc_private)]
#![feature(c_variadic)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(mutable_transmutes)]
#![allow(unused_mut)]
#![allow(unused_imports)]
#![allow(unused_variables)]

extern crate libc;

use libc::*;
use std::mem;
pub type size_t = libc::c_ulong;

extern "C" {
    fn c2rust_test_typed_malloc(_: libc::c_ulong) -> *mut i32;
    fn c2rust_test_typed_realloc(_: *mut i32, _: libc::c_ulong) -> *mut i32;
    fn c2rust_test_typed_free(__ptr: *mut i32);
    fn c2rust_test_typed_calloc(_: libc::c_ulong, _: libc::c_ulong) -> *mut i32;
}

// CHECK-LABEL: final labeling for "calloc1"
unsafe extern "C" fn calloc1() -> *mut i32 {
    // CHECK-DAG: ([[@LINE+1]]: i): addr_of = UNIQUE
    let i = c2rust_test_typed_calloc(
        1 as libc::c_int as libc::c_ulong,
        ::std::mem::size_of::<i32>() as libc::c_ulong,
    );
    if i.is_null() {}

    return i;
}

// CHECK-LABEL: final labeling for "malloc1"
pub unsafe extern "C" fn malloc1(mut cnt: libc::c_int) -> *mut i32 {
    // CHECK-DAG: ([[@LINE+1]]: i): addr_of = UNIQUE, type = READ
    let i = c2rust_test_typed_malloc(::std::mem::size_of::<i32>() as libc::c_ulong);
    let x = *i;
    return i;
}

// CHECK-LABEL: final labeling for "free1"
unsafe extern "C" fn free1(mut i: *mut i32) {
    // CHECK-DAG: ([[@LINE+1]]: i): {{.*}}type = UNIQUE | FREE#
    c2rust_test_typed_free(i);
}

// CHECK-LABEL: final labeling for "realloc1"
unsafe extern "C" fn realloc1(mut i: *mut i32, len: libc::c_ulong) {
    let mut capacity = 1;
    let mut x = 1;
    // CHECK-DAG: ([[@LINE+1]]: mut elem): addr_of = UNIQUE, type = READ | WRITE | OFFSET_ADD | OFFSET_SUB
    let mut elem = i;
    loop {
        if x == capacity {
            capacity *= 2;
            // CHECK-DAG: ([[@LINE+1]]: i): addr_of = UNIQUE, type = FREE
            i = c2rust_test_typed_realloc(i, ::std::mem::size_of::<i32>() as libc::c_ulong);
        }
        *elem = 1;
        elem = elem.offset(1isize);
        if x == 10 {
            break;
        }
        x += 1;
    }
}
