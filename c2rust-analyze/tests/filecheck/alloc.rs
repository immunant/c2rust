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

extern "C" {
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    fn realloc(_: *mut libc::c_void, _: libc::c_ulong) -> *mut libc::c_void;
    fn free(__ptr: *mut libc::c_void);
    fn calloc(_: libc::c_ulong, _: libc::c_ulong) -> *mut libc::c_void;
}

// CHECK-LABEL: final labeling for "calloc1"
unsafe extern "C" fn calloc1() -> *mut i32 {
    // CHECK-DAG: ([[@LINE+1]]: i): addr_of = UNIQUE
    let i = calloc(
        1 as libc::c_int as libc::c_ulong,
        ::std::mem::size_of::<i32>() as libc::c_ulong,
    ) as *mut i32;
    if i.is_null() {}

    return i;
}

// CHECK-LABEL: final labeling for "malloc1"
pub unsafe extern "C" fn malloc1(mut cnt: libc::c_int) -> *mut i32 {
    // CHECK-DAG: ([[@LINE+1]]: i): addr_of = UNIQUE, type = READ
    let i = malloc(::std::mem::size_of::<i32>() as libc::c_ulong) as *mut i32;
    let x = *i;
    return i;
}

// CHECK-LABEL: final labeling for "free1"
unsafe extern "C" fn free1(mut i: *mut i32) {
    // CHECK-DAG: ([[@LINE+1]]: i{{.*}}): {{.*}}type = UNIQUE | FREE#
    free(i as *mut libc::c_void);
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
            // CHECK-DAG: ([[@LINE+2]]: i{{.*}}): addr_of = UNIQUE, type = FREE
            i = realloc(
                i as *mut libc::c_void,
                4 as libc::c_ulong,
            ) as *mut i32;
        }
        *elem = 1;
        elem = elem.offset(1isize);
        if x == 10 {
            break;
        }
        x += 1;
    }
}

// CHECK-LABEL: final labeling for "alloc_and_free"
pub unsafe extern "C" fn alloc_and_free(mut cnt: libc::c_int) {
    // CHECK-DAG: ([[@LINE+1]]: i): addr_of = UNIQUE, type = UNIQUE | FREE#
    let i = malloc(::std::mem::size_of::<i32>() as libc::c_ulong) as *mut i32;
    // CHECK-DAG: ([[@LINE+1]]: i{{.*}}): {{.*}}type = UNIQUE | FREE#
    free(i as *mut libc::c_void);
}
