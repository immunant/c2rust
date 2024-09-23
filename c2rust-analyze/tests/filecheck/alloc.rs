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
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    fn realloc(_: *mut libc::c_void, _: libc::c_ulong) -> *mut libc::c_void;
    fn free(_: *mut libc::c_void);
    fn calloc(_: libc::c_ulong, _: libc::c_ulong) -> *mut libc::c_void;
}

// CHECK-LABEL: final labeling for "malloc_and_free1"
pub unsafe extern "C" fn malloc_and_free1(mut cnt: libc::c_int) {
    // CHECK-DAG: ([[@LINE+1]]: mut i): addr_of = UNIQUE | NON_NULL | STACK, type = READ | UNIQUE | FREE | NON_NULL | HEAP#
    let mut i = malloc(::std::mem::size_of::<i32>() as libc::c_ulong) as *mut i32;
    // Perform an access to constrain the pointee type.  Without a pointee type, some rewrites will
    // be skipped.
    let x = *i;
    // CHECK-DAG: ([[@LINE+1]]: i{{.*}}): {{.*}}type = UNIQUE | FREE | NON_NULL | HEAP#
    free(i as *mut libc::c_void);
}

// CHECK-LABEL: final labeling for "malloc_and_free2"
pub unsafe extern "C" fn malloc_and_free2(mut cnt: libc::c_int) {
    // CHECK-DAG: ([[@LINE+1]]: mut i): addr_of = UNIQUE | NON_NULL | STACK, type = READ | WRITE | UNIQUE | FREE | NON_NULL | HEAP#
    let mut i = malloc(::std::mem::size_of::<i32>() as libc::c_ulong) as *mut i32;
    if !i.is_null() {
        // CHECK-DAG: ([[@LINE+1]]: mut b): addr_of = UNIQUE | NON_NULL | STACK, type = READ | WRITE | UNIQUE | NON_NULL | HEAP#
        let mut b = i;
        *b = 2;
        // CHECK-DAG: ([[@LINE+1]]: i): {{.*}}type = UNIQUE | FREE | NON_NULL | HEAP#
        free(i as *mut libc::c_void);
    }
}

// CHECK-LABEL: final labeling for "malloc_and_free3"
pub unsafe extern "C" fn malloc_and_free3(mut cnt: libc::c_int) {
    // CHECK-DAG: ([[@LINE+1]]: mut i): addr_of = UNIQUE | NON_NULL | STACK, type = READ | WRITE | UNIQUE | FREE | NON_NULL | HEAP#
    let mut i: *mut i32 = malloc(::std::mem::size_of::<i32>() as libc::c_ulong) as *mut i32;
    // CHECK-DAG: ([[@LINE+1]]: mut b): addr_of = UNIQUE | NON_NULL | STACK, type = READ | WRITE | UNIQUE | FREE | NON_NULL | HEAP#
    let mut b: *mut i32 = i;
    *b = 2;
    // CHECK-DAG: ([[@LINE+1]]: b): {{.*}}type = UNIQUE | FREE | NON_NULL | HEAP#
    free(b as *mut libc::c_void);
}

// CHECK-LABEL: final labeling for "calloc_and_free1"
pub unsafe extern "C" fn calloc_and_free1(mut cnt: libc::c_int) {
    // CHECK-DAG: ([[@LINE+1]]: mut i): addr_of = UNIQUE | NON_NULL | STACK, type = READ | UNIQUE | FREE | NON_NULL | HEAP#
    let mut i = calloc(1, ::std::mem::size_of::<i32>() as libc::c_ulong) as *mut i32;
    // Perform an access to constrain the pointee type.  Without a pointee type, some rewrites will
    // be skipped.
    let x = *i;
    // CHECK-DAG: ([[@LINE+1]]: i{{.*}}): {{.*}}type = UNIQUE | FREE | NON_NULL | HEAP#
    free(i as *mut libc::c_void);
}

// CHECK-LABEL: final labeling for "realloc1"
unsafe extern "C" fn realloc1(n: libc::c_ulong) {
    // CHECK-DAG: ([[@LINE+1]]: mut buf): addr_of = UNIQUE | NON_NULL | STACK, type = READ | WRITE | UNIQUE | OFFSET_ADD | OFFSET_SUB | FREE | NON_NULL | HEAP
    let mut buf: *mut i32 = malloc(2 * std::mem::size_of::<i32>() as libc::c_ulong) as *mut i32;
    let mut len = 0;
    let mut capacity = 2;
    memset(buf as *mut libc::c_void, 0, 2 * std::mem::size_of::<i32>() as usize);

    let mut i = 0;
    while i < n {
        if len == capacity {
            capacity *= 2;
            // CHECK-DAG: ([[@LINE+2]]: buf{{.*}}): addr_of = UNIQUE | NON_NULL | STACK, type = UNIQUE | OFFSET_ADD | OFFSET_SUB | FREE | NON_NULL | HEAP
            buf = realloc(
                buf as *mut libc::c_void,
                (capacity * std::mem::size_of::<i32>()) as libc::c_ulong,
            ) as *mut i32;
        }
        *buf.offset(i as isize) = i as i32;
        len += 1;
        i += 1;
    }

    free(buf as *mut libc::c_void);
}


// CHECK-LABEL: final labeling for "malloc_return"
pub unsafe extern "C" fn malloc_return(mut cnt: libc::c_int) -> *mut i32 {
    // CHECK-DAG: ([[@LINE+1]]: mut i): addr_of = UNIQUE | NON_NULL | STACK, type = READ | WRITE | UNIQUE | FREE | NON_NULL | HEAP#
    let mut i: *mut i32 = malloc(::std::mem::size_of::<i32>() as libc::c_ulong) as *mut i32;
    i
}

// CHECK-LABEL: final labeling for "malloc_return_free1"
pub unsafe extern "C" fn malloc_return_free1(mut cnt: libc::c_int) {
    // CHECK-DAG: ([[@LINE+1]]: mut i): addr_of = UNIQUE | NON_NULL | STACK, type = READ | WRITE | UNIQUE | FREE | NON_NULL | HEAP#
    let mut i: *mut i32 = malloc_return(cnt);
    *i = 2;
    // CHECK-DAG: ([[@LINE+1]]: i{{.*}}): {{.*}}type = UNIQUE | FREE | NON_NULL | HEAP#
    free(i as *mut libc::c_void);
}


// Rewrites of malloc/calloc/realloc/memset should use `mem::size_of` to convert byte counts to
// element counts.
// CHECK: let n = byte_len as usize / std::mem::size_of::<i32>();
