#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]
#![feature(const_raw_ptr_to_usize_cast)]
extern crate libc;
extern "C" {
    #[no_mangle]
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn calloc(_: libc::c_ulong, _: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn realloc(_: *mut libc::c_void, _: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn reallocarray(__ptr: *mut libc::c_void, __nmemb: size_t, __size: size_t)
        -> *mut libc::c_void;
    #[no_mangle]
    fn free(__ptr: *mut libc::c_void);
    #[no_mangle]
    fn printf(_: *const libc::c_char, _: ...) -> libc::c_int;
}

use libc::*;
use std::mem;
pub type size_t = libc::c_ulong;
#[derive(Copy, Clone)]
#[repr(C)]
pub struct S {
    pub field: libc::c_int,
    pub field2: libc::c_ulong,
    pub field3: *const S,
}
#[no_mangle]
pub static mut global: *mut S = 0 as *const S as *mut S;
#[no_mangle]
pub unsafe extern "C" fn malloc_wrapper(mut size: size_t) -> *mut libc::c_void {
    return malloc(size);
}
#[no_mangle]
pub unsafe extern "C" fn recur(x: libc::c_int, s: *mut S) {
    if x == 0 {
        return free(s as *mut libc::c_void);
    }

    recur(x - 1, s);
    let y = s;
}
#[no_mangle]
pub unsafe extern "C" fn simple() {
    let mut x = malloc(mem::size_of::<S>() as c_ulong) as *mut S;
    let y = malloc(mem::size_of::<S>() as c_ulong) as *mut S;
    let z = std::ptr::addr_of!((*x).field);
    x = y;
    (*x).field = 10i32;
    (*y).field = (*x).field;
    (*x).field2 = 9u64;
    let k = (*x).field;
    let z = std::ptr::addr_of!((*x).field2);
    (*x).field3 = std::ptr::addr_of!(*x) as *const S;
    recur(3, x);
    let s = *y;
    *x = s;
}
#[no_mangle]
pub unsafe extern "C" fn simple1() {
    let mut x = malloc(mem::size_of::<S>() as c_ulong) as *mut S;
    let z = realloc(x as *mut libc::c_void, mem::size_of::<S>() as c_ulong) as *mut S;

    let x_copy = z;
    (*x_copy).field = 10i32;
    let x_copy_2 = z;
    let x_copy_copy = x_copy;
    let addr_of_copy = std::ptr::addr_of!(x_copy_copy);
    let i_cast = x as usize;
    let x_from_int = i_cast as *const libc::c_void;
    free(x as *mut libc::c_void);
}

#[repr(C)]
pub struct buffer {
    pub ptr: *mut libc::c_char,
    pub used: uint32_t,
    pub size: uint32_t,
}
#[repr(C)]
pub struct chunk {
    mem: *mut buffer,
    offset: off_t,
}
pub unsafe extern "C" fn lighttpd_test(c: *mut chunk) {
    let mut chunks: [iovec; 32] = [iovec {
        iov_base: 0 as *mut libc::c_void,
        iov_len: 0,
    }; 32];
    chunks[10].iov_base = ((*(*c).mem).ptr).offset((*c).offset as isize) as *mut libc::c_void;
}
#[no_mangle]
pub unsafe extern "C" fn exercise_allocator() {
    let mut s: *mut S = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s).field = 10i32;
    printf(b"%i\n\x00" as *const u8 as *const libc::c_char, (*s).field);
    s = realloc(
        s as *mut libc::c_void,
        (2i32 as libc::c_ulong).wrapping_mul(::std::mem::size_of::<S>() as libc::c_ulong),
    ) as *mut S;
    (*s.offset(0isize)).field = 10i32;
    (*s.offset(1isize)).field = 11i32;
    let mut i: libc::c_int = 0i32;
    while i < 2i32 {
        printf(
            b"%i\n\x00" as *const u8 as *const libc::c_char,
            (*s.offset(i as isize)).field,
        );
        i += 1
    }
    s = reallocarray(
        s as *mut libc::c_void,
        3i32 as size_t,
        ::std::mem::size_of::<S>() as libc::c_ulong,
    ) as *mut S;
    (*s.offset(0isize)).field = 10i32;
    (*s.offset(1isize)).field = 11i32;
    (*s.offset(2isize)).field = 12i32;
    let mut i_0: libc::c_int = 0i32;
    while i_0 < 3i32 {
        printf(
            b"%i\n\x00" as *const u8 as *const libc::c_char,
            (*s.offset(i_0 as isize)).field,
        );
        i_0 += 1
    }
    free(s as *mut libc::c_void);
    s = calloc(
        4i32 as libc::c_ulong,
        ::std::mem::size_of::<S>() as libc::c_ulong,
    ) as *mut S;
    (*s.offset(0isize)).field = 10i32;
    (*s.offset(1isize)).field = 11i32;
    (*s.offset(2isize)).field = 12i32;
    (*s.offset(3isize)).field = 13i32;
    let mut i_1: libc::c_int = 0i32;
    while i_1 < 4i32 {
        printf(
            b"%i\n\x00" as *const u8 as *const libc::c_char,
            (*s.offset(i_1 as isize)).field,
        );
        i_1 += 1
    }
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn simple_analysis() {
    let mut s: *mut S = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s).field = 10i32;
    printf(b"%i\n\x00" as *const u8 as *const libc::c_char, (*s).field);
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn inter_function_analysis() {
    let mut s: *mut S = malloc_wrapper(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s).field = 11i32;
    printf(b"%i\n\x00" as *const u8 as *const libc::c_char, (*s).field);
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn analysis2_helper(mut s: *mut S) {
    printf(b"%i\n\x00" as *const u8 as *const libc::c_char, (*s).field);
}
#[no_mangle]
pub unsafe extern "C" fn analysis2() {
    let mut s: *mut S = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s).field = 10i32;
    analysis2_helper(s);
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn no_owner(mut should_free: libc::c_int) {
    global = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    if 0 != should_free {
        free(global as *mut libc::c_void);
    };
}
#[no_mangle]
pub unsafe extern "C" fn invalid() {
    let mut s: *mut S = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s).field = 10i32;
    global = s;
    printf(b"%i\n\x00" as *const u8 as *const libc::c_char, (*s).field);
    printf(
        b"%i\n\x00" as *const u8 as *const libc::c_char,
        (*global).field,
    );
    global = 0 as *mut S;
    free(s as *mut libc::c_void);
}
pub unsafe extern "C" fn testing() {
    let mut x = 10i32;
    let mut y = 32i32;
    let mut ptr = &mut x as *mut i32;
    let ref mut fresh1 = ptr;
    *fresh1 = &mut x as *mut i32;
}
#[no_mangle]
pub unsafe extern "C" fn test_malloc_free() {
    let s = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    free(s);
}
#[no_mangle]
pub unsafe extern "C" fn test_malloc_free_cast() {
    let s = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn foo(bar: *mut libc::c_void) {
    let baz = bar;
}
#[no_mangle]
pub unsafe extern "C" fn test_arg() {
    let mut s = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    foo(s);
    let t = s;
}
#[no_mangle]
pub unsafe extern "C" fn foo_rec(n: i32, bar: *mut libc::c_void) -> *mut libc::c_void {
    if n != 0 {
        let x = foo_rec(n - 1, bar);
        let baz = x;
        return baz;
    }

    bar
}
#[no_mangle]
pub unsafe extern "C" fn test_arg_rec() {
    let mut s = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    let t = foo_rec(3, s);
}
#[no_mangle]
pub unsafe extern "C" fn test_realloc_reassign() {
    let mut s = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    s = realloc(s, 2 * mem::size_of::<S>() as c_ulong);
    free(s);
}
#[no_mangle]
pub unsafe extern "C" fn test_realloc_fresh() {
    let s = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    let p = realloc(s, mem::size_of::<S>() as c_ulong);
    free(p);
}
#[no_mangle]
pub unsafe extern "C" fn test_load_addr() {
    let s = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    let x = (*s);
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn test_overwrite() {
    let mut s = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    let t = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    s = t;
    free(s);
}
#[no_mangle]
pub unsafe extern "C" fn test_store_addr() {
    let s = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s).field = 10i32;
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn test_load_other_store_self() {
    let s = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    let t = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s).field = 10i32;
    (*t).field = (*s).field;
    free(s as *mut libc::c_void);
    free(t as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn test_load_self_store_self() {
    let s = calloc(
        0i32 as libc::c_ulong,
        ::std::mem::size_of::<S>() as libc::c_ulong,
    ) as *mut S;
    (*s).field = (*s).field;
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn test_load_self_store_self_inter() {
    let s = calloc(
        0i32 as libc::c_ulong,
        ::std::mem::size_of::<S>() as libc::c_ulong,
    ) as *mut S;
    let y = (*s).field;
    (*s).field = y;
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn test_ptr_int_ptr() {
    let mut s = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    let x = s as usize;
    s = x as *mut libc::c_void;
    free(s);
}
#[no_mangle]
pub unsafe extern "C" fn test_load_value() {
    let s = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    let ps = std::ptr::addr_of!(s);
    free(*ps);
}
#[no_mangle]
pub unsafe extern "C" fn test_store_value() {
    let mut s = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    let t = s;
    let mut ps = std::ptr::addr_of_mut!(s);
    *ps = t;
    free(s);
}
#[no_mangle]
pub unsafe extern "C" fn test_store_value_field() {
    let mut s = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    let t = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*t).field3 = s;
    (*s).field3 = (*t).field3;
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn test_load_value_store_value() {
    let mut s = malloc(::std::mem::size_of::<S>() as libc::c_ulong);
    let ps = std::ptr::addr_of_mut!(s);
    *ps = *ps;
    free(*ps);
}
#[no_mangle]
pub unsafe extern "C" fn insertion_sort(n: libc::c_int, p: *mut libc::c_int) {
    let mut i: libc::c_int = 1 as libc::c_int;
    while i < n {
        let tmp: libc::c_int = *p.offset(i as isize);
        let mut j: libc::c_int = i;
        while j > 0 as libc::c_int && *p.offset((j - 1 as libc::c_int) as isize) > tmp {
            *p.offset(j as isize) = *p.offset((j - 1 as libc::c_int) as isize);
            j -= 1
        }
        *p.offset(j as isize) = tmp;
        i += 1
    }
}
unsafe fn main_0(mut argc: libc::c_int, mut argv: *mut *mut libc::c_char) -> libc::c_int {
    // simple();
    // exercise_allocator();
    // simple_analysis();
    // analysis2();
    // inter_function_analysis();
    // no_owner(0i32);
    // no_owner(1i32);
    // invalid();
    // testing();
    // simple1();
    // lighttpd_test(std::ptr::null_mut());

    // test_malloc_free();
    // test_malloc_free_cast();
    // test_arg();
    // test_arg_rec();
    // test_realloc_reassign();
    // test_realloc_fresh();
    // test_load_addr();
    // test_overwrite();
    // test_store_addr();
    // test_load_other_store_self();
    test_load_self_store_self();
    // test_load_self_store_self_inter();
    // test_ptr_int_ptr();
    // test_load_value();
    // test_store_value();
    // test_store_value_field();
    // test_load_value_store_value();
    // let nums = &mut [2i32, 5i32, 3i32, 1i32, 6i32];
    // insertion_sort(nums.len() as libc::c_int, nums as *mut libc::c_int);
    return 0i32;
}
pub fn main() {
    let mut args: Vec<*mut libc::c_char> = Vec::new();
    for arg in ::std::env::args() {
        println!("{:?}", arg);
        args.push(
            ::std::ffi::CString::new(arg)
                .expect("Failed to convert argument into CString.")
                .into_raw(),
        );
    }
    args.push(::std::ptr::null_mut());
    unsafe {
        main_0(
            (args.len() - 1) as libc::c_int,
            args.as_mut_ptr() as *mut *mut libc::c_char,
        );
    }
}
