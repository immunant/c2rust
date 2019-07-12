#![allow(dead_code,
         mutable_transmutes,
         non_camel_case_types,
         non_snake_case,
         non_upper_case_globals,
         unused_assignments,
         unused_mut)]
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
pub type size_t = libc::c_ulong;
#[derive ( Copy , Clone )]
#[repr(C)]
pub struct S {
    pub field: libc::c_int,
}
#[no_mangle]
pub static mut global: *mut S = 0 as *const S as *mut S;
#[no_mangle]
pub unsafe extern "C" fn exercise_allocator() {
    let mut s: *mut S =
        malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s).field = 10i32;
    printf(b"%i\n\x00" as *const u8 as *const libc::c_char, (*s).field);
    s =
        realloc(s as *mut libc::c_void,
                (2i32 as
                     libc::c_ulong).wrapping_mul(::std::mem::size_of::<S>() as
                                                     libc::c_ulong)) as
            *mut S;
    (*s.offset(0isize)).field = 10i32;
    (*s.offset(1isize)).field = 11i32;
    let mut i: libc::c_int = 0i32;
    while i < 2i32 {
        printf(b"%i\n\x00" as *const u8 as *const libc::c_char,
               (*s.offset(i as isize)).field);
        i += 1
    }
    s =
        reallocarray(s as *mut libc::c_void, 3i32 as size_t,
                     ::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s.offset(0isize)).field = 10i32;
    (*s.offset(1isize)).field = 11i32;
    (*s.offset(2isize)).field = 12i32;
    let mut i_0: libc::c_int = 0i32;
    while i_0 < 3i32 {
        printf(b"%i\n\x00" as *const u8 as *const libc::c_char,
               (*s.offset(i_0 as isize)).field);
        i_0 += 1
    }
    free(s as *mut libc::c_void);
    s =
        calloc(4i32 as libc::c_ulong,
               ::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s.offset(0isize)).field = 10i32;
    (*s.offset(1isize)).field = 11i32;
    (*s.offset(2isize)).field = 12i32;
    (*s.offset(3isize)).field = 13i32;
    let mut i_1: libc::c_int = 0i32;
    while i_1 < 4i32 {
        printf(b"%i\n\x00" as *const u8 as *const libc::c_char,
               (*s.offset(i_1 as isize)).field);
        i_1 += 1
    }
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn simple_analysis() {
    let mut s: *mut S =
        malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s).field = 10i32;
    printf(b"%i\n\x00" as *const u8 as *const libc::c_char, (*s).field);
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn analysis2_helper(mut s: *mut S) {
    printf(b"%i\n\x00" as *const u8 as *const libc::c_char, (*s).field);
}
#[no_mangle]
pub unsafe extern "C" fn analysis2() {
    let mut s: *mut S =
        malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s).field = 10i32;
    analysis2_helper(s);
    free(s as *mut libc::c_void);
}
#[no_mangle]
pub unsafe extern "C" fn no_owner(mut should_free: libc::c_int) {
    global = malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    if 0 != should_free { free(global as *mut libc::c_void); };
}
#[no_mangle]
pub unsafe extern "C" fn invalid() {
    let mut s: *mut S =
        malloc(::std::mem::size_of::<S>() as libc::c_ulong) as *mut S;
    (*s).field = 10i32;
    global = s;
    printf(b"%i\n\x00" as *const u8 as *const libc::c_char, (*s).field);
    printf(b"%i\n\x00" as *const u8 as *const libc::c_char, (*global).field);
    global = 0 as *mut S;
    free(s as *mut libc::c_void);
}
unsafe fn main_0(mut argc: libc::c_int, mut argv: *mut *mut libc::c_char)
 -> libc::c_int {
    exercise_allocator();
    simple_analysis();
    analysis2();
    no_owner(0i32);
    no_owner(1i32);
    invalid();
    return 0i32;
}
pub fn main() {
    let mut args: Vec<*mut libc::c_char> = Vec::new();
    for arg in ::std::env::args() {
        args.push(::std::ffi::CString::new(arg).expect("Failed to convert argument into CString.").into_raw());
    };
    args.push(::std::ptr::null_mut());
    unsafe {
        ::std::process::exit(main_0((args.len() - 1) as libc::c_int,
                                    args.as_mut_ptr() as
                                        *mut *mut libc::c_char) as i32)
    }
}