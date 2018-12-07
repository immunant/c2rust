#![allow(dead_code,
         mutable_transmutes,
         non_camel_case_types,
         non_snake_case,
         non_upper_case_globals,
         unused_mut)]
#![feature(plugin, custom_attribute)]
#![plugin(c2rust_analysis_plugin)]
#![lifetime_analysis]
#![feature(libc)]
extern crate libc;
extern "C" {
    #[no_mangle]
    fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn realloc(_: *mut libc::c_void, _: libc::c_ulong) -> *mut libc::c_void;
    #[no_mangle]
    fn printf(_: *const libc::c_char, ...) -> libc::c_int;
}
#[derive ( Copy , Clone )]
#[repr(C)]
pub struct S {
    pub field: libc::c_int,
}

pub unsafe extern "C" fn main_0(mut argc: libc::c_int,
                                mut argv: *mut *mut libc::c_char)
 -> libc::c_int {
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
    printf(b"%i\n\x00" as *const u8 as *const libc::c_char,
           (*s.offset(0isize)).field);
    printf(b"%i\n\x00" as *const u8 as *const libc::c_char,
           (*s.offset(1isize)).field);
    return 0i32;
}

#[test]
fn test() {
    unsafe { main_0(1, std::mem::transmute(["pointers"].as_mut_ptr())) };
}
