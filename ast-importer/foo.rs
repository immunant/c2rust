#![allow(dead_code,
         mutable_transmutes,
         non_camel_case_types,
         non_snake_case,
         non_upper_case_globals,
         unused_mut)]
#![feature(libc)]
extern crate libc;
// Comment 1
#[no_mangle]
pub unsafe extern "C" fn foo(mut x: libc::c_int) -> libc::c_int {
    // comment 2
    return x + 1i32;
}