#![feature ( libc )]
#![feature ( i128_type )]
#![feature ( const_ptr_null )]
#![feature ( offset_to )]
#![feature ( const_ptr_null_mut )]
#![allow ( non_upper_case_globals )]
#![allow ( non_camel_case_types )]
#![allow ( non_snake_case )]
#![allow ( dead_code )]
#![allow ( mutable_transmutes )]
extern crate libc;
type __builtin_ms_va_list = *mut libc::c_char;
#[derive ( Copy , Clone )]
#[repr ( C )]
pub struct __NSConstantString_tag {
    isa: *const libc::c_int,
    flags: libc::c_int,
    str: *const libc::c_char,
    length: libc::c_long,
}
type __builtin_va_list = [__va_list_tag; 1];
#[derive ( Copy , Clone )]
#[repr ( C )]
pub struct __va_list_tag {
    gp_offset: libc::c_uint,
    fp_offset: libc::c_uint,
    overflow_arg_area: *mut libc::c_void,
    reg_save_area: *mut libc::c_void,
}
type __uint128_t = u128;
type __NSConstantString = __NSConstantString_tag;
type __int128_t = i128;
unsafe extern "C" fn baz() -> libc::c_int {
    static mut k: libc::c_int = 0i32;
    counter += 1;
    return k + 1i32;
}
static mut counter: libc::c_int = 0;
extern "C" {
    #[no_mangle]
    fn main() -> libc::c_int;
}
#[no_mangle]
pub static mut visible_everywhere: libc::c_int = 9i32;
static mut hello: *const libc::c_char =
    b"hello\x00" as *const u8 as *const libc::c_char;
#[no_mangle]
pub unsafe extern "C" fn entry(buffer_size: libc::c_uint,
                               mut buffer: *mut libc::c_int) -> () {
    static mut world: *const libc::c_char =
        b"world\x00" as *const u8 as *const libc::c_char;
    if buffer_size < 11i32 as libc::c_uint { return; };
    *buffer.offset(0isize) = baz();
    *buffer.offset(1isize) = baz();
    *buffer.offset(2isize) = baz() + 1i32;
    *buffer.offset(baz() as isize) = 4i32;
    *buffer.offset(7isize) = counter;
    counter -= 1;
    baz();
    *buffer.offset(8isize) = counter;
    *buffer.offset(9isize) = *hello.offset(0isize) as libc::c_int;
    *buffer.offset(10isize) = *world.offset(1isize) as libc::c_int;
}
