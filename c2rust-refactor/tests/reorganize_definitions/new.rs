#![feature(libc)]
#![feature(extern_types)]
#![feature(asm)]
#![feature(ptr_wrapping_offset_from)]
#![feature(custom_attribute)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(mutable_transmutes)]
#![allow(unused_mut)]

extern crate libc;

type outside = i32;

pub mod bar {
    use libc;

    // =============== BEGIN bar_h ================

    // Test relative paths
    use crate::outside;
    // Comment on bar_t
    #[repr(C)]
    #[derive(Copy, Clone)]
    pub struct bar_t {
        //test1
        pub alloc: *mut libc::c_char,
        pub data: *mut libc::c_char,
        pub i: outside,
    }
}

pub mod foo {
    use libc;

    // Comment on foo_t
    #[repr(C)]
    #[derive(Copy, Clone)]
    pub struct foo_t {
        pub alloc: *mut libc::c_char,
        pub data: *mut libc::c_char,
    }
}

fn main() {
    println!("hello!");
}
