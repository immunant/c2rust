#![feature(libc)]
#![feature(extern_types)]
#![feature(asm)]
#![feature(ptr_wrapping_offset_from)]
#![feature(custom_attribute)]
#![feature(rustc_private)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(mutable_transmutes)]
#![allow(unused_mut)]

extern crate libc;

type outside = i32;

pub mod bar {

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
    //test2
    use libc;
    static mut Bar: crate::bar::bar_t = unsafe {
        crate::bar::bar_t {
            alloc: 0 as *mut libc::c_char,
            data: 0 as *mut libc::c_char,
            i: 0,
        }
    };
}

pub mod foo {
    use libc;

    use crate::bar::bar_t;
    use crate::bar::Bar;

    // Comment on foo_t

    #[repr(C)]
    #[derive(Copy, Clone)]
    pub struct foo_t {
        pub alloc: *mut libc::c_char,
        pub data: *mut libc::c_char,
    }

    unsafe fn foo() -> *const crate::bar::bar_t {
        &crate::bar::Bar as *const crate::bar::bar_t
    }
}

fn main() {
    println!("hello!");
}
