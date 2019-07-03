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

#[src_loc = "15:0"]
type outside = i32;

pub mod bar {
    use libc;

    #[header_src = "/home/user/some/workspace/foobar/bar.h:5"]
    pub mod bar_h {
        // Test relative paths
        use super::super::outside;

        // Comment on bar_t
        #[derive(Copy, Clone)]
        #[repr(C)]
        #[src_loc = "10:0"]
        pub struct bar_t {
            //test1
            pub alloc: *mut libc::c_char,
            pub data: *mut libc::c_char,
            pub i: outside,
        }
        use super::libc;
    }
}

pub mod foo {
    use libc;

    #[header_src = "/home/user/some/workspace/foobar/bar.h:5"]
    pub mod bar_h {
        // Test relative paths
        use super::super::outside;

        // Comment on bar_t
        #[derive(Copy, Clone)]
        #[repr(C)]
        #[src_loc = "10:0"]
        pub struct bar_t {
            //test2
            pub alloc: *mut libc::c_char,
            pub data: *mut libc::c_char,
            pub i: outside,
        }
        use super::libc;
    }

    // Comment on foo_t
    #[derive(Copy, Clone)]
    #[repr(C)]
    pub struct foo_t {
        pub alloc: *mut libc::c_char,
        pub data: *mut libc::c_char,
    }
}

fn main() {
    println!("hello!");
}
