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

pub mod bar {
    use libc;

    #[header_src = "/home/user/some/workspace/foobar/bar.h"]
    pub mod bar_h {
        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct bar_t {
            pub alloc: *mut libc::c_char,
            pub data: *mut libc::c_char,
        }
        use super::libc;
    }
}

pub mod foo {
    use libc;

    #[header_src = "/home/user/some/workspace/foobar/bar.h"]
    pub mod bar_h {
        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct bar_t {
            pub alloc: *mut libc::c_char,
            pub data: *mut libc::c_char,
        }
        use super::libc;
    }

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
