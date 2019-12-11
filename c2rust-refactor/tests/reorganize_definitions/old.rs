#![feature(libc)]
#![feature(extern_types)]
#![feature(asm)]
#![feature(ptr_wrapping_offset_from)]
#![feature(rustc_private)]
#![register_tool(c2rust)]

#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(mutable_transmutes)]
#![allow(unused_mut)]

extern crate libc;

#[c2rust::src_loc = "15:0"]
type outside = i32;

pub mod bar {
    use libc;

    #[c2rust::header_src = "/home/user/some/workspace/foobar/bar.h:5"]
    pub mod bar_h {
        // Test relative paths
        use super::super::outside;

        // Comment on bar_t
        #[derive(Copy, Clone)]
        #[repr(C)]
        #[c2rust::src_loc = "10:0"]
        pub struct bar_t {
            //test1
            pub alloc: *mut libc::c_char,
            pub data: *mut libc::c_char,
            pub i: outside,
        }
        use super::libc;
    }

    #[c2rust::header_src = "compat.h:6"]
    pub mod compat_h {
        pub struct conflicting {
            pub x: libc::c_char,
        }
    }

    use bar_h::bar_t;

    #[no_mangle]
    static mut Bar: bar_t = unsafe {
        bar_t {
            alloc: 0 as *mut libc::c_char,
            data: 0 as *mut libc::c_char,
            i: 0,
        }
    };
}

pub mod foo {
    use libc;

    #[c2rust::header_src = "/home/user/some/workspace/foobar/bar.h:5"]
    pub mod bar_h {
        // Test relative paths
        use super::super::outside;

        // Comment on bar_t
        #[derive(Copy, Clone)]
        #[repr(C)]
        #[c2rust::src_loc = "10:0"]
        pub struct bar_t {
            //test2
            pub alloc: *mut libc::c_char,
            pub data: *mut libc::c_char,
            pub i: outside,
        }
        use super::libc;

        extern "C" {
            pub static mut Bar: bar_t;
        }
    }

    #[c2rust::header_src = "compat.h:6"]
    pub mod compat_h {
        pub struct conflicting {
            pub y: libc::c_char,
        }
    }
    use bar_h::{Bar, bar_t};
    use compat_h::conflicting;

    // Comment on foo_t
    #[derive(Copy, Clone)]
    #[repr(C)]
    pub struct foo_t {
        pub alloc: *mut libc::c_char,
        pub data: *mut libc::c_char,
    }

    unsafe fn foo() -> *const bar_t {
        let c = conflicting { y: 10 };
        &Bar as *const bar_t
    }
}

fn main() {
    println!("hello!");
}
