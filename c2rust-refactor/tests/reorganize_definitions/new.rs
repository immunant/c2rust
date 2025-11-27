#![feature(extern_types)]
#![feature(rustc_private)]
#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(mutable_transmutes)]
#![allow(unused_mut)]

pub mod compat_h {
    pub struct conflicting {
        pub x: libc::c_char,
    }

    pub struct conflicting_1 {
        pub y: libc::c_char,
    }
}
extern crate libc;

type outside = i32;

pub mod bar {

    // =============== BEGIN bar_h ================

    // Test relative paths
    use crate::outside;
    //test2
    use libc;
    //test1
    type OtherInt = i32;
    // Comment on bar_t

    #[repr(C)]
    #[derive(Copy, Clone)]
    pub struct bar_t {
        //test1
        pub alloc: *mut libc::c_char,
        pub data: *mut libc::c_char,
        pub i: outside,
    }

    type FooInt = i32;

    #[no_mangle]
    static mut Bar: crate::bar::bar_t = crate::bar::bar_t {
        alloc: 0 as *mut libc::c_char,
        data: 0 as *mut libc::c_char,
        i: 0,
    };
}

pub mod foo {
    use libc;

    use crate::bar::bar_t;
    use crate::bar::Bar;
    use crate::compat_h::conflicting_1;

    // Comment on foo_t

    #[repr(C)]
    #[derive(Copy, Clone)]
    pub struct foo_t {
        pub alloc: *mut libc::c_char,
        pub data: *mut libc::c_char,
    }

    unsafe fn foo() -> *const crate::bar::bar_t {
        let _c = crate::compat_h::conflicting_1 { y: 10 };
        &crate::bar::Bar as *const crate::bar::bar_t
    }
}

fn main() {
    println!("hello!");
}
