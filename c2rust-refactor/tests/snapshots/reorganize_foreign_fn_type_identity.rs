#![feature(rustc_private)]
#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]

extern crate libc;

// Each translation unit has its own `buf`, and the two differ in field
// visibility, so they are not unified and the second is renamed. Both units
// declare `fill` taking a `*mut buf` — spelled identically, but naming a
// different type in each unit. The two declarations therefore describe
// different functions and must not be collapsed into one.

pub mod a {
    use libc;

    #[c2rust::header_src = "/home/user/some/workspace/io.h:1"]
    pub mod io_h {
        use super::libc;

        #[repr(C)]
        #[c2rust::src_loc = "2:0"]
        pub struct buf {
            pub len: libc::c_int,
            pad: libc::c_int,
        }

        extern "C" {
            #[c2rust::src_loc = "3:0"]
            pub fn fill(b: *mut buf) -> libc::c_int;
        }
    }

    use io_h::{buf, fill};

    pub unsafe fn run() -> libc::c_int {
        let mut b = std::mem::zeroed::<buf>();
        fill(&mut b)
    }
}

pub mod b {
    use libc;

    #[c2rust::header_src = "/home/user/some/workspace/io.h:1"]
    pub mod io_h {
        use super::libc;

        // Same fields as the other `buf`, but all of them are public, so the
        // two are not interchangeable.
        #[repr(C)]
        #[c2rust::src_loc = "2:0"]
        pub struct buf {
            pub len: libc::c_int,
            pub pad: libc::c_int,
        }

        extern "C" {
            #[c2rust::src_loc = "3:0"]
            pub fn fill(b: *mut buf) -> libc::c_int;
        }
    }

    use io_h::{buf, fill};

    pub unsafe fn run() -> libc::c_int {
        let mut b = std::mem::zeroed::<buf>();
        fill(&mut b)
    }
}

fn main() {}
