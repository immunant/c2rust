#![feature(rustc_private)]
#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]

extern crate libc;

// The same header declares `takes_args` with a different number of
// parameters in each translation unit, which is what a K&R-style `int f()`
// declaration in one unit and a prototyped `int f(int, int)` in another
// turn into. The two declarations are not interchangeable and must not be
// merged into one.

pub mod a {
    use libc;

    #[c2rust::header_src = "/home/user/some/workspace/decl.h:1"]
    pub mod decl_h {
        use super::libc;

        extern "C" {
            #[c2rust::src_loc = "2:0"]
            pub fn takes_args(x: libc::c_int) -> libc::c_int;
        }
    }

    use decl_h::takes_args;

    pub unsafe fn call_one() -> libc::c_int {
        takes_args(1)
    }
}

pub mod b {
    use libc;

    #[c2rust::header_src = "/home/user/some/workspace/decl.h:1"]
    pub mod decl_h {
        use super::libc;

        extern "C" {
            #[c2rust::src_loc = "2:0"]
            pub fn takes_args(x: libc::c_int, y: libc::c_int) -> libc::c_int;
        }
    }

    use decl_h::takes_args;

    pub unsafe fn call_two() -> libc::c_int {
        takes_args(1, 2)
    }
}

fn main() {}
