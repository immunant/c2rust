#![feature(rustc_private)]
#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]

extern crate libc;

// Two translation units declare the same C function with incompatible
// signatures, so the two declarations cannot be collapsed into one and the
// second is renamed to avoid the collision. An `extern` function links
// against its own name, so the renamed declaration has to keep naming the
// original symbol.

pub mod a {
    use libc;

    #[c2rust::header_src = "/home/user/some/workspace/decl.h:1"]
    pub mod decl_h {
        use super::libc;

        extern "C" {
            #[c2rust::src_loc = "2:0"]
            pub fn compute(x: libc::c_int) -> libc::c_int;
        }
    }

    use decl_h::compute;

    pub unsafe fn call() -> libc::c_int {
        compute(1)
    }
}

pub mod b {
    use libc;

    #[c2rust::header_src = "/home/user/some/workspace/decl.h:1"]
    pub mod decl_h {
        use super::libc;

        extern "C" {
            #[c2rust::src_loc = "2:0"]
            pub fn compute(x: libc::c_double) -> libc::c_double;
        }
    }

    use decl_h::compute;

    pub unsafe fn call() -> libc::c_double {
        compute(1.0)
    }
}

fn main() {}
