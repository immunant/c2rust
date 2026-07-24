#![feature(rustc_private)]
#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]

extern crate libc;

// Two translation units declare the same C global with incompatible types,
// so the two declarations cannot be collapsed into one and the second is
// renamed to avoid the collision. An `extern` static links against its own
// name, so the renamed declaration has to keep naming the original symbol.

pub mod a {
    use libc;

    #[c2rust::header_src = "/home/user/some/workspace/cfg.h:1"]
    pub mod cfg_h {
        use super::libc;

        #[derive(Copy, Clone)]
        #[repr(C)]
        #[c2rust::src_loc = "2:0"]
        pub struct a_t {
            pub x: libc::c_int,
        }

        extern "C" {
            #[c2rust::src_loc = "3:0"]
            pub static mut cfg: a_t;
        }
    }

    use cfg_h::cfg;

    pub unsafe fn get() -> libc::c_int {
        cfg.x
    }
}

pub mod b {
    use libc;

    #[c2rust::header_src = "/home/user/some/workspace/cfg.h:1"]
    pub mod cfg_h {
        use super::libc;

        #[derive(Copy, Clone)]
        #[repr(C)]
        #[c2rust::src_loc = "2:0"]
        pub struct b_t {
            pub y: libc::c_double,
        }

        extern "C" {
            #[c2rust::src_loc = "3:0"]
            pub static mut cfg: b_t;
        }
    }

    use cfg_h::cfg;

    pub unsafe fn get() -> libc::c_double {
        cfg.y
    }
}

fn main() {}
