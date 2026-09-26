#![feature(rustc_private)]
#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]

extern crate libc;

// `counter` carries `#[no_mangle]`, so it is visible to the linker from
// another translation unit, but it is private to its module in Rust terms.
// The header declaration in `user` is matched to it and every path to the
// declaration is rewritten to point here, from outside this module.

pub mod def {
    use libc;

    #[no_mangle]
    #[c2rust::src_loc = "10:0"]
    static mut counter: libc::c_int = 0;
}

pub mod user {
    use libc;

    #[c2rust::header_src = "/home/user/some/workspace/counter.h:1"]
    pub mod counter_h {
        use super::libc;

        extern "C" {
            #[c2rust::src_loc = "2:0"]
            pub static mut counter: libc::c_int;
        }
    }

    use counter_h::counter;

    pub unsafe fn read() -> libc::c_int {
        counter
    }
}

fn main() {}
