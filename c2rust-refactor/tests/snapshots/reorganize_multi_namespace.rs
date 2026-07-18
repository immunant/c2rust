#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_imports)]

pub mod provider {
    // A braced struct and a function sharing one spelling: the type lives in
    // the type namespace and the function in the value namespace.
    #[derive(Copy, Clone)]
    pub struct tick {
        pub x: i32,
    }

    pub fn tick(v: tick) -> i32 {
        v.x
    }
}

pub mod dest {
    #[c2rust::header_src = "/home/user/some/workspace/dest.h:1"]
    pub mod dest_h {
        extern "C" {
            #[c2rust::src_loc = "2:0"]
            pub fn tick(x: i32) -> i32;
        }
    }
    // One simple use that imports both the type and the value. The header
    // declaration above must not be moved into this module: the value
    // namespace here is already occupied by this import.
    use crate::provider::tick;

    pub fn go(v: tick) -> i32 {
        tick(v)
    }
}

fn main() {}
