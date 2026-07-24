#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]

pub mod a {
    #[c2rust::header_src = "/home/user/some/workspace/ü.h:1"]
    pub mod ü_h {
        #[c2rust::src_loc = "2:0"]
        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct thing {
            pub x: i32,
        }
    }

    pub fn go() -> i32 {
        let t = ü_h::thing { x: 1 };
        t.x
    }
}

pub mod é {
    #[c2rust::header_src = "/home/user/some/workspace/é.h:3"]
    pub mod é_h {
        #[c2rust::src_loc = "4:0"]
        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct other {
            pub y: i32,
        }
    }

    pub fn go() -> i32 {
        let o = é_h::other { y: 2 };
        o.y
    }
}

fn main() {}
