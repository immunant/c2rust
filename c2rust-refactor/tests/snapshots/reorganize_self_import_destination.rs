#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_imports)]

pub mod dest {
    #[c2rust::header_src = "/home/user/some/workspace/dest.h:1"]
    pub mod dest_h {
        #[derive(Copy, Clone)]
        #[repr(C)]
        #[c2rust::src_loc = "2:0"]
        pub struct item {
            pub x: i32,
        }
    }

    // This import resolves to the declaration above. It must not block that
    // declaration from moving directly into this module; once moved, the
    // import is a self-import and must disappear.
    use self::dest_h::item;

    pub fn get(v: item) -> i32 {
        v.x
    }
}

fn main() {}
