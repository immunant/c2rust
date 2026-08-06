#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_imports)]

pub mod defs {
    #[derive(Copy, Clone)]
    pub struct thing {
        pub x: i32,
    }

    pub const LIMIT: i32 = 10;
}

pub mod user {
    #[c2rust::header_src = "/home/user/some/workspace/user.h:1"]
    pub mod user_h {
        // A glob import in a header module. It has no single ident to merge
        // on, so `reorganize_definitions` must keep it in the header instead
        // of trying to move it (or panicking). Note that items moved out of
        // this header cannot rely on the glob's bindings: bare paths that
        // resolve through it are not canonicalized on the way out.
        pub use crate::defs::*;

        #[c2rust::src_loc = "3:0"]
        #[derive(Copy, Clone)]
        pub struct config {
            pub t: crate::defs::thing,
        }
    }
    use self::user_h::config;

    pub fn go(c: config) -> i32 {
        c.t.x + crate::defs::LIMIT
    }
}

fn main() {}
