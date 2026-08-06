#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]

pub mod stuff {
    // The types the header impls attach to are all defined here, outside any
    // header module, so no moved declaration ever consumes their saved
    // `impls` entries.
    #[derive(Copy, Clone)]
    #[repr(C)]
    pub struct alpha {
        pub x: i32,
    }

    #[derive(Copy, Clone)]
    #[repr(C)]
    pub struct beta {
        pub y: i32,
    }

    #[derive(Copy, Clone)]
    #[repr(C)]
    pub struct gamma {
        pub z: i32,
    }
}

pub mod opts {
    #[c2rust::header_src = "/home/user/some/workspace/opts.h:1"]
    pub mod opts_h {
        pub use crate::stuff::alpha;
        pub use crate::stuff::beta;
        pub use crate::stuff::gamma;

        // Three const-only impls, each on a different self type. All three
        // qualify to be carried alongside a moved declaration, so all three
        // land in the `impls` map, and none of them is ever claimed. They
        // must be reattached to `stuff` in a stable order.
        #[c2rust::src_loc = "2:0"]
        impl alpha {
            #[c2rust::src_loc = "3:0"]
            pub const DEFAULT: i32 = 1;
        }

        #[c2rust::src_loc = "4:0"]
        impl beta {
            #[c2rust::src_loc = "5:0"]
            pub const DEFAULT: i32 = 2;
        }

        #[c2rust::src_loc = "6:0"]
        impl gamma {
            #[c2rust::src_loc = "7:0"]
            pub const DEFAULT: i32 = 3;
        }
    }
    use self::opts_h::{alpha, beta, gamma};

    pub fn go() -> i32 {
        alpha::DEFAULT + beta::DEFAULT + gamma::DEFAULT
    }
}

fn main() {}
