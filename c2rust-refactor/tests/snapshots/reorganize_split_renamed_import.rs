#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_imports)]

pub mod other {
    #[c2rust::header_src = "/home/user/some/workspace/other.h:1"]
    pub mod other_h {
        // This incompatible struct shares the `tick` spelling and is
        // processed first, so the struct in dest.h below is renamed to
        // `tick_1` when both are moved out of their headers.
        #[derive(Copy, Clone)]
        #[repr(C)]
        #[c2rust::src_loc = "2:0"]
        pub struct tick {
            pub y: f64,
        }
    }

    pub fn other_fn() {}
}

pub mod dest {
    #[c2rust::header_src = "/home/user/some/workspace/dest.h:1"]
    pub mod dest_h {
        // A type and a value sharing the `tick` spelling. Both move into
        // `dest`, but the struct is renamed to `tick_1` by the collision
        // with other.h while the static keeps its name, so their new paths
        // differ even though they land in the same module.
        #[derive(Copy, Clone)]
        #[repr(C)]
        #[c2rust::src_loc = "3:0"]
        pub struct tick {
            pub x: i32,
        }

        extern "C" {
            #[c2rust::src_loc = "9:0"]
            pub static tick: i32;
        }
    }

    pub fn dest_fn() {}
}

pub mod user {
    // One simple import resolving in both the type and value namespaces.
    // After the reorganization the two targets have different paths
    // (`tick_1` vs `tick`), so a second import must be added for the value
    // namespace: the retained import only covers the renamed type.
    use crate::dest::dest_h::tick;

    pub fn make(x: i32) -> tick {
        tick { x }
    }

    pub fn read() -> i32 {
        unsafe { tick }
    }
}

fn main() {}
