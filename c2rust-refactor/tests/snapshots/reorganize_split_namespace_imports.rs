#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_imports)]

pub mod wrapper {
    #[c2rust::header_src = "/usr/include/struct_item.h:1"]
    pub mod struct_item_h {
        #[derive(Copy, Clone)]
        #[repr(C)]
        #[c2rust::src_loc = "2:0"]
        pub struct item {
            pub x: i32,
        }
    }
    #[c2rust::header_src = "/usr/include/sys_item.h:1"]
    pub mod sys_item_h {
        use super::struct_item_h::item;
        extern "C" {
            #[c2rust::src_loc = "3:0"]
            pub fn item(v: *const item) -> i32;
        }
    }
    // Two imports of one spelling, each resolving in a single namespace.
    // Their targets both move into `stdlib`, so the rewritten imports become
    // identical and resolve in both namespaces; only one may survive.
    pub use self::struct_item_h::item;
    pub use self::sys_item_h::item;

    pub unsafe fn call(v: item) -> i32 {
        item(&v)
    }
}

fn main() {}
