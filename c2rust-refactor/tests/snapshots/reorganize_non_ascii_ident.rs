#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]

// The destination module's name is one byte long, and the header module it
// contains starts with a two-byte character. `find_destination_id` tests
// whether the header name starts with the module name by slicing the header
// name at `module_ident.len()` *bytes*, which lands in the middle of the `é`
// and panics.
pub mod a {
    #[c2rust::header_src = "/home/user/some/workspace/é.h:1"]
    pub mod é_h {
        #[c2rust::src_loc = "2:0"]
        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct thing {
            pub x: i32,
        }
    }

    pub fn go() -> i32 {
        let t = é_h::thing { x: 1 };
        t.x
    }
}

fn main() {}
