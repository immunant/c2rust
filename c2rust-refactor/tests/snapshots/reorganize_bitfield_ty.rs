#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_imports)]

pub mod foo {
    #[c2rust::header_src = "/home/user/some/workspace/foobar/setup.h:5"]
    pub mod setup_h {
        #[c2rust::src_loc = "3:0"]
        pub type bit = u32;
    }

    #[c2rust::header_src = "/home/user/some/workspace/foobar/data.h:6"]
    pub mod data_h {
        // The string paths in the `bitfield` attributes below resolve
        // through this import.
        use super::setup_h::bit;

        #[repr(C)]
        #[c2rust::src_loc = "4:0"]
        pub struct data_t {
            #[bitfield(name = "flag", ty = "bit", bits = "0..=0")]
            #[bitfield(name = "wide", ty = "::core::ffi::c_uint", bits = "1..=3")]
            pub flag_wide: [u8; 1],
            #[bitfield(padding)]
            pub c2rust_padding: [u8; 3],
        }
    }

    pub fn use_data(d: *const data_h::data_t) -> setup_h::bit {
        unsafe { (*d).flag_wide[0] as setup_h::bit }
    }
}

pub mod bar {
    #[c2rust::header_src = "/home/user/some/workspace/foobar/setup.h:5"]
    pub mod setup_h {
        #[c2rust::src_loc = "3:0"]
        pub type bit = u32;
    }

    #[c2rust::header_src = "/home/user/some/workspace/foobar/data.h:6"]
    pub mod data_h {
        use super::setup_h::bit;

        #[repr(C)]
        #[c2rust::src_loc = "4:0"]
        pub struct data_t {
            #[bitfield(name = "flag", ty = "bit", bits = "0..=0")]
            #[bitfield(name = "wide", ty = "::core::ffi::c_uint", bits = "1..=3")]
            pub flag_wide: [u8; 1],
            #[bitfield(padding)]
            pub c2rust_padding: [u8; 3],
        }
    }

    pub fn use_data(d: *const data_h::data_t) -> setup_h::bit {
        unsafe { (*d).flag_wide[0] as setup_h::bit }
    }
}
