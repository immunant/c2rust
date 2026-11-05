#![feature(extern_types, register_tool)]
#![register_tool(c2rust)]
#![allow(dead_code)]

pub mod first {
    #[c2rust::header_src = "bits.h:1"]
    pub mod bits_h {
        extern "C" {
            #[c2rust::src_loc = "1:1"]
            pub type CallbackContext;
        }

        #[repr(C)]
        #[c2rust::src_loc = "2:1"]
        pub struct DifferentBits {
            #[bitfield(name = "flag", ty = "u32", bits = "0..=0")]
            pub tag: [u8; 1],
            pub callback: for<'a> unsafe extern "C" fn(*mut CallbackContext, &'a ()),
        }

        #[repr(C)]
        #[c2rust::src_loc = "3:1"]
        pub struct SameBits {
            #[bitfield(name = "flag", ty = "u32", bits = "0..=0")]
            pub tag: [u8; 1],
            pub callback: for<'a> unsafe extern "C" fn(*mut CallbackContext, &'a ()),
        }
    }
}

pub mod second {
    #[c2rust::header_src = "bits.h:1"]
    pub mod bits_h {
        extern "C" {
            #[c2rust::src_loc = "1:1"]
            pub type CallbackContext;
        }

        // Identical storage does not imply identical generated accessors.
        #[repr(C)]
        #[c2rust::src_loc = "2:1"]
        pub struct DifferentBits {
            #[bitfield(name = "flag", ty = "u32", bits = "1..=1")]
            pub tag: [u8; 1],
            pub callback: for<'a> unsafe extern "C" fn(*mut CallbackContext, &'a ()),
        }

        #[repr(C)]
        #[c2rust::src_loc = "3:1"]
        pub struct SameBits {
            #[bitfield(name = "flag", ty = "u32", bits = "0..=0")]
            pub tag: [u8; 1],
            pub callback: for<'a> unsafe extern "C" fn(*mut CallbackContext, &'a ()),
        }
    }
}

fn main() {}
