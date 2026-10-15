#![feature(extern_types, register_tool)]
#![register_tool(c2rust)]
#![allow(dead_code)]

pub mod first {
    #[c2rust::header_src = "types.h:1"]
    pub mod types_h {
        #[c2rust::src_loc = "1:1"]
        pub type Accessor = u16;
    }

    pub mod accessors {
        pub use super::types_h::Accessor;
        pub use core::primitive::u32 as Builtin;
    }

    #[c2rust::header_src = "bits.h:1"]
    pub mod bits_h {
        use super::accessors as types;
        use super::types_h::Accessor as Imported;
        use c2rust_bitfields::BitfieldStruct;
        #[c2rust::src_loc = "4:1"]
        pub type C2Rust_Unnamed_0 = u32;
        #[c2rust::src_loc = "5:1"]
        pub type Word = u32;

        extern "C" {
            #[c2rust::src_loc = "1:1"]
            pub type CallbackContext;
        }

        #[derive(BitfieldStruct)]
        #[repr(C)]
        #[c2rust::src_loc = "2:1"]
        pub struct DifferentBits {
            #[bitfield(name = "flag", ty = "u32", bits = "0..=0")]
            pub tag: [u8; 1],
            pub callback: for<'a> unsafe extern "C" fn(*mut CallbackContext, &'a ()),
        }

        #[derive(BitfieldStruct)]
        #[repr(C)]
        #[c2rust::src_loc = "3:1"]
        pub struct SameBits {
            #[bitfield(name = "flag", ty = "C2Rust_Unnamed_0", bits = "0..=0")]
            pub tag: [u8; 1],
            pub callback: for<'a> unsafe extern "C" fn(*mut CallbackContext, &'a ()),
        }

        #[derive(BitfieldStruct)]
        #[repr(C)]
        #[c2rust::src_loc = "6:1"]
        pub struct DifferentType {
            #[bitfield(name = "flag", ty = "Word", bits = "0..=0")]
            pub tag: [u8; 1],
        }

        #[derive(BitfieldStruct)]
        #[repr(C)]
        #[c2rust::src_loc = "8:1"]
        pub struct RelativeDiff {
            #[bitfield(name = "flag", ty = "super::types_h::Accessor", bits = "0..=0")]
            pub tag: [u8; 1],
        }

        #[derive(BitfieldStruct)]
        #[repr(C)]
        #[c2rust::src_loc = "9:1"]
        pub struct ImportDiff {
            #[bitfield(name = "flag", ty = "Imported", bits = "0..=0")]
            pub tag: [u8; 1],
        }

        #[derive(BitfieldStruct)]
        #[repr(C)]
        #[c2rust::src_loc = "10:1"]
        pub struct ModuleImportDiff {
            #[bitfield(name = "flag", ty = "self::types::Accessor", bits = "0..=0")]
            pub tag: [u8; 1],
        }

        #[derive(BitfieldStruct)]
        #[repr(C)]
        #[c2rust::src_loc = "11:1"]
        pub struct ShadowDiff {
            #[bitfield(name = "flag", ty = "i16", bits = "0..=0")]
            pub tag: [u8; 1],
        }

        #[derive(BitfieldStruct)]
        #[repr(C)]
        #[c2rust::src_loc = "12:1"]
        pub struct MixedSame {
            #[bitfield(name = "flag", ty = "self::Word", bits = "0..=0")]
            #[bitfield(name = "other", ty = "types::Builtin", bits = "1..=1")]
            pub tag: [u8; 1],
        }

        #[derive(BitfieldStruct)]
        #[repr(C)]
        #[c2rust::src_loc = "13:1"]
        pub struct MixedExternal {
            #[bitfield(name = "flag", ty = "::core::ffi::c_uint", bits = "0..=0")]
            pub tag: [u8; 1],
        }

        #[derive(BitfieldStruct)]
        #[repr(C)]
        #[c2rust::src_loc = "14:1"]
        pub struct ExternalSame {
            #[bitfield(name = "flag", ty = "std::ffi::c_char", bits = "0..=0")]
            pub tag: [u8; 1],
        }
    }
}

pub mod second {
    #[c2rust::header_src = "types.h:1"]
    pub mod types_h {
        #[c2rust::src_loc = "1:1"]
        pub type Accessor = i32;
    }

    pub mod accessors {
        pub use super::types_h::Accessor;
        pub use core::primitive::u32 as Builtin;
    }

    #[c2rust::header_src = "bits.h:1"]
    pub mod bits_h {
        use super::accessors as types;
        use super::types_h::Accessor as Imported;
        use c2rust_bitfields::BitfieldStruct;
        #[c2rust::src_loc = "7:1"]
        pub type i16 = i32;
        #[c2rust::src_loc = "4:1"]
        pub type C2Rust_Unnamed_1 = u32;
        #[c2rust::src_loc = "5:1"]
        pub type Word = i32;

        extern "C" {
            #[c2rust::src_loc = "1:1"]
            pub type CallbackContext;
        }

        // Identical storage does not imply identical generated accessors.
        #[derive(BitfieldStruct)]
        #[repr(C)]
        #[c2rust::src_loc = "2:1"]
        pub struct DifferentBits {
            #[bitfield(name = "flag", ty = "u32", bits = "1..=1")]
            pub tag: [u8; 1],
            pub callback: for<'a> unsafe extern "C" fn(*mut CallbackContext, &'a ()),
        }

        #[derive(BitfieldStruct)]
        #[repr(C)]
        #[c2rust::src_loc = "3:1"]
        pub struct SameBits {
            #[bitfield(name = "flag", ty = "C2Rust_Unnamed_1", bits = "0..=0")]
            pub tag: [u8; 1],
            pub callback: for<'a> unsafe extern "C" fn(*mut CallbackContext, &'a ()),
        }

        #[derive(BitfieldStruct)]
        #[repr(C)]
        #[c2rust::src_loc = "6:1"]
        pub struct DifferentType {
            #[bitfield(name = "flag", ty = "Word", bits = "0..=0")]
            pub tag: [u8; 1],
        }

        #[derive(BitfieldStruct)]
        #[repr(C)]
        #[c2rust::src_loc = "8:1"]
        pub struct RelativeDiff {
            #[bitfield(name = "flag", ty = "super::types_h::Accessor", bits = "0..=0")]
            pub tag: [u8; 1],
        }

        #[derive(BitfieldStruct)]
        #[repr(C)]
        #[c2rust::src_loc = "9:1"]
        pub struct ImportDiff {
            #[bitfield(name = "flag", ty = "Imported", bits = "0..=0")]
            pub tag: [u8; 1],
        }

        #[derive(BitfieldStruct)]
        #[repr(C)]
        #[c2rust::src_loc = "10:1"]
        pub struct ModuleImportDiff {
            #[bitfield(name = "flag", ty = "types::Accessor", bits = "0..=0")]
            pub tag: [u8; 1],
        }

        #[derive(BitfieldStruct)]
        #[repr(C)]
        #[c2rust::src_loc = "11:1"]
        pub struct ShadowDiff {
            #[bitfield(name = "flag", ty = "i16", bits = "0..=0")]
            pub tag: [u8; 1],
        }

        #[derive(BitfieldStruct)]
        #[repr(C)]
        #[c2rust::src_loc = "12:1"]
        pub struct MixedSame {
            #[bitfield(name = "flag", ty = "u32", bits = "0..=0")]
            #[bitfield(name = "other", ty = "u32", bits = "1..=1")]
            pub tag: [u8; 1],
        }

        #[derive(BitfieldStruct)]
        #[repr(C)]
        #[c2rust::src_loc = "13:1"]
        pub struct MixedExternal {
            #[bitfield(name = "flag", ty = "u32", bits = "0..=0")]
            pub tag: [u8; 1],
        }

        #[derive(BitfieldStruct)]
        #[repr(C)]
        #[c2rust::src_loc = "14:1"]
        pub struct ExternalSame {
            #[bitfield(name = "flag", ty = "::std::ffi::c_char", bits = "0..=0")]
            pub tag: [u8; 1],
        }
    }
}

fn main() {}
