#![feature(extern_types, register_tool)]
#![register_tool(c2rust)]
#![allow(dead_code, improper_ctypes_definitions)]

pub struct Pair<'a, 'b>(pub &'a (), pub &'b ());
pub struct Marker<const N: usize>;

pub mod first {
    #[c2rust::header_src = "callbacks.h:1"]
    pub mod callbacks_h {
        extern "C" {
            #[c2rust::src_loc = "1:1"]
            pub type CallbackContext;
        }

        #[repr(C)]
        #[c2rust::src_loc = "2:1"]
        pub struct Relationships {
            pub f: for<'a, 'b> unsafe fn(*mut CallbackContext, crate::Pair<'a, 'a>, &'b ()),
        }

        #[repr(C)]
        #[c2rust::src_loc = "3:1"]
        pub struct Nested {
            pub f: for<'a> unsafe fn(*mut CallbackContext, &'a (), for<'b> fn(&'a (), &'b ())),
        }

        #[repr(C)]
        #[c2rust::src_loc = "4:1"]
        pub struct Equivalent {
            pub f: for<'a> unsafe fn(*mut CallbackContext, &'a (), for<'b> fn(&'a (), &'b ())),
        }

        #[repr(C)]
        #[c2rust::src_loc = "5:1"]
        pub struct Safety {
            pub f: for<'a> fn(*mut CallbackContext, &'a ()),
        }

        #[repr(C)]
        #[c2rust::src_loc = "6:1"]
        pub struct Abi {
            pub f: for<'a> unsafe extern "C" fn(*mut CallbackContext, &'a ()),
        }

        #[repr(C)]
        #[c2rust::src_loc = "7:1"]
        pub struct Constants {
            pub f: for<'a> unsafe fn(*mut CallbackContext, &'a crate::Marker<1>),
        }

        #[repr(C)]
        #[c2rust::src_loc = "8:1"]
        pub struct Packed {
            pub tag: u8,
            pub f: for<'a> unsafe extern "C" fn(*mut CallbackContext, &'a ()),
        }

        #[repr(C)]
        #[c2rust::src_loc = "9:1"]
        pub struct Aligned {
            pub f: for<'a> unsafe extern "C" fn(*mut CallbackContext, &'a ()),
        }

        #[repr(C)]
        #[c2rust::src_loc = "10:1"]
        pub struct ArrayLength {
            pub tag: [[u64; 0]; 1],
            pub f: for<'a> unsafe extern "C" fn(*mut CallbackContext, &'a ()),
        }
    }
}

pub mod second {
    #[c2rust::header_src = "callbacks.h:1"]
    pub mod callbacks_h {
        extern "C" {
            #[c2rust::src_loc = "1:1"]
            pub type CallbackContext;
        }

        // Equal binder counts do not imply equal lifetime relationships.
        #[repr(C)]
        #[c2rust::src_loc = "2:1"]
        pub struct Relationships {
            pub f: for<'a, 'b> unsafe fn(*mut CallbackContext, crate::Pair<'a, 'b>, &'b ()),
        }

        // Swapping the outer and inner lifetime must keep these separate.
        #[repr(C)]
        #[c2rust::src_loc = "3:1"]
        pub struct Nested {
            pub f: for<'a> unsafe fn(*mut CallbackContext, &'a (), for<'b> fn(&'b (), &'a ())),
        }

        // Renaming bound lifetimes preserves equivalence, even when nested.
        #[repr(C)]
        #[c2rust::src_loc = "4:1"]
        pub struct Equivalent {
            pub f: for<'x> unsafe fn(*mut CallbackContext, &'x (), for<'y> fn(&'x (), &'y ())),
        }

        #[repr(C)]
        #[c2rust::src_loc = "5:1"]
        pub struct Safety {
            pub f: for<'a> unsafe fn(*mut CallbackContext, &'a ()),
        }

        #[repr(C)]
        #[c2rust::src_loc = "6:1"]
        pub struct Abi {
            pub f: for<'a> unsafe fn(*mut CallbackContext, &'a ()),
        }

        #[repr(C)]
        #[c2rust::src_loc = "7:1"]
        pub struct Constants {
            pub f: for<'a> unsafe fn(*mut CallbackContext, &'a crate::Marker<2>),
        }

        #[repr(C, packed)]
        #[c2rust::src_loc = "8:1"]
        pub struct Packed {
            pub tag: u8,
            pub f: for<'a> unsafe extern "C" fn(*mut CallbackContext, &'a ()),
        }

        #[repr(C, align(32))]
        #[c2rust::src_loc = "9:1"]
        pub struct Aligned {
            pub f: for<'a> unsafe extern "C" fn(*mut CallbackContext, &'a ()),
        }

        #[repr(C)]
        #[c2rust::src_loc = "10:1"]
        pub struct ArrayLength {
            pub tag: [[u64; 1]; 1],
            pub f: for<'a> unsafe extern "C" fn(*mut CallbackContext, &'a ()),
        }
    }
}

fn main() {}
