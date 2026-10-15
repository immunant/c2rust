#![feature(c_variadic, register_tool)]
#![register_tool(c2rust)]
#![allow(dead_code)]

pub struct Pair<'a, 'b>(pub &'a (), pub &'b ());

pub mod first {
    #[c2rust::header_src = "s.h:1"]
    pub mod s_h {
        #[derive()]
        #[repr(C)]
        #[c2rust::src_loc = "1:1"]
        pub struct S<'a> {
            pub ap: ::core::ffi::VaListImpl<'a>,
        }

        #[repr(C)]
        #[c2rust::src_loc = "2:1"]
        pub struct Ref<'a> {
            pub r: &'a (),
        }

        #[repr(C)]
        #[c2rust::src_loc = "3:1"]
        pub struct Order<'a, 'b> {
            pub p: crate::Pair<'a, 'b>,
        }

        #[repr(C)]
        #[c2rust::src_loc = "4:1"]
        pub struct RefOrder<'a, 'b> {
            pub x: &'a (),
            pub y: &'b (),
        }
    }

    pub fn use_first(_s: Option<&s_h::S>, _r: s_h::Ref, _o: s_h::Order, _ro: s_h::RefOrder) {}
}

pub mod second {
    #[c2rust::header_src = "s.h:1"]
    pub mod s_h {
        #[derive()]
        #[repr(C)]
        #[c2rust::src_loc = "1:1"]
        pub struct S<'a> {
            pub ap: ::core::ffi::VaListImpl<'a>,
        }

        #[repr(C)]
        #[c2rust::src_loc = "2:1"]
        pub struct Ref<'a> {
            pub r: &'a (),
        }

        // Swapping which parameter each use refers to must keep these separate.
        #[repr(C)]
        #[c2rust::src_loc = "3:1"]
        pub struct Order<'a, 'b> {
            pub p: crate::Pair<'b, 'a>,
        }

        #[repr(C)]
        #[c2rust::src_loc = "4:1"]
        pub struct RefOrder<'a, 'b> {
            pub x: &'b (),
            pub y: &'a (),
        }
    }

    pub fn use_second(_s: Option<&s_h::S>, _r: s_h::Ref, _o: s_h::Order, _ro: s_h::RefOrder) {}
}

fn main() {}
