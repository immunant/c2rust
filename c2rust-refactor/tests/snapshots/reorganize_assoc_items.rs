#![feature(extern_types)]
#![feature(rustc_private)]
#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(mutable_transmutes)]
#![allow(unused_mut)]

pub mod bar {
    #[c2rust::header_src = "/home/user/some/workspace/foobar/bar.h:5"]
    pub mod bar_h {
        #[derive(Clone, Copy)]
        #[repr(transparent)]
        #[c2rust::src_loc = "14:0"]
        pub struct C2Rust_Unnamed_2(pub ::core::ffi::c_uint);

        #[c2rust::src_loc = "14:0"]
        impl C2Rust_Unnamed_2 {
            #[c2rust::src_loc = "15:0"]
            pub const U: Self = Self(42);
        }

        #[derive(Clone, Copy)]
        #[repr(transparent)]
        #[c2rust::src_loc = "11:0"]
        pub struct SomeEnum(pub ::core::ffi::c_uint);

        #[c2rust::src_loc = "11:0"]
        impl SomeEnum {
            #[c2rust::src_loc = "12:0"]
            pub const A: Self = Self(0);
            #[c2rust::src_loc = "13:0"]
            pub const B: Self = Self(1);
        }
    }
}

pub mod foo {
    #[c2rust::header_src = "/home/user/some/workspace/foobar/bar.h:5"]
    pub mod bar_h {
        #[derive(Clone, Copy)]
        #[repr(transparent)]
        #[c2rust::src_loc = "4:0"]
        pub struct C2Rust_Unnamed_3(pub ::core::ffi::c_uint);

        #[c2rust::src_loc = "4:0"]
        impl C2Rust_Unnamed_3 {
            #[c2rust::src_loc = "5:0"]
            pub const V: Self = Self(42);
        }

        #[derive(Clone, Copy)]
        #[repr(transparent)]
        #[c2rust::src_loc = "1:0"]
        pub struct SomeEnum(pub ::core::ffi::c_uint);

        #[c2rust::src_loc = "1:0"]
        impl SomeEnum {
            #[c2rust::src_loc = "2:0"]
            pub const A: Self = Self(0);
            #[c2rust::src_loc = "3:0"]
            pub const B: Self = Self(1);
        }
    }

    unsafe fn foo() {
        let e1 = super::foo::bar_h::SomeEnum::A;
        let e2 = super::bar::bar_h::SomeEnum::B;
        let e3 = super::bar::bar_h::C2Rust_Unnamed_2::U;
        let e4 = super::foo::bar_h::C2Rust_Unnamed_3::V;
    }
}

fn main() {
    println!("hello!");
}
