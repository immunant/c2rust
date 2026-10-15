#![allow(dead_code)]

pub mod lib {
    extern "C" {
        pub static mut TABLE: [::core::ffi::c_int; 0];
        pub static CONST_TABLE: [::core::ffi::c_int; 0];
    }
}

pub mod user {
    extern "C" {
        pub static mut TABLE: [::core::ffi::c_int; 3];
        pub static CONST_TABLE: [::core::ffi::c_int; 3];
    }

    pub unsafe fn get(i: ::core::ffi::c_int) -> ::core::ffi::c_int {
        TABLE[i as usize]
    }

    pub unsafe fn set_first(value: ::core::ffi::c_int) {
        TABLE[0] = value;
    }

    pub unsafe fn ptr() -> *mut ::core::ffi::c_int {
        TABLE.as_mut_ptr()
    }

    pub unsafe fn get_const(i: ::core::ffi::c_int) -> ::core::ffi::c_int {
        CONST_TABLE[i as usize]
    }

    pub unsafe fn size() -> usize {
        ::core::mem::size_of_val(&CONST_TABLE)
    }
}

fn main() {}
