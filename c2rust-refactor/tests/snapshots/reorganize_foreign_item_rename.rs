#![allow(clippy::missing_safety_doc)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(unused_assignments)]
#![allow(unused_mut)]
#![feature(register_tool)]
#![register_tool(c2rust)]

// Transpiled from two translation units that include the same header with
// different `-D` flags:
//
//     /* foreign.h */
//     #ifdef FOREIGN_ALT
//     typedef int cfg_int;
//     extern cfg_int cfg;
//     int compute(int (*p)[4]);
//     #else
//     extern int cfg;
//     int compute(int (*p)[]);
//     #endif
//
//     /* a.c, compiled without `-DFOREIGN_ALT` */
//     #include "foreign.h"
//
//     int a_use(int (*p)[4]) {
//         return cfg + compute(p);
//     }
//
//     /* b.c, compiled with `-DFOREIGN_ALT` */
//     #include "foreign.h"
//
//     int b_use(int (*p)[4]) {
//         return cfg + compute(p);
//     }
//
// The C is strictly conforming. `cfg_int` is an alias for `int`, not a distinct
// type, so both declarations of `cfg` give it the same type. The two `compute`
// signatures are compatible as well: an array type of unknown size is
// compatible with any array type of the same element type (C11 6.7.6.2p6), and
// pointers are compatible when their pointees are, so `int (*)[]` matches
// `int (*)[4]`. Every declaration of `cfg` and of `compute` therefore satisfies
// C11 6.2.7p2 ("All declarations that refer to the same object or function
// shall have compatible type").
//
// Both declarations of each item come from the same header, so they land in the
// same header module. Neither pair can be collapsed there. The two `cfg`
// declarations spell the same type differently, one through the alias, so they
// are not syntactically identical. C's incomplete array type has no Rust
// equivalent and is transpiled as a zero-length array, so the two `compute`
// declarations are not interchangeable either. The second of each pair is
// therefore renamed to clear the collision. Since an `extern` static or
// function links against its own name, the renamed declarations have to keep
// naming the original symbols.
//
// The two pairs survive for different reasons: the transform compares statics
// syntactically, which the alias defeats, but resolves types for functions,
// which collapses the alias and leaves the array sizes to keep the pair apart.

pub mod a {
    #[c2rust::header_src = "/home/user/some/workspace/foreign.h:1"]
    pub mod foreign_h {
        extern "C" {
            #[c2rust::src_loc = "6:1"]
            pub static mut cfg: ::core::ffi::c_int;
            #[c2rust::src_loc = "7:1"]
            pub fn compute(p: *mut [::core::ffi::c_int; 0]) -> ::core::ffi::c_int;
        }
    }
    use self::foreign_h::{cfg, compute};
    #[no_mangle]
    #[c2rust::src_loc = "3:1"]
    pub unsafe extern "C" fn a_use(mut p: *mut [::core::ffi::c_int; 4]) -> ::core::ffi::c_int {
        return cfg + compute(p as *mut [::core::ffi::c_int; 0]);
    }
}

pub mod b {
    #[c2rust::header_src = "/home/user/some/workspace/foreign.h:1"]
    pub mod foreign_h {
        #[c2rust::src_loc = "2:1"]
        pub type cfg_int = ::core::ffi::c_int;
        extern "C" {
            #[c2rust::src_loc = "3:1"]
            pub static mut cfg: cfg_int;
            #[c2rust::src_loc = "4:1"]
            pub fn compute(p: *mut [::core::ffi::c_int; 4]) -> ::core::ffi::c_int;
        }
    }
    pub use self::foreign_h::{cfg, cfg_int, compute};
    #[no_mangle]
    #[c2rust::src_loc = "3:1"]
    pub unsafe extern "C" fn b_use(mut p: *mut [::core::ffi::c_int; 4]) -> ::core::ffi::c_int {
        return cfg as ::core::ffi::c_int + compute(p);
    }
}

fn main() {}
