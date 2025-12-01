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

extern crate libc;

#[c2rust::src_loc = "15:0"]
type outside = i32;

pub mod bar {
    use libc;

    #[c2rust::header_src = "/home/user/some/workspace/foobar/bar.h:5"]
    pub mod bar_h {
        // Test relative paths
        use super::super::outside;

        #[c2rust::src_loc = "11:0"]
        type FooInt = i32;

        // Comment on bar_t
        #[derive(Copy, Clone)]
        #[repr(C)]
        #[c2rust::src_loc = "10:0"]
        pub struct bar_t {
            //test1
            pub alloc: *mut libc::c_char,
            pub data: *mut libc::c_char,
            pub i: outside,
        }

        #[c2rust::src_loc = "8:0"]
        type OtherInt = i32;

        // Import both the statfs64 type and function declaration from
        // libc exactly as they are defined in that crate. However, since
        // both definitions have a private field, the transform shouldn't
        // unify them across crates.
        #[repr(C)]
        #[c2rust::src_loc = "7:0"]
        pub struct statvfs {
            pub f_bsize: libc::c_ulong,
            pub f_frsize: libc::c_ulong,
            pub f_blocks: libc::fsblkcnt_t,
            pub f_bfree: libc::fsblkcnt_t,
            pub f_bavail: libc::fsblkcnt_t,
            pub f_files: libc::fsfilcnt_t,
            pub f_ffree: libc::fsfilcnt_t,
            pub f_favail: libc::fsfilcnt_t,
            pub f_fsid: libc::c_ulong,
            pub f_flag: libc::c_ulong,
            pub f_namemax: libc::c_ulong,
            __f_spare: [libc::c_int; 6],
        }

        // Import both the statfs64 type and function declaration from
        // libc exactly as they are defined in that crate, so reorganize_definitions
        // replaces both of them with the libc definition.
        #[repr(C)]
        #[c2rust::src_loc = "6:0"]
        pub struct statfs64 {
            pub f_type: libc::__fsword_t,
            pub f_bsize: libc::__fsword_t,
            pub f_blocks: u64,
            pub f_bfree: u64,
            pub f_bavail: u64,
            pub f_files: u64,
            pub f_ffree: u64,
            pub f_fsid: libc::fsid_t,
            pub f_namelen: libc::__fsword_t,
            pub f_frsize: libc::__fsword_t,
            pub f_flags: libc::__fsword_t,
            pub f_spare: [libc::__fsword_t; 4],
        }

        extern "C" {
            #[c2rust::src_loc = "5:0"]
            pub fn statvfs(
                path: *const libc::c_char,
                buf: *mut statvfs,
            ) -> libc::c_int;

            #[c2rust::src_loc = "4:0"]
            pub fn statfs64(
                path: *const libc::c_char,
                buf: *mut statfs64,
            ) -> libc::c_int;
        }

        use super::libc;
    }

    #[c2rust::header_src = "compat.h:6"]
    pub mod compat_h {
        pub struct conflicting {
            pub x: libc::c_char,
        }
    }

    use bar_h::bar_t;

    #[no_mangle]
    static mut Bar: bar_t = bar_t {
        alloc: 0 as *mut libc::c_char,
        data: 0 as *mut libc::c_char,
        i: 0,
    };
}

pub mod foo {
    use libc;

    #[c2rust::header_src = "/home/user/some/workspace/foobar/bar.h:5"]
    pub mod bar_h {
        // Test relative paths
        use super::super::outside;

        // Comment on bar_t
        #[derive(Copy, Clone)]
        #[repr(C)]
        #[c2rust::src_loc = "10:0"]
        pub struct bar_t {
            //test2
            pub alloc: *mut libc::c_char,
            pub data: *mut libc::c_char,
            pub i: outside,
        }
        use super::libc;

        // Slightly different version of the structure: all fields are public.
        // This shouldn't get unified either.
        #[repr(C)]
        #[c2rust::src_loc = "7:0"]
        pub struct statvfs {
            pub f_bsize: libc::c_ulong,
            pub f_frsize: libc::c_ulong,
            pub f_blocks: libc::fsblkcnt_t,
            pub f_bfree: libc::fsblkcnt_t,
            pub f_bavail: libc::fsblkcnt_t,
            pub f_files: libc::fsfilcnt_t,
            pub f_ffree: libc::fsfilcnt_t,
            pub f_favail: libc::fsfilcnt_t,
            pub f_fsid: libc::c_ulong,
            pub f_flag: libc::c_ulong,
            pub f_namemax: libc::c_ulong,
            pub __f_spare: [libc::c_int; 6],
        }

        // This one is identical to the libc one
        #[repr(C)]
        #[c2rust::src_loc = "6:0"]
        pub struct statfs64 {
            pub f_type: libc::__fsword_t,
            pub f_bsize: libc::__fsword_t,
            pub f_blocks: u64,
            pub f_bfree: u64,
            pub f_bavail: u64,
            pub f_files: u64,
            pub f_ffree: u64,
            pub f_fsid: libc::fsid_t,
            pub f_namelen: libc::__fsword_t,
            pub f_frsize: libc::__fsword_t,
            pub f_flags: libc::__fsword_t,
            pub f_spare: [libc::__fsword_t; 4],
        }

        extern "C" {
            // Comment on Bar
            pub static mut Bar: bar_t;

            #[c2rust::src_loc = "5:0"]
            pub fn statvfs(
                path: *const libc::c_char,
                buf: *mut statvfs,
            ) -> libc::c_int;

            #[c2rust::src_loc = "4:0"]
            pub fn statfs64(
                path: *const libc::c_char,
                buf: *mut statfs64,
            ) -> libc::c_int;
        }
    }

    #[c2rust::header_src = "compat.h:6"]
    pub mod compat_h {
        pub struct conflicting {
            pub y: libc::c_char,
        }
    }
    use bar_h::{Bar, bar_t};
    use compat_h::conflicting;

    // Comment on foo_t
    #[derive(Copy, Clone)]
    #[repr(C)]
    pub struct foo_t {
        pub alloc: *mut libc::c_char,
        pub data: *mut libc::c_char,
    }

    unsafe fn foo() -> *const bar_t {
        // Use the local definitions.
        let mut buf = unsafe { std::mem::zeroed::<super::bar::bar_h::statvfs>() };
        super::bar::bar_h::statvfs(core::ptr::null(), &mut buf);

        // Use the definitions that have all public fields.
        // The transform should not reuse any of the libc declarations.
        let mut buf = unsafe { std::mem::zeroed::<super::foo::bar_h::statvfs>() };
        super::foo::bar_h::statvfs(core::ptr::null(), &mut buf);

        // Use the definitions that are identical to libc.
        let mut buf = unsafe { std::mem::zeroed::<super::bar::bar_h::statfs64>() };
        super::bar::bar_h::statfs64(core::ptr::null(), &mut buf);

        // Use the definitions that are identical to libc.
        let mut buf = unsafe { std::mem::zeroed::<super::foo::bar_h::statfs64>() };
        super::foo::bar_h::statfs64(core::ptr::null(), &mut buf);

        let _c = conflicting { y: 10 };
        &Bar as *const bar_t
    }
}

fn main() {
    println!("hello!");
}
