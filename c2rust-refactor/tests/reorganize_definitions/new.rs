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

pub mod compat_h {
    pub struct conflicting {
        pub x: libc::c_char,
    }

    pub struct conflicting_1 {
        pub y: libc::c_char,
    }
}
extern crate libc;

type outside = i32;

pub mod bar {

    extern "C" {
        pub fn statvfs(path: *const libc::c_char, buf: *mut crate::bar::statvfs) -> libc::c_int;
    }
    // =============== BEGIN bar_h ================

    // Test relative paths
    use crate::outside;
    //test2
    use libc;
    // Import both the statfs64 type and function declaration from
    // libc exactly as they are defined in that crate. However, since
    // both definitions have a private field, the transform shouldn't
    // unify them across crates.
    #[repr(C)]

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
    // Slightly different version of the structure: all fields are public.
    // This shouldn't get unified either.
    #[repr(C)]

    pub struct statvfs_1 {
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
    //test1
    type OtherInt = i32;
    // Comment on bar_t

    #[repr(C)]
    #[derive(Copy, Clone)]
    pub struct bar_t {
        //test1
        pub alloc: *mut libc::c_char,
        pub data: *mut libc::c_char,
        pub i: outside,
    }

    type FooInt = i32;

    #[no_mangle]
    static mut Bar: crate::bar::bar_t = crate::bar::bar_t {
        alloc: 0 as *mut libc::c_char,
        data: 0 as *mut libc::c_char,
        i: 0,
    };
}

pub mod foo {
    use libc;

    use crate::bar::bar_t;
    use crate::bar::Bar;
    use crate::compat_h::conflicting_1;

    // Comment on foo_t

    #[repr(C)]
    #[derive(Copy, Clone)]
    pub struct foo_t {
        pub alloc: *mut libc::c_char,
        pub data: *mut libc::c_char,
    }

    unsafe fn foo() -> *const crate::bar::bar_t {
        // Use the local definitions.
        let mut buf = unsafe { std::mem::zeroed::<crate::bar::statvfs>() };
        crate::bar::statvfs(core::ptr::null(), &mut buf);

        // Use the definitions that have all public fields.
        // The transform should not reuse any of the libc declarations.
        let mut buf = unsafe { std::mem::zeroed::<crate::bar::statvfs_1>() };
        crate::bar::statvfs(core::ptr::null(), &mut buf);

        // Use the definitions that are identical to libc.
        let mut buf = unsafe { std::mem::zeroed::<::libc::statfs64>() };
        ::libc::statfs64(
            core::ptr::null(),
            &mut buf as *mut _ as *mut ::libc::statfs64,
        );

        // Use the definitions that are identical to libc.
        let mut buf = unsafe { std::mem::zeroed::<::libc::statfs64>() };
        ::libc::statfs64(
            core::ptr::null(),
            &mut buf as *mut _ as *mut ::libc::statfs64,
        );

        let _c = crate::compat_h::conflicting_1 { y: 10 };
        &crate::bar::Bar as *const crate::bar::bar_t
    }
}

fn main() {
    println!("hello!");
}
