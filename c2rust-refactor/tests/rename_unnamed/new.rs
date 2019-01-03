#![feature(extern_types)]
#![feature(asm)]
#![feature(ptr_wrapping_offset_from)]
#![feature(custom_attribute)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(mutable_transmutes)]
#![allow(unused_mut)]

pub mod bar {
    #[header_src = "/home/user/some/workspace/foobar/bar.h"]
    pub mod bar_h {

        #[repr(C)]
        #[derive(Copy, Clone)]
        pub struct unnamed_0 {
            a: usize,
        }

        #[repr(C)]
        #[derive(Copy, Clone)]
        pub struct unnamed_1 {
            x: i32,
            y: i32,
        }

        #[repr(C)]
        #[derive(Copy, Clone)]
        pub struct bar_t {
            u: unnamed_0,
        }
    }
    use self::bar_h::*;
}

pub mod foo {
    #[header_src = "/home/user/some/workspace/foobar/foo.h"]
    pub mod foo_h {

        #[repr(C)]
        #[derive(Copy, Clone)]
        pub struct unnamed_2 {
            b: usize,
        }

        #[repr(C)]
        #[derive(Copy, Clone)]
        pub struct unnamed_3 {
            c: usize,
        }

        #[repr(C)]
        #[derive(Copy, Clone)]
        pub struct foo_t {
            u: unnamed_2,
        }
    }

    use self::foo_h::unnamed_3;
    use self::foo_h::{foo_t, unnamed_2};

    #[repr(C)]
    #[derive(Copy, Clone)]
    pub struct foo_bar {
        u: unnamed_2,
        u2: unnamed_3,
    }
}

pub mod test {
    pub mod unnamed {}
}

struct unnamed_4 {
    d: u32,
}

fn main() {
    let u = unnamed_4 { d: 0 };

    println!("{}", u.d);
}
