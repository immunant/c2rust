#![feature(extern_types)]
#![register_tool(c2rust)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(mutable_transmutes)]
#![allow(unused_mut)]

pub mod bar {
    #[header_src = "/home/user/some/workspace/foobar/bar.h"]
    pub mod bar_h {
        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct C2Rust_Unnamed {
            a: usize,
        }

        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct C2Rust_Unnamed_0 {
            x: i32,
            y: i32,
        }

        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct bar_t {
            u: C2Rust_Unnamed,
        }
    }
    use self::bar_h::*;
}

pub mod foo {
    #[header_src = "/home/user/some/workspace/foobar/foo.h"]
    pub mod foo_h {
        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct C2Rust_Unnamed {
            b: usize,
        }

        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct C2Rust_Unnamed_0 {
            c: usize,
        }

        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct foo_t {
            u: C2Rust_Unnamed,
        }
    }

    use self::foo_h::C2Rust_Unnamed_0;
    use self::foo_h::{foo_t, C2Rust_Unnamed};
    #[derive(Copy, Clone)]
    #[repr(C)]
    pub struct foo_bar {
        u: C2Rust_Unnamed,
        u2: C2Rust_Unnamed_0,
    }
}

pub mod test {
    pub mod C2Rust_Unnamed {}
}

struct C2Rust_Unnamed {
    d: u32,
}

fn main() {
    let u = C2Rust_Unnamed { d: 0 };

    println!("{}", u.d);
}
