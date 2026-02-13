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

        #[repr(C)]
        #[derive(Copy, Clone)]
        pub struct C2Rust_Unnamed_0 {
            a: usize,
        }

        #[repr(C)]
        #[derive(Copy, Clone)]
        pub struct C2Rust_Unnamed_1 {
            x: i32,
            y: i32,
        }

        #[repr(C)]
        #[derive(Copy, Clone)]
        pub struct bar_t {
            u: C2Rust_Unnamed_0,
        }
    }
    use self::bar_h::*;
}

pub mod foo {
    #[header_src = "/home/user/some/workspace/foobar/foo.h"]
    pub mod foo_h {

        #[repr(C)]
        #[derive(Copy, Clone)]
        pub struct C2Rust_Unnamed_2 {
            b: usize,
        }

        #[repr(C)]
        #[derive(Copy, Clone)]
        pub struct C2Rust_Unnamed_3 {
            c: usize,
        }

        #[repr(C)]
        #[derive(Copy, Clone)]
        pub struct foo_t {
            u: C2Rust_Unnamed_2,
        }
    }

    use self::foo_h::foo_t;
    use self::foo_h::C2Rust_Unnamed_2;
    use self::foo_h::C2Rust_Unnamed_3;

    #[repr(C)]
    #[derive(Copy, Clone)]
    pub struct foo_bar {
        u: C2Rust_Unnamed_2,
        u2: C2Rust_Unnamed_3,
    }
}

pub mod test {
    pub mod C2Rust_Unnamed {}
}

struct C2Rust_Unnamed_4 {
    d: u32,
}

fn main() {
    let u = C2Rust_Unnamed_4 { d: 0 };

    println!("{}", u.d);
}
