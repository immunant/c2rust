#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(mutable_transmutes)]
#![allow(unused_mut)]

mod foo;
mod test;

#[cfg(not(source_header = "/some/path/foo.h"))]
pub mod foo_h {
    pub struct foo_struct {
        pub a: i32,
        pub b: i32,
    }

    pub struct foo_point {
        pub x: i32,
        pub y: i32,
    }
}

#[cfg(not(source_header = "/some/path/foo.h"))]
pub mod test_h {
    pub struct test_struct {
        pub a: i32,
        pub b: i32,
    }

    pub struct test_point {
        pub x: i32,
        pub y: i32,
    }
}


#[cfg(not(source_header = "/usr/include/x86_64-linux-gnu/bits/types.h"))]
pub mod types_h {
    pub type __ssize_t = libc::c_long;
    use super::libc;
}

use foo_h::foo_point;
use test_h::test_point;

fn main() {
    let f_point: foo_point = foo_point {x: 1, y: 2};
    let t_point: test_point = test_point {x: 1, y: 2};
}
