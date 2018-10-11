#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(mutable_transmutes)]
#![allow(unused_mut)]

#![feature(libc)]
extern crate libc;

mod foo;
mod test;

#[cfg(not(source_header = "/some/path/foo.h"))]
pub mod foo_h {
    pub struct foo_struct {
        pub foo_struct_a: i32,
        pub foo_struct_b: i32,
    }

    pub struct foo_point {
        pub point_x: i32,
        pub point_y: i32,
    }
}

#[cfg(not(source_header = "/some/path/test.h"))]
pub mod test_h {
    pub struct test_struct {
        pub test_struct_a: i32,
        pub test_struct_b: i32,
    }

    pub struct test_point {
        pub point_x: i32,
        pub point_y: i32,
    }
}


#[cfg(not(source_header = "/usr/include/x86_64-linux-gnu/bits/types.h"))]
pub mod types_h {
    pub type __ssize_t = libc::c_long;
    use super::libc;
}

use self::foo_h::foo_point;
use self::test_h::test_point;

fn main() {
    let f_point: foo_point = foo_point {point_x: 1, point_y: 2};
    println!("{}", f_point.point_x);

    let t_point: test_point = test_point {point_x: 1, point_y: 2};
    println!("{}", t_point.point_x);
}
