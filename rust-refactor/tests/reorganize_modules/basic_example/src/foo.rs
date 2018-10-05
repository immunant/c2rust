pub struct some_foo_struct {
    pub i: i32
}

pub struct foo_struct {
    pub a: i32,
    pub b: i32,
}

#[cfg(not(source_header = "/some/path/foo.h"))]
pub mod foo_h {
    pub struct point {
        pub x: i32,
        pub y: i32,
    }
}
