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

#[cfg(not(source_header = "/some/path/another_foo.h"))]
pub mod another_foo_h {
    pub struct what {
        pub foo_h_a: i32,
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

#[cfg(not(source_header = "/some/path/another_test.h"))]
pub mod another_test_h {
    pub struct what_test {
        pub what_test_a: i32,
        pub what_test_b: i32,
    }
}
