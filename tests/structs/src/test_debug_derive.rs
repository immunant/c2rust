use crate::debug_derive::{rust_kS1, rust_kS2};

// WIP: if we make this derive optional, how can we run the test with it enabled?

// xfail
pub fn test_union() {
    unsafe {
        format!("{rust_kS1:?}");
    }
}

pub fn test_no_union() {
    unsafe {
        format!("{rust_kS2:?}");
    }
}
