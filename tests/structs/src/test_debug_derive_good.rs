use crate::debug_derive_good::{rust_kS1, rust_kS3};

// TODO: if we make this derive optional, how can we run the test with it enabled?

pub fn test_simple_struct() {
    unsafe {
        format!("{rust_kS1:?}");
    }
}

pub fn test_struct_containing_va_list() {
    unsafe {
        format!("{rust_kS3:?}");
    }
}