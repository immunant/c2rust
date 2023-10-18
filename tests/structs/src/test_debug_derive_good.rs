use crate::debug_derive_good::rust_kS1;

// WIP: if we make this derive optional, how can we run the test with it enabled?

pub fn test_simple_struct() {
    unsafe {
        format!("{rust_kS1:?}");
    }
}
