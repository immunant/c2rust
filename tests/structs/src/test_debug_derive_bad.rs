//! xfail

mod debug_derive_bad;
use debug_derive_bad::rust_kS2;

// TODO: if we make this derive optional, how can we run the test with it enabled?

pub fn test_union() {
    unsafe {
        format!("{rust_kS2:?}");
    }
}
