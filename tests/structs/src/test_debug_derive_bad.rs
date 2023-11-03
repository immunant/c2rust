//! xfail

mod debug_derive_bad;
use debug_derive_bad::rust_kS2;

pub fn test_union() {
    unsafe {
        format!("{rust_kS2:?}");
    }
}
