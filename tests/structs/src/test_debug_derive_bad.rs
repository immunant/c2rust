//! xfail

mod debug_derive_bad;
use debug_derive_bad::rust_kStructWithUnion;

pub fn test_union() {
    unsafe {
        format!("{rust_kStructWithUnion:?}");
    }
}
