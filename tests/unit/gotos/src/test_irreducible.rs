use crate::irreducible::rust_irreducible;
use std::ffi::c_int;

#[link(name = "test")]
extern "C" {
    fn irreducible(_: c_int) -> c_int;
}

#[test]
pub fn test_irreducible() {
    unsafe {
        for i in 0..20 {
            assert_eq!(rust_irreducible(i), irreducible(i));
        }
    }
}
