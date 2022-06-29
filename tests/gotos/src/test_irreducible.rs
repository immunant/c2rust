use crate::irreducible::rust_irreducible;
use libc::c_int;

#[link(name = "test")]
extern "C" {
    fn irreducible(_: c_int) -> c_int;
}

pub fn test_irreducible() {
    unsafe {
        for i in 0..20 {
            assert_eq!(rust_irreducible(i), irreducible(i));
        }
    }
}
