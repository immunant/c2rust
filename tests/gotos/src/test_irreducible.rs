extern crate libc;

use self::libc::c_int;
use irreducible::rust_irreducible;

#[link(name = "test")]
extern "C" {
    fn irreducible(_: c_int) -> c_int;
}

#[cfg_attr(test, test)]
pub fn test_irreducible() {
    unsafe {
        for i in 0..20 {
            assert_eq!(rust_irreducible(i), irreducible(i));
        }
    }
}
