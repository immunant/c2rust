extern crate libc;

use crate::anonymous_decls::rust_k;
use self::libc::{c_int, c_uint};

#[cfg_attr(test, test)]
pub fn test_anonymous_decl() {
    unsafe {
        assert_eq!(rust_k.j.l, 0);
    }
}
