extern crate libc;

use anonymous_decls::k;
use self::libc::{c_int, c_uint};

pub fn test_anonymous_decl() {
    unsafe {
        assert_eq!(k.j.l, 0);
    }
}
