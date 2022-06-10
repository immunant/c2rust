use crate::nofnargs::rust_nofnargs;
use crate::noop::rust_noop;

use libc::c_int;

#[link(name = "test")]
extern "C" {
    fn noop();

    fn nofnargs() -> c_int;
}

pub fn test_noop() {
    unsafe {
        noop();
        rust_noop();
    }
}

pub fn test_nofnargs() {
    let ret = unsafe { nofnargs() };
    let rust_ret = unsafe { rust_nofnargs() };

    assert_eq!(ret, 0);
    assert_eq!(rust_ret, 0);
}
