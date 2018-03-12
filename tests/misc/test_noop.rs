extern crate libc;

use noop::noop as rust_noop;
use nofnargs::nofnargs as rust_nofnargs;

use self::libc::c_int;

extern "C" {
    #[no_mangle]
    fn noop();

    #[no_mangle]
    fn nofnargs() -> c_int;
}

pub fn test_noop() {
    unsafe {
        noop();
        rust_noop();
    }
}

pub fn test_nofnargs() {
    let ret = unsafe {
        nofnargs()
    };
    let rust_ret = unsafe {
        rust_nofnargs()
    };

    assert_eq!(ret, 0);
    assert_eq!(rust_ret, 0);
}
