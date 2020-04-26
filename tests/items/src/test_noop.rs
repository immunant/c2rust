extern crate libc;

use noop::rust_noop;
use nofnargs::rust_nofnargs;

use self::libc::c_int;

#[link(name = "test")]
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
