extern crate libc;

use linking::{l as rust_l, w as rust_w};
use self::libc::c_int;

extern "C" {
    #[no_mangle]
    fn l() -> c_int;

    #[no_mangle]
    fn w() -> c_int;
}

pub fn test_linking() {
    let mut ret = unsafe {
        l()
    };

    let mut rust_ret = unsafe {
        rust_l()
    };

    assert_eq!(ret, rust_ret);
    assert_eq!(ret, 3);
    assert_eq!(rust_ret, 3);

    ret = unsafe {
        w()
    };

    rust_ret = unsafe {
        rust_w()
    };

    assert_eq!(ret, rust_ret);
    assert_eq!(ret, 4);
    assert_eq!(rust_ret, 4);
}