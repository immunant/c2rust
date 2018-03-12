extern crate libc;

use switch::{switch_val as rust_switch_val};
use self::libc::c_int;

extern "C" {
    #[no_mangle]
    fn switch_val(_: c_int) -> c_int;
}

pub fn test_switch() {
    let val = unsafe {
        switch_val(1)
    };
    let rust_val = unsafe {
        rust_switch_val(1)
    };

    assert_eq!(val, rust_val);
    assert_eq!(val, 2);

    let val = unsafe {
        switch_val(2)
    };
    let rust_val = unsafe {
        rust_switch_val(2)
    };

    assert_eq!(val, rust_val);
    assert_eq!(val, 4);

    let val = unsafe {
        switch_val(10)
    };
    let rust_val = unsafe {
        rust_switch_val(10)
    };

    assert_eq!(val, rust_val);
    assert_eq!(val, 11);
}
