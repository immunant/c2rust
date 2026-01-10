use crate::call_only_once::rust_assert_call_only_once;
use std::ffi::c_int;

#[link(name = "test")]
extern "C" {
    fn assert_call_only_once() -> c_int;
}

#[test]
pub fn test_called_only_once() {
    let c_called_value;
    let rust_called_value;

    unsafe {
        c_called_value = assert_call_only_once();
        rust_called_value = rust_assert_call_only_once();
    }

    assert_eq!(c_called_value, rust_called_value);
}
