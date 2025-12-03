use crate::modules::rust_modules;
use std::ffi::c_uint;

#[link(name = "test")]
extern "C" {
    fn modules();
}

#[test]
pub fn test_modules() {}
