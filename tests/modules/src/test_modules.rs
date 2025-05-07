use crate::modules::rust_modules;
use std::ffi::c_uint;

#[link(name = "test")]
extern "C" {
    fn modules();
}

pub fn test_modules() {}
