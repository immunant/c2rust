//! feature_custom_attribute

extern crate libc;

use modules::rust_modules;
use self::libc::c_uint;

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn modules();
}

pub fn test_modules() {
}
