extern crate libc;

use modules::rust_modules;
use self::libc::c_uint;

#[link(name = "test")]
extern "C" {
    fn modules();
}

pub fn test_modules() {
}
