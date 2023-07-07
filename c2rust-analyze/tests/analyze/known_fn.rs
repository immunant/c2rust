use std::ffi::{c_char, c_int};

extern "C" {
    fn access(
        path: *const c_char,
        amode: c_int,
    ) -> c_int;
}

pub fn main() {
    let path = b".\0" as *const u8 as *const c_char;
    unsafe {
        access(path, 0);
    }
}
