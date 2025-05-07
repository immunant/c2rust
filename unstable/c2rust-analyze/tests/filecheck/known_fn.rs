use std::ffi::{c_char, c_int};

extern "C" {
    fn access(
        path: *const c_char,
        amode: c_int,
    ) -> c_int;
}

// CHECK-LABEL: final labeling for "known_fn"
pub fn known_fn() {
    // CHECK-DAG: ([[@LINE+3]]: path): addr_of = UNIQUE | NON_NULL | STACK, type = READ | UNIQUE | OFFSET_ADD | NON_NULL | STACK#
    // CHECK-LABEL: type assignment for "known_fn":
    // CHECK-DAG: ([[@LINE+1]]: path): &[i8]
    let path = b".\0" as *const u8 as *const c_char;
    unsafe {
        access(path, 0);
    }
}
