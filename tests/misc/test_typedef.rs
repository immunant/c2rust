extern crate libc;

use typedef::rust_entry;

use self::libc::c_int;

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn entry() -> c_int;
}

pub fn test_typedef() {
    let ret = unsafe {
        entry()
    };
    let rust_ret = unsafe {
        rust_entry()
    };

    assert_eq!(ret, 0);
    assert_eq!(rust_ret, 0);
}
