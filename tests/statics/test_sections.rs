extern crate libc;

use sections::*;
use self::libc::{c_int, c_uint};

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    static section_me: c_uint;
    #[no_mangle]
    static section_me2: c_int;
    #[no_mangle]
    static section_me3: c_uint;
    #[no_mangle]
    static section_me4: c_uint;
    #[no_mangle]
    static section_me5: c_uint;
    #[no_mangle]
    static section_foo_b_field: Foo;
}

pub fn test_sectioned_statics() {
    unsafe {
        assert_eq!(rust_section_me, u32::max_value());
        assert_eq!(rust_section_me2, 0i32);
        assert_eq!(rust_section_me3, 3u32);
        assert_eq!(rust_section_me4, 2u32);
        assert_eq!(rust_section_me5, 2u32);
        assert_eq!(section_foo_b_field.a, 1);
        assert_eq!(section_foo_b_field.b, -1);
        assert_eq!(section_foo_b_field.c, 1.2);
        assert_eq!(rust_section_num_params, 2);
    }
}
