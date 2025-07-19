//! extern_crate_memoffset

use crate::variable_offsetof::{rust_get_offset, rust_get_offset2, size_t};

#[link(name = "test")]
extern "C" {
    fn get_offset(_: size_t) -> size_t;
    fn get_offset2(_: size_t) -> size_t;
}

#[test]
pub fn test_get_offset() {
    for idx in 0..3 {
        let rust_ret = unsafe { rust_get_offset(idx) };
        let c_ret = unsafe { get_offset(idx) };

        assert_eq!(rust_ret, c_ret);

        let rust_ret2 = unsafe { rust_get_offset2(idx) };
        let c_ret2 = unsafe { get_offset2(idx) };

        assert_eq!(rust_ret2, c_ret2);
    }
}
