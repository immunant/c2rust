use crate::comments::{rust_test_fn, CONSTANT, CONSTANT1};

pub fn test_comments() {
    let val = unsafe { rust_test_fn() };
    assert_eq!(6, val);
}
