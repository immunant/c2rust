use crate::comments::{CONSTANT, CONSTANT1, rust_test_fn};

pub fn test_comments() {
    let val = unsafe { rust_test_fn() };
    assert_eq!(6, val);
}
