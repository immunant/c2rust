#[cfg(none)]
#[test]
pub fn test_simple_test() {
    use crate::goto_error::rust_simple_test;
    unsafe {
        assert_eq!(rust_simple_test(10), 1);
        assert_eq!(rust_simple_test(5), 2);
        assert_eq!(rust_simple_test(0), 3);
    }
}

#[test]
pub fn test_goto_error() {
    use crate::goto_error::rust_goto_error;
    unsafe {
        assert_eq!(rust_goto_error(10), 15);
        assert_eq!(rust_goto_error(20), 18);
        assert_eq!(rust_goto_error(1), 2);
        assert_eq!(rust_goto_error(0), -4);
    }
}

#[test]
pub fn dummy_test() {}
