#[test]
pub fn test_goto_error() {
    use crate::goto_error::rust_goto_error_a;
    use crate::goto_error::rust_goto_error_b;
    unsafe {
        assert_eq!(rust_goto_error_a(10), 15);
        assert_eq!(rust_goto_error_a(20), 18);
        assert_eq!(rust_goto_error_a(1), 2);
        assert_eq!(rust_goto_error_a(0), -4);

        assert_eq!(rust_goto_error_b(10), 15);
        assert_eq!(rust_goto_error_b(18), 18);
        assert_eq!(rust_goto_error_b(1), 2);
        assert_eq!(rust_goto_error_b(2), -4);
    }
}

#[test]
pub fn dummy_test() {}
