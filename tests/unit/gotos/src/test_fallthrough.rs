#[test]
pub fn test_switch_fallthrough_2_cases() {
    use crate::fallthrough::rust_fallthrough_2;
    unsafe {
        assert_eq!(rust_fallthrough_2(0), 2);
        assert_eq!(rust_fallthrough_2(1), 2);
    }
}

#[test]
pub fn test_switch_fallthrough_3_cases() {
    use crate::fallthrough::rust_fallthrough_3;
    unsafe {
        assert_eq!(rust_fallthrough_3(0), 3);
        assert_eq!(rust_fallthrough_3(1), 3);
        assert_eq!(rust_fallthrough_3(2), 3);
    }
}

#[test]
pub fn test_switch_fallthrough_no_default() {
    use crate::fallthrough::rust_fallthrough_without_default;
    unsafe {
        assert_eq!(rust_fallthrough_without_default(0), 3);
        assert_eq!(rust_fallthrough_without_default(1), 3);
        assert_eq!(rust_fallthrough_without_default(2), 3);
        assert_eq!(rust_fallthrough_without_default(4), 4);
    }
}

#[test]
pub fn test_fallthrough_with_early_return() {
    use crate::fallthrough::rust_fallthrough_with_early_return;
    unsafe {
        assert_eq!(rust_fallthrough_with_early_return(0), 6);
        assert_eq!(rust_fallthrough_with_early_return(1), 6);
        assert_eq!(rust_fallthrough_with_early_return(2), 5);
        assert_eq!(rust_fallthrough_with_early_return(3), 25);
        assert_eq!(rust_fallthrough_with_early_return(4), 22);
        assert_eq!(rust_fallthrough_with_early_return(5), 18);
    }
}
