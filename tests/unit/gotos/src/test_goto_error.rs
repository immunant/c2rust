#[test]
pub fn test_goto_error_a() {
    use crate::goto_error::rust_goto_error_a;
    unsafe {
        assert_eq!(rust_goto_error_a(10), 15);
        assert_eq!(rust_goto_error_a(20), 18);
        assert_eq!(rust_goto_error_a(1), 2);
        assert_eq!(rust_goto_error_a(0), -4);
    }
}

#[test]
pub fn test_goto_error_b() {
    use crate::goto_error::rust_goto_error_b;
    unsafe {
        assert_eq!(rust_goto_error_b(10), 15);
        assert_eq!(rust_goto_error_b(18), 18);
        assert_eq!(rust_goto_error_b(1), 2);
        assert_eq!(rust_goto_error_b(2), -4);
    }
}

#[test]
pub fn goto_error_but_its_all_gotos() {
    use crate::goto_error::rust_goto_error_but_its_all_gotos;
    unsafe {
        assert_eq!(rust_goto_error_but_its_all_gotos(10), 15);
        assert_eq!(rust_goto_error_but_its_all_gotos(18), 18);
        assert_eq!(rust_goto_error_but_its_all_gotos(1), 2);
        assert_eq!(rust_goto_error_but_its_all_gotos(2), -4);
    }
}

#[test]
pub fn test_goto_errors() {
    use crate::goto_error::rust_goto_errors;
    unsafe {
        assert_eq!(rust_goto_errors(10), 15);
        assert_eq!(rust_goto_errors(19), 21);
        assert_eq!(rust_goto_errors(2), 5);
        assert_eq!(rust_goto_errors(1), -9);
    }
}

#[test]
pub fn test_goto_success() {
    use crate::goto_error::rust_goto_success;
    unsafe {
        assert_eq!(rust_goto_success(10), 15);
        assert_eq!(rust_goto_success(19), 18);
        assert_eq!(rust_goto_success(2), 5);
        assert_eq!(rust_goto_success(1), -6);
    }
}
