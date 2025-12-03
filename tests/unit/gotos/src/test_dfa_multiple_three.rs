use crate::dfa_binary_multiple_three::rust_multiple_three;

use std::ffi::CString;

#[test]
pub fn test_multiple_three() {
    let n1 = CString::new(format!("{:b}", 4529465 * 3 + 0)).unwrap();
    let n2 = CString::new(format!("{:b}", 65424738 * 3 + 1)).unwrap();
    let n3 = CString::new(format!("{:b}", 98078783 * 3 + 2)).unwrap();
    let n4 = CString::new("010100150101010001").unwrap();

    unsafe {
        assert_eq!(rust_multiple_three(n1.as_ptr()), 1);
        assert_eq!(rust_multiple_three(n2.as_ptr()), 0);
        assert_eq!(rust_multiple_three(n3.as_ptr()), 0);
        assert_eq!(rust_multiple_three(n4.as_ptr()), 2);
    }
}
