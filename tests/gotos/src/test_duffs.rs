use crate::duffs::rust_copy;

#[test]
pub fn test_multiple_three() {
    let mut from = [1, 2, 3, 8, 2, 9, 8, 1, 8, 4, 5, 6, 2, 89, 0, 2, 3, 4, 56, 8];
    let mut to = [0; 20];

    unsafe {
        rust_copy(to.as_mut_ptr(), from.as_mut_ptr(), 20);
    }

    assert_eq!(from, to);
}
