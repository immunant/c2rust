use crate::early_returns::rust_early_returns;

#[test]
pub fn test_early_returns() {
    unsafe {
        assert_eq!(rust_early_returns(2), 2);
        assert_eq!(rust_early_returns(3), 1);
        assert_eq!(rust_early_returns(4), 1);
        assert_eq!(rust_early_returns(5), 0);
    }
}
