use add::add as rust_add;

extern "C" {
    #[no_mangle]
    fn add(left: u32, right: u32) -> u32;
}

pub fn test_addition() {
    let sum = unsafe { add(1, 2) };
    let rust_sum = unsafe { rust_add(1, 2) };

    assert_eq!(sum, 3);
    assert_eq!(rust_sum, 3);
}
