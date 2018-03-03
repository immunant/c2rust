use noop::noop as rust_noop;

extern "C" {
    #[no_mangle]
    fn noop();
}

pub fn test_noop() {
    unsafe {
        noop();
        rust_noop();
    }
}
