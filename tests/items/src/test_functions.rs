use functions::rust_coreutils_static_assert;

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn coreutils_static_assert();
}

pub fn test_coreutils_static_assert() {
    unsafe {
        coreutils_static_assert();
        rust_coreutils_static_assert();
    }
}
