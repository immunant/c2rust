extern crate libc;

use call_extern_varargs::rust_call_printf;

#[link(name = "test")]
extern "C" {
    #[no_mangle]
    fn call_printf();
}

// This test ensures we are able to define and call vararg prototypes
// that get linked in (IE printf)
pub fn test_call_printf() {
    unsafe {
        call_printf();
        rust_call_printf();
    }
}
