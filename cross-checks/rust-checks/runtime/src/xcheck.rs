pub const UNKNOWN_TAG: u8 = 0;
pub const FUNCTION_ENTRY_TAG: u8 = 1;
pub const FUNCTION_EXIT_TAG: u8 = 2;
pub const FUNCTION_ARG_TAG: u8 = 3;
pub const FUNCTION_RETURN_TAG: u8 = 4;

#[cfg(any(feature = "xcheck-with-dlsym", feature = "xcheck-with-weak"))]
#[inline]
unsafe fn call_rb_xcheck_sym<T>(sym: *mut T, tag: u8, val: u64) {
    if !sym.is_null() {
        let rb_xcheck_fn: unsafe extern "C" fn(u8, u64) = ::std::mem::transmute(sym);
        rb_xcheck_fn(tag, val);
    } else {
        // FIXME: or do nothing???
        eprintln!("Called rb_xcheck({},{:x})", tag, val);
    }
}

// Wrapper for rb_xcheck that uses dlsym() to locate rb_xcheck dynamically
// at run-time, allowing us to override it with LD_PRELOAD
#[cfg(feature = "xcheck-with-dlsym")]
unsafe fn rb_xcheck(tag: u8, val: u64) {
    extern crate libc;
    static mut RB_XCHECK_SYM: *mut libc::c_void = ::std::ptr::null_mut();
    static RB_XCHECK_INIT: ::std::sync::Once = ::std::sync::ONCE_INIT;
    RB_XCHECK_INIT.call_once(|| {
        let rb_xcheck_name = ::std::ffi::CString::new("rb_xcheck").unwrap();
        RB_XCHECK_SYM = libc::dlsym(libc::RTLD_DEFAULT, rb_xcheck_name.as_ptr());
    });
    call_rb_xcheck_sym(RB_XCHECK_SYM, tag, val);
}

// Wrapper for rb_xcheck that uses (unsuccessfully) weak symbols to locate
// rb_xcheck in such a way that LD_PRELOAD can override it
#[cfg(feature = "xcheck-with-weak")]
#[deprecated(note = "this does not work correctly, please use xcheck-with-dlsym for now")]
unsafe fn rb_xcheck(tag: u8, val: u64) {
    extern "C" {
        #[link_name = "rb_xcheck"]
        #[linkage = "extern_weak"]
        static RB_XCHECK_SYM: *mut u8;
    }
    call_rb_xcheck_sym(RB_XCHECK_SYM, tag, val);
}

// The default wrapper for rb_xcheck, which uses a strong global symbol
// This is the only approach that requires that libclevrbuf.so is linked in
#[cfg(not(any(feature = "xcheck-with-dlsym", feature = "xcheck-with-weak")))]
extern "C" {
    #[no_mangle]
    fn rb_xcheck(tag: u8, val: u64);
}

#[inline]
pub fn xcheck<I: Iterator<Item = (u8, u64)>>(checks: I) {
    for (tag, val) in checks {
        unsafe { rb_xcheck(tag, val) }
    }
}
