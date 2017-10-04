
pub const FUNCTION_CALL_TAG: u8 = 0;
pub const FUNCTION_ARG_TAG: u8 = 1;
pub const FUNCTION_RETURN_TAG: u8 = 2;

#[cfg(any(feature="xcheck-with-dlsym", feature="xcheck-with-weak"))]
#[inline]
unsafe fn call_rb_xcheck_sym<T>(sym: *mut T, tag: u8, val: u64) {
    if !sym.is_null() {
        let rb_xcheck_fn: unsafe extern fn(u8, u64) = ::std::mem::transmute(sym);
        rb_xcheck_fn(tag, val);
    } else {
        // FIXME: or do nothing???
        println!("Called rb_xcheck({},{:x})", tag, val);
    }
}

#[cfg(feature="xcheck-with-dlsym")]
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

#[cfg(feature="xcheck-with-weak")]
unsafe fn rb_xcheck(tag: u8, val: u64) {
    extern {
        #[link_name = "rb_xcheck"]
        #[linkage = "extern_weak"]
        static RB_XCHECK_SYM: *mut u8;
    }
    call_rb_xcheck_sym(RB_XCHECK_SYM, tag, val);
}

#[cfg(not(any(feature="xcheck-with-dlsym", feature="xcheck-with-weak")))]
extern {
    #[no_mangle]
    fn rb_xcheck(tag: u8, val: u64);
}

#[inline]
pub fn xcheck(tag: u8, val: u64) {
    unsafe { rb_xcheck(tag, val) }
}
