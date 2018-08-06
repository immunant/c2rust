#![feature(libc)]

extern crate libc;

// Wrapper for rb_xcheck that uses dlsym() to locate rb_xcheck dynamically
// at run-time, loading it from a library specified with the RB_XCHECK_LIB variable
#[no_mangle]
pub unsafe extern fn rb_xcheck(tag: u8, val: u64) {
    extern crate libc;
    static mut RB_XCHECK_FN: Option<unsafe extern fn(u8, u64)> = None;
    static RB_XCHECK_INIT: std::sync::Once = std::sync::ONCE_INIT;
    RB_XCHECK_INIT.call_once(|| {
        use std::os::unix::ffi::OsStrExt;
        let lib_path = std::env::var_os("RB_XCHECK_LIB")
            .expect("Variable RB_XCHECK_LIB not set");
        let lib = libc::dlopen(lib_path.as_bytes().as_ptr() as *const i8, libc::RTLD_NOW);
        if lib.is_null() {
            panic!("Could not load rb_xcheck library from: {:?}", lib_path);
        }

        let rb_xcheck_name = std::ffi::CString::new("rb_xcheck").unwrap();
        let rb_xcheck_sym = libc::dlsym(lib, rb_xcheck_name.as_ptr());
        if rb_xcheck_sym.is_null() {
            panic!("Could not find rb_xcheck() symbol in: {:?}", lib_path);
        }
        RB_XCHECK_FN = Some(std::mem::transmute(rb_xcheck_sym))
    });
    RB_XCHECK_FN.unwrap()(tag, val);
}
