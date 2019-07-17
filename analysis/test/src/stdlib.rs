extern "C" {
    #[no_mangle]
    pub fn malloc(_: libc::c_ulong) -> *mut libc::c_void;
}
