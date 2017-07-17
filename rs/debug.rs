#![feature(untagged_unions)]

#[no_mangle]
pub static mut _syslog : i32 = 0i32;

#[no_mangle]
pub static mut _debug : i32 = 0i32;

#[no_mangle]
pub unsafe extern fn mc_set_debug(mut debug : i32) {
    _debug = debug;
}

#[no_mangle]
pub unsafe extern fn mc_get_debug() -> i32 { _debug }

#[no_mangle]
pub unsafe extern fn mc_set_syslog(mut syslog : i32) {
    _syslog = syslog;
}
