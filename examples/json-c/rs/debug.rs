
#[no_mangle]
pub static mut _syslog: i32 = 0i32;

#[no_mangle]
pub static mut _debug: i32 = 0i32;

pub unsafe fn mc_set_debug(mut debug: i32) {
    _debug = debug;
}
#[export_name = "mc_set_debug"]
pub unsafe extern "C" fn mc_set_debug_wrapper(debug: i32) {
    mc_set_debug(debug)
}

pub unsafe fn mc_get_debug() -> i32 {
    _debug
}
#[export_name = "mc_get_debug"]
pub unsafe extern "C" fn mc_get_debug_wrapper() -> i32 {
    mc_get_debug()
}

pub unsafe fn mc_set_syslog(mut syslog: i32) {
    _syslog = syslog;
}
#[export_name = "mc_set_syslog"]
pub unsafe extern "C" fn mc_set_syslog_wrapper(syslog: i32) {
    mc_set_syslog(syslog)
}
