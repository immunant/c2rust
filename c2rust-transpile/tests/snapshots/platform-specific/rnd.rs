#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]
extern "C" {
    fn abs(_: std::ffi::c_int) -> std::ffi::c_int;
}
pub type __int32_t = std::ffi::c_int;
pub type __uint32_t = std::ffi::c_uint;
pub type int32_t = __int32_t;
pub type uint32_t = __uint32_t;
#[no_mangle]
pub static mut cur_rand_seed: uint32_t = 0 as std::ffi::c_int as uint32_t;
#[no_mangle]
pub unsafe extern "C" fn set_rand_seed(mut s: uint32_t) {
    cur_rand_seed = s;
}
#[no_mangle]
pub unsafe extern "C" fn get_rand_seed() -> uint32_t {
    let INCREMENT: uint32_t = 1 as std::ffi::c_int as uint32_t;
    let MULTIPLIER: uint32_t = 0x15a4e35 as std::ffi::c_int as uint32_t;
    cur_rand_seed = MULTIPLIER.wrapping_mul(cur_rand_seed).wrapping_add(INCREMENT);
    let mut ret: uint32_t = abs(cur_rand_seed as int32_t) as uint32_t;
    return ret;
}
