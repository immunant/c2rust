#![allow(
    dead_code,
    mutable_transmutes,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]
#![feature(stdsimd)]
#[cfg(target_arch = "arm")]
pub use core::arch::arm::__yield;
#[cfg(target_arch = "aarch64")]
pub use core::arch::aarch64::__yield;
#[no_mangle]
pub unsafe extern "C" fn spin() {
    __yield();
}
