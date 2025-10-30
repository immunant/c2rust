#![allow(
    dead_code,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    unused_assignments,
    unused_mut
)]

#[derive(Copy, Clone)]
#[repr(C)]
pub struct S0 {}

#[derive(Clone)]
#[repr(u8)]
#[derive(Copy)]
#[derive(Debug)]
pub struct S1 {}

#[cfg(not(test))]
#[derive(Debug, Copy, Clone)]
#[repr(u32)]
pub struct S2 {}

#[derive(Debug)]
pub struct S3 {}
