#![feature(never_type)]
#![feature(asm)]

#![cfg_attr(feature="xcheck-with-dlsym", feature(const_fn))]
#![cfg_attr(feature="xcheck-with-dlsym", feature(const_ptr_null_mut))]
#![cfg_attr(feature="xcheck-with-dlsym", feature(libc))]
#![cfg_attr(feature="xcheck-with-weak",  feature(linkage))]
#![cfg_attr(feature="libc-hash",         feature(libc))]

#[cfg(feature="djb2-ssse3")]
extern crate simd;

#[cfg(feature="libc-hash")]
extern crate libc;

pub mod xcheck;
pub mod hash;
pub mod macros;

