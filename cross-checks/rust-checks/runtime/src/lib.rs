#![feature(never_type)]
#![cfg_attr(feature = "xcheck-with-dlsym", feature(const_fn))]
#![cfg_attr(feature = "xcheck-with-dlsym", feature(const_ptr_null_mut))]
#![cfg_attr(feature = "xcheck-with-dlsym", feature(libc))]
#![cfg_attr(feature = "xcheck-with-weak", feature(linkage))]
#![cfg_attr(feature = "libc-hash", feature(libc))]
#![cfg_attr(feature = "djb2-ssse3", feature(portable_simd))]
#![no_std]

#[cfg(feature = "libc-hash")]
extern crate libc;

pub mod hash;
pub mod macros;
pub mod xcheck;
