
#![cfg_attr(feature="xcheck-with-dlsym", feature(const_fn))]
#![cfg_attr(feature="xcheck-with-dlsym", feature(const_ptr_null_mut))]
#![cfg_attr(feature="xcheck-with-dlsym", feature(libc))]
#![cfg_attr(feature="xcheck-with-weak", feature(linkage))]

#[cfg(feature="djb2-ssse3")]
extern crate simd;

pub mod xcheck;
pub mod djb2;
pub mod hash;

