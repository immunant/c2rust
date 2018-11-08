#![feature(
    align_offset,
    libc,
)]
extern crate libc;

pub mod array;
pub mod block_ptr;
pub mod util;

pub use self::array::CArray;
pub use self::block_ptr::CBlockPtr;
