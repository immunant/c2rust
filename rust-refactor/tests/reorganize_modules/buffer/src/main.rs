#![feature(libc)]
#![feature(extern_types)]
#![feature(asm)]
#![feature(ptr_wrapping_offset_from)]

#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(mutable_transmutes)]
#![allow(unused_mut)]


extern crate libc;


pub mod buffer;

pub mod test;

fn main() { test::main() }
