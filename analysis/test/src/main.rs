#![feature(extern_types)]
#![feature(label_break_value)]
#![feature(rustc_private)]

#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(mutable_transmutes)]
#![allow(unused_mut)]

#[path = "pointers.rs"]
pub mod pointers;
#[path = "stdlib.rs"]
pub mod stdlib;

fn main() { pointers::main() }

