#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(mutable_transmutes)]
#![allow(unused_mut)]

#![feature(libc)]
extern crate libc;

mod foo;
mod test;


fn main() {
    test::main()
}
