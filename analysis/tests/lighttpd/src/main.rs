#![feature(extern_types)]
#![feature(label_break_value)]
#![feature(rustc_private)]
#![feature(c_variadic)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(mutable_transmutes)]
#![allow(unused_mut)]
#![allow(unused_imports)]
#![allow(unused_variables)]

#[path = "lighttpd.rs"]
pub mod lighttpd;

use lighttpd::lighttpd_test;

fn main() {
    unsafe {
        lighttpd_test();
    }
}
