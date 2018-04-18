#![feature(plugin, custom_attribute)]
#![plugin(cross_check_plugin(config_file = "fibo.c2r"))]
#![cross_check]

#[macro_use]
extern crate cross_check_derive;
#[macro_use]
extern crate cross_check_runtime;

extern crate xcheck_dlsym;

mod fibo;

fn main() {
    for i in 0..5 {
        //println!("fibo({})={}", i, fibo(i));
        fibo::fibo(i);
    }
}
