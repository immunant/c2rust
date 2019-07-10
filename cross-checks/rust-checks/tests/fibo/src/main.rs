#![feature(plugin, custom_attribute)]
#![plugin(c2rust_xcheck_plugin(config_file = "fibo_globs.c2r"))]
#![cross_check]

#[macro_use]
extern crate c2rust_xcheck_derive;
#[macro_use]
extern crate c2rust_xcheck_runtime;

extern crate c2rust_xcheck_backend_dynamic_dlsym;

mod fibo;

fn main() {
    for i in 0..5 {
        //println!("fibo({})={}", i, fibo(i));
        fibo::fibo(i);
    }
}
