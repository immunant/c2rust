#![feature(rustc_private)]
#![feature(c_variadic)]

#[path = "pointers.rs"]
pub mod pointers;
#[path = "stdlib.rs"]
pub mod stdlib;

fn main() {
    pointers::main()
}
