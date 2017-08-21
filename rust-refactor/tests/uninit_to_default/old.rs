use std::mem::{self, uninitialized};
fn main() {
    unsafe {
        let x: i32 = ::std::mem::uninitialized();
        let y: char = mem::uninitialized();
        let z: bool = uninitialized();
        let x: i32 = 7;
        let x: i32;
        let x: i32 = f();
    }
}

fn f() -> i32 {
    17
}
