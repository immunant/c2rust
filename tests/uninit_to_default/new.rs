use std::mem::{self, uninitialized};
fn main() {
    unsafe {
        let x: i32 = (0i32);
        let y: char = ('\u{0}');
        let z: bool = (false);
        let x: i32 = 7;
        let x: i32;
        let x: i32 = f();
    }
}

fn f() -> i32 {
    17
}
