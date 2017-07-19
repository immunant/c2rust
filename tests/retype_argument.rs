// retype_argument \
//      -c tests/retype_argument.rs:8:14::arg \
//      -c tests/retype_argument.rs:15:18::arg \
//      -c tests/retype_argument.rs:19:25::arg \
//      -c tests/retype_argument.rs:23:26::arg \
//      -c tests/retype_argument.rs:27:30::arg \
//      u8 '__old as u8' '__new as u32'
fn f(x: i32, y: i32) -> i32 {
    x + y
}

struct S;

impl S {
    fn g(x: i32, y: i32) -> i32 {
        x + y
    }

    fn h1(self, x: i32, y: i32) -> i32 {
        x + y
    }

    fn h2(&self, x: i32, y: i32) -> i32 {
        x + y
    }

    fn h3(&mut self, x: i32, y: i32) -> i32 {
        x + y
    }
}

struct Dummy {
    array: [u8; 10],
}

fn main() {
    println!("1 + 1 = {}", f(1, 1));
    println!("1 + 1 = {}", S::g(1, 1));
    println!("1 + 1 = {}", S.h1(1, 1));
    println!("1 + 1 = {}", S.h2(1, 1));
    println!("1 + 1 = {}", S.h3(1, 1));
}
