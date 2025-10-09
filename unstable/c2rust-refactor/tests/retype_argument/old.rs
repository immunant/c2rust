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

fn main() {
    println!("1 + 1 = {}", f(1, 1));
    println!("1 + 1 = {}", S::g(1, 1));
    println!("1 + 1 = {}", S.h1(1, 1));
    println!("1 + 1 = {}", S.h2(1, 1));
    println!("1 + 1 = {}", S.h3(1, 1));
}
