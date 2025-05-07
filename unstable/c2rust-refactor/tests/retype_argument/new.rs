fn f(x: i32, y: u8) -> i32 {
    x + y as i32
}

struct S;

impl S {
    fn g(x: i32, y: u8) -> i32 {
        x + y as i32
    }

    fn h1(self, x: i32, y: u8) -> i32 {
        x + y as i32
    }

    fn h2(&self, x: i32, y: u8) -> i32 {
        x + y as i32
    }

    fn h3(&mut self, x: i32, y: u8) -> i32 {
        x + y as i32
    }
}

fn main() {
    println!("1 + 1 = {}", f(1, 1 as u8));
    println!("1 + 1 = {}", S::g(1, 1 as u8));
    println!("1 + 1 = {}", S.h1(1, 1 as u8));
    println!("1 + 1 = {}", S.h2(1, 1 as u8));
    println!("1 + 1 = {}", S.h3(1, 1 as u8));
}
