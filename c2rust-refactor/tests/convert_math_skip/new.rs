#![allow(unused)]

fn sin(x: f64) -> f64 {
    x + 1.0f64
}

fn main() {
    let angle = 3.14159f64 / 2.0f64;
    let result = sin(angle);
    println!("sin({}) = {}", angle, result);

    let value = sin(1.0f64);
    println!("sin(1.0) = {}", value);
}
