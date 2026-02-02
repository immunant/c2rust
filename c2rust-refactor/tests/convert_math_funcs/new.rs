#![allow(unused)]

extern "C" {
    #[no_mangle]
    fn sin(x: f64) -> f64;
}

fn main() {
    unsafe {
        let angle = 3.14159 / 2.0;
        let result = angle.sin();
        println!("sin({}) = {}", angle, result);

        let x = 1.5;
        let y = x.sin();
        println!("sin({}) = {}", x, y);

        // Multiple calls in one expression
        let z = 0.0.sin() + 1.0.sin();

        // Nested call
        let w = 0.5.sin().sin();
    }
}
