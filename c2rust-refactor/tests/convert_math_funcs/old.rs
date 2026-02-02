#![allow(unused)]

extern "C" {
    #[no_mangle]
    fn sin(x: f64) -> f64;
}

fn main() {
    unsafe {
        let angle = 3.14159 / 2.0;
        let result = sin(angle);
        println!("sin({}) = {}", angle, result);
        
        let x = 1.5;
        let y = sin(x);
        println!("sin({}) = {}", x, y);
        
        // Multiple calls in one expression
        let z = sin(0.0) + sin(1.0);
        
        // Nested call
        let w = sin(sin(0.5));
    }
}
