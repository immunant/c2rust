#![allow(unused)]

extern "C" {
    #[no_mangle]
    fn sin(x: f64) -> f64;
    #[no_mangle]
    fn sinf(x: f32) -> f32;
    #[no_mangle]
    fn sinl(x: f64) -> f64;
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

        // Test sinf (f32 variant)
        let angle_f32: f32 = 1.5;
        let result_f32 = angle_f32.sin();
        println!("sinf({}) = {}", angle_f32, result_f32);

        // Test sinl (long double variant)
        let angle_ld = 2.0;
        let result_ld = angle_ld.sin();
        println!("sinl({}) = {}", angle_ld, result_ld);

        // Mixed calls
        let mixed = 1.0.sin() + 1.0.sin() as f64 + 1.0.sin();
    }
}
