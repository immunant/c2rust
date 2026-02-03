#![allow(unused)]

extern "C" {
    #[no_mangle]
    fn sin(x: f64) -> f64;
    #[no_mangle]
    fn sinf(x: f32) -> f32;
    #[no_mangle]
    fn sinl(x: f64) -> f64;
    #[no_mangle]
    fn cos(x: f64) -> f64;
    #[no_mangle]
    fn tan(x: f64) -> f64;
    #[no_mangle]
    fn sqrt(x: f64) -> f64;
    #[no_mangle]
    fn pow(x: f64, y: f64) -> f64;
    #[no_mangle]
    fn log(x: f64) -> f64;
    #[no_mangle]
    fn exp(x: f64) -> f64;
    #[no_mangle]
    fn fabs(x: f64) -> f64;
}

fn main() {
    unsafe {
        let angle = 3.14159f64 / 2.0f64;
        let result = angle.sin();
        println!("sin({}) = {}", angle, result);

        // Multiple calls in one expression
        let z = 0.0f64.sin() + 1.0f64.sin();

        // Nested call
        let w = 0.5f64.sin().sin();

        // Test sinf (f32 variant)
        let angle_f32: f32 = 1.5;
        let result_f32 = angle_f32.sin();
        println!("sinf({}) = {}", angle_f32, result_f32);

        // Test sinl (long double variant)
        let angle_ld = 2.0f64;
        let result_ld = angle_ld.sin();
        println!("sinl({}) = {}", angle_ld, result_ld);

        // Mixed calls
        let mixed = 1.0f64.sin() + 1.0f32.sin() as f64 + 1.0f64.sin();

        // Additional math functions
        let c = 0.25f64.cos();
        let t = 0.25f64.tan();
        let s = 4.0f64.sqrt();
        let p = 2.0f64.powf(3.0f64);
        let l = 2.0f64.ln();
        let e = 1.0f64.exp();
        let a = (-1.0f64).abs();
    }
}
