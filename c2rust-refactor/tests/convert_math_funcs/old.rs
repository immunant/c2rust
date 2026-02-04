#![allow(unused)]

extern "C" {
    #[no_mangle]
    fn sin(x: f64) -> f64;
    #[no_mangle]
    fn sinf(x: f32) -> f32;
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
        let result = sin(angle);
        println!("sin({}) = {}", angle, result);

        // Multiple calls in one expression
        let z = sin(0.0f64) + sin(1.0f64);

        // Nested call
        let w = sin(sin(0.5f64));

        // Test sinf (f32 variant)
        let angle_f32: f32 = 1.5;
        let result_f32 = sinf(angle_f32);
        println!("sinf({}) = {}", angle_f32, result_f32);

        // Mixed calls
        let mixed = sin(1.0f64) + sinf(1.0f32) as f64;

        // Additional math functions
        let c = cos(0.25f64);
        let t = tan(0.25f64);
        let s = sqrt(4.0f64);
        let p = pow(2.0f64, 3.0f64);
        let l = log(2.0f64);
        let e = exp(1.0f64);
        let a = fabs(-1.0f64);
    }
}
