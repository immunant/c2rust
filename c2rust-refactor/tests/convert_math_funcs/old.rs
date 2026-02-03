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

        // Test sinf (f32 variant)
        let angle_f32: f32 = 1.5;
        let result_f32 = sinf(angle_f32);
        println!("sinf({}) = {}", angle_f32, result_f32);

        // Test sinl (long double variant)
        let angle_ld = 2.0;
        let result_ld = sinl(angle_ld);
        println!("sinl({}) = {}", angle_ld, result_ld);

        // Mixed calls
        let mixed = sin(1.0) + sinf(1.0) as f64 + sinl(1.0);

        // Additional math functions
        let c = cos(0.25);
        let t = tan(0.25);
        let s = sqrt(4.0);
        let p = pow(2.0, 3.0);
        let l = log(2.0);
        let e = exp(1.0);
        let a = fabs(-1.0);
    }
}
