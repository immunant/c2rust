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
    fn asin(x: f64) -> f64;
    #[no_mangle]
    fn acos(x: f64) -> f64;
    #[no_mangle]
    fn atan(x: f64) -> f64;
    #[no_mangle]
    fn sinh(x: f64) -> f64;
    #[no_mangle]
    fn cosh(x: f64) -> f64;
    #[no_mangle]
    fn tanh(x: f64) -> f64;
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
    #[no_mangle]
    fn abs(x: i32) -> i32;
    #[no_mangle]
    fn labs(x: i64) -> i64;
    #[no_mangle]
    fn llabs(x: i64) -> i64;
    #[no_mangle]
    fn floor(x: f64) -> f64;
    #[no_mangle]
    fn floorf(x: f32) -> f32;
    #[no_mangle]
    fn ceil(x: f64) -> f64;
    #[no_mangle]
    fn ceilf(x: f32) -> f32;
    #[no_mangle]
    fn round(x: f64) -> f64;
    #[no_mangle]
    fn trunc(x: f64) -> f64;
    #[no_mangle]
    fn truncf(x: f32) -> f32;
    #[no_mangle]
    fn atan2(y: f64, x: f64) -> f64;
    #[no_mangle]
    fn hypot(x: f64, y: f64) -> f64;
    #[no_mangle]
    fn fmax(x: f64, y: f64) -> f64;
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
        let asn = asin(0.5f64);
        let acs = acos(0.5f64);
        let atn = atan(1.0f64);
        let snh = sinh(1.0f64);
        let csh = cosh(1.0f64);
        let tnh = tanh(1.0f64);
        let s = sqrt(4.0f64);
        let p = pow(2.0f64, 3.0f64);
        let l = log(2.0f64);
        let e = exp(1.0f64);
        let a = fabs(-1.0f64);

        // Integer absolute value functions
        let i = abs(-42i32);
        let j = labs(-123456789i64);
        let k = llabs(-987654321i64);

        // Rounding functions
        let fl = floor(3.7f64);
        let flf = floorf(3.7f32);
        let ce = ceil(3.2f64);
        let cef = ceilf(3.2f32);
        let rnd = round(3.5f64);
        let tr = trunc(3.9f64);
        let trf = truncf(3.9f32);

        // Binary functions
        let at2 = atan2(1.0f64, 1.0f64);
        let hyp = hypot(3.0f64, 4.0f64);
        let mx = fmax(1.0f64, 2.0f64);
    }
}
