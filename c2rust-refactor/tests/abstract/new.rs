fn main() {
    let x = ::add(1, 2);
    let x = ::add(::add(1, 2), 3);

    let x = ::sub::<u8>(1_u8, 2_u8);
    let x = ::sub::<u32>(1_u32, 2_u32);
    let x = ::sub::<f64>(1_f64, 2_f64);
}
unsafe fn add(x: i32, y: i32) -> i32 {
    x + y
}
unsafe fn sub<T: Sub<T, Result = T>>(x: T, y: T) -> T {
    x - y
}
