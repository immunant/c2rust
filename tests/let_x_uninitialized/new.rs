fn main() {
    let x = ::std::mem::uninitialized();
    let y: u32 = ::std::mem::uninitialized();
    let z: Option<& /* aeiou */ u32> = ::std::mem::uninitialized();
    let (a, b) = ::std::mem::uninitialized();
}
