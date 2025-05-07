struct S {
    A: u32,
    C: u32,
}
static mut AC: S = S { A: 1, C: 1 };

#[no_mangle]
static B: u32 = 1;

fn main() {
    println!("{} {} {}", AC.A, B, AC.C);
}
