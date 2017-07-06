#[no_mangle]
static A: u32 = 1;
#[no_mangle]
static B: u32 = 1;
#[no_mangle]
static C: u32 = 1;

fn main() {
    println!("{} {} {}", A, B, C);
}
