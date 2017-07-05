static A: u32 = 1;
static B: u32 = 1;
static C: u32 = 1;

fn main() {
    println!("{} {} {}", A, B, C);
    {
        let A = 999;
        let B = 999;
        println!("shadowed: {} {} {}", A, B, C);
    }
}
