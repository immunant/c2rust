fn main() {
    let x = 1 + 2;
    let x = 1 + 2 + 3;

    #[allow(arithmetic_overflow)] // Deliberately abstract an overflowing subtraction.
    let x = 1_u8 - 2_u8;
    #[allow(arithmetic_overflow)]
    let x = 1_u32 - 2_u32;
    let x = 1_f64 - 2_f64;
}
