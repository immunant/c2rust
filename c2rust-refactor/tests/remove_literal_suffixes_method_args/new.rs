// Test for method argument suffix removal
fn main() {
    // These should have argument suffixes removed
    let wrapped = 1u32.wrapping_add(2);  // Should be: 1u32.wrapping_add(2)
    let clamped = 50u8.clamp(0u8, 100u8);   // Should be: 50u8.clamp(0, 100)
    let min_val = 1.0f32.min(2.0f32);       // Should be: 1.0f32.min(2.0)

    // Test with variables
    let x = 10u32;
    let y = x.wrapping_mul(5);           // Should be: x.wrapping_mul(5)

    // Test with multiple arguments
    let z = 100i32.clamp(-50i32, 50i32);    // Should be: 100i32.clamp(-50, 50)

    // Test with different types
    let a = 1i64.saturating_add(2);      // Should be: 1i64.saturating_add(2)
    let b = 10u16.saturating_sub(5);     // Should be: 10u16.saturating_sub(5)

    println!("{} {} {} {} {} {} {}", wrapped, clamped, min_val, y, z, a, b);
}
