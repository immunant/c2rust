/// Test case for checking the rewriter's handling of operator precedence.  The `test_one_plus_one`
/// and `test_f_plus_one` rewrites should both be semantics-preserving on this program.

fn f(x: i32) -> i32 {
    x + 1
}

fn main() {
    println!("f(2) = {}", f(2));

    println!("2 + 10 = {}", 2 + 10);
    println!("10 + 2 = {}", 10 + 2);
    println!("2 + 2 = {}", 2 + 2);
    // Higher precedence than +
    println!("2 * 10 = {}", 2 * 10);
    println!("10 * 2 = {}", 10 * 2);
    // Lower precedence than +
    println!("2 << 3 = {}", 2 << 3);
    println!("3 << 2 = {}", 3 << 2);

    println!("f(2 + 3) = {}", f(2 + 3));
    println!("f(2 * 3) = {}", f(2 * 3));
    println!("f(2 << 3) = {}", f(2 << 3));
}
