/// Test case for checking the rewriter's handling of operator precedence.  The `test_one_plus_one`
/// and `test_f_plus_one` rewrites should both be semantics-preserving on this program.

fn f(x: i32) -> i32 {
    x + 1
}

fn main() {
    println!("f(2) = {}", f(1 + 1));

    println!("2 + 10 = {}", 1 + 1 + 10);
    println!("10 + 2 = {}", 10 + (1 + 1));
    println!("2 + 2 = {}", 1 + 1 + (1 + 1));
    // Higher precedence than +
    println!("2 * 10 = {}", (1 + 1) * 10);
    println!("10 * 2 = {}", 10 * (1 + 1));
    // Lower precedence than +
    println!("2 << 3 = {}", 1 + 1 << 3);
    println!("3 << 2 = {}", 3 << 1 + 1);

    println!("f(2 + 3) = {}", f(1 + 1 + 3));
    println!("f(2 * 3) = {}", f((1 + 1) * 3));
    println!("f(2 << 3) = {}", f(1 + 1 << 3));
}
