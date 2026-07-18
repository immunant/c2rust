fn broken_and_retyped(x: i32) -> i32 {
    missing_function();
    x + 1
}

fn main() {
    another_missing_function();
}
