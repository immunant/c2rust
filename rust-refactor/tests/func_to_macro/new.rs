fn f(x: i32, y: i32) -> i32 {
    x + y
}

fn main() {
    mac!(1, 1);
    mac!(1 + 1, 2 * 2);
    mac!(mac!(1, 2), 3);
}
