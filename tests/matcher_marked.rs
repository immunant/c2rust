/*
    rewrite_expr marked!(__e)+__f __f+__e
    -c tests/matcher_marked.rs:7:21
    */

fn main() {
    println!("{}", (1 + 2) + (3 + 4));
}
