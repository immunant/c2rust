struct Test {
    val: u32,
}

fn f(s: Test) -> u32 {
    let Test { val: x } = s;
    x
}

struct Test2(u32);

fn g(s: Test2) -> u32 {
    let Test2(x) = s;
    x
}

fn main() {
    println!("{}", f(Test { val: 1234 }));
    println!("{}", g(Test2(1234)));
}
