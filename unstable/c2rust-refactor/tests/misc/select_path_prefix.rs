mod a {
    fn f() {}
    static S: i32 = 0;

    mod a2 {
        fn f() {}
        static S: i32 = 0;
    }
}

mod b {
    fn f() {}
    fn g() {}
}

mod c {
    fn g() {}
}

fn main() {
}
