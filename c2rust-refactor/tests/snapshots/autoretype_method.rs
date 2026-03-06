struct S;

impl S {
    fn f(&self) {}
}

fn main() {
    S.f();

    // Something to rewrite, to force generation of old.new
    1 + 1;
}
