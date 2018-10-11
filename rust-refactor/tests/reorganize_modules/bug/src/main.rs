pub mod foo {
    #[cfg(not(source_header = "/some/path/foo.h"))]
    pub mod foo_h {
        #[derive(Clone, Copy)]
        pub struct Foo {
            pub a: i32,
        }
    }
}

use self::foo::foo_h::Foo;

fn main() {
    let f = Foo {a:1};
    println!("{}", f.a);
}
