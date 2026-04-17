#![feature(extern_types)]
#![feature(register_tool)]
#![register_tool(c2rust)]

pub mod foreign1 {
    pub struct Foo(i32);
    #[c2rust::header_src = "foo.h:1"]
    pub mod foo_h {
        pub use super::Foo;
    }
    #[c2rust::header_src = "baz.h:1"]
    pub mod baz_h {
        // reorganize_definitions should preserve this import
        pub use super::foo_h::Foo;
        #[c2rust::src_loc = "2:1"]
        pub type FooPtr = *mut Foo;
    }
}

pub mod foreign2 {
    #[c2rust::header_src = "foo.h:1"]
    pub mod foo_h {
        extern "C" {
            #[c2rust::src_loc = "1:1"]
            pub type Foo;
        }
        #[c2rust::src_loc = "2:1"]
        pub type FooPtr = *mut Foo;
    }
    #[c2rust::header_src = "baz.h:1"]
    pub mod baz_h {
        pub use super::foo_h::Foo;
        #[c2rust::src_loc = "2:1"]
        pub type FooPtr = *mut Foo;
    }
}

fn main() {
    println!("hello!");
}
