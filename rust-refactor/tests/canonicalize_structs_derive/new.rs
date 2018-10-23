mod a {
    #[derive(Clone, Copy)]
    pub struct TheStruct {
        pub x: i32,
    }
}

mod b {
    struct Before;

    struct After;
}

fn main() {
    let a_struct = ::a::TheStruct { x: 100 };
    let b_struct = ::a::TheStruct { x: 100 };
}
