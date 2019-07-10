mod a {
    #[derive(Clone, Copy)]
    pub struct TheStruct {
        pub x: i32,
    }
}

mod b {
    struct Before;

    #[derive(Clone, Copy)]
    pub struct TheStruct {
        pub x: i32,
    }

    struct After;
}

fn main() {
    let a_struct = crate::a::TheStruct { x: 100 };
    let b_struct = crate::b::TheStruct { x: 100 };
}
