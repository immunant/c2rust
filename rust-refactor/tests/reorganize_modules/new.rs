pub mod foo_h {}

pub mod some_struct {
    pub struct foo_struct {
        pub x: i32,
        pub y: i32,
    }
    pub struct foo_struct2 {
        pub x: i32,
        pub y: i32,
    }
}

use some_struct::foo_struct;
use some_struct::foo_struct2;

fn main() {

    let f: foo_struct = foo_struct {x: 0, y:0};
}
