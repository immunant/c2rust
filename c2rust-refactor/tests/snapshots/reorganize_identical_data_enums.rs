#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(dead_code)]

pub mod first {
    #[c2rust::header_src = "types.h:1"]
    pub mod types_h {
        #[repr(C)]
        #[c2rust::src_loc = "1:1"]
        pub enum E {
            V(i32),
        }
    }

    pub use types_h::E;
}

pub mod second {
    #[c2rust::header_src = "types.h:1"]
    pub mod types_h {
        #[repr(C)]
        #[c2rust::src_loc = "1:1"]
        pub enum E {
            V(i32),
        }
    }

    pub use types_h::E;
}

fn main() {
    let _ = first::E::V(1);
    let _ = second::E::V(2);
}
