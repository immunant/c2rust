#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]

pub mod stuff {
    // The type the header impls attach to is defined here, outside any
    // header module.
    #[derive(Copy, Clone)]
    #[repr(C)]
    pub struct opts {
        pub x: i32,
    }
}

pub mod opts {
    #[c2rust::header_src = "/home/user/some/workspace/opts.h:1"]
    pub mod opts_h {
        pub use crate::stuff::opts;

        // A const-only impl whose self type is defined outside the header
        // modules. Its saved `impls` entry is keyed by the external def,
        // which no moved declaration ever consumes.
        #[c2rust::src_loc = "2:0"]
        impl opts {
            #[c2rust::src_loc = "3:0"]
            pub const DEFAULT_X: i32 = 3;
        }

        // An impl containing a non-const item; it never qualifies for
        // saving in the first place.
        #[c2rust::src_loc = "4:0"]
        impl opts {
            #[c2rust::src_loc = "5:0"]
            pub fn reset(&mut self) {
                self.x = 0;
            }
        }
    }
    use self::opts_h::opts;

    pub fn go() -> i32 {
        let mut o = opts { x: opts::DEFAULT_X };
        o.reset();
        o.x
    }
}

fn main() {}
