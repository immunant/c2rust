#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_imports)]

pub mod first {
    #[c2rust::header_src = "/home/user/some/workspace/first.h:1"]
    pub mod first_h {
        // An `extern` block without an explicit ABI string defaults to "C",
        // so this declaration must merge with the explicit `extern "C"`
        // declaration of the same function in second.h below.
        extern {
            #[c2rust::src_loc = "3:0"]
            pub fn compute(x: i32) -> i32;
        }
    }
    use self::first_h::compute;

    pub fn call_first(x: i32) -> i32 {
        unsafe { compute(x) }
    }
}

pub mod second {
    #[c2rust::header_src = "/home/user/some/workspace/second.h:1"]
    pub mod second_h {
        extern "C" {
            #[c2rust::src_loc = "2:0"]
            pub fn compute(x: i32) -> i32;
        }
    }
    use self::second_h::compute;

    pub fn call_second(x: i32) -> i32 {
        unsafe { compute(x) }
    }
}

fn main() {}
