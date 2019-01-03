#![feature(extern_types)]
#![feature(asm)]
#![feature(ptr_wrapping_offset_from)]
#![feature(custom_attribute)]

#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(mutable_transmutes)]
#![allow(unused_mut)]

pub mod bar {
    #[header_src = "/home/user/some/workspace/foobar/bar.h"]
    pub mod bar_h {
        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct unnamed {
            a: usize,
        }

        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct unnamed_0 {
            x: i32,
            y: i32,
        }

        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct bar_t {
            u: unnamed,
        }
    }
    use self::bar_h::*;
}

pub mod foo {
    #[header_src = "/home/user/some/workspace/foobar/foo.h"]
    pub mod foo_h {
        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct unnamed {
            b: usize,
        }

        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct unnamed_0 {
            c: usize,
        }
        
        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct foo_t {
            u: unnamed,
        }
    }

    use self::foo_h::{unnamed, foo_t};
    use self::foo_h::unnamed_0;
    #[derive(Copy, Clone)]
    #[repr(C)]
    pub struct foo_bar {
        u: unnamed,
        u2: unnamed_0,
    }
}

pub mod test {
    pub mod unnamed {
    }
}

struct unnamed {
    d: u32, 
}

fn main() {
    let u = unnamed {
        d: 0
    };

    println!("{}", u.d);
}
