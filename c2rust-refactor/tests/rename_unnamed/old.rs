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
        pub struct C2RustUnnamed {
            a: usize,
        }

        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct C2RustUnnamed_0 {
            x: i32,
            y: i32,
        }

        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct bar_t {
            u: C2RustUnnamed,
        }
    }
    use self::bar_h::*;
}

pub mod foo {
    #[header_src = "/home/user/some/workspace/foobar/foo.h"]
    pub mod foo_h {
        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct C2RustUnnamed {
            b: usize,
        }

        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct C2RustUnnamed_0 {
            c: usize,
        }
        
        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct foo_t {
            u: C2RustUnnamed,
        }
    }

    use self::foo_h::{C2RustUnnamed, foo_t};
    use self::foo_h::C2RustUnnamed_0;
    #[derive(Copy, Clone)]
    #[repr(C)]
    pub struct foo_bar {
        u: C2RustUnnamed,
        u2: C2RustUnnamed_0,
    }
}

pub mod test {
    pub mod C2RustUnnamed {
    }
}

struct C2RustUnnamed {
    d: u32, 
}

fn main() {
    let u = C2RustUnnamed {
        d: 0
    };

    println!("{}", u.d);
}
