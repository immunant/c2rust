#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]

// The destination all the standard library headers below are merged into.
// It has to already exist for the `BEGIN`/`END` header comments to make it
// into the output.
pub mod stdlib {
    pub const STDLIB_MARKER: i32 = 0;
}

pub mod api {
    // Both headers are under `/usr/include`, so they are treated as standard
    // library headers and all of their declarations land in the same `stdlib`
    // destination module. Each header mixes a regular item with foreign items,
    // and `aa.h` declares two different ABIs.
    //
    // `into_items` groups the foreign items by ABI into one `extern` block
    // each and hoists every block ahead of every regular item, so:
    //
    // - the relative order of the two `extern` blocks comes from iterating a
    //   `HashMap<Abi, _>` and varies from run to run, and
    // - the `BEGIN`/`END` header comments are assigned while walking the
    //   declarations in source order, before the hoisting, so they describe a
    //   layout that isn't the one that gets emitted.
    #[c2rust::header_src = "/usr/include/aa.h:1"]
    pub mod aa_h {
        #[c2rust::src_loc = "2:0"]
        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct aa_ty {
            pub x: i32,
        }

        extern "C" {
            #[c2rust::src_loc = "3:0"]
            pub fn aa_c_fn() -> i32;
        }

        extern "sysv64" {
            #[c2rust::src_loc = "4:0"]
            pub fn aa_sysv_fn() -> i32;
        }
    }

    #[c2rust::header_src = "/usr/include/bb.h:5"]
    pub mod bb_h {
        extern "C" {
            #[c2rust::src_loc = "6:0"]
            pub fn bb_c_fn() -> i32;
        }

        #[c2rust::src_loc = "7:0"]
        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct bb_ty {
            pub y: i32,
        }
    }

    pub unsafe fn go() -> i32 {
        let a = aa_h::aa_ty { x: 1 };
        let b = bb_h::bb_ty { y: 2 };
        a.x + b.y + aa_h::aa_c_fn() + aa_h::aa_sysv_fn() + bb_h::bb_c_fn()
    }
}

fn main() {}
