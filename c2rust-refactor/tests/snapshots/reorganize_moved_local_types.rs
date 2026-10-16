#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(dead_code, non_camel_case_types, unused_imports)]

pub mod other {
    #[c2rust::header_src = "word.h:1"]
    pub mod word_h {
        #[c2rust::src_loc = "1:1"]
        pub type word_0 = u64;
    }
}

pub mod source {
    #[derive(Copy, Clone)]
    #[repr(C)]
    pub union Coord {
        pub integer: i32,
        pub real: f32,
    }

    pub type word_0 = u32;

    pub struct LocalGeneric<T>(pub T);

    #[c2rust::header_src = "geometry.h:1"]
    pub mod geometry_h {
        use super::word_0;
        use super::Coord;
        use super::LocalGeneric;

        #[repr(C)]
        #[c2rust::src_loc = "2:1"]
        pub struct Bounds {
            pub coords: [Coord; 4],
            pub generic: LocalGeneric<u8>,
        }

        #[c2rust::src_loc = "3:1"]
        pub const KIND: word_0 = 2 as word_0;

        // These imports remain with the header's function, but Bounds and
        // KIND move to a shared module and still need to name their types.
        pub unsafe fn read(coord: Coord, generic: LocalGeneric<u8>) -> word_0 {
            coord.integer as word_0 + generic.0 as word_0
        }
    }

    // This import is subsumed by the same-name header alias collected above.
    // The constant must still refer to the original, narrower source alias.
    #[c2rust::header_src = "constants.h:1"]
    pub mod constants_h {
        use super::word_0;

        #[c2rust::src_loc = "1:1"]
        pub const OTHER_KIND: word_0 = 3 as word_0;
    }

    pub fn kind() -> word_0 {
        geometry_h::KIND
    }
}

fn main() {}
