#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]

pub mod thing {
    #[c2rust::header_src = "/home/user/some/workspace/thing.h:1"]
    pub mod thing_h {
        #[c2rust::src_loc = "2:0"]
        #[derive(Copy, Clone)]
        #[repr(C)]
        pub struct point {
            pub x: i32,
        }
    }

    // A macro sharing the `point` spelling with the struct in the header
    // above. Macros live in the macro namespace, so the two names do not
    // collide and the struct should still move into this module. But
    // `item_namespaces` classifies every item it doesn't recognize as
    // `TypeNS`, `MacroDef` included, so `update_module_info_items` records
    // `point` as occupying this module's *type* namespace and
    // `find_destination_id` rejects `thing` as a destination for the struct.
    //
    // Note there is deliberately no `use self::thing_h::point;` here: an
    // import of the struct would land in `import_targets` and let the
    // conflict check see that the two `point`s are the same definition,
    // masking the misclassification.
    macro_rules! point {
        () => {
            0
        };
    }

    pub fn go() -> i32 {
        let p = thing_h::point { x: point!() };
        p.x
    }
}

fn main() {}
