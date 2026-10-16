#![feature(register_tool)]
#![register_tool(c2rust)]
#![allow(dead_code)]

pub mod definition {
    #[no_mangle]
    pub static TABLE: [u8; 3] = [10, 20, 30];
}

pub mod caller {
    #[c2rust::header_src = "table.h:1"]
    pub mod table_h {
        extern "C" {
            #[c2rust::src_loc = "1:1"]
            pub static TABLE: [u8; 0];
        }
    }

    pub unsafe fn get(index: usize) -> u8 {
        *table_h::TABLE.as_ptr().add(index)
    }
}

fn main() {}
