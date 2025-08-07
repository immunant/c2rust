union example_union {
    field1: u32,
    field2: u64,
}

unsafe fn go(u: example_union) -> u32 {
    u.field1
}

fn main () { }
