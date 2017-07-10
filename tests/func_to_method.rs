struct S { x: u32 }

impl S {
}

fn by_val(s: S) -> u32 {
    s.x
}

fn by_ref(s: &S) -> u32 {
    s.x
}

fn by_mut_ref(s: &mut S, new_x: u32) {
    s.x = new_x;
}

fn by_ref_with_lt<'a>(s: &'a S) -> &'a u32 {
    &s.x
}

fn main() {
    let mut s = S { x: 0 };
    println!("by_ref: {}", by_ref(&s));
    by_mut_ref(&mut s, 1);
    println!("by_ref_with_lt: {}", by_ref_with_lt(&s));
    println!("by_val: {}", by_val(s));
}

