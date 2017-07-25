/*
   -c tests/func_to_method.rs:12:1:dest
   -c tests/func_to_method.rs:15:11::arg
   -c tests/func_to_method.rs:19:11::arg
   -c tests/func_to_method.rs:23:27::arg
   -c tests/func_to_method.rs:27:23::arg
   -c tests/func_to_method.rs:31:1
   func_to_method
   */
struct S { x: u32 }

impl S {
}

fn by_val(s: S) -> u32 {
    s.x
}

fn by_ref(s: &S) -> u32 {
    s.x
}

fn by_mut_ref(new_x: u32, s: &mut S) {
    s.x = new_x;
}

fn by_ref_with_lt<'a>(s: &'a S) -> &'a u32 {
    &s.x
}

fn static_method(x: i32, y: i32) -> i32 {
    x + y
}

fn main() {
    let mut s = S { x: 0 };
    println!("by_ref: {}", by_ref(&s));
    by_mut_ref(1, &mut s);
    println!("by_ref_with_lt: {}", by_ref_with_lt(&s));
    println!("by_val: {}", by_val(s));
    println!("static_method: {}", static_method(100, -100));
}

