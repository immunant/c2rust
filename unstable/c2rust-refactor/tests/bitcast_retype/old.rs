struct S {
    x: i32,
}

unsafe fn get_x(s: &S) -> i32 {
    s.x
}

unsafe fn set_x(s: &mut S, x: i32) {
    s.x = x;
}

unsafe fn get_x_addr(s: &S) -> &i32 {
    &s.x
}

unsafe fn get_x_addr_mut(s: &mut S) -> &mut i32 {
    &mut s.x
}

unsafe fn set_x_2(s: &mut S, x: i32) {
    *get_x_addr_mut(s) = x;
}

unsafe fn set_x_3(s: &mut S, x: i32) {
    set_x(s, x);
}

fn main() {}
