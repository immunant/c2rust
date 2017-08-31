struct S {
    x: u32,
}

unsafe fn get_x(s: &S) -> u32 {
    ::std::mem::transmute::<i32, u32>(::std::mem::transmute::<u32, i32>(s.x))
}

unsafe fn set_x(s: &mut S, x: u32) {
    *::std::mem::transmute::<&mut u32, &mut i32>(&mut s.x) = ::std::mem::transmute::<u32, i32>(x);
}

unsafe fn get_x_addr(s: &S) -> &u32 {
    ::std::mem::transmute::<&i32, &u32>(&*::std::mem::transmute::<&u32, &i32>(&s.x))
}

unsafe fn get_x_addr_mut(s: &mut S) -> &mut u32 {
    ::std::mem::transmute::<&mut i32, &mut u32>(
        &mut *::std::mem::transmute::<&mut u32, &mut i32>(&mut s.x),
    )
}

unsafe fn set_x_2(s: &mut S, x: u32) {
    *::std::mem::transmute::<&mut u32, &mut i32>(get_x_addr_mut(s)) =
        ::std::mem::transmute::<u32, i32>(x);
}

unsafe fn set_x_3(s: &mut S, x: u32) {
    set_x(
        s,
        ::std::mem::transmute::<i32, u32>(::std::mem::transmute::<u32, i32>(x)),
    );
}

fn main() {}
