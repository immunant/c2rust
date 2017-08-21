extern "C" {
    fn printf(s: &str, ...);
}

fn main() {
    unsafe {
        printf("int {:}", (1) as i32);
        printf("char {:}", (65) as u8 as char);
        printf(
            "multi {:} {:} {:x}",
            (65) as i32,
            (65) as u8 as char,
            (65) as u32,
        );
        printf(
            "star {:*} {:*.*} {:.*}",
            (1) as usize,
            (2) as i32,
            (3) as usize,
            (4) as usize,
            (5) as i32,
            (6) as usize,
            (7) as i32,
        );
    }
}
