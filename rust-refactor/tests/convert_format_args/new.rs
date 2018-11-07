extern "C" {
    fn printf(s: &str, ...);
}

fn main() {
    unsafe {
        printf(format_args!("int {:}", 1 as i32));
        printf(format_args!("char {:}", 65 as u8 as char));
        printf(format_args!(
            "multi {:} {:} {:x}",
            65 as i32, 65 as u8 as char, 65 as u32
        ));
        printf(format_args!(
            "star {:*} {:*.*} {:.*}",
            1 as usize, 2 as i32, 3 as usize, 4 as usize, 5 as i32, 6 as usize, 7 as i32
        ));
    }
}
