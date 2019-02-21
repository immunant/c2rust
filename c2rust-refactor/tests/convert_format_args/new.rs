extern "C" {
    fn printf(s: &str, ...);
}

fn main() {
    unsafe {
        printf(format_args!("int {:}", 1 as libc::c_int));
        printf(format_args!("char {:}", 65 as u8 as char));
        printf(format_args!(
            "multi {:} {:} {:x}",
            65 as libc::c_int, 65 as u8 as char, 65 as libc::c_uint
        ));
        printf(format_args!(
            "star {:*} {:*.*} {:.*}",
            1 as usize,
            2 as libc::c_int,
            3 as usize,
            4 as usize,
            5 as libc::c_int,
            6 as usize,
            7 as libc::c_int
        ));
    }
}
