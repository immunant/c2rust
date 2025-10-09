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

        // Needs to be properly implemented still
        // printf("star %*d %*.*d %.*d", 1, 2, 3, 4, 5, 6, 7);

        // Used to trigger a bug with macro collapsing, fixed in 3a721469
        printf(format_args!(
            "{:}{:}{:}",
            27i32 as u8 as char, '(' as i32 as u8 as char, 'B' as i32 as u8 as char
        ));
    }
}
