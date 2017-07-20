/*
mark_arg_uses 0 target ; convert_format_string
    -c tests/convert_format_string.rs:6:5
*/
extern "C" {
    fn printf(s: &str, ...);
}

fn main() {
    unsafe {
        printf("int %d", 1);
        printf("char %c", 65);
        printf("multi %d %c %x", 65, 65, 65);
        printf("star %*d %*.*d %.*d", 1, 2, 3, 4, 5, 6, 7);
    }
}
