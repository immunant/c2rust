/*
convert_format_string
    -c tests/convert_format_string.rs:13:16
    -c tests/convert_format_string.rs:14:16
    -c tests/convert_format_string.rs:15:16
*/
extern "C" {
    fn printf(s: &str, ...);
}

fn main() {
    unsafe {
        printf("int %d", 1);
        printf("char %c", 65);
        printf("multi %d %c %x", 65, 65, 65);
    }
}
