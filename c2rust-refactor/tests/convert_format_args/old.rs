extern "C" {
    fn printf(s: &str, ...);
}

fn main() {
    unsafe {
        printf("int %d", 1);
        printf("char %c", 65);
        printf("multi %d %c %x", 65, 65, 65);

        // Needs to be properly implemented still
        // printf("star %*d %*.*d %.*d", 1, 2, 3, 4, 5, 6, 7);

        // Used to trigger a bug with macro collapsing, fixed in 3a721469
        printf("%c%c%c\x00", 27i32,
               '(' as i32, 'B' as i32);
    }
}
