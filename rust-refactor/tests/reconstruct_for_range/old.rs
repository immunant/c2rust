fn main() {
    let mut i;

    i = 0;
    'a: while (i < 10) {
        println!("{}", i);
        i = i + 1;
    }

    i = 0;
    'a: while (i < 10) {
        println!("{}", i);
        i = i + 2;
    }
}
