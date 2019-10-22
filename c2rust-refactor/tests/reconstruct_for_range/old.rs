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

    i = 0;
    'a: while (i < 10) {
        i += 2;
        println!("{}", i);
        i = i + 1;
    }
}
