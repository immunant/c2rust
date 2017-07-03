fn main() {
    let mut i;

    i = 0;
    'a: while (i < 10) {
        println!("{}", i);
        i = i + 1;
    }

    /*
    // This part of the test is disabled because it triggers a bug in libsyntax `pprust`: the
    // pretty printer does not put parentheses around `0..10` in `(0..10).step_by(2)`.
    i = 0;
    'a: while (i < 10) {
        println!("{}", i);
        i = i + 2;
    }
    */
}
