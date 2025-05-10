fn main() {
    let (mut i, mut j, mut k);

    i = 0;
    'a: while (i < 10) {
        println!("{}", i);
        i = i + 1;
    }

    j = 0;
    'b: while (j < 10) {
        println!("{}", j);
        j = j + 2;
    }

    k = 0;
    'c: while (k < 10) {
        k += 2;
        println!("{}", k);
        k = k + 1;
    }
}
