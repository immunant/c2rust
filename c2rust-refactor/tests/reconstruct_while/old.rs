fn main() {
    let mut i = 0;
    'a: loop {
        if !(i < 10) {
            break;
        }
        println!("{}", i);
        i = i + 1;
    }
}
