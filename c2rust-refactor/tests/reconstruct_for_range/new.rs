fn main() {
    let mut i;
    
    'a: for i in 0..10 {
        println!("{}", i);
    }
    'a: for i in (0..10).step_by(2 as usize) {
        println!("{}", i);
    }

    i = 0;
    'a: while (i < 10) {
        i += 2;
        println!("{}", i);
        i = i + 1;
    }
}
