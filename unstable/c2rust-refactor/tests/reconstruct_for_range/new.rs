fn main() {
    let (mut i, mut j, mut k);
    
    'a: for i in 0..10 {
        println!("{}", i);
    }
    'b: for j in (0..10).step_by(2 as usize) {
        println!("{}", j);
    }

    k = 0;
    'c: while (k < 10) {
        k += 2;
        println!("{}", k);
        k = k + 1;
    }
}
