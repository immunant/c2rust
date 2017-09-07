fn main() {
    let mut i;
    
    'a: for i in 0..10 {
        println!("{}", i);
    }
    'a: for i in (0..10).step_by(2) {
        println!("{}", i);
    }
}
