static mut S: u8 = 0;

fn main() {
    unsafe {
        let x: u8 = S;
        S = x;
        {
            let r: &u8 = &S;
        }
        {
            let r: &mut u8 = &mut S;
        }
        println!("S = {}", S);
    }
}
