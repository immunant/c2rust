static mut S: char = 0 as char;

fn main() {
    unsafe {
        let x: u8 = S as u8;
        S = x as char;
        {
            let r: &u8 = &*(&S as *const char as *const u8);
        }
        {
            let r: &mut u8 = &mut *(&mut S as *mut char as *mut u8);
        }
        println!("S = {}" , *(&S as *const char as *const u8));
    }
}
