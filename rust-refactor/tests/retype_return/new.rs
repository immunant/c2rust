fn get_char() -> char {
    b'!' as char
}

fn main() {
    let c: u8 = get_char() as u8;
    println!("{}", c);
}
