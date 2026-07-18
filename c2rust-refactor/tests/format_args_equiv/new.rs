fn main() {
    let _ = (println!("literal a"), println!("literal b"));
    let _ = (println!("{:x}", 1), println!("{:?}", 1));
}
