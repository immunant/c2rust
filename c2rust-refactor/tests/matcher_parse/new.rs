fn main() {
    let _ = dbg!(1u8);
    let _ = dbg!(dbg!(2u16) + dbg!(3));
    let _ = dbg!(dbg!(4u64).saturating_add(dbg!(5)));
}
