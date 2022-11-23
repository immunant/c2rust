fn safe_fn() {
    println!("This is safe rust");
    let mut s = "testing";
    let s_ref = &s;
    // Deref a reference to ensure this is not analyzed as a raw ptr deref by
    // the dynamic analysis
    println!("{}", *s_ref);
}
