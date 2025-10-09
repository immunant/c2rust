fn main() {
    // Just make sure this doesn't crash (see #862).
    let x = 1;
    let _ = std::ptr::addr_of!(x);
}
