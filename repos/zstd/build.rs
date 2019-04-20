#[cfg(target_os = "linux")]
fn main() {
    // add linux dependencies here below
     println!("cargo:rustc-link-lib=z");
}

#[cfg(target_os = "macos")]
fn main() {
    // add macos dependencies below
    unimplemented!();
}