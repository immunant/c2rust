#[cfg(target_os = "linux")]
fn main() {
    println!("cargo:rustc-link-lib=pcre");
    println!("cargo:rustc-link-lib=crypt");
    println!("cargo:rustc-link-lib=z");
}

#[cfg(target_os = "macos")]
fn main() {
    unimplemented!();
}
