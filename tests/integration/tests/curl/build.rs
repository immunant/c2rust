#[cfg(all(unix, not(target_os = "macos")))]
fn main() {
    // add unix dependencies below
    println!("cargo:rustc-link-lib=nghttp2");
    println!("cargo:rustc-link-lib=idn2");
    println!("cargo:rustc-link-lib=rtmp");
    println!("cargo:rustc-link-lib=psl");
    println!("cargo:rustc-link-lib=ssl");
    println!("cargo:rustc-link-lib=crypto");
    println!("cargo:rustc-link-lib=ldap");
    println!("cargo:rustc-link-lib=lber");
    println!("cargo:rustc-link-lib=brotlidec");
    println!("cargo:rustc-link-lib=z");
    println!("cargo:rustc-link-lib=zstd");
}

#[cfg(target_os = "macos")]
fn main() {
    // add macos dependencies below
    // println!("cargo:rustc-flags=-l edit");
}
