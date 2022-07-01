use std::env;
use std::path::Path;
use std::process::Command;
use std::str;

fn main() {
    // Add the toolchain lib/ directory to `-L`.  This fixes the linker error "cannot find
    // -lLLVM-13-rust-1.60.0-nightly".
    let out = Command::new("rustup")
        .args(&["which", "rustc"])
        .output()
        .unwrap();
    assert!(out.status.success());
    let rustc_path = Path::new(str::from_utf8(&out.stdout).unwrap().trim_end());
    let lib_dir = rustc_path.parent().unwrap().parent().unwrap().join("lib");
    println!("cargo:rustc-link-search={}", lib_dir.display());

    let out = Command::new(env::var("RUSTC").unwrap())
        .arg("--print=sysroot")
        .output()
        .expect("Could not invoke rustc to find rust sysroot");
    assert!(out.status.success());
    let sysroot = str::from_utf8(&out.stdout).unwrap();
    println!("cargo:rustc-env=RUST_SYSROOT={}", sysroot)
}