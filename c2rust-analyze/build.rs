use std::env;
use std::path::Path;
use std::process::Command;
use std::str;

fn main() {
    let rustc = env::var("RUSTC").unwrap();
    // Add the toolchain lib/ directory to `-L`.  This fixes the linker error "cannot find
    // -lLLVM-13-rust-1.60.0-nightly".
    let out = Command::new(&rustc)
        .args(&["--print", "sysroot"])
        .output().unwrap();
    assert!(out.status.success());
    let sysroot = Path::new(str::from_utf8(&out.stdout).unwrap().trim_end());
    let lib_dir = sysroot.join("lib");
    println!("cargo:rustc-link-search={}", lib_dir.display());

    let target = env::var("HOST").unwrap();
    let target_lib_dir = lib_dir.join("rustlib").join(target).join("lib");
    println!("cargo:rustc-env=C2RUST_TARGET_LIB_DIR={}", target_lib_dir.display());
}
