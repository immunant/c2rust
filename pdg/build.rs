use std::path::Path;
use std::process::Command;
use std::str;

use color_eyre::eyre;
use color_eyre::eyre::eyre;

fn main() -> eyre::Result<()> {
    color_eyre::install()?;
    // Add the toolchain lib/ directory to `-L`.  This fixes the linker error "cannot find
    // -lLLVM-13-rust-1.60.0-nightly".
    let out = Command::new("rustup")
        .args(&["which", "rustc"])
        .output()
        .or_else(|_| Command::new("which").args(&["rustc"]).output())?;
    assert!(out.status.success());
    let rustc_path = Path::new(str::from_utf8(&out.stdout)?.trim_end());
    let lib_dir = rustc_path
        .parent()
        .ok_or(eyre!("`which rustc` has no parent directory"))?
        .parent()
        .ok_or(eyre!("`which rustc` has no 2nd parent directory"))?
        .join("lib");
    println!("cargo:rustc-link-search={}", lib_dir.display());
    Ok(())
}
