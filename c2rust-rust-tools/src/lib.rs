use log::warn;
use std::path::Path;
use std::process::Command;

/// The Rust edition used by code emitted by `c2rust`.
pub const EDITION: &str = "2021";

pub fn rustfmt(rs_path: &Path) {
    let status = Command::new("rustfmt")
        .args(["--edition", EDITION])
        .arg(rs_path)
        .status();

    // TODO Rust 1.65 use let else
    let status = match status {
        Ok(status) => status,
        Err(e) => {
            warn!("rustfmt not found; code may not be well-formatted: {e}");
            return;
        }
    };

    if !status.success() {
        warn!("rustfmt failed; code may not be well-formatted: {status}");
    }
}

pub fn rustc(rs_path: &Path, crate_name: &str) {
    // There's no good way to not create an output with `rustc`,
    // so just create an `.rlib` and then delete it immediately.
    let rlib_path = rs_path.with_file_name(format!("lib{crate_name}.rlib"));
    let status = Command::new("rustc")
        .args([
            "+nightly-2023-04-15",
            "--crate-type",
            "lib",
            "--edition",
            EDITION,
            "--crate-name",
            crate_name,
            "-Awarnings", // Disable warnings.
            "-o",
        ])
        .args([&rlib_path, rs_path])
        .status();
    assert!(status.unwrap().success());
    fs_err::remove_file(&rlib_path).unwrap();
}
