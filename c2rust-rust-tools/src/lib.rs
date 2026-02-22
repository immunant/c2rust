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
