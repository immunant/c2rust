use log::warn;
use std::path::Path;
use std::process::Command;

/// The Rust edition used by code emitted by `c2rust`.
pub const EDITION: &str = "2021";

#[must_use]
pub struct Rustfmt<'a> {
    rs_path: &'a Path,
    check: bool,
    expect_error: bool,
}

pub fn rustfmt(rs_path: &Path) -> Rustfmt {
    Rustfmt {
        rs_path,
        check: false,
        expect_error: false,
    }
}

impl<'a> Rustfmt<'a> {
    pub fn check(self, check: bool) -> Self {
        Self { check, ..self }
    }

    pub fn expect_error(self, expect_error: bool) -> Self {
        Self {
            expect_error,
            ..self
        }
    }

    pub fn run(self) {
        let Self {
            rs_path,
            check,
            expect_error,
        } = self;
        let check = if expect_error { true } else { check };
        run_rustfmt(rs_path, check, expect_error)
    }
}

fn run_rustfmt(rs_path: &Path, check: bool, expect_error: bool) {
    assert!(!expect_error || check);

    let mut cmd = Command::new("rustfmt");
    cmd.args(["--edition", EDITION]);
    cmd.arg(rs_path);
    if check {
        cmd.arg("--check");
    }
    let status = cmd.status();

    // TODO Rust 1.65 use let else
    let status = match status {
        Ok(status) => status,
        Err(e) => {
            warn!("rustfmt not found; code may not be well-formatted: {e}");
            return;
        }
    };

    if check {
        if expect_error {
            assert!(
                !status.success(),
                "expected error, but rustfmt succeeded: {cmd:?}"
            );
        } else {
            assert!(status.success(), "rustfmt failed with {status}: {cmd:?}");
        }
    } else if !status.success() {
        warn!("rustfmt failed with {status}; code may not be well-formatted: {cmd:?}");
    }
}

#[must_use]
pub struct Rustc<'a> {
    rs_path: &'a Path,
    crate_name: Option<&'a str>,
    expect_error: bool,
}

pub fn rustc(rs_path: &Path) -> Rustc {
    Rustc {
        rs_path,
        crate_name: None,
        expect_error: false,
    }
}

impl<'a> Rustc<'a> {
    pub fn crate_name(self, crate_name: &'a str) -> Self {
        Self {
            crate_name: Some(crate_name),
            ..self
        }
    }

    pub fn expect_error(self, expect_error: bool) -> Self {
        Self {
            expect_error,
            ..self
        }
    }

    pub fn run(self) {
        let Self {
            rs_path,
            crate_name,
            expect_error,
        } = self;
        let crate_name =
            crate_name.unwrap_or_else(|| rs_path.file_stem().unwrap().to_str().unwrap());
        run_rustc(rs_path, crate_name, expect_error);
    }
}

fn run_rustc(rs_path: &Path, crate_name: &str, expect_error: bool) {
    // There's no good way to not create an output with `rustc`,
    // so just create an `.rlib` and then delete it immediately.
    let rlib_path = rs_path.with_file_name(format!("lib{crate_name}.rlib"));
    let mut cmd = Command::new("rustc");
    cmd.args([
        "+nightly-2023-04-15",
        "--crate-type",
        "lib",
        "--edition",
        EDITION,
        "--crate-name",
        crate_name,
        "-Awarnings", // Disable warnings.
        "-o",
    ]);
    cmd.args([&rlib_path, rs_path]);
    let status = cmd.status().unwrap();
    if status.success() {
        fs_err::remove_file(&rlib_path).unwrap();
    }
    if expect_error {
        assert!(
            !status.success(),
            "expected error, but rustc succeeded: {cmd:?}"
        );
    } else {
        assert!(status.success(), "rustc failed with {status}: {cmd:?}");
    }
}
