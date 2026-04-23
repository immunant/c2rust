use itertools::Itertools;
use log::warn;
use std::collections::HashSet;
use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;
use std::io;
use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::str;
use std::str::FromStr;

use crate::RustEdition::Edition2021;
use crate::RustEdition::Edition2024;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub enum RustEdition {
    /// The default is edition 2021 because `c2rust-refactor`,
    /// based on `nightly-2022-11-03`, only understands up to edition 2021.
    #[default]
    Edition2021,
    Edition2024,
}

impl RustEdition {
    pub const ALL: &[Self] = &[Edition2021, Edition2024];

    pub const fn as_str(&self) -> &'static str {
        match self {
            Edition2021 => "2021",
            Edition2024 => "2024",
        }
    }

    /// The toolchain to use for this edition.
    /// This is returned in the `+{toolchain}` format
    /// that can be passed to `rustup` wrappers.
    pub const fn toolchain(&self) -> &'static str {
        match self {
            // 1.70 (1.68 for syn v2.0, 1.70 for sparse registry)
            Edition2021 => "+nightly-2023-04-15",
            // This doesn't really need to be pinned, but pin it for stability.
            Edition2024 => "+nightly-2026-03-03",
        }
    }
}

impl Display for RustEdition {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl FromStr for RustEdition {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let choices = Self::ALL;
        choices
            .into_iter()
            .copied()
            .find(|choice| choice.as_str() == s)
            .ok_or_else(|| format!("{s} not one of {}", choices.iter().join(" ,")))
    }
}

#[must_use]
pub struct Rustfmt<'a> {
    rs_path: &'a Path,
    edition: RustEdition,
    check: bool,
    expect_error: bool,
}

pub fn rustfmt(rs_path: &Path) -> Rustfmt {
    Rustfmt {
        rs_path,
        edition: Default::default(),
        check: false,
        expect_error: false,
    }
}

impl<'a> Rustfmt<'a> {
    pub fn edition(self, edition: RustEdition) -> Self {
        Self { edition, ..self }
    }

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
            edition,
            check,
            expect_error,
        } = self;
        let check = if expect_error { true } else { check };
        run_rustfmt(rs_path, edition, check, expect_error)
    }
}

fn run_rustfmt(rs_path: &Path, edition: RustEdition, check: bool, expect_error: bool) {
    assert!(!expect_error || check);

    let mut cmd = Command::new("rustfmt");
    cmd.args([edition.toolchain(), "--edition", edition.as_str()]);
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
    edition: RustEdition,
    crate_name: Option<&'a str>,
    expect_error: bool,
    imported_crates: &'a [&'a str],
}

pub fn rustc(rs_path: &Path) -> Rustc {
    Rustc {
        rs_path,
        edition: Default::default(),
        crate_name: None,
        expect_error: false,
        imported_crates: Default::default(),
    }
}

impl<'a> Rustc<'a> {
    pub fn edition(self, edition: RustEdition) -> Self {
        Self { edition, ..self }
    }

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

    pub fn expect_unresolved_imports(self, imported_crates: &'a [&'a str]) -> Self {
        Self {
            imported_crates,
            ..self
        }
    }

    pub fn run(self) {
        let Self {
            rs_path,
            edition,
            crate_name,
            expect_error,
            imported_crates,
        } = self;
        let crate_name =
            crate_name.unwrap_or_else(|| rs_path.file_stem().unwrap().to_str().unwrap());
        run_rustc(rs_path, edition, crate_name, expect_error, &imported_crates);
    }
}

fn run_rustc(
    rs_path: &Path,
    edition: RustEdition,
    crate_name: &str,
    expect_error: bool,
    imported_crates: &[&str],
) {
    let rs = fs_err::read_to_string(rs_path).unwrap();
    for imported_crate in imported_crates {
        assert!(rs.contains(&format!("::{imported_crate}")));
    }

    // There's no good way to not create an output with `rustc`,
    // so just create an `.rlib` and then delete it immediately.
    let rlib_path = rs_path.with_file_name(format!("lib{crate_name}.rlib"));
    let mut cmd = Command::new("rustc");
    cmd.args([
        edition.toolchain(),
        "--crate-type",
        "lib",
        "--edition",
        edition.as_str(),
        "--crate-name",
        crate_name,
        "-Awarnings", // Disable warnings.
        "-o",
    ]);
    cmd.args([&rlib_path, rs_path]);
    let output = cmd.output().unwrap();
    let status = output.status;
    if status.success() {
        fs_err::remove_file(&rlib_path).unwrap();
    }
    io::stdout().write_all(&output.stdout).unwrap();
    io::stderr().write_all(&output.stderr).unwrap();

    // Using `rustc` itself to build snapshots that reference crates (like `libc`)
    // is difficult because we don't know the appropriate
    // `--extern libc=/path/to/liblibc-XXXXXXXXXXXXXXXX.rlib` to pass.
    // So we instead just check that these errors are as expected
    // based on the expected crates to be imported (`imported_crates`).
    let stderr = str::from_utf8(&output.stderr).unwrap();
    let mut error_lines = stderr
        .split('\n')
        // .split(|&b| b == b'\n')
        .filter(|line| line.starts_with("error[E"))
        .collect::<HashSet<_>>();
    dbg!(&error_lines);
    for imported_crate in imported_crates {
        // For `::{imported_crate}::*`.
        let absolute_path = match edition {
            Edition2021 => format!("error[E0433]: failed to resolve: could not find `{imported_crate}` in the list of imported crates"),
            Edition2024 => format!("error[E0433]: cannot find `{imported_crate}` in the crate root"),
        };
        // For `use ::{imported_crate}*`.
        let absolute_use_path = format!("error[E0432]: unresolved import `{imported_crate}`");
        // For `{imported_crate}::*`.
        let relative_path = match edition {
            Edition2021 => format!("error[E0433]: failed to resolve: use of undeclared crate or module `{imported_crate}`"),
            Edition2024 => format!("error[E0433]: cannot find module or crate `{imported_crate}` in this scope"),
        };

        let absolute_path = absolute_path.as_str();
        let absolute_use_path = absolute_use_path.as_str();
        let relative_path = relative_path.as_str();
        dbg!(absolute_path);
        dbg!(absolute_use_path);
        dbg!(relative_path);

        // Pre-compute to avoid `||` short-circuiting.
        let absolute_path = error_lines.remove(absolute_path);
        let absolute_use_path = error_lines.remove(absolute_use_path);
        let relative_path = error_lines.remove(relative_path);

        assert!(absolute_path || absolute_use_path || relative_path);
    }
    if !imported_crates.is_empty() {
        dbg!(&error_lines);
        assert!(error_lines.is_empty());
        return;
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

/// Replace all non-alphanumeric characters and `-_.` with `_`s
/// so that we have a sanitized, idiomatic file name that excludes weird characters,
/// even if they're technically allowed in a file name.
pub fn sanitize_file_name(file_name: &str) -> String {
    file_name
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || c == '-' || c == '_' || c == '.' {
                c
            } else {
                '_'
            }
        })
        .collect()
}
