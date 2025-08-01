use log::warn;
use std::{
    collections::HashSet,
    env,
    ffi::OsString,
    fmt::{self, Display, Formatter},
    fs::{self, File},
    path::{Path, PathBuf},
    process::Command,
};

use c2rust_build_paths::find_llvm_config;
use clap::Parser;

#[derive(Default)]
pub struct Analyze {
    path: PathBuf,
}

#[derive(Debug, Clone)]
struct EnvVar {
    pub var: String,
    pub value: String,
}

impl Display for EnvVar {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let Self { var, value } = self;
        write!(f, "{var}={value}")
    }
}

impl From<String> for EnvVar {
    fn from(env_var: String) -> Self {
        let (var, value) = env_var.split_once('=').unwrap_or((&env_var, ""));
        Self {
            var: var.to_owned(),
            value: value.to_owned(),
        }
    }
}

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct AnalyzeArgs {
    /// Allow `c2rust-analyze` to crash during analysis (it can either run successfully or crash).
    /// All output is still captured as normal.
    #[arg(long)]
    allow_crash: bool,

    /// Environment variables for `c2rust-analyze`.
    #[clap(long, value_parser)]
    env: Vec<EnvVar>,

    /// Enable catching panics during analysis and rewriting.  This behavior is enabled by default
    /// when running the tool manually, but for testing we disable it to detect errors more easily.
    /// Tests that are meant to exercise the panic-catching behavior can explicitly enable it with
    /// this flag.
    #[arg(long)]
    catch_panics: bool,

    /// Comma-separated list of paths to rewrite.  Any item whose path does not start with a prefix
    /// from this list will be marked non-rewritable (`FIXED`).
    #[clap(long)]
    rewrite_paths: Option<OsString>,

    /// Use `todo!()` placeholders in shims for casts that must be implemented manually.
    ///
    /// When a function requires a shim, and the shim requires a cast that can't be generated
    /// automatically, the default is to cancel rewriting of the function.  With this option,
    /// rewriting proceeds as normal, and shim generation emits `todo!()` in place of each
    /// unsupported cast.
    #[clap(long)]
    use_manual_shims: bool,
}

impl AnalyzeArgs {
    /// Parse args from a Rust file.
    ///
    /// The args are posix shell arguments (see [`shlex`]) on lines starting with `//! `.
    /// Since all args are optional, all args start with `--`,
    /// so only lines starting with `--` are considered,
    /// and other lines can contain normal comments.
    pub fn parse_from_file(rs_path: &Path) -> Self {
        let file_contents = fs::read_to_string(rs_path).unwrap();
        let args = file_contents
            .split('\n')
            .filter_map(|line| line.trim().strip_prefix("//! "))
            .filter(|args| args.starts_with("--"))
            .flat_map(|args| {
                shlex::split(args).unwrap_or_else(|| {
                    panic!("failed to split directive line as a posix shell line")
                })
            });
        // clap expects an argv0
        let args = ["c2rust-analyze-test".into()].into_iter().chain(args);
        match Self::try_parse_from(args) {
            Ok(args) => args,
            Err(e) => {
                let _ = e.print();
                // Since we're in a test, we need to panic, not exit (the whole process vs. this test thread)
                // and clap may exit without an error (e.x. `--help`),
                // but then the output won't be shown for the test.
                panic!("clap error parsing c2rust-analyze-test args");
            }
        }
    }
}

pub enum CrateType {
    Bin,
    Lib,
    Rlib,
    Dylib,
    Cdylib,
    Staticlib,
    ProcMacro,
}

impl From<CrateType> for &'static str {
    fn from(crate_type: CrateType) -> Self {
        use CrateType::*;
        match crate_type {
            Bin => "bin",
            Lib => "lib",
            Rlib => "rlib",
            Dylib => "dylib",
            Cdylib => "cdylib",
            Staticlib => "staticlib",
            ProcMacro => "proc-macro",
        }
    }
}

pub struct CrateOptions {
    pub edition: u16,
    pub crate_type: CrateType,
}

impl Default for CrateOptions {
    fn default() -> Self {
        CrateOptions {
            edition: 2021,
            crate_type: CrateType::Rlib,
        }
    }
}

impl Analyze {
    pub fn resolve() -> Self {
        let current_exe = env::current_exe().unwrap();
        let bin_deps_dir = current_exe.parent().unwrap();
        let bin_dir = bin_deps_dir.parent().unwrap();
        let path = bin_dir.join(env!("CARGO_PKG_NAME"));
        Self { path }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    fn run_with_(
        &self,
        rs_path: &Path,
        mut modify_cmd: impl FnMut(&mut Command),
        crate_options: Option<CrateOptions>,
    ) -> PathBuf {
        let dir = Path::new(env!("CARGO_MANIFEST_DIR"));
        let lib_dir = Path::new(env!("C2RUST_TARGET_LIB_DIR"));

        let rs_path = dir.join(rs_path); // allow relative paths, or override with an absolute path

        let crate_options = crate_options.unwrap_or_default();
        let args = AnalyzeArgs::parse_from_file(&rs_path);

        let output_path = {
            let mut file_name = rs_path.file_name().unwrap().to_owned();
            file_name.push(".analysis.txt");
            rs_path.with_file_name(file_name)
        };
        let output_stdout = File::create(&output_path).unwrap();
        let output_stderr = File::try_clone(&output_stdout).unwrap();

        let mut cmd = Command::new(&self.path);

        // Simulate an invocation by [`cargo_wrapper`].
        cmd.env("RUSTC_WRAPPER", &self.path); // Run as `rustc`, not `cargo`.
        cmd.env("CARGO_PRIMARY_PACKAGE", ""); // `cargo` sets this.
        cmd.env("CARGO_BIN_NAME", rs_path.file_name().unwrap()); // `cargo` sets this.
        cmd.arg("rustc"); // `cargo` passes this to `$RUSTC_WRAPPER`.

        if !args.catch_panics {
            cmd.env("C2RUST_ANALYZE_TEST_DONT_CATCH_PANIC", "1");
        }
        if args.use_manual_shims {
            cmd.env("C2RUST_ANALYZE_USE_MANUAL_SHIMS", "1");
        }
        if let Some(ref rewrite_paths) = args.rewrite_paths {
            cmd.env("C2RUST_ANALYZE_REWRITE_PATHS", rewrite_paths);
        }
        cmd.arg(&rs_path)
            .arg("-L")
            .arg(lib_dir)
            .args([
                "--crate-type",
                crate_options.crate_type.into(),
                "--edition",
                &crate_options.edition.to_string(),
            ])
            .stdout(output_stdout)
            .stderr(output_stderr);
        cmd.envs(args.env.iter().map(|EnvVar { var, value }| (var, value)));
        modify_cmd(&mut cmd);
        let status = cmd.status().unwrap();
        if !status.success() && !args.allow_crash {
            let message = format!(
                "c2rust-analyze failed with status {status}:\n> {cmd:?} > {output_path:?} 2>&1\n"
            );
            let output = fs::read_to_string(&output_path).unwrap();
            let max_len = 10000;
            if output.len() > max_len {
                let (output_start, output_end) = output.split_at(output.len() / 2);
                let output_start = &output_start[..max_len / 2];
                let output_end = &output_end[output_end.len() - max_len / 2..];
                panic!("\n{message}\n{output_start}\n\n...\n\n{output_end}\n{message}");
            } else {
                panic!("\n{message}\n{output}\n{message}");
            };
        }
        output_path
    }

    pub fn run_with(
        &self,
        rs_path: impl AsRef<Path>,
        modify_cmd: impl FnMut(&mut Command),
        crate_options: Option<CrateOptions>,
    ) -> PathBuf {
        self.run_with_(rs_path.as_ref(), modify_cmd, crate_options)
    }

    pub fn run(&self, rs_path: impl AsRef<Path>) -> PathBuf {
        self.run_with(rs_path, |_| {}, None)
    }
}

pub struct FileCheck {
    path: PathBuf,
}

impl FileCheck {
    pub fn resolve() -> Self {
        let path = env::var_os("FILECHECK")
            .map(PathBuf::from)
            .unwrap_or_else(|| {
                let llvm_config = find_llvm_config().expect("llvm-config not found");
                let output = Command::new(llvm_config)
                    .args(&["--bindir"])
                    .output()
                    .ok()
                    .filter(|output| output.status.success())
                    .expect("llvm-config error");
                let bin_dir =
                    PathBuf::from(String::from_utf8(output.stdout).unwrap().trim().to_owned());
                bin_dir.join("FileCheck")
            });
        Self { path }
    }

    fn run_(&self, path: &Path, input: &Path) {
        let mut cmd = Command::new(&self.path);
        let input_file = File::open(input).unwrap();
        cmd.arg(path).stdin(input_file);
        let status = cmd.status().unwrap();
        assert!(
            status.success(),
            "\nFileCheck failed with status {status}:\n> {cmd:?} > {input:?}\n",
        );
    }

    pub fn run(&self, path: impl AsRef<Path>, input: impl AsRef<Path>) {
        self.run_(path.as_ref(), input.as_ref())
    }
}

fn list_all_tests<C: FromIterator<String>>() -> C {
    let current_exe = env::current_exe().unwrap();
    let output = Command::new(&current_exe)
        .args(["--list"])
        .output()
        .unwrap();
    let stdout = std::str::from_utf8(&output.stdout).unwrap();
    stdout
        .lines()
        .filter_map(|s| s.strip_suffix(": test"))
        .map(|s| s.to_owned())
        .collect()
}

fn get_absolute_main_test_path(main_test_path: &Path) -> PathBuf {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    manifest_dir
        .join("tests")
        .join(main_test_path.file_name().unwrap())
}

pub fn test_dir_for(main_test_path: impl AsRef<Path>, absolute: bool) -> PathBuf {
    fn inner(main_test_path: &Path) -> PathBuf {
        main_test_path
            .parent()
            .unwrap()
            .join(main_test_path.file_stem().unwrap())
    }

    let main_test_path = main_test_path.as_ref();
    if absolute {
        inner(&get_absolute_main_test_path(main_test_path))
    } else {
        inner(main_test_path)
    }
}

fn list_all_test_filestems_in(dir: &Path) -> impl Iterator<Item = String> {
    dir.read_dir()
        .unwrap()
        .map(|dirent| dirent.unwrap())
        .map(|dirent| dirent.file_name().into_string().unwrap())
        .filter_map(|file_name| file_name.strip_suffix(".rs").map(|s| s.to_owned()))
}

pub fn check_for_missing_tests_for(main_test_path: impl AsRef<Path>) {
    let main_test_path = main_test_path.as_ref();
    let abs_test_dir = test_dir_for(main_test_path, true);
    let rel_test_dir = test_dir_for(main_test_path, false);
    let test_names = list_all_tests::<HashSet<_>>();
    let missing_tests = list_all_test_filestems_in(&abs_test_dir)
        .filter(|test_name| !test_names.contains(test_name))
        .collect::<Vec<_>>();
    for test_name in &missing_tests {
        let test_path = rel_test_dir.join(format!("{test_name}.rs"));
        warn!("missing a `#[test] fn {test_name}` for {test_path:?}");
    }
    assert!(missing_tests.is_empty(), "see missing tests above");
}
