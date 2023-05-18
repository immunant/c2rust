use std::{
    collections::HashSet,
    env,
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
    dont_catch_panic: bool,
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

impl Analyze {
    pub fn resolve() -> Self {
        let current_exe = env::current_exe().unwrap();
        let bin_deps_dir = current_exe.parent().unwrap();
        let bin_dir = bin_deps_dir.parent().unwrap();
        let path = bin_dir.join(env!("CARGO_PKG_NAME"));
        Self {
            path,
            dont_catch_panic: false,
        }
    }

    pub fn dont_catch_panic(self) -> Self {
        Self {
            dont_catch_panic: true,
            ..self
        }
    }

    fn run_with_(&self, rs_path: &Path, mut modify_cmd: impl FnMut(&mut Command)) -> PathBuf {
        let dir = Path::new(env!("CARGO_MANIFEST_DIR"));
        let lib_dir = Path::new(env!("C2RUST_TARGET_LIB_DIR"));

        let rs_path = dir.join(rs_path); // allow relative paths, or override with an absolute path

        let args = AnalyzeArgs::parse_from_file(&rs_path);

        let output_path = {
            let mut file_name = rs_path.file_name().unwrap().to_owned();
            file_name.push(".analysis.txt");
            rs_path.with_file_name(file_name)
        };
        let output_stdout = File::create(&output_path).unwrap();
        let output_stderr = File::try_clone(&output_stdout).unwrap();

        let mut cmd = Command::new(&self.path);
        if self.dont_catch_panic {
            cmd.env("C2RUST_ANALYZE_TEST_DONT_CATCH_PANIC", "1");
        }
        cmd.arg(&rs_path)
            .arg("-L")
            .arg(lib_dir)
            .arg("--crate-type")
            .arg("rlib")
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
            panic!("\n{message}\n{output}\n{message}");
        }
        output_path
    }

    pub fn run_with(
        &self,
        rs_path: impl AsRef<Path>,
        modify_cmd: impl FnMut(&mut Command),
    ) -> PathBuf {
        self.run_with_(rs_path.as_ref(), modify_cmd)
    }

    pub fn run(&self, rs_path: impl AsRef<Path>) -> PathBuf {
        self.run_with(rs_path, |_| {})
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
        eprintln!("missing a `#[test] fn {test_name}` for {test_path:?}");
    }
    assert!(missing_tests.is_empty(), "see missing tests above");
}
