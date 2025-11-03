use std::collections::{HashMap, HashSet};
use std::env::current_dir;
use std::fmt::{self, Debug, Display, Formatter};
use std::fs;
use std::ops::Index;
use std::path::Path;
use std::process::Command;
use std::str::from_utf8;

use c2rust_transpile::{ReplaceMode, TranspilerConfig, GENERATED_RUST_TOOLCHAIN};
use itertools::Itertools;
use tempfile::NamedTempFile;

fn config() -> TranspilerConfig {
    TranspilerConfig {
        dump_untyped_context: false,
        dump_typed_context: false,
        pretty_typed_context: false,
        dump_function_cfgs: false,
        json_function_cfgs: false,
        dump_cfg_liveness: false,
        dump_structures: false,
        verbose: false,
        debug_ast_exporter: false,
        incremental_relooper: true,
        fail_on_multiple: false,
        filter: None,
        debug_relooper_labels: false,
        prefix_function_names: None,
        translate_asm: true,
        use_c_loop_info: true,
        use_c_multiple_info: true,
        simplify_structures: true,
        panic_on_translator_failure: false,
        emit_modules: false,
        fail_on_error: true,
        replace_unsupported_decls: ReplaceMode::Extern,
        translate_valist: true,
        overwrite_existing: true,
        reduce_type_annotations: false,
        reorganize_definitions: false,
        enabled_warnings: Default::default(),
        emit_no_std: false,
        output_dir: None,
        translate_const_macros: Default::default(),
        translate_fn_macros: Default::default(),
        disable_refactoring: false,
        preserve_unused_functions: false,
        log_level: log::LevelFilter::Warn,
        emit_build_files: false,
        binaries: Vec::new(),
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Arch {
    X86_64,
    AArch64,
}

impl Arch {
    pub const fn name(&self) -> &'static str {
        use Arch::*;
        match *self {
            X86_64 => "x86_64",
            AArch64 => "aarch64",
        }
    }
}

impl Display for Arch {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(self.name())
    }
}

impl Debug for Arch {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(self.name())
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Os {
    Linux,
    MacOs,
}

impl Os {
    pub const fn name(&self) -> &'static str {
        use Os::*;
        match *self {
            Linux => "linux",
            MacOs => "macos",
        }
    }
}

impl Display for Os {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(self.name())
    }
}

impl Debug for Os {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(self.name())
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Target {
    X86_64UnknownLinuxGnu,
    X86_64AppleDarwin,
    AArch64UnknownLinuxGnu,
    AArch64AppleDarwin,
}

impl Target {
    pub const ALL: &[Self] = &[
        Self::X86_64UnknownLinuxGnu,
        Self::X86_64AppleDarwin,
        Self::AArch64UnknownLinuxGnu,
        Self::AArch64AppleDarwin,
    ];

    pub const fn rust_name(&self) -> &'static str {
        use Target::*;
        match *self {
            X86_64UnknownLinuxGnu => "x86_64-unknown-linux-gnu",
            X86_64AppleDarwin => "x86_64-apple-darwin",
            AArch64UnknownLinuxGnu => "aarch64-unknown-linux-gnu",
            AArch64AppleDarwin => "aarch64-apple-darwin",
        }
    }

    pub const fn clang_name(&self) -> &'static str {
        // These seem to be the same, but there may be some differences if we add more targets.
        self.rust_name()
    }

    pub const fn zig_name(&self) -> &'static str {
        use Target::*;
        match *self {
            X86_64UnknownLinuxGnu => "x86_64-linux-gnu",
            X86_64AppleDarwin => "x86_64-macos",
            AArch64UnknownLinuxGnu => "aarch64-linux-gnu",
            AArch64AppleDarwin => "aarch64-macos",
        }
    }

    pub const fn arch(&self) -> Arch {
        use Arch::*;
        use Target::*;
        match *self {
            X86_64UnknownLinuxGnu | X86_64AppleDarwin => X86_64,
            AArch64UnknownLinuxGnu | AArch64AppleDarwin => AArch64,
        }
    }

    pub const fn os(&self) -> Os {
        use Os::*;
        use Target::*;
        match *self {
            X86_64UnknownLinuxGnu | AArch64UnknownLinuxGnu => Linux,
            X86_64AppleDarwin | AArch64AppleDarwin => MacOs,
        }
    }
}

impl Display for Target {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(self.rust_name())
    }
}

impl Debug for Target {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(self.rust_name())
    }
}

impl From<(Arch, Os)> for Target {
    fn from((arch, os): (Arch, Os)) -> Self {
        use Arch::*;
        use Os::*;
        use Target::*;
        let target = match (arch, os) {
            (X86_64, Linux) => X86_64UnknownLinuxGnu,
            (X86_64, MacOs) => X86_64AppleDarwin,
            (AArch64, Linux) => AArch64UnknownLinuxGnu,
            (AArch64, MacOs) => AArch64AppleDarwin,
        };
        assert_eq!(target.arch(), arch);
        assert_eq!(target.os(), os);
        target
    }
}

impl TryFrom<&str> for Target {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        for &target in Self::ALL {
            if target.rust_name() == value {
                return Ok(target);
            }
        }
        Err(())
    }
}

fn parse_cc1_commands(stderr: &str) -> Vec<Vec<String>> {
    stderr
        .split('\n')
        .filter_map(|line| {
            let cc1 = "\"-cc1\"";
            let i = line.find(cc1)?;
            let args = &line[i..];
            let args = shell_words::split(args).unwrap();
            Some(args)
        })
        .collect()
}

fn cc1_command_to_driver_command(mut cc1_args: Vec<String>) -> Vec<String> {
    let mut driver_args = Vec::new();

    #[derive(PartialEq, Eq)]
    enum Next {
        Other,
        ResourceDir,
        ISystem,
        Define,
    }

    let mut next = Next::Other;

    for arg in cc1_args.drain(..) {
        if next != Next::Other {
            next = Next::Other;
            driver_args.push(arg);
            continue;
        }
        match arg.as_str() {
            "-nostdsysteminc" => {
                driver_args.push("-nostdinc".into());
            }
            "-nobuiltininc" => {
                driver_args.push(arg);
            }
            "-resource-dir" => {
                next = Next::ResourceDir;
                driver_args.push(arg);
            }
            "-isystem" => {
                next = Next::ISystem;
                driver_args.push(arg);
            }
            "-D" => {
                next = Next::Define;
                driver_args.push(arg);
            }
            _ => {}
        }
    }

    driver_args
}

struct TargetArgs {
    target: Target,

    /// `\0`-separated.
    args: String,
}

impl TargetArgs {
    /// Use `zig cc` to determine the args needed to cross-compile to `target`.
    /// `zig cc` is used because it can be otherwise quite difficult
    /// to get the right headers for other OSes.
    pub fn find(target: Target) -> Self {
        let empty_c = NamedTempFile::new().unwrap();
        let empty_o = NamedTempFile::new().unwrap();
        let mut cmd = Command::new("zig");
        cmd.args([
            "cc",
            "-target",
            target.zig_name(), //
            // `-###` will print the cc1 commands.
            "-###",
            // `-x c` is needed to tell `zig cc` this is a C file, since it isn't a `*.c`.
            "-x",
            "c",
            // Don't use `-fsyntax-only`.  `zig cc` doesn't handle it correctly.
            // Don't use `-o /dev/null`.  `zig cc` doesn't handle it correctly.
            "-c",
            "-o",
        ])
        .args([empty_o.path(), empty_c.path()]);
        let output = cmd.output().unwrap();
        let stderr = from_utf8(&output.stderr).unwrap();
        if !output.status.success() {
            eprintln!("> {cmd:?}");
            eprintln!("{stderr}")
        }
        assert!(output.status.success());
        let mut cc1_cmds = parse_cc1_commands(stderr);
        // There should only be one cc1 command.
        assert!(cc1_cmds.len() == 1);
        let cc1_cmd = cc1_cmds.pop().unwrap();
        let driver_cmd = cc1_command_to_driver_command(cc1_cmd);
        let args = driver_cmd.join("\0");
        Self { target, args }
    }

    pub const fn target(&self) -> Target {
        self.target
    }

    /// The `zig cc` args needed to cross-compile to [`Self::target`].
    pub const fn zig_cc_args(&self) -> [&str; 2] {
        ["-target", self.target().zig_name()]
    }

    pub fn clang_args_iter(&self) -> impl Iterator<Item = &str> {
        [
            "-target",
            self.target.clang_name(),
            // Undefine `__BLOCKS__` because `c2rust-ast-exporter` doesn't handle them at all.
            // macOS headers use `__BLOCKS__` and `^` block pointers.
            "-U",
            "__BLOCKS__",
        ]
        .into_iter()
        .chain(self.args.split('\0'))
    }

    /// The `clang` args needed to cross-compile to [`Self::target`].
    pub fn clang_args(&self) -> Vec<&str> {
        self.clang_args_iter().collect()
    }
}

impl Display for TargetArgs {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let args = self.clang_args_iter().join(" ");
        write!(f, "{args}")
    }
}

struct AllTargetArgs {
    all: Vec<TargetArgs>,
}

impl AllTargetArgs {
    pub fn find() -> Self {
        let all = Target::ALL
            .into_iter()
            .copied()
            .map(TargetArgs::find)
            .collect();
        Self { all }
    }

    pub fn get(&self, target: Target) -> &TargetArgs {
        self.all
            .iter()
            .find(|args| args.target() == target)
            .unwrap()
    }
}

impl Index<Target> for AllTargetArgs {
    type Output = TargetArgs;

    fn index(&self, index: Target) -> &Self::Output {
        self.get(index)
    }
}

impl TargetArgs {
    /// `platform` can be any platform-specific string.
    /// It could be the `target_arch`, `target_os`, some combination, or something else.
    pub fn transpile(&self, c_path: &Path, platform: Option<&'static str>) {
        let cwd = current_dir().unwrap();
        let c_path = c_path.strip_prefix(&cwd).unwrap();
        // The crate name can't have `.`s in it, so use the file stem.
        // This is also why we set it explicitly with `--crate-name`,
        // as once we add `.{platform}`, the crate name derived from
        // the file name won't be valid anymore.
        let crate_name = c_path.file_stem().unwrap().to_str().unwrap();
        let original_rs_path = c_path.with_extension("rs");
        // We need to move the `.rs` file to a platform-specific name
        // so that they don't overwrite each other.
        let rs_path = match platform {
            None => original_rs_path.clone(),
            Some(platform) => original_rs_path.with_extension(format!("{platform}.rs")),
        };

        {
            let c_path = c_path.display();
            let rs_path = rs_path.display();
            let target = self.target().rust_name();
            println!("transpiling {c_path} to {rs_path} for --target {target}");
        }

        let o_path = NamedTempFile::new().unwrap();
        let status = Command::new("zig")
            .arg("cc")
            .args(self.zig_cc_args())
            .args([
                "-w", // Disable warnings.
                "-c", "-o", // `zig cc` doesn't work with `-fsyntax-only` or `-o /dev/null`.
            ])
            .args([o_path.path(), c_path])
            .status()
            .unwrap();
        assert!(status.success());

        let mut extra_args = self.clang_args();
        extra_args.push("-w"); // Disable warnings.

        let (_temp_dir, temp_path) =
            c2rust_transpile::create_temp_compile_commands(&[c_path.to_owned()]);
        c2rust_transpile::transpile(config(), &temp_path, &extra_args);

        if platform.is_some() {
            fs::rename(&original_rs_path, &rs_path).unwrap();
        }

        let edition = "2021";

        let status = Command::new("rustfmt")
            .args(["--edition", edition])
            .arg(&rs_path)
            .status();
        assert!(status.unwrap().success());

        let rs = fs::read_to_string(&rs_path).unwrap();
        let debug_expr = format!("cat {}", rs_path.display());

        let snapshot_name = match platform {
            None => "transpile".into(),
            Some(platform) => format!("transpile-{platform}"),
        };
        insta::assert_snapshot!(snapshot_name, &rs, &debug_expr);

        // Using rustc itself to build snapshots that reference libc is difficult because we don't know
        // the appropriate --extern libc=/path/to/liblibc-XXXXXXXXXXXXXXXX.rlib to pass. Skip for now,
        // as we've already compared the literal text.
        if rs.contains("libc::") {
            eprintln!(
                "warning: skipping compiling {} with rustc since it depends on libc",
                rs_path.display()
            );
            return;
        }

        // Don't need to worry about platform clashes here, as this is immediately deleted.
        let rlib_path = format!("lib{crate_name}.rlib");
        let status = Command::new("rustc")
            .args([
                &format!("+{GENERATED_RUST_TOOLCHAIN}"),
                "--crate-type",
                "lib",
                "--edition",
                edition,
                "--target",
                self.target().rust_name(),
                "--crate-name",
                crate_name,
                "-o",
                &rlib_path,
                "-Awarnings", // Disable warnings.
            ])
            .arg(&rs_path)
            .status();
        assert!(status.unwrap().success());
        fs::remove_file(&rlib_path).unwrap();
    }
}

impl AllTargetArgs {
    pub fn check_if_targets_are_installed(&self) {
        let output = Command::new("rustup")
            .args([
                &format!("+{GENERATED_RUST_TOOLCHAIN}"),
                "target",
                "list",
                "--installed",
            ])
            .output()
            .unwrap();
        assert!(output.status.success());
        let installed_targets = from_utf8(&output.stdout)
            .unwrap()
            .trim()
            .split_whitespace()
            .collect::<HashSet<_>>();
        let uninstalled_targets = self
            .all
            .iter()
            .map(|args| args.target().rust_name())
            .filter(|target| !installed_targets.contains(target))
            .join(" ");
        if !uninstalled_targets.is_empty() {
            panic!("not all targets installed, run:\nrustup +{GENERATED_RUST_TOOLCHAIN} target add {uninstalled_targets}\n");
        }
    }

    pub fn transpile(&self, c_path: &Path, get_platform: impl Fn(Target) -> Option<&'static str>) {
        let mut platforms = HashMap::<Option<&'static str>, Vec<&TargetArgs>>::new();
        for target in self.all.iter() {
            let platform = get_platform(target.target());
            platforms.entry(platform).or_default().push(target);
        }
        for (platform, targets) in platforms {
            for target_args in targets {
                target_args.transpile(c_path, platform);
            }
        }
    }
}

#[test]
fn transpile_all() {
    let targets = AllTargetArgs::find();
    targets.check_if_targets_are_installed();

    insta::glob!("snapshots/*.c", |path| targets.transpile(path, |_| None));
    insta::glob!("snapshots/os-specific/*.c", |path| targets
        .transpile(path, |target| Some(target.os().name())));
    insta::glob!("snapshots/arch-specific/*.c", |path| targets
        .transpile(path, |target| Some(target.arch().name())));
}
