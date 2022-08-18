#![feature(rustc_private)]
extern crate rustc_ast;
extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_mir_transform;
extern crate rustc_session;
extern crate rustc_span;

mod arg;
mod callbacks;
mod hooks;
mod instrument;
mod into_operand;
mod mir_utils;
mod point;
mod runtime_conversions;
mod util;

use crate::callbacks::{MirTransformCallbacks, INSTRUMENTER};

use std::{
    cmp::Ordering,
    env,
    ffi::{OsStr, OsString},
    io::{Read, Seek, Write},
    iter,
    path::{Path, PathBuf},
    process::{self, Command, ExitStatus},
};

use fs_err::OpenOptions;
use rustc_driver::{RunCompiler, TimePassesCallbacks};
use rustc_session::config::CrateType;
use std::io::SeekFrom;

use anyhow::{anyhow, bail, ensure, Context};
use camino::Utf8Path;
use cargo_metadata::MetadataCommand;
use clap::Parser;

/// Instrument memory accesses for dynamic analysis.
#[derive(Debug, Parser)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Path to the metadata output file storing instrumentation locations.
    #[clap(long, value_parser)]
    metadata: PathBuf,

    /// Path to the `c2rust-analysis-rt` crate if you want to use a local version of it (vs. the crates.io one).
    /// This is not used unless `--set-runtime` is also passed.
    #[clap(long, value_parser)]
    runtime_path: Option<PathBuf>,

    /// Add the runtime as an optional dependency to the instrumented crate using `cargo add`.
    #[clap(long)]
    set_runtime: bool,

    /// `cargo` args.
    cargo_args: Vec<OsString>,
}

fn exit_with_status(status: ExitStatus) {
    process::exit(status.code().unwrap_or(1))
}

/// Resolve the current `rustc` sysroot using `rustc --print sysroot`.
///
/// Normally, `rustc` looks up the sysroot by the location of its own binary.
/// This works because the `rustc` on `$PATH` is actually `rustup`,
/// and `rustup` invokes the real `rustc`, which is in a location relative to the sysroot.
/// As we invoke `rustc_driver` directly here, we are `rustc`,
/// and thus we have to explicitly specify the sysroot that the real `rustc` would normally use.
///
/// Note that the sysroot contains the toolchain and host target name,
/// but this has no effect on cross-compiling.
/// Every toolchain's `rustc` is able to itself cross-compile.
/// I'm not sure why the host target needs to be in the sysroot directory name, but it is.
///
/// Also note that this sysroot lookup should be done at runtime,
/// not at compile-time in the `build.rs`,
/// as the toolchain locations could be different
/// from where this binary was compiled and where it is running
/// (it could be on a different machine with a different `$RUSTUP_HOME`).
///
/// TODO(kkysen) deduplicate this with `rustc_private_link::SysRoot::resolve`
fn resolve_sysroot() -> anyhow::Result<PathBuf> {
    let rustc = env::var_os("RUSTC").unwrap_or_else(|| "rustc".into());
    let output = Command::new(rustc)
        .args(&["--print", "sysroot"])
        .output()
        .context("could not invoke `rustc` to find rust sysroot")?;
    // trim, but `str::trim` doesn't exist on `[u8]`
    let path = output
        .stdout
        .as_slice()
        .split(|c| c.is_ascii_whitespace())
        .next()
        .unwrap_or_default();
    let path = if cfg!(unix) {
        use std::os::unix::ffi::OsStrExt;

        OsStr::from_bytes(path)
    } else {
        // Windows is hard, so just require UTF-8
        let path = std::str::from_utf8(path).context("`rustc --print sysroot` is not UTF-8")?;
        OsStr::new(path)
    };
    let path = Path::new(path).to_owned();
    // `rustc` reports a million errors if the sysroot is wrong, so try to check first.
    ensure!(
        path.is_dir(),
        "invalid sysroot (not a dir): {}",
        path.display()
    );
    Ok(path)
}

/// Insert feature flags as the first arguments following the `cargo` subcommand.
///
/// We can't insert them at the end because they could come after a `--` and thus be ignored.
/// And we can't insert them at the beginning before the `cargo` subcommand argument,
/// as `--features` is an argument of the subcommands, not `cargo` itself.
///
/// If there are no arguments, we don't insert the feature flags, as:
/// * it would panic on splicing/insertion
/// * we don't want to add the feature flags anyways, as `cargo` without arguments is already an error
/// * we don't want to obfuscate that error with an error about unexpected feature flags
fn add_feature(cargo_args: &mut Vec<OsString>, features: &[&str]) {
    let insertion_point = 1;
    if cargo_args.len() >= insertion_point {
        cargo_args.splice(
            insertion_point..insertion_point,
            iter::once(&"--features").chain(features).map(|s| s.into()),
        );
    }
}

struct Cargo {
    path: PathBuf,
}

impl Cargo {
    pub fn new() -> Self {
        let path = env::var_os("CARGO")
            .unwrap_or_else(|| "cargo".into())
            .into();
        Self { path }
    }

    pub fn metadata(&self) -> MetadataCommand {
        let mut cmd = MetadataCommand::new();
        cmd.cargo_path(&self.path);
        cmd
    }

    pub fn command(&self) -> Command {
        let mut cmd = Command::new(&self.path);
        cmd.env("CARGO_TARGET_DIR", "instrument.target");
        cmd
    }

    pub fn run(&self, f: impl FnOnce(&mut Command)) -> anyhow::Result<()> {
        let mut cmd = self.command();
        f(&mut cmd);
        let status = cmd.status()?;
        if !status.success() {
            eprintln!("error running: {cmd:?}");
            exit_with_status(status);
        }
        Ok(())
    }
}

const RUSTC_WRAPPER_VAR: &str = "RUSTC_WRAPPER";
const RUST_SYSROOT_VAR: &str = "RUST_SYSROOT";
const METADATA_VAR: &str = "C2RUST_INSTRUMENT_METADATA_PATH";

/// Read a [`PathBuf`] from the [`mod@env`]ironment that should've been set by the [`cargo_wrapper`].
fn env_path_from_wrapper(var: &str) -> anyhow::Result<PathBuf> {
    let path = env::var_os(var)
        .ok_or_else(|| anyhow!("the `cargo` wrapper should've `${var}` for the `rustc` wrapper"))?;
    Ok(path.into())
}

/// Check if the current [`rustc_wrapper`] invocation is for the primary `cargo` package,
/// as determined by `$CARGO_PRIMARY_PACKAGE`.
fn is_primary_package() -> bool {
    env::var("CARGO_PRIMARY_PACKAGE").is_ok()
}

/// Check if the current [`rustc_wrapper`] invocation is a binary crate,
/// i.e., if `--crate-type bin` was specified.
///
/// This uses the [`rustc_driver`] and [`rustc_session`] APIs
/// to check this exactly as `rustc` would.
fn is_bin_crate(at_args: &[String]) -> anyhow::Result<bool> {
    let args = rustc_driver::args::arg_expand_all(at_args);
    let matches = rustc_driver::handle_options(&args)
        .ok_or_else(|| anyhow!("failed to parse `rustc` args"))?;
    let session_options = rustc_session::config::build_session_options(&matches);
    let is_bin = session_options.crate_types.contains(&CrateType::Executable);
    Ok(is_bin)
}

/// Read the name of the current binary crate being compiled, if it is a binary crate ([`is_bin_crate`]).
///
/// Note that despite setting `--crate-type bin` and [`is_bin_crate`] being true,
/// there is no name set for build scripts.
/// That's how we can detect them.
fn bin_crate_name() -> Option<PathBuf> {
    env::var_os("CARGO_BIN_NAME").map(PathBuf::from)
}

/// Detect if the current [`rustc_wrapper`] is for compiling a build script.
///
/// `c2rust-analysis-rt` is not yet built for the build script,
/// so trying to compile it will fail.
/// Plus, we don't need to and don't want to instrument the build script.
///
/// We check if it is a build script by checking if it's a `--crate-type bin`
/// and yet has no `$CARGO_BIN_NAME`, which is set for normal binary crates.
///
/// Another solution (that `miri` uses) is to always specify the `--target`,
/// even if it's the host target.  Then `cargo` thinks it's cross-compiling,
/// and always forwards `--target` to `rustc` for native compilations,
/// but for host compilations like build scripts and proc-macros,
/// it doesn't specify `--target`.
///
/// This would work more robustly if we were also instrumenting dependencies,
/// as our currently solution would no longer work, but we aren't.
///
/// On the other hand, the `--target` solution has a drawback
/// in that there are many ways to specify the target:
/// * `--target`
/// * `$CARGO_BUILD_TARGET`
/// * `targets` in `rust-toolchain.toml`
/// * `targets` in `.cargo/config.toml`
/// * and maybe some other places as well
/// All the resolution is in `cargo`, but we have to decide
/// if we're going to supply `--target $HOST` before `cargo` runs,
/// so we have to check all of those places ourselves to make sure
/// that we're not overriding the true cross-compilation the user wants.
///
/// Compared to the current solution, this seems harder,
/// so we're sticking with the current solution for now as long as it works.
fn is_build_script(at_args: &[String]) -> anyhow::Result<bool> {
    Ok(bin_crate_name().is_none() && is_bin_crate(at_args)?)
}

/// Run as a `rustc` wrapper (a la `$RUSTC_WRAPPER`/[`RUSTC_WRAPPER_VAR`]).
fn rustc_wrapper() -> anyhow::Result<()> {
    let mut at_args = env::args().skip(1).collect::<Vec<_>>();
    // We also want to avoid proc-macro crates,
    // but those must be separate crates, so we should be okay.
    let should_instrument = is_primary_package() && !is_build_script(&at_args)?;
    let sysroot = env_path_from_wrapper(RUST_SYSROOT_VAR)?;
    let sysroot: &Utf8Path = sysroot.as_path().try_into()?;
    at_args.extend(["--sysroot".into(), sysroot.as_str().into()]);
    let result = if should_instrument {
        RunCompiler::new(&at_args, &mut MirTransformCallbacks).run()
    } else {
        // Always use the dynamically linked `librustc_driver-{hash}.so`,
        // as it is guaranteed to be the same version as the instrumented version.
        // Furthermore, we can't accidentally load the wrong `librustc_driver-{hash}.so`,
        // as it contains its hash.
        // This also avoids an extra `rustc` (and potentially `rustup` `rustc`) invocation.
        RunCompiler::new(&at_args, &mut TimePassesCallbacks::default()).run()
    };
    // `ErrorReported` means the error has already been reported to the user,
    // so we just have to fail/exit with a failing exit code.
    // There is no `impl Error for ErrorReported`.
    result.map_err(|_| anyhow!("`rustc` failed"))?;
    if should_instrument {
        INSTRUMENTER.finalize(&env_path_from_wrapper(METADATA_VAR)?)?;
    }
    Ok(())
}

/// Set `$RUST_TOOLCHAIN` to the toolchain channel specified in `rust-toolchain.toml`.
/// This ensures that we use a toolchain compatible with the `rustc` private crates that we linked to.
fn set_rust_toolchain() -> anyhow::Result<()> {
    let toml = include_str!("../rust-toolchain.toml");
    // Couldn't find an `include_toml!` macro to do this at compile time.
    let doc = toml.parse::<toml_edit::Document>()?;
    let channel = doc["toolchain"]["channel"].as_str();
    if let Some(toolchain) = channel {
        env::set_var("RUSTUP_TOOLCHAIN", toolchain);
    }
    Ok(())
}

/// Run as a `cargo` wrapper/plugin, the default invocation.
fn cargo_wrapper(rustc_wrapper: &Path) -> anyhow::Result<()> {
    let Args {
        metadata: metadata_path,
        runtime_path,
        set_runtime,
        mut cargo_args,
    } = Args::parse();

    set_rust_toolchain()?;

    // Resolve the sysroot once in the [`cargo_wrapper`]
    // so that we don't need all of the [`rustc_wrapper`]s to have to do it.
    let sysroot = resolve_sysroot()?;

    let cargo = Cargo::new();

    let cargo_metadata = cargo.metadata().exec()?;
    let root_package = cargo_metadata
        .root_package()
        .ok_or_else(|| anyhow!("no root package found by `cargo`"))?;

    if set_runtime {
        cargo.run(|cmd| {
            cmd.args(&["add", "--optional", "c2rust-analysis-rt"]);
            if let Some(runtime) = runtime_path {
                // Since it's a local path, we don't need the internet,
                // and running it offline saves a slow index sync.
                cmd.args(&["--offline", "--path"]).arg(runtime);
            }
        })?;
    }

    // Create the metadata file for the [`rustc_wrapper`]s to append to.
    // Note that we can't truncate it because in the case that no inputs have changed,
    // `cargo` won't recompile, so a new metadata file won't be regenerated.
    // We should keep the old one in that case.
    let mut metadata_file = OpenOptions::new()
        .create(true)
        .truncate(false)
        .open(&metadata_path)?;

    let old_len = metadata_file.metadata()?.len();

    cargo.run(|cmd| {
        // Enable the runtime dependency.
        add_feature(&mut cargo_args, &["c2rust-analysis-rt"]);
        cmd.args(cargo_args)
            .env(RUSTC_WRAPPER_VAR, rustc_wrapper)
            .env(RUST_SYSROOT_VAR, &sysroot)
            .env(METADATA_VAR, &metadata_path);
    })?;

    if old_len == 0 {
        // If the `old_len` is 0, then the metadata file is fresh.
        // There was no old metadata that we need to delete now.
    } else {
        let new_len = metadata_file.metadata()?.len();
        use Ordering::*;
        match new_len.cmp(&old_len) {
            Less => bail!(
                "metadata should never shrink: {old_len} to {new_len} bytes: {}",
                metadata_path.display()
            ),
            Equal => {
                // The metadata was never updated.
                // This means no inputs changed and `cargo` never recompiled,
                // so that old metadata is still valid.
            }
            Greater => {
                // New metadata was appended.
                // We now need to delete the old metadata.
                // Simply seek to the new metadata, read it into a buffer,
                // seek back to the beginning, write it, and then truncate to the correct length.
                let len = new_len - old_len;
                metadata_file.seek(SeekFrom::Start(old_len))?;
                let mut buf = Vec::with_capacity(len.try_into().unwrap());
                metadata_file.read_to_end(&mut buf)?;
                metadata_file.rewind()?;
                metadata_file.write_all(&buf)?;
                metadata_file.set_len(len)?;
            }
        }
    }

    Ok(())
}

fn main() -> anyhow::Result<()> {
    env_logger::init();

    let own_exe = env::current_exe()?;

    let wrapping_rustc = env::var_os(RUSTC_WRAPPER_VAR).as_deref() == Some(own_exe.as_os_str());
    if wrapping_rustc {
        rustc_wrapper()
    } else {
        cargo_wrapper(&own_exe)
    }
}
