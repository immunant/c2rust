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
    borrow::Borrow,
    borrow::Cow,
    env,
    ffi::{OsStr, OsString},
    iter,
    path::{Path, PathBuf},
    process::{self, Command, ExitStatus},
};

use rustc_driver::{RunCompiler, TimePassesCallbacks};
use rustc_session::config::CrateType;

use anyhow::{anyhow, ensure, Context};
use clap::{AppSettings, Parser};
use tempfile::NamedTempFile;

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

    /// Set `$RUSTFLAGS` for the instrumented `cargo`.
    ///
    /// This allows setting `$RUSTFLAGS` for the inner `cargo` when `c2rust-instrument` is invoked via `cargo run`, for example.
    /// If `$RUSTFLAGS` is already set, these `--rustflags` are appended with a space.
    //
    // The reason this exists is twofold:
    //
    // 1. It would be convenient if `cargo` itself had such a `--rustflags` argument,
    //    so at least we can recreate it here ourselves.
    //
    // 2. If this binary is invoked by something like `cargo run --bin c2rust-instrument`,
    //    then it's impossible to set `$RUSTFLAGS` only for the inner `cargo` that you want to add them to
    //    without also adding them to the outer `cargo run` `cargo`.
    //    `--rustflags` lets you do that easily.
    #[clap(long)]
    rustflags: Option<OsString>,

    /// `cargo` args.
    cargo_args: Vec<OsString>,
}

/// `cargo` args that we intercept.
#[derive(Debug, Parser)]
#[clap(setting = AppSettings::IgnoreErrors)]
struct InterceptedCargoArgs {
    #[clap(long, value_parser)]
    manifest_path: Option<PathBuf>,

    /// Need this so `--` is allowed.
    /// Not actually used.
    extra_args: Vec<OsString>,
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
/// TODO(kkysen) deduplicate this with `c2rust_build_paths::SysRoot::resolve`
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
    #[cfg(unix)]
    let path = {
        use std::os::unix::ffi::OsStrExt;

        OsStr::from_bytes(path)
    };
    #[cfg(not(unix))]
    let path = {
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

    pub fn command(&self) -> Command {
        Command::new(&self.path)
    }

    pub fn run(&self, f: impl FnOnce(&mut Command) -> anyhow::Result<()>) -> anyhow::Result<()> {
        let mut cmd = self.command();
        f(&mut cmd)?;
        let status = cmd.status()?;
        if !status.success() {
            eprintln!("error ({status}) running: {cmd:?}");
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
    let sysroot = sysroot
        .as_path()
        .to_str()
        .ok_or_else(|| anyhow!("sysroot path is not UTF-8: {}", sysroot.display()))?;
    at_args.extend(["--sysroot".into(), sysroot.into()]);
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

/// A new metadata file, which is a temporary [`NamedTempFile`],
/// and the [`Path`] it should end up as.
pub struct MetadataFile {
    /// The original and intended metadata path.
    path: PathBuf,

    /// The new, temporary metadata file.
    file: NamedTempFile,
}

impl MetadataFile {
    /// The old, original, intended, and final [`Path`] for the [`Metadata`].
    ///
    /// This is the location that existing [`Metadata`] may be at,
    /// and where the [`Metadata`] will end after this program exits.
    ///
    /// [`Metadata`]: c2rust_analysis_rt::metadata::Metadata
    pub fn final_path(&self) -> &Path {
        &self.path
    }

    /// The new, temporary [`Path`] for the [`Metadata`].
    ///
    /// This is the location of the new [`Metadata`] created
    /// during the `cargo` invocation in [`cargo_wrapper`]
    /// and later used (appended to) inside of the [`rustc_wrapper`]s.
    /// It will later be moved back to [`Self::final_path`] if it is valid (i.e., not empty).
    ///
    /// [`Metadata`]: c2rust_analysis_rt::metadata::Metadata
    pub fn temp_path(&self) -> &Path {
        self.file.path()
    }

    /// Create a new [`MetadataFile`] that consists of
    /// a temporary ([`NamedTempFile`]) metadata file for new [`Metadata`].
    ///
    /// This also creates the directory `path` is in if it does not already exist.
    ///
    /// Also, see [`Self::final_path`] and [`Self::temp_path`]
    /// for an explanation of the locations and uses of the [`Path`]s.
    ///
    /// [`Metadata`]: c2rust_analysis_rt::metadata::Metadata
    pub fn new(path: PathBuf) -> anyhow::Result<Self> {
        let metadata_file_name = path
            .file_name()
            .ok_or_else(|| anyhow!("--metadata has no file name: {}", path.display()))?;
        let metadata_dir = path.parent();

        if let Some(metadata_dir) = metadata_dir {
            fs_err::create_dir_all(metadata_dir)?;
        }

        let metadata_dir = metadata_dir.unwrap_or_else(|| Path::new("."));

        let prefix = {
            let mut prefix = metadata_file_name.to_owned();
            prefix.push(".");
            prefix
        };
        let file = tempfile::Builder::new()
            .prefix(&prefix)
            .suffix(".new")
            .tempfile_in(metadata_dir)
            .context("create new (temp) metadata file")?;
        Ok(Self { path, file })
    }

    /// If the metadata file is not empty,
    /// then it is a valid replacement for the (possibly) existing metadata file,
    /// so move it into place.
    /// Otherwise, [`NamedTempFile::close`] the file, removing it.
    ///
    /// Note that there is no `impl `[`Drop`]` for `[`MetadataFile`] for a few reasons:
    /// * A [`Drop`] `impl` prevents us from moving out of [`Self`],
    ///   which is necessary to call [`NamedTempFile::close`], which takes `self`.
    /// * A [`Drop`] `impl` would provide a safeguard for early `return`s (like through `?`) and panics,
    ///   but in that case, that's not quite what we want to do,
    ///   since that then may move an incomplete temporary [`MetadataFile`] into place,
    ///   overwriting the old but valid [`MetadataFile`].
    ///   Instead, by keeping the automatic [`Drop`] `impl`,
    ///   it always calls [`NamedTempFile`]'s inner [`TempPath::drop`](tempfile::TempPath::drop),
    ///   which deletes the temporary [`MetadataFile`],
    ///   and which I believe is the safer behavior here.
    ///
    /// TODO(kkysen)
    /// There is a bug in the current implementation,
    /// very related to [#632](https://github.com/immunant/c2rust/issues/632).
    ///
    /// By disabling incremental compilation in [`MirTransformCallbacks`]
    /// instead of `cargo clean`ing the primary package,
    /// which was done in [#626](https://github.com/immunant/c2rust/pull/626)
    /// to fix [#624](https://github.com/immunant/c2rust/pull/624)
    /// and [#625](https://github.com/immunant/c2rust/pull/625),
    /// we now create complete [`Metadata`]s in each [`rustc_wrapper`] call,
    /// and thus largely avoid the issue of managing incremental updates to the [`Metadata`].
    ///
    /// However, although the `rustc`s and [`rustc_wrapper`]s are atomic in this sense,
    /// `cargo` is not.  That is, if none of the inputs changed,
    /// `cargo` can skip calls to `rustc`/`$RUSTC_WRAPPER`.
    /// We handle this general case in [`MetadataFile`] where all of the instrument crate targets
    /// are either all rebuilt or none of them are rebuilt,
    /// but if, for example, only a binary target needs rebuilding,
    /// `cargo` will rebuild only that binary target
    /// and not rebuild the library target again.
    /// Thus, we'll overwrite the previous metadata file containing the library and binary targets
    /// with a new metadata file containing only the binary target.
    ///
    /// The solution that I see to this, also proposed and discussed in
    /// [#632](https://github.com/immunant/c2rust/issues/632) for similar reasons,
    /// is to store a separate metadata file per target, i.e. per `rustc` call,
    /// and thus lower [`MetadataFile`] into [`rustc_wrapper`] instead of [`cargo_wrapper`].
    /// This would also fix any issues in [#632](https://github.com/immunant/c2rust/issues/632).
    ///
    /// [`Metadata`]: c2rust_analysis_rt::metadata::Metadata
    pub fn close(self) -> anyhow::Result<()> {
        if self.file.as_file().metadata()?.len() > 0 {
            fs_err::rename(self.file.path(), &self.path)?;
        } else {
            self.file.close()?;
        }
        Ok(())
    }
}

trait OsStringJoin {
    fn join(&mut self, sep: &OsStr) -> OsString;
}

impl<I, T> OsStringJoin for I
where
    I: Iterator<Item = T>,
    T: Borrow<OsStr>,
{
    fn join(&mut self, sep: &OsStr) -> OsString {
        match self.next() {
            None => OsString::new(),
            Some(first_elt) => {
                // estimate lower bound of capacity needed
                let (lower, _) = self.size_hint();
                let mut result = OsString::with_capacity(sep.len() * lower);
                result.push(first_elt.borrow());
                self.for_each(|elt| {
                    result.push(sep);
                    result.push(elt.borrow());
                });
                result
            }
        }
    }
}

/// Run as a `cargo` wrapper/plugin, the default invocation.
fn cargo_wrapper(rustc_wrapper: &Path) -> anyhow::Result<()> {
    let Args {
        metadata: metadata_path,
        runtime_path,
        set_runtime,
        rustflags,
        mut cargo_args,
    } = Args::parse();

    let args_for_cargo =
        iter::once(OsStr::new("cargo")).chain(cargo_args.iter().map(OsString::as_os_str));
    let InterceptedCargoArgs {
        manifest_path,
        extra_args: _,
    } = InterceptedCargoArgs::parse_from(args_for_cargo);

    let manifest_path = manifest_path.as_deref();
    let manifest_dir = manifest_path.and_then(|path| path.parent());

    set_rust_toolchain()?;

    // Resolve the sysroot once in the [`cargo_wrapper`]
    // so that we don't need all of the [`rustc_wrapper`]s to have to do it.
    let sysroot = resolve_sysroot()?;

    let cargo = Cargo::new();

    if set_runtime {
        cargo.run(|cmd| {
            cmd.args(&["add", "--optional", "c2rust-analysis-rt"]);
            if let Some(mut runtime) = runtime_path {
                if manifest_dir.is_some() {
                    runtime = fs_err::canonicalize(runtime)?;
                }
                // Since it's a local path, we don't need the internet,
                // and running it offline saves a slow index sync.
                cmd.args(&["--offline", "--path"]).arg(runtime);
            }
            if let Some(manifest_path) = manifest_path {
                cmd.arg("--manifest-path").arg(manifest_path);
            }
            Ok(())
        })?;
    }

    let metadata_file = MetadataFile::new(metadata_path)?;

    cargo.run(|cmd| {
        let cargo_target_dir = manifest_dir
            .unwrap_or_else(|| Path::new("."))
            .join("instrument.target");

        // The [`rustc_wrapper`] might run in a different working directory if `--manifest-path` was passed.
        let metadata_path = metadata_file.temp_path();
        let metadata_path = if !metadata_path.is_absolute() && manifest_dir.is_some() {
            Cow::Owned(fs_err::canonicalize(metadata_path)?)
        } else {
            Cow::Borrowed(metadata_path)
        };

        let rustflags = [
            env::var_os("RUSTFLAGS"),
            Some("-A warnings".into()),
            rustflags,
        ]
        .into_iter()
        .flatten()
        .join(OsStr::new(" "));

        // Enable the runtime dependency.
        add_feature(&mut cargo_args, &["c2rust-analysis-rt"]);

        cmd.args(cargo_args)
            .env(RUSTC_WRAPPER_VAR, rustc_wrapper)
            .env(RUST_SYSROOT_VAR, &sysroot)
            .env("CARGO_TARGET_DIR", &cargo_target_dir)
            .env("RUSTFLAGS", &rustflags)
            .env(METADATA_VAR, metadata_path.as_ref());
        Ok(())
    })?;

    metadata_file.close()?;

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
