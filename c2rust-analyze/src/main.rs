#![feature(rustc_private)]
extern crate either;
extern crate rustc_arena;
extern crate rustc_ast;
extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_type_ir;

mod analyze;
mod annotate;
mod borrowck;
mod context;
mod dataflow;
mod equiv;
mod known_fn;
mod labeled_ty;
mod last_use;
mod log;
mod panic_detail;
mod pointee_type;
mod pointer_id;
mod recent_writes;
mod rewrite;
mod trivial;
mod type_desc;
mod util;

use crate::log::init_logger;
use analyze::AnalysisCallbacks;
use anyhow::anyhow;
use anyhow::ensure;
use anyhow::Context;
use clap::{ArgAction, Parser, ValueEnum};
use rustc_driver::RunCompiler;
use rustc_driver::TimePassesCallbacks;
use rustc_session::config::CrateType;
use std::borrow::Borrow;
use std::env;
use std::ffi::OsStr;
use std::ffi::OsString;
use std::iter;
use std::path::Path;
use std::path::PathBuf;
use std::process;
use std::process::Command;
use std::process::ExitStatus;

/// Statically analyze and try to lift to safe Rust.
#[derive(Debug, Parser)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Set `$RUSTFLAGS` for the wrapped `cargo`.
    ///
    /// This allows setting `$RUSTFLAGS` for the inner `cargo` when `c2rust-analyze` is invoked via `cargo run`, for example.
    /// If `$RUSTFLAGS` is already set, these `--rustflags` are appended with a space.
    //
    // The reason this exists is twofold:
    //
    // 1. It would be convenient if `cargo` itself had such a `--rustflags` argument,
    //    so at least we can recreate it here ourselves.
    //
    // 2. If this binary is invoked by something like `cargo run --bin c2rust-analyze`,
    //    then it's impossible to set `$RUSTFLAGS` only for the inner `cargo` that you want to add them to
    //    without also adding them to the outer `cargo run` `cargo`.
    //    `--rustflags` lets you do that easily.
    #[clap(long)]
    rustflags: Option<OsString>,

    /// Comma-separated list of paths to rewrite.  Any item whose path does not start with a prefix
    /// from this list will be marked non-rewritable (`FIXED`).
    #[clap(long, action(ArgAction::Append))]
    rewrite_paths: Vec<OsString>,

    /// Whether to rewrite source files on disk.  The default is to print the rewritten source code
    /// to stdout as part of the tool's debug output.
    #[clap(long, value_enum)]
    rewrite_mode: Option<RewriteMode>,

    /// Synonym for `--rewrite-mode inplace`, kept around for backward compatibility.
    #[clap(long, hide(true), conflicts_with("rewrite_mode"))]
    rewrite_in_place: bool,

    /// Use `todo!()` placeholders in shims for casts that must be implemented manually.
    ///
    /// When a function requires a shim, and the shim requires a cast that can't be generated
    /// automatically, the default is to cancel rewriting of the function.  With this option,
    /// rewriting proceeds as normal, and shim generation emits `todo!()` in place of each
    /// unsupported cast.
    #[clap(long)]
    use_manual_shims: bool,

    /// Add "start/end of def" annotations as comments around each definition.  These annotations
    /// are used by `scripts/extract_working_defs.py` to locate specific defs in the rewritten
    /// code.
    #[clap(long)]
    annotate_def_spans: bool,

    /// Completely disable the `borrowck` pass.  All pointers will be given the `UNIQUE`
    /// permission; none will be wrapped in `Cell`.
    #[clap(long)]
    skip_borrowck: bool,

    /// Read a list of defs that should be marked non-rewritable (`FIXED`) from this file path.
    /// Run `c2rust-analyze` without this option and check the debug output for a full list of defs
    /// in the crate being analyzed; the file passed to this option should list a subset of those
    /// defs.
    #[clap(long)]
    fixed_defs_list: Option<PathBuf>,

    /// Read a list of defs that should always be rewritable (not `FIXED`) from this file path.
    /// This suppresses the rewriter's default behavior of skipping over defs that encounter
    /// analysis or rewriting errors.
    #[clap(long)]
    force_rewrite_defs_list: Option<PathBuf>,

    /// Read a list of defs on which the pointee type analysis should be skipped.
    #[clap(long)]
    skip_pointee_defs_list: Option<PathBuf>,

    /// `cargo` args.
    cargo_args: Vec<OsString>,
}

/// `cargo` args that we intercept.
#[derive(Debug, Parser)]
#[clap(ignore_errors = true)]
struct InterceptedCargoArgs {
    #[clap(long, value_parser)]
    manifest_path: Option<PathBuf>,

    /// Need this so `--` is allowed.  Not actually used,
    /// as we're just intercepting a few args and passing the rest through.
    extra_args: Vec<OsString>,
}

#[derive(Clone, Copy, Debug, ValueEnum)]
enum RewriteMode {
    /// Do not write rewritten code to disk.
    #[value(name = "none")]
    None,
    /// Apply rewrites to the original source files in-place.
    #[value(name = "inplace")]
    InPlace,
    /// Save rewritten code to a separate file alongside each source file.
    #[value(name = "alongside")]
    Alongside,
    /// Rewrite each function separately, and write the results for each to a separate file.
    #[value(name = "pointwise")]
    Pointwise,
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
/// It's unclear why the host target needs to be in the sysroot directory name, but it is.
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
            ::log::error!("error ({status}) running: {cmd:?}");
            exit_with_status(status);
        }
        Ok(())
    }
}

const RUSTC_WRAPPER_VAR: &str = "RUSTC_WRAPPER";
const RUST_SYSROOT_VAR: &str = "RUST_SYSROOT";

/// Read a [`PathBuf`] from the [`mod@env`]ironment that should've been set by the [`cargo_wrapper`].
fn env_path_from_wrapper(var: &str) -> anyhow::Result<PathBuf> {
    let path = env::var_os(var).ok_or_else(|| {
        anyhow!("the `cargo` wrapper should've set `${{${var}}}` for the `rustc` wrapper")
    })?;
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
/// We check if it is a build script by checking if it's a `--crate-type bin`
/// and yet has no `$CARGO_BIN_NAME`, which is set for normal binary crates.
///
/// Another solution (that `miri` uses) is to always specify the `--target`,
/// even if it's the host target.  Then `cargo` thinks it's cross-compiling,
/// and always forwards `--target` to `rustc` for native compilations,
/// but for host compilations like build scripts and proc-macros,
/// it doesn't specify `--target`.
///
/// This would work more robustly if we were also analyzing dependencies,
/// as our current solution would no longer work, but we aren't.
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

/// Check whether "no Cargo" mode is enabled.  In this mode, `c2rust-analyze` behaves like `rustc`
/// instead of like `cargo`.  For example, `C2RUST_ANALYZE_NO_CARGO=1 c2rust-analyze foo.rs` will
/// process `foo.rs` much like `rustc foo.rs` does.
fn in_no_cargo_mode() -> bool {
    env::var_os("C2RUST_ANALYZE_NO_CARGO").is_some()
}

/// Run as a `rustc` wrapper (a la `$RUSTC_WRAPPER`/[`RUSTC_WRAPPER_VAR`]).
fn rustc_wrapper() -> anyhow::Result<()> {
    let no_cargo = in_no_cargo_mode();
    let mut at_args = if no_cargo {
        env::args().collect::<Vec<_>>()
    } else {
        env::args().skip(1).collect::<Vec<_>>()
    };
    // We also want to avoid proc-macro crates,
    // but those must be separate crates, so we should be okay.
    let is_primary_compilation = (is_primary_package() && !is_build_script(&at_args)?) || no_cargo;

    let sysroot = env_path_from_wrapper(RUST_SYSROOT_VAR).or_else(|_| resolve_sysroot())?;
    let sysroot = sysroot
        .as_path()
        .to_str()
        .ok_or_else(|| anyhow!("sysroot path is not UTF-8: {}", sysroot.display()))?;
    at_args.extend(["--sysroot".into(), sysroot.into()]);
    let result = if is_primary_compilation {
        let dont_catch = env::var_os("C2RUST_ANALYZE_TEST_DONT_CATCH_PANIC").is_some();
        if !dont_catch {
            panic_detail::set_hook();
        }

        RunCompiler::new(&at_args, &mut AnalysisCallbacks).run()
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
        rustflags,
        rewrite_paths,
        mut rewrite_mode,
        rewrite_in_place,
        use_manual_shims,
        annotate_def_spans,
        skip_borrowck,
        fixed_defs_list,
        force_rewrite_defs_list,
        skip_pointee_defs_list,
        cargo_args,
    } = Args::parse();

    let args_for_cargo =
        iter::once(OsStr::new("cargo")).chain(cargo_args.iter().map(OsString::as_os_str));
    let InterceptedCargoArgs {
        manifest_path,
        extra_args: _,
    } = InterceptedCargoArgs::parse_from(args_for_cargo);

    let manifest_path = manifest_path.as_deref();
    let _manifest_dir = manifest_path.and_then(|path| path.parent());

    if rewrite_in_place {
        // `rewrite_in_place` and `rewrite_mode` are annotated as conflicting options, so if both
        // are set, `Args::parse()` should have exited with an error.
        assert!(rewrite_mode.is_none());
        rewrite_mode = Some(RewriteMode::InPlace);
    }

    set_rust_toolchain()?;

    // Resolve the sysroot once in the [`cargo_wrapper`]
    // so that we don't need all of the [`rustc_wrapper`]s to have to do it.
    let sysroot = resolve_sysroot()?;

    let cargo = Cargo::new();

    cargo.run(|cmd| {
        let rustflags = [
            env::var_os("RUSTFLAGS"),
            Some("-A warnings".into()),
            rustflags,
        ]
        .into_iter()
        .flatten()
        .join(OsStr::new(" "));

        cmd.args(cargo_args)
            .env(RUSTC_WRAPPER_VAR, rustc_wrapper)
            .env(RUST_SYSROOT_VAR, &sysroot)
            .env("RUSTFLAGS", &rustflags);

        if let Some(ref fixed_defs_list) = fixed_defs_list {
            cmd.env("C2RUST_ANALYZE_FIXED_DEFS_LIST", fixed_defs_list);
        }

        if let Some(ref force_rewrite_defs_list) = force_rewrite_defs_list {
            cmd.env("C2RUST_ANALYZE_FORCE_REWRITE_LIST", force_rewrite_defs_list);
        }

        if let Some(ref skip_pointee_defs_list) = skip_pointee_defs_list {
            cmd.env("C2RUST_ANALYZE_SKIP_POINTEE_LIST", skip_pointee_defs_list);
        }

        if !rewrite_paths.is_empty() {
            let rewrite_paths = rewrite_paths.join(OsStr::new(","));
            cmd.env("C2RUST_ANALYZE_REWRITE_PATHS", rewrite_paths);
        }

        if let Some(rewrite_mode) = rewrite_mode {
            let val = match rewrite_mode {
                RewriteMode::None => "none",
                RewriteMode::InPlace => "inplace",
                RewriteMode::Alongside => "alongside",
                RewriteMode::Pointwise => "pointwise",
            };
            cmd.env("C2RUST_ANALYZE_REWRITE_MODE", val);
        }

        if use_manual_shims {
            cmd.env("C2RUST_ANALYZE_USE_MANUAL_SHIMS", "1");
        }

        if annotate_def_spans {
            cmd.env("C2RUST_ANALYZE_ANNOTATE_DEF_SPANS", "1");
        }

        if skip_borrowck {
            cmd.env("C2RUST_ANALYZE_SKIP_BORROWCK", "1");
        }

        Ok(())
    })?;

    Ok(())
}

fn main() -> anyhow::Result<()> {
    init_logger();

    let own_exe = env::current_exe()?;

    let wrapping_rustc = env::var_os(RUSTC_WRAPPER_VAR).as_deref() == Some(own_exe.as_os_str())
        || in_no_cargo_mode();
    if wrapping_rustc {
        rustc_wrapper()
    } else {
        cargo_wrapper(&own_exe)
    }
}
