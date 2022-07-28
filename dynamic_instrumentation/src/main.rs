#![feature(rustc_private)]
extern crate rustc_driver;
extern crate rustc_session;
extern crate rustc_span;

use std::{
    env,
    ffi::OsString,
    path::{Path, PathBuf},
    process::{self, Command, ExitStatus},
};

use anyhow::anyhow;
use c2rust_dynamic_instrumentation::{MirTransformCallbacks, INSTRUMENTER};
use camino::Utf8Path;
use cargo_metadata::MetadataCommand;
use clap::Parser;
use rustc_driver::{RunCompiler, TimePassesCallbacks};

/// Instrument memory accesses for dynamic analysis.
#[derive(Debug, Parser)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Path to the metadata output file storing instrumentation locations.
    #[clap(long, value_parser)]
    metadata: PathBuf,

    /// `cargo` args.
    cargo_args: Vec<OsString>,
}

fn exit_with_status(status: ExitStatus) {
    process::exit(status.code().unwrap_or(1))
}

fn get_sysroot_fast() -> Option<PathBuf> {
    let sysroot = [
        env::var_os("RUSTUP_HOME")?,
        "toolchains".into(),
        env::var_os("RUSTUP_TOOLCHAIN")?,
    ]
    .into_iter()
    .collect();
    Some(sysroot)
}

fn get_sysroot_slow() -> anyhow::Result<PathBuf> {
    todo!("use `rustc --print sysroot` to support non-`rustup` cases, which @fw-immunant uses")
}

/// Resolve the current `rustc` sysroot.
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
fn get_sysroot() -> anyhow::Result<PathBuf> {
    get_sysroot_fast()
        .ok_or(())
        .or_else(|()| get_sysroot_slow())
}

fn main() -> anyhow::Result<()> {
    let rustc_wrapper_var = "RUSTC_WRAPPER";
    let metadata_var = "C2RUST_INSTRUMENT_METADATA_PATH";

    let own_exe = env::current_exe()?;

    let wrapping_rustc = env::var_os(rustc_wrapper_var).as_deref() == Some(own_exe.as_os_str());
    if wrapping_rustc {
        let is_primary_package = env::var("CARGO_PRIMARY_PACKAGE").is_ok();
        let should_instrument = is_primary_package;
        let mut at_args = env::args().skip(1).collect::<Vec<_>>();
        let sysroot = get_sysroot()?;
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
            let metadata =
                env::var_os(metadata_var).ok_or_else(|| anyhow!("we should've set this"))?;
            INSTRUMENTER.finalize(Path::new(&metadata))?;
        }
    } else {
        let Args {
            metadata,
            cargo_args,
        } = Args::parse();

        let cargo = env::var_os("CARGO").unwrap_or_else(|| "cargo".into());

        let cargo_metadata = MetadataCommand::new().cargo_path(&cargo).exec()?;

        let root_package = cargo_metadata
            .root_package()
            .ok_or_else(|| anyhow!("no root package found by `cargo`"))?;

        let status = Command::new(&cargo)
            .args(&["clean", "--package", root_package.name.as_str()])
            .status()?;
        if !status.success() {
            exit_with_status(status);
        }

        let status = Command::new(&cargo)
            .args(cargo_args)
            .env(rustc_wrapper_var, &own_exe)
            .env(metadata_var, metadata)
            .status()?;
        exit_with_status(status);
    }
    Ok(())
}
