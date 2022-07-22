#![feature(rustc_private)]
extern crate rustc_driver;
extern crate rustc_session;
extern crate rustc_span;

use std::{
    env,
    ffi::OsString,
    hash::Hash,
    io::ErrorKind,
    path::{Path, PathBuf},
    process::{self, Command, ExitStatus},
    str::FromStr,
};

use c2rust_dynamic_instrumentation::{MirTransformCallbacks, INSTRUMENTER};
use camino::Utf8Path;
use cargo_metadata::{Metadata, MetadataCommand, Package, Target};
use clap::Parser;
use color_eyre::eyre;
use color_eyre::eyre::eyre;
use is_executable::IsExecutable;
use rustc_driver::RunCompiler;
use rustc_session::config::Options;
use serde::{Deserialize, Serialize};

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

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize, Deserialize)]
enum CrateType {
    Bin,
    RLib,
    DyLib,
    CDyLib,
    StaticLib,
    ProcMacro,
}

impl FromStr for CrateType {
    type Err = eyre::Report;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use CrateType::*;
        Ok(match s {
            "bin" => Bin,
            // `rustc_session::config::CrateType` doesn't have a separate [`Lib`] variant.
            "lib" | "rlib" => RLib,
            "dylib" => DyLib,
            "cdylib" => CDyLib,
            "staticlib" => StaticLib,
            "proc-macro" => ProcMacro,
            _ => return Err(eyre!("unknown crate type: {s}")),
        })
    }
}

impl From<rustc_session::config::CrateType> for CrateType {
    fn from(crate_type: rustc_session::config::CrateType) -> Self {
        use rustc_session::config::CrateType::*;
        match crate_type {
            Executable => Self::Bin,
            Dylib => Self::DyLib,
            Rlib => Self::RLib,
            Staticlib => Self::StaticLib,
            Cdylib => Self::CDyLib,
            ProcMacro => Self::ProcMacro,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Serialize, Deserialize)]
enum CrateEdition {
    E2015,
    E2018,
    E2021,
}

impl From<cargo_metadata::Edition> for CrateEdition {
    fn from(edition: cargo_metadata::Edition) -> Self {
        use cargo_metadata::Edition::*;
        match edition {
            E2015 => Self::E2015,
            E2018 => Self::E2018,
            E2021 => Self::E2021,
            _ => todo!("when `rustc_span::edition::Edition` gets a new edition, it'll cause a compile error"),
        }
    }
}

impl From<rustc_span::edition::Edition> for CrateEdition {
    fn from(edition: rustc_span::edition::Edition) -> Self {
        use rustc_span::edition::Edition::*;
        match edition {
            Edition2015 => Self::E2015,
            Edition2018 => Self::E2018,
            Edition2021 => Self::E2021,
        }
    }
}

/// A `crate` name, canonicalized to an identifier (i.e. `-`s are replaced with `_`).
#[derive(PartialEq, Eq, Hash, Debug, Serialize, Deserialize)]
struct CrateName {
    name: String,
}

impl CrateName {
    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }
}

impl From<&str> for CrateName {
    fn from(name: &str) -> Self {
        Self {
            name: name.replace('-', "_"),
        }
    }
}

#[derive(Eq, Debug, Serialize, Deserialize)]
struct CrateTarget {
    crate_name: CrateName,
    src_path: PathBuf,
    crate_types: Vec<CrateType>,
    edition: CrateEdition,
}

impl CrateTarget {
    /// A set of stable parts of a [`CrateTarget`] that are meant to be checked for equality.
    fn stable_parts(&self) -> impl Eq + Hash + '_ {
        // Sometimes the [`CrateType`]s can change to `[]` depending on if tests are being built,
        // so exclude that from equality checks.
        (
            self.crate_name.as_str(),
            self.src_path.as_path(),
            self.edition,
        )
    }
}

impl PartialEq for CrateTarget {
    fn eq(&self, other: &Self) -> bool {
        self.stable_parts() == other.stable_parts()
    }
}

impl Hash for CrateTarget {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.stable_parts().hash(state);
    }
}

impl CrateTarget {
    pub fn from_metadata_target(package: &Package, target: &Target) -> eyre::Result<Self> {
        Ok(Self {
            crate_name: package.name.as_str().into(),
            src_path: target.src_path.clone().into(),
            crate_types: target
                .crate_types
                .iter()
                .map(|s| s.as_str())
                .map(CrateType::from_str)
                .collect::<Result<Vec<_>, _>>()?,
            edition: target.edition.clone().into(),
        })
    }

    pub fn from_metadata_package(package: &Package) -> eyre::Result<Vec<Self>> {
        package
            .targets
            .iter()
            .map(|target| Self::from_metadata_target(package, target))
            .collect()
    }

    pub fn from_session_options(
        session_options: Options,
        free_matches: &[String],
    ) -> eyre::Result<Self> {
        let src_path = match free_matches {
            [src_path] => {
                if src_path == "-" {
                    // We don't actually ever read from this, so I think this is a fine translation to a real [`Path`].
                    "/dev/stdin".into()
                } else {
                    fs_err::canonicalize(src_path)?
                }
            }
            free_matches => {
                return Err(eyre!(
                    "`rustc` args `matches.free` is not a single source path: {free_matches:?}"
                ))
            }
        };
        let Options {
            crate_name,
            crate_types,
            edition,
            ..
        } = session_options;
        let crate_name = crate_name.ok_or_else(|| eyre!("no crate_name specified by `cargo`"))?;
        Ok(Self {
            crate_name: crate_name.as_str().into(),
            src_path,
            crate_types: crate_types.into_iter().map(CrateType::from).collect(),
            edition: edition.into(),
        })
    }

    pub fn from_metadata(metadata: &Metadata) -> eyre::Result<Vec<Self>> {
        Self::from_metadata_package(
            metadata
                .root_package()
                .ok_or_else(|| eyre!("no root package found by `cargo`"))?,
        )
    }

    pub fn from_rustc_args(at_args: &[String]) -> eyre::Result<Self> {
        let args = rustc_driver::args::arg_expand_all(at_args);
        let matches = rustc_driver::handle_options(&args)
            .ok_or_else(|| eyre!("failed to parse `rustc` args"))?;
        let session_options = rustc_session::config::build_session_options(&matches);
        let crate_target = CrateTarget::from_session_options(session_options, &matches.free)?;
        Ok(crate_target)
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct InstrumentInfo {
    crate_targets: Vec<CrateTarget>,
    metadata: PathBuf,
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

fn get_sysroot_slow() -> eyre::Result<PathBuf> {
    todo!("use `rustc --print sysroot` to support non-`rustup` cases, which @fw-immunant uses")
}

fn get_sysroot() -> eyre::Result<PathBuf> {
    get_sysroot_fast()
        .ok_or(())
        .or_else(|()| get_sysroot_slow())
}

fn passthrough_rustc(at_args: &[String], sysroot: &Path) -> eyre::Result<ExitStatus> {
    // We can skip a `rustup` `rustc` -> real `rustc` resolution by just invoking the real `rustc` ourselves.
    // TODO(kkysen) this might not work without `rustup`
    let rustc = sysroot.join("bin/rustc");
    let status = Command::new(rustc).args(at_args.iter().skip(1)).status()?;
    Ok(status)
}

fn instrument_rustc(mut at_args: Vec<String>, sysroot: &Path, metadata: &Path) -> eyre::Result<()> {
    // Normally, `rustc` looks up the sysroot by the location of its own binary.
    // This works because the `rustc` on `$PATH` is actually `rustup`,
    // and `rustup` invokes the real `rustc`, which is in a location relative to the sysroot.
    // As we invoke `rustc_driver` directly here, we are `rustc`,
    // and thus we have to explicitly specify the sysroot that the real `rustc` would normally use.
    //
    // Note that the sysroot contains the toolchain and host target name,
    // but this has no effect on cross-compiling.
    // Every toolchain's `rustc` is able to itself cross-compile.
    // I'm not sure why the host target needs to be in the sysroot directory name, but it is.
    //
    // Also note that this sysroot lookup should be done at runtime,
    // not at compile-time in the `build.rs`,
    // as the toolchain locations could be different
    // from where this binary was compiled and where it is running
    // (it could be on a different machine with a different `$RUSTUP_HOME`).
    let sysroot: &Utf8Path = sysroot.try_into()?;
    at_args.extend(["--sysroot".into(), sysroot.as_str().into()]);
    RunCompiler::new(&at_args, &mut MirTransformCallbacks)
        .run()
        .map_err(|_| eyre!("`rustc` failed"))?;
    INSTRUMENTER.finalize(metadata).map_err(|e| eyre!(e))?;
    Ok(())
}

/// Delete all files that we want to be regenerated
/// so that we always have fully-up-to-date, non-incremental metadata.
fn delete_metadata_and_dependencies(
    info: &InstrumentInfo,
    cargo_metadata: &Metadata,
) -> eyre::Result<()> {
    if let Err(e) = fs_err::remove_file(&info.metadata) {
        if e.kind() != ErrorKind::NotFound {
            return Err(e.into());
        }
    }

    // TODO(kkysen) Figure out a way to know which profile is being used
    // Until then, just search them all and delete all of them.

    // TODO(kkysen) We probably have to delete binaries that have different names from the crates.

    // Delete all executables in `target/${profile}/deps/` starting with the crate name + `-`.
    for profile_dir in cargo_metadata.target_directory.read_dir()? {
        let profile_dir = profile_dir?;
        if !profile_dir.file_type()?.is_dir() {
            continue;
        }
        for artifact in profile_dir.path().join("deps").read_dir()? {
            let artifact = artifact?;
            if !artifact.file_type()?.is_file() {
                continue;
            }
            let file_name = artifact.file_name();
            let file_name = file_name.to_str();
            for crate_target in &info.crate_targets {
                let prefix = format!("{}-", crate_target.crate_name.as_str());
                // [`Path::starts_with`] only checks whole components at once,
                // and `OsStr::starts_with` doesn't exist yet.
                if file_name
                    .map(|name| name.starts_with(&prefix))
                    .unwrap_or_default()
                {
                    let artifact = artifact.path();
                    if artifact.is_executable() {
                        fs_err::remove_file(&artifact)?;
                    }
                }
            }
        }
    }

    Ok(())
}

fn main() -> eyre::Result<()> {
    color_eyre::install()?;
    env_logger::init();

    let rustc_wrapper_var = "RUSTC_WRAPPER";
    let instrument_info_var = "C2RUST_INSTRUMENT_INFO";

    let own_exe = env::current_exe()?;

    let wrapping_rustc = env::var_os(rustc_wrapper_var).as_deref() == Some(own_exe.as_os_str());
    if wrapping_rustc {
        let sysroot = get_sysroot()?;
        let at_args = env::args().skip(1).collect::<Vec<_>>();
        let crate_target = CrateTarget::from_rustc_args(&at_args)?;
        let info = env::var(instrument_info_var)?;
        let info = serde_json::from_str::<InstrumentInfo>(&info)?;
        let should_instrument = info.crate_targets.contains(&crate_target);
        if should_instrument {
            instrument_rustc(at_args, &sysroot, &info.metadata)?;
        } else {
            let status = passthrough_rustc(&at_args, &sysroot)?;
            exit_with_status(status);
        }
    } else {
        let Args {
            metadata,
            cargo_args,
        } = Args::parse();

        let cargo_metadata = MetadataCommand::new().exec()?;
        let crate_targets = CrateTarget::from_metadata(&cargo_metadata)?;
        let info = InstrumentInfo {
            crate_targets,
            metadata,
        };

        delete_metadata_and_dependencies(&info, &cargo_metadata)?;

        // We could binary encode this, but it's likely very short,
        // so just json encode it, so it's also human readable and inspectable.
        let info = serde_json::to_string(&info)?;

        let cargo = env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
        let status = Command::new(cargo)
            .args(cargo_args)
            .env(rustc_wrapper_var, &own_exe)
            .env(instrument_info_var, info)
            .status()?;
        exit_with_status(status);
    }
    Ok(())
}
