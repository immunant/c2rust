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

mod instrument_memory;

use instrument_memory::InstrumentMemoryOps;

use std::{
    env,
    ffi::{OsStr, OsString},
    path::{Path, PathBuf},
    process::{self, Command, ExitStatus},
};

use rustc_ast::ast::{Item, ItemKind, Visibility, VisibilityKind};
use rustc_ast::node_id::NodeId;
use rustc_ast::ptr::P;
use rustc_const_eval::transform::validate;
use rustc_driver::Compilation;
use rustc_driver::{RunCompiler, TimePassesCallbacks};
use rustc_interface::interface::Compiler;
use rustc_interface::Queries;
use rustc_middle::mir::MirPass;
use rustc_middle::ty::query::{ExternProviders, Providers};
use rustc_middle::ty::WithOptConstParam;
use rustc_session::Session;
use rustc_span::def_id::LocalDefId;
use rustc_span::symbol::Ident;
use rustc_span::DUMMY_SP;

use anyhow::{anyhow, Context};
use camino::Utf8Path;
use cargo_metadata::MetadataCommand;
use clap::Parser;
use lazy_static::lazy_static;

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

/// Lookup the sysroot fast using `rustup` environment variables.
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

/// Lookup the sysroot slow, but in a more reliable way using `rustc --print sysroot`.
///
/// TODO(kkysen) deduplicate this with `rustc_private_link::SysRoot::resolve`
fn get_sysroot_slow() -> anyhow::Result<PathBuf> {
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
    Ok(path)
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

/// Insert the feature flags as the first arguments following the `cargo` subcommand.
///
/// We can't insert them at the end because they could come after a `--` and thus be ignored.
/// And we can't insert them at the beginning before the `cargo` subcommand argument,
/// as `--features` is an argument of the subcommands, not `cargo` itself.
///
/// If there are no arguments, we don't insert the feature flags, as:
/// * it would panic on splicing/insertion
/// * we don't want to add the feature flags anyways, as `cargo` without arguments is already an error
/// * we don't want to obfuscate that error with an error about unexpected feature flags
fn add_runtime_feature(cargo_args: &mut Vec<OsString>) {
    let insertion_point = 1;
    if cargo_args.len() >= insertion_point {
        cargo_args.splice(
            insertion_point..insertion_point,
            ["--features", "c2rust-analysis-rt"]
                .iter()
                .map(|s| s.into()),
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
            exit_with_status(status);
        }
        Ok(())
    }
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
            mut cargo_args,
        } = Args::parse();

        let cargo = Cargo::new();

        let cargo_metadata = cargo.metadata().exec()?;
        let root_package = cargo_metadata
            .root_package()
            .ok_or_else(|| anyhow!("no root package found by `cargo`"))?;

        cargo.run(|cmd| {
            cmd.args(&["clean", "--package", root_package.name.as_str()]);
        })?;

        cargo.run(|cmd| {
            add_runtime_feature(&mut cargo_args);
            cmd.args(cargo_args)
                .env(rustc_wrapper_var, &own_exe)
                .env(metadata_var, metadata);
        })?;
    }
    Ok(())
}

lazy_static! {
    static ref INSTRUMENTER: InstrumentMemoryOps = InstrumentMemoryOps::new();
}

struct MirTransformCallbacks;

impl rustc_driver::Callbacks for MirTransformCallbacks {
    fn config(&mut self, config: &mut rustc_interface::Config) {
        config.override_queries = Some(override_queries);
    }

    fn after_parsing<'tcx>(
        &mut self,
        _compiler: &Compiler,
        queries: &'tcx Queries<'tcx>,
    ) -> Compilation {
        let parse = queries.parse().unwrap();
        let mut parse = parse.peek_mut();
        parse.items.push(P(Item {
            attrs: Vec::new(),
            id: NodeId::from_u32(0),
            span: DUMMY_SP,
            vis: Visibility {
                kind: VisibilityKind::Inherited,
                span: DUMMY_SP,
                tokens: None,
            },
            ident: Ident::from_str("c2rust_analysis_rt"),
            kind: ItemKind::ExternCrate(None),
            tokens: None,
        }));
        Compilation::Continue
    }
}

fn override_queries(
    _sess: &Session,
    providers: &mut Providers,
    _extern_providers: &mut ExternProviders,
) {
    providers.mir_built = |tcx, def: WithOptConstParam<LocalDefId>| {
        let mut providers = Providers::default();
        rustc_mir_build::provide(&mut providers);

        let steal_mir = (providers.mir_built)(tcx, def);
        let mut mir = steal_mir.steal();

        let body_did = def.did.to_def_id();
        let fn_ty = tcx.type_of(body_did);
        if fn_ty.is_fn() && !tcx.is_const_fn(body_did) && !tcx.is_static(body_did) {
            INSTRUMENTER.instrument_fn(tcx, &mut mir, body_did);

            validate::Validator {
                when: "After dynamic instrumentation".to_string(),
                mir_phase: mir.phase,
            }
            .run_pass(tcx, &mut mir);
        }

        tcx.alloc_steal_mir(mir)
    };
}
