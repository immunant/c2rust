// Silence warnings and errors from the use of specialization below
// which is an incomplete feature that rustc warns about.
// TODO: switch to min_specialization, but that isn't a suitable
// replacement yet.
#![allow(incomplete_features)]
#![feature(
    rustc_private,
    trace_macros,
    specialization,
    box_patterns,
    generator_trait,
    drain_filter,
    label_break_value,
    let_else,
    never_type
)]
#![cfg_attr(feature = "profile", feature(proc_macro_hygiene))]

extern crate rustc_arena;
extern crate rustc_ast;
extern crate rustc_ast_pretty;
extern crate rustc_codegen_ssa;
extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_hash;
extern crate rustc_hir;
extern crate rustc_incremental;
extern crate rustc_index;
extern crate rustc_infer;
extern crate rustc_interface;
extern crate rustc_lexer;
extern crate rustc_lint;
extern crate rustc_metadata;
extern crate rustc_middle;
extern crate rustc_parse;
extern crate rustc_privacy;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_type_ir;
extern crate rustc_typeck;
extern crate smallvec;

mod ast_builder;
mod macros;

pub mod ast_manip;

pub mod util;

pub mod rewrite;

pub mod analysis;

pub mod pick_node;
pub mod span_fix;

pub mod contains_mark;
pub mod illtyped;
pub mod path_edit;
pub mod reflect;
pub mod resolve;
pub mod type_map;

pub mod matcher;

pub mod collapse;
pub mod driver;
pub mod node_map;

pub mod command;
pub mod file_io;
pub mod interact;
pub mod plugin;

pub mod mark_adjust;
pub mod print_spans;
pub mod select;
pub mod transform;

mod context;

use cargo::core::TargetKind;
use cargo_util::paths;
use log::{info, warn};
use rustc_ast::NodeId;
use rustc_interface::interface;
use std::collections::HashSet;
use std::env;
use std::path::{Path, PathBuf};
use std::str::{self, FromStr};
use std::sync::Arc;

use crate::ast_builder::IntoSymbol;

pub use crate::context::RefactorCtxt;

#[derive(Clone, Debug)]
pub struct Cursor {
    file: String,
    line: u32,
    col: u32,
    label: Option<String>,
    kind: Option<String>,
}

impl Cursor {
    pub fn new(
        file: String,
        line: u32,
        col: u32,
        label: Option<String>,
        kind: Option<String>,
    ) -> Self {
        Cursor {
            file,
            line,
            col,
            label,
            kind,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Mark {
    id: usize,
    label: Option<String>,
}

impl Mark {
    pub fn new(id: usize, label: Option<String>) -> Self {
        Mark { id, label }
    }
}

#[derive(Clone, Debug)]
pub struct Command {
    pub name: String,
    pub args: Vec<String>,
}

#[derive(Clone, Debug)]
pub enum CargoTarget {
    All,
    AllBins,
    Bin(String),
    Lib,
}

#[derive(Clone, Debug)]
pub enum RustcArgSource {
    CmdLine(Vec<String>),
    Cargo(CargoTarget),
}

#[derive(Clone, Debug)]
struct RustcArgs {
    kind: Option<TargetKind>,
    args: Vec<String>,
    cwd: Option<PathBuf>,
}

pub struct Options {
    pub rewrite_modes: Vec<file_io::OutputMode>,
    pub commands: Vec<Command>,
    pub rustc_args: RustcArgSource,
    pub cursors: Vec<Cursor>,
    pub marks: Vec<Mark>,

    pub plugins: Vec<String>,
    pub plugin_dirs: Vec<String>,
}

/// Try to find the rustup installation that provides the rustc at the given path.  The input path
/// should be normalized already.
#[cfg_attr(feature = "profile", flame)]
fn get_rustup_path(rustc: &Path) -> Option<PathBuf> {
    use std::ffi::OsStr;
    use std::fs;

    // `rustc` is already normalized, which resolves the `rustc` -> `rustup` symlink if one is
    // present.
    if rustc.file_name() == Some(OsStr::new("rustup")) {
        return Some(rustc.to_owned());
    }

    // Otherwise, check for a rustup binary installed alongside rustc.  If they're the same size,
    // we assume they're the same file (hardlinked or copied).
    let rustup = rustc.with_file_name("rustup");
    let rustc_meta = fs::metadata(&rustc).ok()?;
    let rustup_meta = fs::metadata(&rustup).ok()?;
    if rustc_meta.len() == rustup_meta.len() {
        return Some(rustup);
    }

    None
}

#[cfg_attr(feature = "profile", flame)]
fn get_rustc_executable(path: &Path) -> String {
    use std::process::{Command, Stdio};

    let resolved = paths::resolve_executable(path).unwrap();
    if let Some(rustup_path) = get_rustup_path(&resolved) {
        let proc = Command::new(rustup_path)
            .arg("which")
            .arg("rustc")
            .stdout(Stdio::piped())
            .spawn()
            .unwrap();
        let output = proc.wait_with_output().unwrap();
        assert!(output.status.success());
        let s = str::from_utf8(&output.stdout).unwrap();
        return s.trim().to_owned();
    }

    resolved.to_str().unwrap().to_owned()
}

#[cfg_attr(feature = "profile", flame)]
fn get_rustc_arg_strings(src: RustcArgSource) -> Vec<RustcArgs> {
    match src {
        RustcArgSource::CmdLine(mut args) => {
            let mut rustc_args = RustcArgs {
                kind: None,
                args: vec![get_rustc_executable(Path::new("rustc"))],
                cwd: None,
            };
            rustc_args.args.append(&mut args);
            vec![rustc_args]
        }
        RustcArgSource::Cargo(target) => get_rustc_cargo_args(target),
    }
}

#[cfg_attr(feature = "profile", flame)]
fn get_rustc_cargo_args(target_type: CargoTarget) -> Vec<RustcArgs> {
    use cargo::core::compiler::{CompileMode, Context, DefaultExecutor, Executor, Unit};
    use cargo::core::{PackageId, Target, Verbosity, Workspace};
    use cargo::ops;
    use cargo::ops::CompileOptions;
    use cargo::util::errors::CargoResult;
    use cargo::util::important_paths::find_root_manifest_for_wd;
    use cargo::Config;
    use cargo_util::ProcessBuilder;
    use std::sync::Mutex;

    let config = Config::default().unwrap();
    config.shell().set_verbosity(Verbosity::Quiet);
    let mode = CompileMode::Check { test: false };
    let compile_opts = CompileOptions::new(&config, mode).unwrap();

    let manifest_path = find_root_manifest_for_wd(config.cwd()).unwrap();
    let ws = Workspace::new(&manifest_path, &config).unwrap();

    struct LoggingExecutor {
        default: DefaultExecutor,
        target_pkg: PackageId,
        target_type: CargoTarget,
        pkg_args: Mutex<Vec<RustcArgs>>,
    }

    impl LoggingExecutor {
        fn maybe_record_cmd(&self, cmd: &ProcessBuilder, id: &PackageId, target: &Target) -> bool {
            if id != &self.target_pkg {
                return false;
            }

            let do_record = match (&self.target_type, &target.kind()) {
                (CargoTarget::All, TargetKind::Lib(..)) => true,
                (CargoTarget::All, TargetKind::Bin) => true,
                (CargoTarget::AllBins, TargetKind::Bin) => true,
                (CargoTarget::Bin(bin), TargetKind::Bin) => target.name() == bin,
                (CargoTarget::Lib, TargetKind::Lib(..)) => true,
                _ => false,
            };
            if !do_record {
                return false;
            }

            let args = cmd
                .get_args()
                .iter()
                .map(|os| os.to_str().unwrap().to_owned())
                .collect();
            let mut g = self.pkg_args.lock().unwrap();

            let cwd = cmd.get_cwd().map(Path::to_path_buf);

            // TODO: We should be topologically sorting the crates here so that
            // we refactor dependencies before crates that depend on them, but
            // for now we don't support workspaces, so there can only be one
            // lib.
            let args = RustcArgs {
                kind: Some(target.kind().clone()),
                args,
                cwd,
            };
            if let TargetKind::Lib(..) = target.kind() {
                g.insert(0, args);
            } else {
                g.push(args);
            }

            true
        }
    }

    impl Executor for LoggingExecutor {
        fn init<'a, 'cfg>(&self, cx: &Context<'a, 'cfg>, unit: &Unit) {
            self.default.init(cx, unit);
        }

        fn exec(
            &self,
            cmd: &ProcessBuilder,
            id: PackageId,
            target: &Target,
            mode: CompileMode,
            _on_stdout_line: &mut dyn FnMut(&str) -> CargoResult<()>,
            _on_stderr_line: &mut dyn FnMut(&str) -> CargoResult<()>,
        ) -> CargoResult<()> {
            self.maybe_record_cmd(&cmd, &id, target);
            self.default
                .exec(cmd, id, target, mode, &mut |_| Ok(()), &mut |_| Ok(()))
        }

        fn force_rebuild(&self, unit: &Unit) -> bool {
            if unit.pkg.package_id() == self.target_pkg {
                return true;
            }
            self.default.force_rebuild(unit)
        }
    }

    let exec = Arc::new(LoggingExecutor {
        default: DefaultExecutor,
        target_pkg: ws.current().unwrap().package_id(),
        target_type,
        pkg_args: Mutex::new(vec![]),
    });
    let exec_dyn: Arc<dyn Executor> = exec.clone();

    let _ = ops::compile_with_exec(&ws, &compile_opts, &exec_dyn);

    let mut arg_vec = exec.pkg_args.lock().unwrap().clone();

    for args in &mut arg_vec {
        let rustc = config.load_global_rustc(Some(&ws)).unwrap();
        args.args.insert(0, get_rustc_executable(&rustc.path));
        info!("cargo-provided rustc args = {:?}", args);
    }

    arg_vec
}

fn rebuild() {
    use cargo::core::compiler::CompileMode;
    use cargo::core::{Verbosity, Workspace};
    use cargo::ops;
    use cargo::ops::CompileOptions;
    use cargo::util::important_paths::find_root_manifest_for_wd;
    use cargo::Config;

    let config = Config::default().unwrap();
    config.shell().set_verbosity(Verbosity::Quiet);
    let mode = CompileMode::Check { test: false };
    let compile_opts = CompileOptions::new(&config, mode).unwrap();

    let manifest_path = find_root_manifest_for_wd(config.cwd()).unwrap();
    let ws = Workspace::new(&manifest_path, &config).unwrap();
    ops::compile(&ws, &compile_opts).expect("Could not rebuild crate");
}

#[cfg_attr(feature = "profile", flame)]
pub fn lib_main(opts: Options) -> interface::Result<()> {
    env_logger::init();
    rustc_driver::install_ice_hook();
    info!("Begin refactoring");

    // Make sure we compile with the toolchain version that the refactoring tool
    // is built against.
    if let Some(toolchain_ver) = option_env!("RUSTUP_TOOLCHAIN") {
        env::set_var("RUSTUP_TOOLCHAIN", toolchain_ver);
    }

    // Shut the compiler up while refactoring
    let mut rustflags = env::var_os("RUSTFLAGS").unwrap_or_default();
    rustflags.push(" -Awarnings");
    env::set_var("RUSTFLAGS", rustflags);

    rustc_driver::catch_fatal_errors(move || main_impl(opts)).and_then(|x| x)
}

fn main_impl(opts: Options) -> interface::Result<()> {
    let target_args = get_rustc_arg_strings(opts.rustc_args.clone());
    if target_args.is_empty() {
        warn!("Could not derive any rustc invocations for refactoring");
    }
    let multiple_refactorings = target_args.len() > 1;
    for rustc_args in target_args {
        let mut marks = HashSet::new();
        for m in &opts.marks {
            let label = m.label.as_ref().map_or("target", |s| s).into_symbol();
            marks.insert((NodeId::from_usize(m.id), label));
        }

        if let Some(ref cwd) = rustc_args.cwd {
            env::set_current_dir(cwd).expect("Error changing current directory");
        }

        // TODO: interface::run_compiler() here and create a RefactorState with the
        // callback. RefactorState should know how to reset the compiler when needed
        // and can handle querying the compiler.

        if !opts.cursors.is_empty() {
            let config = driver::create_config(&rustc_args.args);
            driver::run_compiler(config, None, |compiler| {
                compiler.enter(|queries| {
                    let expanded_crate = queries.expansion().unwrap().take().0;
                    for c in &opts.cursors {
                        let kind_result =
                            c.kind.clone().map_or(Ok(pick_node::NodeKind::Any), |s| {
                                pick_node::NodeKind::from_str(&s)
                            });
                        let kind = match kind_result {
                            Ok(k) => k,
                            Err(_) => {
                                info!("Bad cursor kind: {:?}", c.kind.as_ref().unwrap());
                                continue;
                            }
                        };

                        let id = match pick_node::pick_node_at_loc(
                            &expanded_crate,
                            compiler.session(),
                            kind,
                            &c.file,
                            c.line,
                            c.col,
                        ) {
                            Some(info) => info.id,
                            None => {
                                info!(
                                    "Failed to find {:?} at {}:{}:{}",
                                    kind, c.file, c.line, c.col
                                );
                                continue;
                            }
                        };

                        let label = c.label.as_ref().map_or("target", |s| s).into_symbol();

                        info!("label {:?} as {:?}", id, label);

                        marks.insert((id, label));
                    }
                })
            });
        }

        let mut cmd_reg = command::Registry::new();
        transform::register_commands(&mut cmd_reg);
        mark_adjust::register_commands(&mut cmd_reg);
        pick_node::register_commands(&mut cmd_reg);
        print_spans::register_commands(&mut cmd_reg);
        select::register_commands(&mut cmd_reg);
        analysis::register_commands(&mut cmd_reg);
        reflect::register_commands(&mut cmd_reg);
        command::register_commands(&mut cmd_reg);

        plugin::load_plugins(&opts.plugin_dirs, &opts.plugins, &mut cmd_reg);

        let config = driver::create_config(&rustc_args.args);

        if opts.commands.len() == 1 && opts.commands[0].name == "interact" {
            interact::interact_command(&opts.commands[0].args, config, cmd_reg);
        } else {
            let file_io = Arc::new(file_io::RealFileIO::new(opts.rewrite_modes.clone()));
            driver::run_refactoring(config, cmd_reg, file_io, marks, |mut state| {
                for cmd in opts.commands.clone() {
                    if &cmd.name == "interact" {
                        panic!("`interact` must be the only command");
                    } else {
                        match state.run(&cmd.name, &cmd.args) {
                            Ok(_) => {}
                            Err(e) => {
                                eprintln!("{:?}", e);
                                std::process::exit(1);
                            }
                        }
                    }
                }

                state.save_crate();
            });
        }

        // We need to rebuild the crate metadata if this was a library and we
        // are refactoring binaries that may depend on it.
        if multiple_refactorings {
            if let Some(TargetKind::Lib(..)) = rustc_args.kind {
                rebuild();
            }
        }
    }

    dump_profile();

    Ok(())
}

#[cfg(feature = "profile")]
fn dump_profile() {
    flame::dump_html(&mut std::fs::File::create("flame-graph.html").unwrap()).unwrap();
}

#[cfg(not(feature = "profile"))]
fn dump_profile() {}
