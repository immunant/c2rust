#![feature(
    rustc_private,
    trace_macros,
    specialization,
    box_patterns,
    generator_trait,
    vec_remove_item,
)]
#![cfg_attr(feature = "profile", feature(proc_macro_hygiene))]

extern crate arena;
extern crate bincode;
extern crate cargo;
extern crate clap;
extern crate diff;
extern crate ena;
extern crate env_logger;
extern crate failure;
extern crate indexmap;
extern crate libc;
#[macro_use]
extern crate json;
#[macro_use]
extern crate log;
extern crate regex;
extern crate rustc;
extern crate rustc_codegen_utils;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_incremental;
extern crate rustc_interface;
extern crate rustc_lint;
extern crate rustc_metadata;
extern crate rustc_privacy;
extern crate rustc_resolve;
extern crate rustc_target;
#[macro_use]
extern crate smallvec;
extern crate c2rust_ast_builder;
extern crate syntax;
extern crate syntax_ext;
extern crate syntax_pos;
extern crate c2rust_analysis_rt;

#[cfg(feature = "profile")]
extern crate flame;
#[cfg(feature = "profile")]
#[macro_use]
extern crate flamer;

#[macro_use]
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
mod scripting;

use cargo::util::paths;
use rustc_interface::interface;
use std::collections::HashSet;
use std::env;
use std::path::{Path, PathBuf};
use std::str::{self, FromStr};
use std::sync::Arc;
use syntax::ast::NodeId;

use c2rust_ast_builder::IntoSymbol;

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
pub enum RustcArgSource {
    CmdLine(Vec<String>),
    Cargo,
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
fn get_rustc_arg_strings(src: RustcArgSource) -> Vec<String> {
    match src {
        RustcArgSource::CmdLine(mut args) => {
            let mut rustc_args = vec![get_rustc_executable(Path::new("rustc"))];
            rustc_args.append(&mut args);
            rustc_args
        }
        RustcArgSource::Cargo => get_rustc_cargo_args(),
    }
}

#[cfg_attr(feature = "profile", flame)]
fn get_rustc_cargo_args() -> Vec<String> {
    use cargo::core::compiler::{CompileMode, Context, DefaultExecutor, Executor, Unit};
    use cargo::core::manifest::TargetKind;
    use cargo::core::{maybe_allow_nightly_features, PackageId, Target, Workspace, Verbosity};
    use cargo::ops;
    use cargo::ops::CompileOptions;
    use cargo::util::important_paths::find_root_manifest_for_wd;
    use cargo::util::{CargoResult, ProcessBuilder};
    use cargo::Config;
    use std::sync::Mutex;

    // `cargo`-built `libcargo` is always on the `dev` channel, so `maybe_allow_nightly_features`
    // really does allow nightly features.
    maybe_allow_nightly_features();

    let config = Config::default().unwrap();
    config.shell().set_verbosity(Verbosity::Quiet);
    let mode = CompileMode::Check { test: false };
    let compile_opts = CompileOptions::new(&config, mode).unwrap();

    let manifest_path = find_root_manifest_for_wd(config.cwd()).unwrap();
    let ws = Workspace::new(&manifest_path, &config).unwrap();

    struct LoggingExecutor {
        default: DefaultExecutor,
        target_pkg: PackageId,
        pkg_args: Mutex<Option<Vec<String>>>,
    }

    impl LoggingExecutor {
        fn maybe_record_cmd(&self, cmd: &ProcessBuilder, id: &PackageId, target: &Target) -> bool {
            if id != &self.target_pkg {
                return false;
            }

            let mut g = self.pkg_args.lock().unwrap();
            match target.kind() {
                // `lib` builds take priority.  Otherwise, take the first available bin.
                &TargetKind::Lib(_) => {}
                &TargetKind::Bin if g.is_none() => {}
                _ => return false,
            }

            let args = cmd
                .get_args()
                .iter()
                .map(|os| os.to_str().unwrap().to_owned())
                .collect();
            *g = Some(args);

            true
        }
    }

    impl Executor for LoggingExecutor {
        fn init(&self, cx: &Context, unit: &Unit) {
            self.default.init(cx, unit);
        }

        fn exec(
            &self,
            cmd: ProcessBuilder,
            id: PackageId,
            target: &Target,
            mode: CompileMode,
        ) -> CargoResult<()> {
            if self.maybe_record_cmd(&cmd, &id, target) {
                Ok(())
            } else {
                self.default.exec(cmd, id, target, mode)
            }
        }

        fn exec_json(
            &self,
            cmd: ProcessBuilder,
            id: PackageId,
            target: &Target,
            mode: CompileMode,
            handle_stdout: &mut FnMut(&str) -> CargoResult<()>,
            handle_stderr: &mut FnMut(&str) -> CargoResult<()>,
        ) -> CargoResult<()> {
            if self.maybe_record_cmd(&cmd, &id, target) {
                Ok(())
            } else {
                self.default
                    .exec_json(cmd, id, target, mode, handle_stdout, handle_stderr)
            }
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
        target_pkg: ws.current().unwrap().package_id().clone(),
        pkg_args: Mutex::new(None),
    });
    let exec_dyn: Arc<Executor> = exec.clone();

    let _ = ops::compile_with_exec(&ws, &compile_opts, &exec_dyn);

    let g = exec.pkg_args.lock().unwrap();
    let opt_args = g.as_ref().unwrap();

    let mut args = Vec::with_capacity(1 + opt_args.len());
    let rustc = config.rustc(Some(&ws)).unwrap();
    args.push(get_rustc_executable(&rustc.path));
    args.extend(opt_args.iter().cloned());
    info!("cargo-provided rustc args = {:?}", args);
    args
}

#[cfg_attr(feature = "profile", flame)]
pub fn lib_main(opts: Options) -> interface::Result<()> {
    env_logger::init();
    info!("Begin refactoring");

    // Make sure we compile with the toolchain version that the refactoring tool
    // is built against.
    if let Some(toolchain_ver) = option_env!("RUSTUP_TOOLCHAIN") {
        env::set_var("RUSTUP_TOOLCHAIN", toolchain_ver);
    }

    rustc_driver::report_ices_to_stderr_if_any(move || main_impl(opts)).and_then(|x| x)
}

fn main_impl(opts: Options) -> interface::Result<()> {
    let mut marks = HashSet::new();
    for m in &opts.marks {
        let label = m.label.as_ref().map_or("target", |s| s).into_symbol();
        marks.insert((NodeId::from_usize(m.id), label));
    }

    let rustc_args = get_rustc_arg_strings(opts.rustc_args.clone());

    // TODO: interface::run_compiler() here and create a RefactorState with the
    // callback. RefactorState should know how to reset the compiler when needed
    // and can handle querying the compiler.

    if opts.cursors.len() > 0 {
        let config = driver::create_config(&rustc_args);
        driver::run_compiler(config, None, |compiler| {
            let expanded_crate = compiler.expansion().unwrap().take().0;
            for c in &opts.cursors {
                let kind_result = c.kind.clone().map_or(Ok(pick_node::NodeKind::Any), |s| {
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

    let config = driver::create_config(&rustc_args);

    if opts.commands.len() == 1 && opts.commands[0].name == "interact" {
        interact::interact_command(&opts.commands[0].args, config, cmd_reg);
    } else if opts.commands.len() == 1 && opts.commands[0].name == "script" {
        assert_eq!(opts.commands[0].args.len(), 1);
        scripting::run_lua_file(
            Path::new(&opts.commands[0].args[0]),
            config,
            cmd_reg,
            opts.rewrite_modes,
        )
        .expect("Error loading user script");
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

    dump_profile();

    Ok(())
}

#[cfg(feature = "profile")]
fn dump_profile() {
    flame::dump_html(&mut std::fs::File::create("flame-graph.html").unwrap()).unwrap();
}

#[cfg(not(feature = "profile"))]
fn dump_profile() {}
