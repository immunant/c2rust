#![feature(
    rustc_private,
    trace_macros,
)]
extern crate arena;
extern crate bincode;
extern crate ena;
extern crate indexmap;
extern crate libc;
extern crate cargo;
extern crate clap;
extern crate diff;
extern crate env_logger;
#[macro_use]
extern crate error_chain;
#[macro_use] extern crate json;
#[macro_use] extern crate log;
extern crate regex;
extern crate rustc;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_metadata;
extern crate rustc_privacy;
extern crate rustc_resolve;
extern crate rustc_target;
extern crate rustc_codegen_utils;
#[macro_use] extern crate smallvec;
extern crate syntax;
extern crate syntax_ext;
extern crate syntax_pos;
extern crate c2rust_ast_builder;
extern crate c2rust_analysis_rt;

#[macro_use] mod macros;

pub mod ast_manip;

pub mod util;

pub mod rewrite;

pub mod analysis;

pub mod span_fix;
pub mod pick_node;

pub mod path_edit;
pub mod illtyped;
pub mod api;
pub mod contains_mark;
pub mod reflect;
pub mod type_map;
pub mod resolve;

pub mod matcher;

pub mod driver;
pub mod collapse;
pub mod node_map;

pub mod command;
pub mod file_io;
pub mod interact;
pub mod plugin;

pub mod transform;
pub mod mark_adjust;
pub mod select;
pub mod print_spans;

use std::collections::HashSet;
use std::env;
use std::path::{Path, PathBuf};
use std::str::{self, FromStr};
use std::sync::Arc;
use cargo::util::paths;
use syntax::ast::NodeId;
use rustc::ty;
use rustc_data_structures::sync::Lock;

use c2rust_ast_builder::IntoSymbol;



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
        Mark {
            id,
            label,
        }
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

fn get_rustc_executable(path: &Path) -> String {
    use std::process::{Command, Stdio};

    let resolved = paths::resolve_executable(path).unwrap();
    if let Some(rustup_path) = get_rustup_path(&resolved) {
        let proc = Command::new(rustup_path).arg("which").arg("rustc")
            .stdout(Stdio::piped())
            .spawn().unwrap();
        let output = proc.wait_with_output().unwrap();
        assert!(output.status.success());
        let s = str::from_utf8(&output.stdout).unwrap();
        return s.trim().to_owned();
    }

    resolved.to_str().unwrap().to_owned()
}

fn get_rustc_arg_strings(src: RustcArgSource) -> Vec<String> {
    use std::sync::{Arc, Mutex};
    use cargo::Config;
    use cargo::core::{Workspace, PackageId, Target, maybe_allow_nightly_features};
    use cargo::core::compiler::{CompileMode, Executor, DefaultExecutor, Context, Unit};
    use cargo::core::manifest::TargetKind;
    use cargo::ops;
    use cargo::ops::CompileOptions;
    use cargo::util::{ProcessBuilder, CargoResult};
    use cargo::util::important_paths::find_root_manifest_for_wd;

    // `cargo`-built `libcargo` is always on the `dev` channel, so `maybe_allow_nightly_features`
    // really does allow nightly features.
    maybe_allow_nightly_features();

    match src {
        RustcArgSource::CmdLine(mut args) => {
            let mut rustc_args = vec!(get_rustc_executable(Path::new("rustc")));
            rustc_args.append(&mut args);
            return rustc_args;
        }
        RustcArgSource::Cargo => {},
    }

    let config = Config::default().unwrap();
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
        fn maybe_record_cmd(&self,
                            cmd: &ProcessBuilder,
                            id: &PackageId,
                            target: &Target) {
            if id != &self.target_pkg {
                return;
            }

            let mut g = self.pkg_args.lock().unwrap();
            match target.kind() {
                // `lib` builds take priority.  Otherwise, take the first available bin.
                &TargetKind::Lib(_) => {},
                &TargetKind::Bin if g.is_none() => {},
                _ => return,
            }

            let args = cmd.get_args().iter()
                .map(|os| os.to_str().unwrap().to_owned())
                .collect();
            *g = Some(args);
        }
    }

    impl Executor for LoggingExecutor {
        fn init(&self, cx: &Context, unit: &Unit) {
            self.default.init(cx, unit);
        }

        fn exec(&self,
                cmd: ProcessBuilder,
                id: &PackageId,
                target: &Target,
                mode: CompileMode) -> CargoResult<()> {
            self.maybe_record_cmd(&cmd, id, target);
            self.default.exec(cmd, id, target, mode)
        }

        fn exec_json(&self,
                     cmd: ProcessBuilder,
                     id: &PackageId,
                     target: &Target,
                     mode: CompileMode,
                     handle_stdout: &mut FnMut(&str) -> CargoResult<()>,
                     handle_stderr: &mut FnMut(&str) -> CargoResult<()>) -> CargoResult<()> {
            self.maybe_record_cmd(&cmd, id, target);
            self.default.exec_json(cmd, id, target, mode, handle_stdout, handle_stderr)
        }

        fn force_rebuild(&self, unit: &Unit) -> bool {
            if unit.pkg.package_id() == &self.target_pkg {
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

    ops::compile_with_exec(&ws, &compile_opts, &exec_dyn).unwrap();

    let g = exec.pkg_args.lock().unwrap();
    let opt_args = g.as_ref().unwrap();

    let mut args = Vec::with_capacity(1 + opt_args.len());
    let rustc = config.rustc(Some(&ws)).unwrap();
    args.push(get_rustc_executable(&rustc.path));
    args.extend(opt_args.iter().cloned());
    info!("cargo-provided rustc args = {:?}", args);
    args
}

fn main_impl(opts: Options) {
    let mut marks = HashSet::new();
    for m in &opts.marks {
        let label = m.label.as_ref().map_or("target", |s| s).into_symbol();
        marks.insert((NodeId::from_usize(m.id), label));
    }

    let rustc_args = get_rustc_arg_strings(opts.rustc_args.clone());

    if opts.cursors.len() > 0 {
        driver::run_compiler(&rustc_args, None, driver::Phase::Phase2, |krate, cx| {
            for c in &opts.cursors {
                let kind_result = c.kind.clone().map_or(Ok(pick_node::NodeKind::Any),
                                                        |s| pick_node::NodeKind::from_str(&s));
                let kind = match kind_result {
                    Ok(k) => k,
                    Err(_) => {
                        info!("Bad cursor kind: {:?}", c.kind.as_ref().unwrap());
                        continue;
                    },
                };

                let id = match pick_node::pick_node_at_loc(
                        &krate, &cx, kind, &c.file, c.line, c.col) {
                    Some(info) => info.id,
                    None => {
                        info!("Failed to find {:?} at {}:{}:{}",
                                 kind, c.file, c.line, c.col);
                        continue;
                    },
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

    if opts.commands.len() == 1 && opts.commands[0].name == "interact" {
        interact::interact_command(&opts.commands[0].args,
                                   rustc_args,
                                   cmd_reg);
    } else {
        let mut state = command::RefactorState::from_rustc_args(
            &rustc_args,
            cmd_reg,
            Arc::new(file_io::RealFileIO::new(opts.rewrite_modes)),
            marks,
        );

        state.load_crate();

        for cmd in opts.commands.clone() {
            if &cmd.name == "interact" {
                panic!("`interact` must be the only command");
            } else {
                match state.run(&cmd.name, &cmd.args) {
                    Ok(_)=> {},
                    Err(e) => {
                        eprintln!("{:?}", e);
                        std::process::exit(1);
                    }

                }
            }
        }

        state.save_crate();
    }
}

pub fn lib_main(opts: Options) {
    env_logger::init();

    // Make sure we compile with the toolchain version that the refactoring tool
    // is built against.
    env::set_var("RUSTUP_TOOLCHAIN", env!("RUSTUP_TOOLCHAIN"));

    ty::tls::GCX_PTR.set(&Lock::new(0), || {
        syntax::with_globals(move || {
            main_impl(opts);
        });
    });
}
