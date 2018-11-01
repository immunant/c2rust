#![feature(rustc_private)]
extern crate cargo;
extern crate env_logger;
extern crate getopts;
extern crate idiomize;
#[macro_use] extern crate log;
extern crate syntax;
extern crate rustc;
extern crate rustc_data_structures;
extern crate rust_ast_builder;

use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::str::{self, FromStr};
use cargo::util::paths;
use syntax::ast::NodeId;
use rustc::ty;
use rustc_data_structures::sync::Lock;

use idiomize::{
    driver, transform, rewrite, pick_node, interact, command, mark_adjust,
    plugin, select, analysis, print_spans, reflect
};

use rust_ast_builder::IntoSymbol;



#[derive(Clone, Debug)]
struct Cursor {
    file: String,
    line: u32,
    col: u32,
    label: Option<String>,
    kind: Option<String>,
}

#[derive(Clone, Debug)]
struct Mark {
    id: usize,
    label: Option<String>,
}

#[derive(Clone, Debug)]
struct Command {
    name: String,
    args: Vec<String>,
}

#[derive(Clone, Debug)]
enum RustcArgSource {
    CmdLine(Vec<String>),
    Cargo,
}

struct Options {
    rewrite_mode: rewrite::files::RewriteMode,
    commands: Vec<Command>,
    rustc_args: RustcArgSource,
    cursors: Vec<Cursor>,
    marks: Vec<Mark>,

    plugins: Vec<String>,
    plugin_dirs: Vec<String>,
}

fn find<T: PartialEq<U>, U: ?Sized>(xs: &[T], x: &U) -> Option<usize> {
    for i in 0 .. xs.len() {
        if &xs[i] == x {
            return Some(i);
        }
    }
    None
}

fn print_usage(prog: &str, opts: &getopts::Options) {
    let brief = format!("Usage: {} [options] transform [args...] -- [rustc args...]", prog);
    print!("{}", opts.usage(&brief));
}

fn parse_opts(argv: Vec<String>) -> Option<Options> {
    use getopts::{HasArg, Occur};
    let mut opts = getopts::Options::new();
    opts.opt("r", "rewrite-mode",
        "output rewritten code `inplace`, `alongside` the original, \
           or `print` to screen? (default: print)",
        "MODE", HasArg::Yes, Occur::Optional);
    opts.opt("c", "cursor",
        "a cursor position, used to filter some rewrite operations",
        "FILE:LINE:COL[:LABEL[:KIND]]", HasArg::Yes, Occur::Multi);
    opts.opt("m", "mark",
        "a marked node indicated by its ID, and a label for that mark",
        "ID[:LABEL]", HasArg::Yes, Occur::Multi);
    opts.opt("h", "help",
        "display usage information",
        "", HasArg::No, Occur::Optional);
    opts.opt("p", "",
        "name of a plugin to load",
        "PLUGIN", HasArg::Yes, Occur::Multi);
    opts.opt("P", "",
        "search dir for plugins",
        "DIR", HasArg::Yes, Occur::Multi);
    opts.opt("", "cargo",
        "get rustc arguments from cargo",
        "", HasArg::No, Occur::Optional);


    let mut rustc_args = None;

    // Separate idiomize args from rustc args
    let local_args = match find(&argv, "--") {
        Some(idx) => {
            let mut argv = argv;
            let mut rest = argv.split_off(idx);
            // Replace "--" with the full path to rustc
            rest[0] = get_rustc_executable(Path::new("rustc"));
            rustc_args = Some(RustcArgSource::CmdLine(rest));
            argv
        },
        None => {
            argv
        },
    };


    // Parse idiomize args
    let prog = &local_args[0];

    let m = match opts.parse(&local_args[1..]) {
        Ok(m) => m,
        Err(e) => {
            info!("{}", e.to_string());
            return None;
        },
    };

    if m.opt_present("h") {
        print_usage(prog, &opts);
        return None;
    }

    // Parse rewrite mode
    let rewrite_mode = match m.opt_str("rewrite-mode") {
        Some(mode_str) => match &mode_str as &str {
            "inplace" => rewrite::files::RewriteMode::InPlace,
            "alongside" => rewrite::files::RewriteMode::Alongside,
            "print" => rewrite::files::RewriteMode::Print,
            "diff" => rewrite::files::RewriteMode::PrintDiff,
            _ => {
                info!("Unknown rewrite mode: {}", mode_str);
                return None;
            },
        },
        None => rewrite::files::RewriteMode::Print,
    };

    // Parse cursors
    let cursor_strs = m.opt_strs("cursor");
    let mut cursors = Vec::with_capacity(cursor_strs.len());
    for s in &cursor_strs {
        let mut parts = s.split(':');

        let file = match parts.next() {
            Some(x) => x.to_owned(),
            None => {
                info!("Bad cursor string: {:?}", s);
                return None;
            },
        };

        let line = match parts.next().map(|s| u32::from_str(s).map_err(|_| s)) {
            Some(Ok(x)) => x,
            Some(Err(s)) => {
                info!("Bad cursor line number: {:?}", s);
                return None;
            },
            None => {
                info!("Bad cursor string: {:?}", s);
                return None;
            }
        };

        let col = match parts.next().map(|s| u32::from_str(s).map_err(|_| s)) {
            Some(Ok(x)) => x,
            Some(Err(s)) => {
                info!("Bad cursor column number: {:?}", s);
                return None;
            },
            None => {
                info!("Bad cursor string: {:?}", s);
                return None;
            }
        };

        let label = match parts.next() {
            Some(s) if s.len() > 0 => Some(s.to_owned()),
            _ => None,
        };

        let kind = parts.next().map(|s| s.to_owned());

        if parts.next().is_some() {
            info!("Bad cursor string: {:?}", s);
            return None;
        }


        cursors.push(Cursor {
            file: file,
            line: line,
            col: col,
            label: label,
            kind: kind,
        });
    }

    // Parse marks
    let mark_strs = m.opt_strs("mark");
    let mut marks = Vec::with_capacity(mark_strs.len());
    for s in &mark_strs {
        let mut parts = s.split(':');

        let id = match parts.next().map(|s| usize::from_str(s).map_err(|_| s)) {
            Some(Ok(x)) => x,
            Some(Err(s)) => {
                info!("Bad mark node ID: {:?}", s);
                return None;
            },
            None => {
                info!("Bad mark string: {:?}", s);
                return None;
            }
        };

        let label = parts.next().map(|s| s.to_owned());

        if parts.next().is_some() {
            info!("Bad mark string: {:?}", s);
            return None;
        }


        marks.push(Mark {
            id: id,
            label: label,
        });
    }


    // Get plugin options
    let plugins = m.opt_strs("p").to_owned();
    let plugin_dirs = m.opt_strs("P").to_owned();


    // Handle --cargo
    if m.opt_present("cargo") {
        if rustc_args.is_some() {
            println!("can't combine --cargo with explicit rustc args");
            print_usage(prog, &opts);
            return None;
        }
        rustc_args = Some(RustcArgSource::Cargo);
    }

    // Get final rustc_args value
    if rustc_args.is_none() {
        println!("must either provide rustc arguments or set --cargo");
        print_usage(prog, &opts);
        return None;
    }
    let rustc_args = rustc_args.unwrap();


    // Parse command names + args
    let mut commands = Vec::new();
    let mut cur_command = None;
    for arg in m.free {
        if &arg == ";" {
            if let Some(cmd) = cur_command.take() {
                commands.push(cmd);
            } else {
                info!("Expected command before ';'");
                return None;
            }
        } else if cur_command.is_none() {
            cur_command = Some(Command {
                name: arg,
                args: Vec::new(),
            });
        } else {
            cur_command.as_mut().unwrap().args.push(arg);
        }
    }
    if let Some(cmd) = cur_command.take() {
        commands.push(cmd);
    }


    Some(Options {
        rewrite_mode,
        commands,
        rustc_args,
        cursors,
        marks,
        plugins,
        plugin_dirs,
    })
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
    use cargo::core::{Workspace, PackageId, Target};
    use cargo::core::compiler::{CompileMode, Executor, DefaultExecutor, Context, Unit};
    use cargo::core::manifest::TargetKind;
    use cargo::ops;
    use cargo::ops::CompileOptions;
    use cargo::util::{ProcessBuilder, CargoResult};
    use cargo::util::important_paths::find_root_manifest_for_wd;

    match src {
        RustcArgSource::CmdLine(strs) => return strs,
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
        marks.insert((NodeId::new(m.id), label));
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
        let rewrite_mode = opts.rewrite_mode;
        let rw_handler = Box::new(move |fm, s: &str| {
            rewrite::files::rewrite_mode_callback(rewrite_mode, fm, s);
        });

        let mut state = command::RefactorState::from_rustc_args(
            &rustc_args, cmd_reg, Some(rw_handler), None, marks);

        state.load_crate();

        for cmd in opts.commands.clone() {
            if &cmd.name == "interact" {
                panic!("`interact` must be the only command");
            } else {
                state.run(&cmd.name, &cmd.args);
            }
        }

        state.save_crate();
    }
}

fn main() {
    env_logger::init();

    let args = std::env::args().collect::<Vec<_>>();
    let opts = match parse_opts(args) {
        Some(x) => x,
        None => return,
    };

    ty::tls::GCX_PTR.set(&Lock::new(0), || {
        syntax::with_globals(move || {
            main_impl(opts);
        });
    });
}
