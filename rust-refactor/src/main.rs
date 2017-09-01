#![feature(rustc_private)]
extern crate env_logger;
extern crate getopts;
extern crate idiomize;
#[macro_use] extern crate log;
extern crate syntax;

use std::collections::HashSet;
use std::str::FromStr;
use syntax::ast::NodeId;

use idiomize::{
    file_rewrite, driver, transform, span_fix, rewrite, pick_node, interact, command, mark_adjust,
    plugin, select, analysis, script, print_spans
};

use idiomize::command::CommandState;
use idiomize::util::IntoSymbol;



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

struct Options {
    rewrite_mode: file_rewrite::RewriteMode,
    commands: Vec<Command>,
    rustc_args: Vec<String>,
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


    // Separate idiomize args from rustc args
    let (local_args, mut rustc_args) = match find(&argv, "--") {
        Some(idx) => {
            let mut argv = argv;
            let rest = argv.split_off(idx);
            (argv, rest)
        },
        None => {
            info!("Expected `--` followed by rustc arguments");
            print_usage(&argv[0], &opts);
            return None;
        },
    };

    // Replace "--" with the program name
    rustc_args[0] = "rustc".to_owned();


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
            "inplace" => file_rewrite::RewriteMode::InPlace,
            "alongside" => file_rewrite::RewriteMode::Alongside,
            "print" => file_rewrite::RewriteMode::Print,
            _ => {
                info!("Unknown rewrite mode: {}", mode_str);
                return None;
            },
        },
        None => file_rewrite::RewriteMode::Print,
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

fn main() {
    env_logger::init().unwrap();

    let args = std::env::args().collect::<Vec<_>>();
    let opts = match parse_opts(args) {
        Some(x) => x,
        None => return,
    };

    let mut marks = HashSet::new();
    for m in &opts.marks {
        let label = m.label.as_ref().map_or("target", |s| s).into_symbol();
        marks.insert((NodeId::new(m.id), label));
    }

    if opts.cursors.len() > 0 {
        driver::run_compiler(&opts.rustc_args, None, driver::Phase::Phase2, |krate, cx| {
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
    command::register_misc_commands(&mut cmd_reg);
    transform::register_transform_commands(&mut cmd_reg);
    mark_adjust::register_commands(&mut cmd_reg);
    print_spans::register_commands(&mut cmd_reg);
    select::register_commands(&mut cmd_reg);
    analysis::register_commands(&mut cmd_reg);

    plugin::load_plugins(&opts.plugin_dirs, &opts.plugins, &mut cmd_reg);

    if opts.commands.len() == 1 && opts.commands[0].name == "interact" {
        interact::interact_command(&opts.commands[0].args,
                                   opts.rustc_args,
                                   cmd_reg);
    } else {
        let mut state = script::RefactorState::new(opts.rustc_args, cmd_reg, marks);
        let rewrite_mode = opts.rewrite_mode;
        state.rewrite_handler(move |fm, s| {
            file_rewrite::rewrite_mode_callback(rewrite_mode, fm, s);
        });

        for cmd in opts.commands.clone() {
            if &cmd.name == "interact" {
                panic!("`interact` must be the only command");
            } else {
                state.run(&cmd.name, &cmd.args);
            }
        }
    }
}
