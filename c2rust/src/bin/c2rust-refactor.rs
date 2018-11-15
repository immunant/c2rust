extern crate env_logger;
#[macro_use] extern crate log;
#[macro_use] extern crate clap;
extern crate c2rust_refactor;

use std::str::FromStr;
use clap::{App, ArgMatches};

use c2rust_refactor::{file_io, RustcArgSource, Options, Cursor, Mark, Command};

fn main() {
    let yaml = load_yaml!("../refactor.yaml");
    let args = App::from_yaml(yaml).get_matches();

    let opts = match parse_opts(&args) {
        Some(x) => x,
        None => return,
    };

    c2rust_refactor::lib_main(opts);
}

fn parse_opts(args: &ArgMatches) -> Option<Options> {
    // Parse rewrite mode
    let rewrite_modes = match args.values_of("rewrite-mode") {
        Some(values) => values.map(|s| match s {
            "inplace" => file_io::OutputMode::InPlace,
            "alongside" => file_io::OutputMode::Alongside,
            "print" => file_io::OutputMode::Print,
            "diff" => file_io::OutputMode::PrintDiff,
            "json" => file_io::OutputMode::Json,
            _ => unreachable!(),
        }).collect(),
        None => vec![file_io::OutputMode::Print],
    };

    // Parse cursors
    let cursor_strs = args.values_of_lossy("cursor").unwrap_or(vec![]);
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


        cursors.push(Cursor::new(file, line, col, label, kind));
    }

    // Parse marks
    let mark_strs = args.values_of_lossy("mark").unwrap_or(vec![]);
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


        marks.push(Mark::new(id, label));
    }


    // Get plugin options
    let plugins = args.values_of_lossy("plugin-name").unwrap_or(vec![]);
    let plugin_dirs = args.values_of_lossy("plugin-dir").unwrap_or(vec![]);


    // Handle --cargo and rustc-args
    let rustc_args = match args.values_of_lossy("rustc-args") {
        Some(args) => {
            RustcArgSource::CmdLine(args)
        }
        None => {
            assert!(args.is_present("cargo"));
            RustcArgSource::Cargo
        }
    };

    // Parse command names + args
    let mut commands = Vec::new();
    let mut cur_command = None;
    for arg in args.values_of("transforms").unwrap() {
        if arg == ";" {
            if let Some(cmd) = cur_command.take() {
                commands.push(cmd);
            } else {
                info!("Expected command before ';'");
                return None;
            }
        } else if cur_command.is_none() {
            cur_command = Some(Command {
                name: arg.to_string(),
                args: Vec::new(),
            });
        } else {
            cur_command.as_mut().unwrap().args.push(arg.to_string());
        }
    }
    if let Some(cmd) = cur_command.take() {
        commands.push(cmd);
    }


    Some(Options {
        rewrite_modes,
        commands,
        rustc_args,
        cursors,
        marks,
        plugins,
        plugin_dirs,
    })
}
