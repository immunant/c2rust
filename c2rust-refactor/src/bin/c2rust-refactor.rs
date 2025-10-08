use clap::{load_yaml, App, ArgMatches};
use log::info;
use std::fs::File;
use std::io::Read;
use std::process;
use std::str::FromStr;

use c2rust_refactor::{file_io, CargoTarget, Command, Cursor, Mark, Options, RustcArgSource};

fn main() {
    let yaml = load_yaml!("../refactor.yaml");
    let args = App::from_yaml(yaml).get_matches();

    let opts = match parse_opts(&args) {
        Some(x) => x,
        None => return,
    };

    let ret = match c2rust_refactor::lib_main(opts) {
        Ok(()) => 0,
        Err(_) => 1,
    };
    process::exit(ret);
}

fn parse_opts(args: &ArgMatches) -> Option<Options> {
    // Parse rewrite mode
    let rewrite_modes = match args.values_of("rewrite-mode") {
        Some(values) => values
            .map(|s| match s {
                "inplace" => file_io::OutputMode::InPlace,
                "alongside" => file_io::OutputMode::Alongside,
                "print" => file_io::OutputMode::Print,
                "diff" => file_io::OutputMode::PrintDiff,
                "json" => file_io::OutputMode::Json,
                "marks" => file_io::OutputMode::Marks,
                _ => unreachable!(),
            })
            .collect(),
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
            }
        };

        let line = match parts.next().map(|s| u32::from_str(s).map_err(|_| s)) {
            Some(Ok(x)) => x,
            Some(Err(s)) => {
                info!("Bad cursor line number: {:?}", s);
                return None;
            }
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
            }
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
            }
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
        Some(args) => RustcArgSource::CmdLine(args),
        None => {
            assert!(args.is_present("cargo"));
            let target = if let Some(bin) = args.value_of("bin") {
                CargoTarget::Bin(bin.to_string())
            } else if args.is_present("bins") {
                CargoTarget::AllBins
            } else if args.is_present("lib") {
                CargoTarget::Lib
            } else {
                CargoTarget::All
            };
            RustcArgSource::Cargo(target)
        }
    };

    // Parse command names + args
    let transforms_file = match args.value_of("transforms-file") {
        Some(file_name) => {
            let mut file = File::open(file_name).unwrap_or_else(|e| {
                panic!("Could not open transforms file {:?}: {}", file_name, e);
            });
            let mut buf = String::new();
            file.read_to_string(&mut buf).unwrap_or_else(|e| {
                panic!("Could not read transforms file {:?}: {}", file_name, e);
            });
            buf
        }
        None => String::new(),
    };
    let transforms: Box<dyn Iterator<Item = String>> = match args.value_of("transforms-file") {
        Some(_) => Box::new(shlex::Shlex::new(&transforms_file)),
        None => Box::new(args.values_of("transforms").unwrap().map(String::from)),
    };
    let mut commands = Vec::new();
    let mut cur_command = None;
    for arg in transforms {
        if arg == ";" {
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
        rewrite_modes,
        commands,
        rustc_args,
        cursors,
        marks,
        plugins,
        plugin_dirs,
    })
}
