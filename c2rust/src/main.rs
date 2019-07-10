#[macro_use(crate_version, crate_authors, load_yaml)]
extern crate clap;
use clap::{App, AppSettings, SubCommand};
use std::env;
use std::ffi::OsStr;
use std::process::{exit, Command};

fn main() {
    let subcommand_yamls = [
        load_yaml!("transpile.yaml"),
        load_yaml!("refactor.yaml"),
        load_yaml!("analysis.yaml"),
    ];
    let matches = App::new("C2Rust")
        .version(crate_version!())
        .author(crate_authors!(", "))
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .subcommands(
            subcommand_yamls
                .iter()
                .map(|yaml| SubCommand::from_yaml(yaml)),
        )
        .get_matches();

    let mut os_args = env::args_os();
    os_args.next(); // Skip executable name
    let arg_name = os_args.next().and_then(|name| name.into_string().ok());
    match (&arg_name, matches.subcommand_name()) {
        (Some(arg_name), Some(subcommand)) if arg_name == subcommand => {
            invoke_subcommand(&subcommand, os_args);
        }
        _ => {
            eprintln!("{:?}", arg_name);
            panic!("Could not match subcommand");
        }
    };
}

fn invoke_subcommand<I, S>(subcommand: &str, args: I)
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let mut ld_library_path = String::from(env!("RUSTLIB"));
    if let Ok(old_library_path) = env::var("LD_LIBRARY_PATH") {
        ld_library_path = format!("{}:{}", ld_library_path, old_library_path);
    }

    // Assumes the subcommand executable is in the same directory as this driver
    // program.
    let cmd_path = std::env::current_exe().expect("Cannot get current executable path");
    let mut cmd_path = cmd_path.as_path().canonicalize().unwrap();
    cmd_path.pop(); // remove current executable
    cmd_path.push(format!("c2rust-{}", subcommand));
    assert!(cmd_path.exists(), format!("{:?} is missing", cmd_path));
    exit(
        Command::new(cmd_path.into_os_string())
            .args(args)
            .env("LD_LIBRARY_PATH", ld_library_path)
            .status()
            .expect("SubCommand failed to start")
            .code()
            .unwrap_or(-1),
    );
}
