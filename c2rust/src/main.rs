use anyhow::anyhow;
use clap::{crate_authors, App, AppSettings, Arg};
use is_executable::IsExecutable;
use std::borrow::Cow;
use std::collections::HashMap;
use std::env;
use std::ffi::OsStr;
use std::path::PathBuf;
use std::process;
use std::process::Command;
use std::str;

/// A `c2rust` sub-command.
struct SubCommand {
    /// The path to the [`SubCommand`]'s executable,
    /// if it was found (see [`Self::find_all`]).
    /// Otherwise [`None`] if it is a known [`SubCommand`] (see [`Self::known`]).
    path: Option<PathBuf>,
    /// The name of the [`SubCommand`], i.e. in `c2rust-{name}`.
    name: Cow<'static, str>,
}

impl SubCommand {
    /// Find all [`SubCommand`]s adjacent to the current (`c2rust`) executable.
    /// They are of the form `c2rust-{name}`.
    pub fn find_all() -> anyhow::Result<Vec<Self>> {
        let c2rust = env::current_exe()?;
        let c2rust_name = c2rust
            .file_name()
            .ok_or_else(|| anyhow!("no file name for c2rust: {}", c2rust.display()))?
            .to_str()
            .ok_or_else(|| anyhow!("c2rust file name is not UTF-8: {}", c2rust.display()))?;
        let dir = c2rust
            .parent()
            .ok_or_else(|| anyhow!("no directory: {}", c2rust.display()))?;
        let mut sub_commands = Vec::new();
        for entry in dir.read_dir()? {
            let entry = entry?;
            let file_type = entry.file_type()?;
            let path = entry.path();
            let name = path
                .file_name()
                .and_then(|name| name.to_str())
                .and_then(|name| name.strip_prefix(c2rust_name))
                .and_then(|name| name.strip_prefix('-'))
                .map(|name| name.to_owned())
                .map(Cow::from)
                .filter(|_| file_type.is_file() || file_type.is_symlink())
                .filter(|_| path.is_executable());
            if let Some(name) = name {
                sub_commands.push(Self {
                    path: Some(path),
                    name,
                });
            }
        }
        Ok(sub_commands)
    }

    /// Get all known [`SubCommand`]s.  These have no [`SubCommand::path`].
    /// Even if the subcommand executables aren't there, we can still suggest them.
    pub fn known() -> impl Iterator<Item = Self> {
        ["transpile", "refactor", "instrument", "pdg", "analyze"]
            .into_iter()
            .map(|name| Self {
                path: None,
                name: name.into(),
            })
    }

    /// Get all known ([`Self::known`]) and actual, found ([`Self::find_all`]) subcommands,
    /// putting the known ones first so that the found ones overwrite them and take precedence.
    pub fn all() -> anyhow::Result<impl Iterator<Item = Self>> {
        Ok(Self::known().chain(Self::find_all()?))
    }

    pub fn invoke<I, S>(&self, args: I) -> anyhow::Result<()>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        let path = self.path.as_ref().ok_or_else(|| {
            anyhow!(
                "known subcommand not found (probably not built): {}",
                self.name
            )
        })?;
        let status = Command::new(path).args(args).status()?;
        process::exit(status.code().unwrap_or(1));
    }
}

fn main() -> anyhow::Result<()> {
    let sub_commands = SubCommand::all()?.collect::<Vec<_>>();
    let sub_commands = sub_commands
        .iter()
        .map(|cmd| (cmd.name.as_ref(), cmd))
        .collect::<HashMap<_, _>>();

    // If the subcommand matches, don't use `clap` at all.
    //
    // I can't seem to get `clap` to pass through all arguments as is,
    // like the ones with hyphens like `--metadata`,
    // even though I've set [`Arg::allow_hyphen_values`].
    // This is faster anyways.
    // I also tried a single "subcommand" argument with [`Arg::possible_values`],
    // but that had the same problem passing through all arguments as well.
    //
    // Furthermore, doing it this way correctly forwards `--help` through to the subcommand
    // instead of `clap` intercepting it and displaying the top-level `--help`.
    let mut args = env::args_os();
    let sub_command = args.nth(1);
    let sub_command = sub_command
        .as_ref()
        .and_then(|arg| arg.to_str())
        .and_then(|name| sub_commands.get(name));

    if let Some(sub_command) = sub_command {
        return sub_command.invoke(args);
    }

    // If we didn't get a subcommand, then use `clap` for parsing and error/help messages.
    let matches = App::new("C2Rust")
        .version(env!("CARGO_PKG_VERSION"))
        .author(crate_authors!(", "))
        .settings(&[AppSettings::SubcommandRequiredElseHelp])
        .subcommands(sub_commands.keys().map(|name| {
            clap::SubCommand::with_name(name).arg(
                Arg::with_name("args")
                    .multiple(true)
                    .allow_hyphen_values(true),
            )
        }))
        .get_matches();
    let sub_command_name = matches
        .subcommand_name()
        .ok_or_else(|| anyhow!("no subcommand"))?;
    sub_commands[sub_command_name].invoke(args)
}
