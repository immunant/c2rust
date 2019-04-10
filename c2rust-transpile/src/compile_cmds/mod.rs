use std::collections::HashSet;
use std::ffi::OsStr;
use std::fs::File;
use std::path::{Path, PathBuf};

use failure::Error;
use regex::Regex;

#[derive(Deserialize, Debug)]
pub struct CompileCmd {
    /// The working directory of the compilation. All paths specified in the command
    /// or file fields must be either absolute or relative to this directory.
    directory: PathBuf,
    /// The main translation unit source processed by this compilation step. This is
    /// used by tools as the key into the compilation database. There can be multiple
    /// command objects for the same file, for example if the same source file is compiled
    /// with different configurations.
    pub file: PathBuf,
    /// The compile command executed. After JSON unescaping, this must be a valid command
    /// to rerun the exact compilation step for the translation unit in the environment
    /// the build system uses. Parameters use shell quoting and shell escaping of quotes,
    /// with ‘"’ and ‘\’ being the only special characters. Shell expansion is not supported.
    command: Option<String>,
    /// The compile command executed as list of strings. Either arguments or command is required.
    #[serde(default)]
    arguments: Vec<String>,
    /// The name of the output created by this compilation step. This field is optional. It can
    /// be used to distinguish different processing modes of the same input file.
    output: Option<String>,
}

impl CompileCmd {
    pub fn abs_file(&self) -> PathBuf {
        match self.file.is_absolute() {
            true => self.file.clone(),
            false => self.directory.join(&self.file),
        }
    }
}

///GNU GCC treats all of the following extensions as C++
const CPP_EXTS: [&str; 7] = ["C", "cc", "cpp", "CPP", "c++", "cp", "cxx"];

fn filter_likely_cpp(cmds: Vec<CompileCmd>) -> Vec<CompileCmd> {
    let mut cpp_exts: HashSet<&OsStr> = HashSet::new();
    cpp_exts.extend(CPP_EXTS.iter().map(OsStr::new));

    cmds.into_iter()
        .filter(|c| {
            let key = c.file.extension().unwrap();
            !cpp_exts.contains(key)
        })
        .collect::<Vec<CompileCmd>>()
}

/// some build scripts repeatedly compile the same input file with different
/// command line flags thus creating multiple outputs. We remove any duplicates
/// in the order we see them and warn the user.
fn filter_duplicate_cmds(v: Vec<CompileCmd>) -> Vec<CompileCmd> {
    let mut seen = HashSet::new();
    let mut cmds = vec![];

    for cmd in v {
        if seen.contains(&cmd.file) {
            eprintln!(
                "warning: skipping duplicate compilation cmd for {}",
                cmd.file.to_str().unwrap()
            );
            continue;
        }
        seen.insert(cmd.file.clone());
        cmds.push(cmd)
    }

    cmds
}

/// Read `compile_commands` file and optionally ignore any entries not matching `filter`.
pub fn get_compile_commands(
    compile_commands: &Path,
    filter: &Option<Regex>,
) -> Result<Vec<CompileCmd>, Error> {
    let f = File::open(compile_commands)?; // open read-only

    // Read the JSON contents of the file as an instance of `Value`
    let v: Vec<CompileCmd> = serde_json::from_reader(f)?;

    // apply the filter argument, if any
    let v = if let &Some(ref re) = filter {
        v.into_iter()
            .filter(|c| re.is_match(c.file.to_str().unwrap()))
            .collect::<Vec<CompileCmd>>()
    } else {
        v
    };

    let v = filter_likely_cpp(v);

    let v = filter_duplicate_cmds(v);

    Ok(v)
}
