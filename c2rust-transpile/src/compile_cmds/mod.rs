use std::collections::{HashMap, HashSet};
use std::ffi::OsStr;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use failure::Error;
use regex::Regex;

#[derive(Deserialize, Debug, Default, Clone)]
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
            false => {
                let path = self.directory.join(&self.file);
                let e = format!("could not canonicalize {}", path.display());
                path.canonicalize().expect(&e)
            },
        }
    }
}

#[derive(Deserialize, Debug, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum LinkType {
    Exe,
    Shared,
    Static,
}

impl LinkType {
    pub fn is_library(&self) -> bool {
        match self {
            LinkType::Exe => false,
            LinkType::Shared => true,
            LinkType::Static => true,
        }
    }

    pub fn as_cargo_types(&self) -> &str {
        match self {
            LinkType::Exe => "\"rlib\"",
            LinkType::Shared => "\"cdylib\"",
            LinkType::Static => "\"staticlib\", \"rlib\"",
        }
    }
}

#[derive(Deserialize, Debug)]
pub struct LinkCmd {
    /// All input files going into this link
    pub inputs: Vec<String>,
    /// The output file; this is taken from the `CompileCmd`
    #[serde(default)]
    pub output: Option<String>,
    /// List of libraries to link in (without `-l` prefix)
    pub libs: Vec<String>,
    /// List of library directories
    pub lib_dirs: Vec<PathBuf>,
    /// What type of binary we're building
    pub r#type: LinkType,
    /// Input files in `CompileCmd` form
    #[serde(default)]
    pub cmd_inputs: Vec<Rc<CompileCmd>>,
    #[serde(default)]
    pub top_level: bool,
}

/// Convert a linear vector of `CompileCmd`s into a DAG of `LinkCmd`s and `CompileCmd`s
fn build_link_commands(mut v: Vec<Rc<CompileCmd>>) -> Result<Vec<LinkCmd>, Error> {
    let mut output_map = HashMap::new();
    for (idx, ccmd) in v.iter().enumerate() {
        if let Some(ref output) = ccmd.output {
            output_map.insert(output, idx);
        }
    }

    let mut seen_ccmds = HashSet::new();
    let mut res = vec![];
    for (idx, ccmd) in v.iter().enumerate() {
        let lcmd = match ccmd.file.strip_prefix("/c2rust/link/") {
            Ok(lcmd) => lcmd.to_str().unwrap(),
            Err(_) => continue
        };
        let mut lcmd: LinkCmd = serde_bencode::from_str(lcmd)?;

        lcmd.output = ccmd.output.clone();
        for inp in &lcmd.inputs {
            if let Some(ccmd_idx) = output_map.get(&inp) {
                let inp_ccmd = Rc::clone(&v[*ccmd_idx]);
                lcmd.cmd_inputs.push(inp_ccmd);
                seen_ccmds.insert(*ccmd_idx);
            }
        }

        res.push(lcmd);
        seen_ccmds.insert(idx);
    }

    // TODO: add binaries

    // Check if we have left-over compile commands; if we do,
    // bind them to the crate itself (which becomes a `staticlib` or `rlib`)
    let mut idx = 0;
    v.retain(|_| { idx += 1; !seen_ccmds.contains(&(idx - 1)) });
    if !v.is_empty() {
        let lcmd = LinkCmd {
            // FIXME: this doesn't catch all of them; do we need to???
            inputs: v.iter().filter_map(|ccmd| ccmd.output.clone()).collect(),
            output: None,
            libs: vec![],
            lib_dirs: vec![],
            r#type: LinkType::Static,
            cmd_inputs: v,
            top_level: true,
        };
        res.push(lcmd);
    }

    Ok(res)
}

///GNU GCC treats all of the following extensions as C++
const CPP_EXTS: [&str; 7] = ["C", "cc", "cpp", "CPP", "c++", "cp", "cxx"];
const ASM_EXTS: [&str; 3] = ["S", "s", "asm"];

/// The compile commmands database may contain C++ and assembly files
/// that we are unable to process. Detect and filter out such entries.
fn filter_likely_unsupported(cmds: Vec<Rc<CompileCmd>>) -> Vec<Rc<CompileCmd>> {
    let mut unsupported_exts: HashSet<&OsStr> = HashSet::new();
    unsupported_exts.extend(CPP_EXTS.iter().map(OsStr::new));
    unsupported_exts.extend(ASM_EXTS.iter().map(OsStr::new));

    cmds.into_iter()
        .filter(|c| {
            let key = c.file.extension().unwrap();
            !unsupported_exts.contains(key)
        })
        .collect::<Vec<Rc<CompileCmd>>>()
}

/// some build scripts repeatedly compile the same input file with different
/// command line flags thus creating multiple outputs. We remove any duplicates
/// in the order we see them and warn the user.
fn filter_duplicate_cmds(v: Vec<Rc<CompileCmd>>) -> Vec<Rc<CompileCmd>> {
    let mut seen = HashSet::new();
    let mut cmds = vec![];

    for cmd in v {
        let absf = cmd.abs_file();
        if seen.contains(&absf) {
            warn!("Skipping duplicate compilation cmd for {}", absf.display());
            continue;
        }
        seen.insert(absf);
        cmds.push(cmd)
    }

    cmds
}

/// Read `compile_commands` file and optionally ignore any entries not matching `filter`.
pub fn get_compile_commands(
    compile_commands: &Path,
    filter: &Option<Regex>,
) -> Result<Vec<LinkCmd>, Error> {
    let f = File::open(compile_commands)?; // open read-only

    // Read the JSON contents of the file as an instance of `Value`
    let v: Vec<Rc<CompileCmd>> = serde_json::from_reader(f)?;

    // apply the filter argument, if any
    let v = if let &Some(ref re) = filter {
        v.into_iter()
            .filter(|c| re.is_match(c.file.to_str().unwrap()))
            .collect::<Vec<Rc<CompileCmd>>>()
    } else {
        v
    };

    let mut v = build_link_commands(v)?; 

    for cmd in &mut v {
        let v = std::mem::replace(&mut cmd.cmd_inputs, vec![]);
        let v = filter_likely_unsupported(v);
        let v = filter_duplicate_cmds(v);
        cmd.cmd_inputs = v;
    }

    Ok(v)
}
