#![feature(rustc_private)]
#![feature(label_break_value)]
extern crate dtoa;
extern crate rustc_target;
extern crate serde_cbor;
extern crate syntax;
extern crate syntax_pos;
#[macro_use]
extern crate indexmap;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate c2rust_ast_builder;
extern crate c2rust_ast_exporter;
extern crate clap;
extern crate itertools;
extern crate libc;
extern crate regex;
extern crate serde_json;
#[macro_use]
extern crate log;
extern crate fern;
extern crate strum;
#[macro_use]
extern crate strum_macros;
#[macro_use]
extern crate failure;

#[macro_use]
mod diagnostics;

pub mod build_files;
pub mod c_ast;
pub mod cfg;
pub mod convert_type;
pub mod renamer;
pub mod rust_ast;
pub mod translator;
pub mod with_stmts;

use std::collections::HashSet;
use std::fs::{self, File};
use std::io::prelude::*;
use std::io;
use std::path::{Path, PathBuf};
use std::process;
use std::ffi::OsStr;

use failure::Error;
use regex::Regex;

use c2rust_ast_exporter as ast_exporter;
use c_ast::Printer;
use c_ast::*;
pub use diagnostics::Diagnostic;

use build_files::{get_build_dir, emit_build_files};
use std::prelude::v1::Vec;
pub use translator::ReplaceMode;

/// Configuration settings for the translation process
#[derive(Debug)]
pub struct TranspilerConfig {
    // Debug output options
    pub dump_untyped_context: bool,
    pub dump_typed_context: bool,
    pub pretty_typed_context: bool,
    pub dump_function_cfgs: bool,
    pub json_function_cfgs: bool,
    pub dump_cfg_liveness: bool,
    pub dump_structures: bool,
    // Options that control translation
    pub incremental_relooper: bool,
    pub fail_on_multiple: bool,
    pub filter: Option<Regex>,
    pub debug_relooper_labels: bool,
    pub cross_checks: bool,
    pub cross_check_backend: String,
    pub cross_check_configs: Vec<String>,
    pub prefix_function_names: Option<String>,
    pub translate_asm: bool,
    pub use_c_loop_info: bool,
    pub use_c_multiple_info: bool,
    pub simplify_structures: bool,
    pub panic_on_translator_failure: bool,
    pub emit_modules: bool,
    pub fail_on_error: bool,
    pub replace_unsupported_decls: ReplaceMode,
    pub translate_valist: bool,
    pub overwrite_existing: bool,
    pub reduce_type_annotations: bool,
    pub reorganize_definitions: bool,
    pub enabled_warnings: HashSet<Diagnostic>,
    pub emit_no_std: bool,
    pub output_dir: Option<PathBuf>,

    // Options that control build files
    /// Emit `Cargo.toml` and one of `main.rs`, `lib.rs`
    pub emit_build_files: bool,
    /// Names the translation unit containing the main function
    pub main: Option<String>,
}

const DUPLICATE_CMDS_EMSG: &str = "
Error, expected one compiler invocation per input source file. The compile
commands database (compile_commands.json) contains multiple invocations
for the following input source file(s)";

/// Main entry point to transpiler. Called from CLI tools with the result of
/// clap::App::get_matches().
pub fn transpile(tcfg: TranspilerConfig, cc_db: &Path, extra_clang_args: &[&str]) {
    diagnostics::init(tcfg.enabled_warnings.clone());

    let cmds = get_compile_commands(cc_db).expect(&format!(
        "Could not parse compile commands from {}",
        cc_db.to_string_lossy()
    ));

    // apply the filter argument, if any
    let cmds = match tcfg.filter {
        Some(ref re) => cmds
            .into_iter()
            .filter(|c| re.is_match(c.file.to_str().unwrap()))
            .collect::<Vec<CompileCmd>>(),
        None => cmds,
    };

    // filter out likely C++ files
    let cpp_ext = Some(OsStr::new("cpp"));
    let cmds = cmds
        .into_iter()
        .filter(|c| c.file.extension() != cpp_ext)
        .collect::<Vec<CompileCmd>>();

    // some build scripts repeatedly compile the same input file with different
    // command line flags thus creating multiple outputs. We don't handle such
    // cases since we don't know what compiler flags to use for the generated
    // Rust code or how to link the result.
    let mut sorted = cmds
        .iter()
        .map(|cmd| cmd.abs_file())
        .collect::<Vec<PathBuf>>();
    sorted.sort();
    let mut duplicates = sorted
        .windows(2)
        .filter(|w| w[0] == w[1])
        .map(|w| w[0].to_str().unwrap())
        .collect::<Vec<&str>>();
    duplicates.dedup(); // report each duplicate once
    if duplicates.len() > 0 {
        eprintln!("{}:\n{}", DUPLICATE_CMDS_EMSG, duplicates.join(",\n"));
        return;
    }

    // we may need to specify path to system include dir on macOS
    let clang_args: Vec<String> = get_isystem_args();
    let mut clang_args: Vec<&str> = clang_args
        .iter()
        .map(AsRef::as_ref)
        .collect();
    clang_args.extend_from_slice(extra_clang_args);

    let modules = cmds
        .iter()
        .filter_map(|cmd|
            transpile_single(
                &tcfg,
                cmd.abs_file().as_path(),
                cc_db,
                extra_clang_args))
        .collect::<Vec<PathBuf>>();

    if tcfg.emit_build_files {
        let build_dir = get_build_dir(&tcfg, cc_db);
        let crate_file = emit_build_files(&tcfg, &build_dir, modules);
        // We only run the reorganization refactoring if we emitted a fresh crate file
        if let Some(output_file) = crate_file {
            if tcfg.reorganize_definitions {
                reorganize_definitions(&build_dir, &output_file)
                    .unwrap_or_else(|e| {
                        warn!("Failed to reorganize definitions. {}", e.as_fail());
                    })
            }
        }
    }
}

/// Ensure that clang can locate the system headers on macOS 10.14+.
///
/// MacOS 10.14 does not have a `/usr/include` folder even if Xcode
/// or the command line developer tools are installed as explained in
/// this [thread](https://forums.developer.apple.com/thread/104296).
/// It is possible to install a package which puts the headers in
/// `/usr/include` but the user doesn't have to since we can find
/// the system headers we need by running `xcrun --show-sdk-path`.
fn get_isystem_args() -> Vec<String>  {
    let mut args = vec![];
    if cfg!(target_os = "macos") {
        let usr_incl = Path::new("/usr/include");
        if !usr_incl.exists() {
            let output = process::Command::new("xcrun")
                .args(&["--show-sdk-path"])
                .output()
                .expect("failed to run `xcrun` subcommand");
            let mut sdk_path = String::from_utf8(output.stdout).unwrap();
            let olen = sdk_path.len();
            sdk_path.truncate(olen - 1);
            sdk_path.push_str("/usr/include");

            args.push("-isystem".to_owned());
            args.push(sdk_path);
        }
    }
    args
}

fn invoke_refactor(build_dir: &PathBuf, crate_path: &PathBuf) -> Result<(), Error> {
    // Make sure the crate builds cleanly
    let status = process::Command::new("cargo")
        .args(&["check"])
        .env("RUSTFLAGS", "-Awarnings")
        .current_dir(build_dir)
        .status()?;
    if !status.success() {
        return Err(format_err!("Crate does not compile."));
    }

    // Assumes the subcommand executable is in the same directory as this program.
    let cmd_path = std::env::current_exe().expect("Cannot get current executable path");
    let mut cmd_path = cmd_path.as_path().canonicalize().unwrap();
    cmd_path.pop(); // remove current executable
    cmd_path.push(format!("c2rust-refactor"));
    assert!(cmd_path.exists(), format!("{:?} is missing", cmd_path));
    let crate_path = crate_path
        .strip_prefix(build_dir)
        .unwrap()
        .to_str()
        .unwrap();
    let args = ["--rewrite-mode", "inplace", "reorganize_definitions", "--", crate_path];
    let status = process::Command::new(cmd_path.into_os_string())
        .args(&args)
        .current_dir(build_dir)
        .status()?;
    if status.success() {
        Ok(())
    } else {
        Err(format_err!(
            "Refactoring failed. Please fix errors above and re-run:\n    c2rust refactor {}",
            args.join(" "),
        ))
    }
}

fn reorganize_definitions(build_dir: &PathBuf, crate_path: &PathBuf) -> Result<(), Error> {
    invoke_refactor(build_dir, crate_path)?;
    // fix the formatting of the output of `c2rust-refactor`
    let status = process::Command::new("cargo")
        .args(&["fmt"])
        .current_dir(build_dir)
        .status()?;
    if status.success() {
        Ok(())
    } else {
        Err(format_err!("cargo fmt failed"))
    }
}

#[derive(Deserialize, Debug)]
struct CompileCmd {
    /// The working directory of the compilation. All paths specified in the command
    /// or file fields must be either absolute or relative to this directory.
    directory: PathBuf,
    /// The main translation unit source processed by this compilation step. This is
    /// used by tools as the key into the compilation database. There can be multiple
    /// command objects for the same file, for example if the same source file is compiled
    /// with different configurations.
    file: PathBuf,
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
            true  => self.file.clone(),
            false => self.directory.join(&self.file)
        }
    }
}

fn get_compile_commands(compile_commands: &Path) -> Result<Vec<CompileCmd>, Error> {
    let f = File::open(compile_commands)?; // open read-only

    // Read the JSON contents of the file as an instance of `Value`
    let v = serde_json::from_reader(f)?;

    Ok(v)
}

fn transpile_single(
    tcfg: &TranspilerConfig,
    input_path: &Path,
    cc_db: &Path,
    extra_clang_args: &[&str],
) -> Option<PathBuf> {

    let output_path = get_output_path(tcfg, input_path);
    if output_path.exists() && !tcfg.overwrite_existing {
        println!("Skipping existing file {}", output_path.display());
        return Some(output_path);
    }

    let file = input_path.file_name().unwrap().to_str().unwrap();
    println!("Transpiling {}", file);

    // Extract the untyped AST from the CBOR file
    let untyped_context = match ast_exporter::get_untyped_ast(input_path, cc_db, extra_clang_args) {
        Err(e) => {
            eprintln!("Error: {:}", e);
            process::exit(1);
        }
        Ok(cxt) => cxt,
    };

    if tcfg.dump_untyped_context {
        println!("CBOR Clang AST");
        println!("{:#?}", untyped_context);
    }

    // Convert this into a typed AST
    let typed_context = {
        let mut conv = ConversionContext::new(&untyped_context);
        conv.convert(&untyped_context);
        conv.typed_context
    };

    if tcfg.dump_typed_context {
        println!("Clang AST");
        println!("{:#?}", typed_context);
    }

    if tcfg.pretty_typed_context {
        println!("Pretty-printed Clang AST");
        println!("{:#?}", Printer::new(io::stdout()).print(&typed_context));
    }

    // Perform the translation
    let main_file = input_path.with_extension("");
    let translated_string = translator::translate(typed_context, &tcfg, main_file);

    let mut file = match File::create(&output_path) {
        Ok(file) => file,
        Err(e) => panic!("Unable to open file for writing: {}", e),
    };

    match file.write_all(translated_string.as_bytes()) {
        Ok(()) => (),
        Err(e) => panic!("Unable to write translation to file: {}", e),
    };

    Some(output_path)
}

fn get_output_path(
    tcfg: &TranspilerConfig,
    input_path: &Path,
) -> PathBuf {
    let mut path_buf = PathBuf::from(input_path);

    // When an output file name is not explictly specified, we should convert files
    // with dashes to underscores, as they are not allowed in rust file names.
    let file_name = path_buf
        .file_name()
        .unwrap()
        .to_str()
        .unwrap()
        .replace('-', "_");

    path_buf.set_file_name(file_name);
    path_buf.set_extension("rs");

    if let Some(output_dir) = &tcfg.output_dir {
        // Place the source files in output_dir/src/
        let mut output_path = output_dir.clone();
        output_path.push("src");
        if !output_path.exists() {
            fs::create_dir_all(&output_path).expect(&format!(
                "couldn't create source directory: {}",
                output_path.display()
            ));
        }
        // FIXME: replicate the subdirectory structure as well???
        // this currently puts all the output files in the same directory
        output_path.push(path_buf.file_name().unwrap());
        output_path
    } else {
        path_buf
    }
}
