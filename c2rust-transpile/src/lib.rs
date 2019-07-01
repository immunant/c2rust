#![feature(rustc_private)]
#![feature(label_break_value)]

extern crate colored;
extern crate dtoa;
extern crate rustc_data_structures;
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
mod compile_cmds;
pub mod convert_type;
pub mod renamer;
pub mod rust_ast;
pub mod translator;
pub mod with_stmts;
mod source_map;

use std::collections::HashSet;
use std::fs::{self, File};
use std::io;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::process;

use failure::Error;
use regex::Regex;

use crate::c_ast::Printer;
use crate::c_ast::*;
pub use crate::diagnostics::Diagnostic;
use c2rust_ast_exporter as ast_exporter;

use crate::build_files::{emit_build_files, get_build_dir};
use crate::compile_cmds::get_compile_commands;
pub use crate::translator::ReplaceMode;
use std::prelude::v1::Vec;

type PragmaVec = Vec<(&'static str, Vec<&'static str>)>;
type PragmaSet = indexmap::IndexSet<(&'static str, &'static str)>;
type CrateSet = indexmap::IndexSet<&'static str>;
type TranspileResult = (PathBuf, Option<PragmaVec>, Option<CrateSet>);

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
    pub verbose: bool,
    pub debug_ast_exporter: bool,

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
    pub translate_const_macros: bool,
    pub disable_refactoring: bool,

    // Options that control build files
    /// Emit `Cargo.toml` and one of `main.rs`, `lib.rs`
    pub emit_build_files: bool,
    /// Names the translation unit containing the main function
    pub main: Option<String>,
}

/// Main entry point to transpiler. Called from CLI tools with the result of
/// clap::App::get_matches().
pub fn transpile(tcfg: TranspilerConfig, cc_db: &Path, extra_clang_args: &[&str]) {
    diagnostics::init(tcfg.enabled_warnings.clone());

    let cmds = get_compile_commands(cc_db, &tcfg.filter).expect(&format!(
        "Could not parse compile commands from {}",
        cc_db.to_string_lossy()
    ));

    // we may need to specify path to system include dir on macOS
    let clang_args: Vec<String> = get_isystem_args();
    let mut clang_args: Vec<&str> = clang_args.iter().map(AsRef::as_ref).collect();
    clang_args.extend_from_slice(extra_clang_args);

    let results = cmds
        .iter()
        .map(|cmd| transpile_single(&tcfg, cmd.abs_file().as_path(), cc_db, extra_clang_args))
        .collect::<Vec<TranspileResult>>();
    let mut modules = vec![];
    let mut modules_skipped = false;
    let mut pragmas = PragmaSet::new();
    let mut crates = CrateSet::new();
    for res in results {
        let (module, pragma_vec, crate_set) = res;
        modules.push(module);

        if let Some(pv) = pragma_vec {
            for (key, vals) in pv {
                for val in vals {
                    pragmas.insert((key, val));
                }
            }
        } else {
            modules_skipped = true;
        }

        if let Some(cs) = crate_set {
            crates.extend(cs);
        }
    }
    pragmas.sort();
    crates.sort();

    if tcfg.emit_build_files {
        if modules_skipped {
            // If we skipped a file, we may not have collected all required pragmas
            warn!("Can't emit build files after incremental transpiler run; skipped.");
            return;
        }
        let build_dir = get_build_dir(&tcfg, cc_db);
        let crate_file = emit_build_files(&tcfg, &build_dir, modules, pragmas, crates);
        // We only run the reorganization refactoring if we emitted a fresh crate file
        if crate_file.is_some() && !tcfg.disable_refactoring {
            if tcfg.reorganize_definitions {
                reorganize_definitions(&build_dir).unwrap_or_else(|e| {
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
fn get_isystem_args() -> Vec<String> {
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

fn invoke_refactor(build_dir: &PathBuf) -> Result<(), Error> {
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
    let args = [
        "--cargo",
        "--rewrite-mode",
        "inplace",
        "rename_unnamed",
        ";",
        "reorganize_definitions",
    ];
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

fn reorganize_definitions(build_dir: &PathBuf) -> Result<(), Error> {
    invoke_refactor(build_dir)?;
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

fn transpile_single(
    tcfg: &TranspilerConfig,
    input_path: &Path,
    cc_db: &Path,
    extra_clang_args: &[&str],
) -> TranspileResult {
    let output_path = get_output_path(tcfg, input_path);
    if output_path.exists() && !tcfg.overwrite_existing {
        println!("Skipping existing file {}", output_path.display());
        return (output_path, None, None);
    }

    let file = input_path.file_name().unwrap().to_str().unwrap();
    println!("Transpiling {}", file);
    if !input_path.exists() {
        warn!(
            "Input C file {} does not exist, skipping!",
            input_path.display()
        );
    }

    if tcfg.verbose {
        println!("Additional Clang arguments: {}", extra_clang_args.join(" "));
    }

    // Extract the untyped AST from the CBOR file
    let untyped_context = match ast_exporter::get_untyped_ast(
        input_path,
        cc_db,
        extra_clang_args,
        tcfg.debug_ast_exporter,
    ) {
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
        let conv = ConversionContext::new(&untyped_context);
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
    let (translated_string, pragmas, crates) =
        translator::translate(typed_context, &tcfg, input_path.to_path_buf());

    let mut file = match File::create(&output_path) {
        Ok(file) => file,
        Err(e) => panic!("Unable to open file for writing: {}", e),
    };

    match file.write_all(translated_string.as_bytes()) {
        Ok(()) => (),
        Err(e) => panic!("Unable to write translation to file: {}", e),
    };

    (output_path, Some(pragmas), Some(crates))
}

fn get_output_path(tcfg: &TranspilerConfig, input_path: &Path) -> PathBuf {
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
