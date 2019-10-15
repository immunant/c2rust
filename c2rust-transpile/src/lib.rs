#![feature(rustc_private)]
#![feature(label_break_value)]
#![feature(box_patterns)]

extern crate colored;
extern crate dtoa;
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

use crate::build_files::{emit_build_files, get_build_dir, emit_workspace_files};
use crate::compile_cmds::get_compile_commands;
use crate::convert_type::RESERVED_NAMES;
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
    pub log_level: log::LevelFilter,

    // Options that control build files
    /// Emit `Cargo.toml` and `lib.rs`
    pub emit_build_files: bool,
    /// Names of translation units containing main functions that we should make
    /// into binaries
    pub binaries: Vec<String>,
}

impl TranspilerConfig {
    fn is_binary(&self, file: &Path) -> bool {
        let file = Path::new(file.file_stem().unwrap());
        let name = get_module_name(file, false, false, false).unwrap();
        self.binaries.contains(&name)
    }

    fn crate_name(&self) -> String {
        self.output_dir.as_ref().and_then(
            |x| x.file_name().map(|x| x.to_string_lossy().into_owned())
        ).unwrap_or_else(|| "c2rust".into())
    }
}

fn char_to_ident(c: char) -> char {
    if c.is_alphanumeric() { c } else { '_' }
}

fn str_to_ident<S: AsRef<str>>(s: S) -> String {
    s.as_ref().chars().map(char_to_ident).collect()
}

/// Make sure that name:
/// - does not contain illegal characters,
/// - does not clash with reserved keywords.
fn str_to_ident_checked(filename: &Option<String>, check_reserved: bool) -> Option<String> {
    // module names cannot contain periods or dashes
    filename.as_ref().map(str_to_ident).map(|module| {
        // make sure the module name does not clash with keywords
        if check_reserved && RESERVED_NAMES.contains(&module.as_str()) {
            format!("r#{}", module)
        } else {
            module
        }
    })
}

fn get_module_name(
    file: &Path,
    check_reserved: bool,
    keep_extension: bool,
    full_path: bool
) -> Option<String> {
    let is_rs = file.extension().map(|ext| ext == "rs").unwrap_or(false);
    let fname = if is_rs {
        file.file_stem()
    } else {
        file.file_name()
    };
    let fname = &fname.unwrap().to_str().map(String::from);
    let mut name = str_to_ident_checked(fname, check_reserved).unwrap();
    if keep_extension && is_rs {
        name.push_str(".rs");
    }
    let file = if full_path {
        file.with_file_name(name)
    } else {
        Path::new(&name).to_path_buf()
    };
    file.to_str().map(String::from)
}

/// Main entry point to transpiler. Called from CLI tools with the result of
/// clap::App::get_matches().
pub fn transpile(tcfg: TranspilerConfig, cc_db: &Path, extra_clang_args: &[&str]) {
    diagnostics::init(tcfg.enabled_warnings.clone(), tcfg.log_level);

    let lcmds = get_compile_commands(cc_db, &tcfg.filter).expect(&format!(
        "Could not parse compile commands from {}",
        cc_db.to_string_lossy()
    ));

    // we may need to specify path to system include dir on macOS
    let clang_args: Vec<String> = get_isystem_args();
    let mut clang_args: Vec<&str> = clang_args.iter().map(AsRef::as_ref).collect();
    clang_args.extend_from_slice(extra_clang_args);

    let mut workspace_members = vec![];
    let build_dir = get_build_dir(&tcfg, cc_db);
    for lcmd in &lcmds {
        let cmds = &lcmd.cmd_inputs;
        let lcmd_name = lcmd.output
            .as_ref()
            .map(|output| {
                let output_path = Path::new(output);
                output_path
                    .file_stem()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_owned()
            })
            .unwrap_or_else(|| tcfg.crate_name());
        let build_dir = build_dir.join(&lcmd_name);

        // Compute the common ancestor of all input files
        // FIXME: this is quadratic-time in the length of the ancestor path
        let mut ancestor_path = cmds
            .first()
            .map(|cmd| cmd.abs_file())
            .unwrap_or_else(PathBuf::new);
        for cmd in &cmds[1..] {
            let cmd_path = cmd.abs_file();
            ancestor_path = ancestor_path
                .ancestors()
                .find(|a| cmd_path.starts_with(a))
                .map(ToOwned::to_owned)
                .unwrap_or_else(PathBuf::new);
        }

        let results = cmds
            .iter()
            .map(|cmd| transpile_single(&tcfg, cmd.abs_file(),
                                        &ancestor_path,
                                        &build_dir,
                                        cc_db,
                                        extra_clang_args))
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
            let crate_file = emit_build_files(&tcfg, &lcmd_name, &build_dir,
                                              modules, pragmas, crates,
                                              &lcmd);
            // We only run the reorganization refactoring if we emitted a fresh crate file
            if crate_file.is_some() && !tcfg.disable_refactoring {
                if tcfg.reorganize_definitions {
                    reorganize_definitions(&build_dir).unwrap_or_else(|e| {
                        warn!("Failed to reorganize definitions. {}", e.as_fail());
                    })
                }
            }
        }

        workspace_members.push(lcmd_name);
    }
    if tcfg.emit_build_files {
        emit_workspace_files(&tcfg, &build_dir, workspace_members);
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
    input_path: PathBuf,
    ancestor_path: &Path,
    build_dir: &Path,
    cc_db: &Path,
    extra_clang_args: &[&str],
) -> TranspileResult {
    let output_path = get_output_path(tcfg, &input_path, ancestor_path, build_dir);
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
        input_path.as_path(),
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
        if conv.invalid_clang_ast && tcfg.fail_on_error {
            panic!("Clang AST was invalid");
        }
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
        translator::translate(typed_context, &tcfg, input_path);

    let mut file = match File::create(&output_path) {
        Ok(file) => file,
        Err(e) => panic!("Unable to open file {} for writing: {}", output_path.display(), e),
    };

    match file.write_all(translated_string.as_bytes()) {
        Ok(()) => (),
        Err(e) => panic!("Unable to write translation to file {}: {}", output_path.display(), e),
    };

    (output_path, Some(pragmas), Some(crates))
}

fn get_output_path(
    tcfg: &TranspilerConfig,
    input_path: &PathBuf,
    ancestor_path: &Path,
    build_dir: &Path,
) -> PathBuf {
    let mut path_buf = input_path.clone();

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

    if tcfg.output_dir.is_some() {
        let path_buf = path_buf.strip_prefix(ancestor_path)
            .expect("Couldn't strip common ancestor path");

        // Place the source files in build_dir/src/
        let mut output_path = build_dir.to_path_buf();
        output_path.push("src");
        for elem in path_buf.iter() {
            let path = Path::new(elem);
            let name = get_module_name(&path, false, true, false).unwrap();
            output_path.push(name);
        }

        // Create the parent directory if it doesn't exist
        let parent = output_path.parent().unwrap();
        if !parent.exists() {
            fs::create_dir_all(&parent).expect(&format!(
                "couldn't create source directory: {}",
                parent.display()
            ));
        }
        output_path
    } else {
        path_buf
    }
}
