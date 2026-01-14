#![allow(clippy::too_many_arguments)]

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
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::{env, io};

use crate::compile_cmds::CompileCmd;
use failure::Error;
use itertools::Itertools;
use log::{info, warn};
use regex::Regex;
use serde_derive::Serialize;
pub use tempfile::TempDir;

use crate::c_ast::Printer;
use crate::c_ast::*;
pub use crate::diagnostics::Diagnostic;
use c2rust_ast_exporter as ast_exporter;

use crate::build_files::{emit_build_files, get_build_dir, CrateConfig};
use crate::compile_cmds::get_compile_commands;
use crate::convert_type::RESERVED_NAMES;
pub use crate::translator::ReplaceMode;
use std::prelude::v1::Vec;

type PragmaVec = Vec<(&'static str, Vec<&'static str>)>;
type PragmaSet = indexmap::IndexSet<(&'static str, &'static str)>;
type CrateSet = indexmap::IndexSet<ExternCrate>;
type TranspileResult = Result<(PathBuf, PragmaVec, CrateSet), ()>;

#[derive(Default, Debug)]
pub enum TranslateMacros {
    /// Don't translate any macros.
    None,

    /// Translate the conservative subset of macros known to always work.
    #[default]
    Conservative,

    /// Try to translate more, but this is experimental and not guaranteed to work.
    ///
    /// For const-like macros, this works in some cases.
    /// For function-like macros, this doesn't really work at all yet.
    Experimental,
}

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
    pub emit_c_decl_map: bool,

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
    pub translate_const_macros: TranslateMacros,
    pub translate_fn_macros: TranslateMacros,
    pub disable_rustfmt: bool,
    pub disable_refactoring: bool,
    pub preserve_unused_functions: bool,
    pub log_level: log::LevelFilter,

    // Options that control build files
    /// Emit `Cargo.toml` and `lib.rs`
    pub emit_build_files: bool,

    /// Names of translation units containing main functions that we should make
    /// into binaries
    pub binaries: Vec<String>,

    pub c2rust_dir: Option<PathBuf>,
}

impl TranspilerConfig {
    fn binary_name_from_path(file: &Path) -> String {
        let file = Path::new(file.file_stem().unwrap());
        get_module_name(file, false, false, false).unwrap()
    }

    fn is_binary(&self, file: &Path) -> bool {
        let module_name = Self::binary_name_from_path(file);
        self.binaries.contains(&module_name)
    }

    fn check_if_all_binaries_used(
        &self,
        transpiled_modules: impl IntoIterator<Item = impl AsRef<Path>>,
    ) -> bool {
        let module_names = transpiled_modules
            .into_iter()
            .map(|module| Self::binary_name_from_path(module.as_ref()))
            .collect::<HashSet<_>>();
        let mut ok = true;
        for binary in &self.binaries {
            if !module_names.contains(binary) {
                ok = false;
                warn!("binary not used: {binary}");
            }
        }
        if !ok {
            let module_names = module_names.iter().format(", ");
            info!("candidate modules for binaries are: {module_names}");
        }
        ok
    }

    fn crate_name(&self) -> String {
        self.output_dir
            .as_ref()
            .and_then(|dir| dir.file_name())
            .map(|fname| str_to_ident_checked(fname.to_string_lossy().as_ref(), true))
            .unwrap_or_else(|| "c2rust_out".into())
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ExternCrate {
    C2RustBitfields,
    C2RustAsmCasts,
    F128,
    NumTraits,
    Memoffset,
    Libc,
}

#[derive(Serialize)]
struct ExternCrateDetails {
    name: &'static str,
    ident: String,
    macro_use: bool,
    version: &'static str,
    path: Option<PathBuf>,
}

impl ExternCrateDetails {
    pub fn new(
        name: &'static str,
        version: &'static str,
        macro_use: bool,
        path: Option<PathBuf>,
    ) -> Self {
        Self {
            name,
            ident: name.replace('-', "_"),
            macro_use,
            version,
            path,
        }
    }

    /// An external (to c2rust) dependency.
    pub fn external(name: &'static str, version: &'static str, macro_use: bool) -> Self {
        Self::new(name, version, macro_use, None)
    }

    /// An internal (to c2rust) dependency.
    pub fn internal(name: &'static str, macro_use: bool, c2rust_dir: Option<&Path>) -> Self {
        Self::new(
            name,
            env!("CARGO_PKG_VERSION"),
            macro_use,
            c2rust_dir.map(|dir| dir.join(name)),
        )
    }
}

impl ExternCrate {
    fn with_details(&self, c2rust_dir: Option<&Path>) -> ExternCrateDetails {
        use ExternCrate::*;
        match self {
            C2RustBitfields => ExternCrateDetails::internal("c2rust-bitfields", true, c2rust_dir),
            C2RustAsmCasts => ExternCrateDetails::internal("c2rust-asm-casts", true, c2rust_dir),
            F128 => ExternCrateDetails::external("f128", "0.2", false),
            NumTraits => ExternCrateDetails::external("num-traits", "0.2", true),
            Memoffset => ExternCrateDetails::external("memoffset", "0.5", true),
            Libc => ExternCrateDetails::external("libc", "0.2", false),
        }
    }
}

fn char_to_ident(c: char) -> char {
    if c.is_alphanumeric() {
        c
    } else {
        '_'
    }
}

fn str_to_ident(s: &str) -> String {
    s.chars().map(char_to_ident).collect()
}

/// Make sure that name:
/// - does not contain illegal characters,
/// - does not clash with reserved keywords.
fn str_to_ident_checked(s: &str, check_reserved: bool) -> String {
    let s = str_to_ident(s);

    // make sure the name does not clash with keywords
    if check_reserved && RESERVED_NAMES.contains(&s.as_str()) {
        format!("r#{}", s)
    } else {
        s
    }
}

fn get_module_name(
    file: &Path,
    check_reserved: bool,
    keep_extension: bool,
    full_path: bool,
) -> Option<String> {
    let is_rs = file.extension().map(|ext| ext == "rs").unwrap_or(false);
    let fname = if is_rs {
        file.file_stem()
    } else {
        file.file_name()
    };
    let fname = fname.unwrap().to_str().unwrap();
    let mut name = str_to_ident_checked(fname, check_reserved);
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

pub fn create_temp_compile_commands(sources: &[PathBuf]) -> (TempDir, PathBuf) {
    // If we generate the same path here on every run, then we can't run
    // multiple transpiles in parallel, so we need a unique path. But clang
    // won't read this file unless it is named exactly "compile_commands.json",
    // so we can't change the filename. Instead, create a temporary directory
    // with a unique name, and put the file there.
    let temp_dir = tempfile::Builder::new()
        .prefix("c2rust-")
        .tempdir()
        .expect("Failed to create temporary directory for compile_commands.json");
    let temp_path = temp_dir.path().join("compile_commands.json");

    let compile_commands: Vec<CompileCmd> = sources
        .iter()
        .map(|source_file| {
            let absolute_path = fs::canonicalize(source_file)
                .unwrap_or_else(|_| panic!("Could not canonicalize {}", source_file.display()));

            CompileCmd {
                directory: PathBuf::from("."),
                file: absolute_path.clone(),
                arguments: vec![
                    "clang".to_string(),
                    absolute_path.to_str().unwrap().to_owned(),
                ],
                command: None,
                output: None,
            }
        })
        .collect();

    let json_content = serde_json::to_string(&compile_commands).unwrap();
    let mut file =
        File::create(&temp_path).expect("Failed to create temporary compile_commands.json");
    file.write_all(json_content.as_bytes())
        .expect("Failed to write to temporary compile_commands.json");
    (temp_dir, temp_path)
}

/// Main entry point to transpiler. Called from CLI tools with the result of
/// clap::App::get_matches().
pub fn transpile(tcfg: TranspilerConfig, cc_db: &Path, extra_clang_args: &[&str]) {
    diagnostics::init(tcfg.enabled_warnings.clone(), tcfg.log_level);

    let build_dir = get_build_dir(&tcfg, cc_db);

    let lcmds = get_compile_commands(cc_db, &tcfg.filter).unwrap_or_else(|_| {
        panic!(
            "Could not parse compile commands from {}",
            cc_db.to_string_lossy()
        )
    });

    // Specify path to system include dir on macOS 10.14 and later. Disable the blocks extension.
    let clang_args: Vec<String> = get_extra_args_macos();
    let mut clang_args: Vec<&str> = clang_args.iter().map(AsRef::as_ref).collect();
    clang_args.extend_from_slice(extra_clang_args);

    let mut top_level_ccfg = None;
    let mut workspace_members = vec![];
    let mut num_transpiled_files = 0;
    let mut transpiled_modules = Vec::new();

    for lcmd in &lcmds {
        let cmds = &lcmd.cmd_inputs;
        let lcmd_name = lcmd
            .output
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
        let build_dir = if lcmd.top_level {
            build_dir.to_path_buf()
        } else {
            build_dir.join(&lcmd_name)
        };

        // Compute the common ancestor of all input files
        // FIXME: this is quadratic-time in the length of the ancestor path
        let mut ancestor_path = cmds
            .first()
            .map(|cmd| {
                let mut dir = cmd.abs_file();
                dir.pop(); // discard the file part
                dir
            })
            .unwrap_or_else(PathBuf::new);
        if cmds.len() > 1 {
            for cmd in &cmds[1..] {
                let cmd_path = cmd.abs_file();
                ancestor_path = ancestor_path
                    .ancestors()
                    .find(|a| cmd_path.starts_with(a))
                    .map(ToOwned::to_owned)
                    .unwrap_or_else(PathBuf::new);
            }
        }

        let results = cmds
            .iter()
            .map(|cmd| {
                transpile_single(
                    &tcfg,
                    &cmd.abs_file(),
                    &ancestor_path,
                    &build_dir,
                    cc_db,
                    &clang_args,
                )
            })
            .collect::<Vec<TranspileResult>>();
        let mut modules = vec![];
        let mut modules_skipped = false;
        let mut pragmas = PragmaSet::new();
        let mut crates = CrateSet::new();
        for res in results {
            match res {
                Ok((module, pragma_vec, crate_set)) => {
                    modules.push(module);
                    crates.extend(crate_set);

                    num_transpiled_files += 1;
                    for (key, vals) in pragma_vec {
                        for val in vals {
                            pragmas.insert((key, val));
                        }
                    }
                }
                Err(_) => {
                    modules_skipped = true;
                }
            }
        }
        pragmas.sort();
        crates.sort();

        transpiled_modules.extend(modules.iter().cloned());

        if tcfg.emit_build_files {
            if modules_skipped {
                // If we skipped a file, we may not have collected all required pragmas
                warn!("Can't emit build files after incremental transpiler run; skipped.");
                return;
            }

            let ccfg = CrateConfig {
                crate_name: lcmd_name.clone(),
                modules,
                pragmas,
                crates,
                link_cmd: lcmd,
            };
            if lcmd.top_level {
                top_level_ccfg = Some(ccfg);
            } else {
                let crate_file = emit_build_files(&tcfg, &build_dir, Some(ccfg), None);
                reorganize_definitions(&tcfg, &build_dir, crate_file)
                    .unwrap_or_else(|e| warn!("Reorganizing definitions failed: {}", e));
                workspace_members.push(lcmd_name);
            }
        }
    }

    if num_transpiled_files == 0 {
        warn!("No C files found in compile_commands.json; nothing to do.");
        return;
    }

    if tcfg.emit_build_files {
        let crate_file =
            emit_build_files(&tcfg, &build_dir, top_level_ccfg, Some(workspace_members));
        reorganize_definitions(&tcfg, &build_dir, crate_file)
            .unwrap_or_else(|e| warn!("Reorganizing definitions failed: {}", e));
    }

    tcfg.check_if_all_binaries_used(&transpiled_modules);
}

/// Ensure that clang can locate the system headers on macOS 10.14+.
///
/// MacOS 10.14 does not have a `/usr/include` folder even if Xcode
/// or the command line developer tools are installed as explained in
/// this [thread](https://forums.developer.apple.com/thread/104296).
/// It is possible to install a package which puts the headers in
/// `/usr/include` but the user doesn't have to since we can find
/// the system headers we need by running `xcrun --show-sdk-path`.
fn get_extra_args_macos() -> Vec<String> {
    let mut args = vec![];
    if cfg!(target_os = "macos") {
        let usr_incl = Path::new("/usr/include");
        if !usr_incl.exists() {
            let output = Command::new("xcrun")
                .args(["--show-sdk-path"])
                .output()
                .expect("failed to run `xcrun` subcommand");
            let mut sdk_path = String::from_utf8(output.stdout).unwrap();
            let olen = sdk_path.len();
            sdk_path.truncate(olen - 1);
            sdk_path.push_str("/usr/include");

            args.push("-isystem".to_owned());
            args.push(sdk_path);
        }

        // disable Apple's blocks extension; see https://github.com/immunant/c2rust/issues/229
        args.push("-fno-blocks".to_owned());
    }
    args
}

fn invoke_refactor(build_dir: &Path) -> Result<(), Error> {
    // Make sure the crate builds cleanly
    let status = Command::new("cargo")
        .args(["check"])
        .env("RUSTFLAGS", "-Awarnings")
        .current_dir(build_dir)
        .status()?;
    if !status.success() {
        return Err(failure::format_err!("Crate does not compile."));
    }

    // Assumes the subcommand executable is in the same directory as this program.
    let refactor = env::current_exe()
        .expect("Cannot get current executable path")
        .with_file_name("c2rust-refactor");
    let args = [
        "--cargo",
        "--rewrite-mode",
        "inplace",
        "rename_unnamed",
        ";",
        "reorganize_definitions",
    ];
    let status = Command::new(&refactor)
        .args(args)
        .current_dir(build_dir)
        .status()
        .map_err(|e| {
            let refactor = refactor.display();
            failure::format_err!("unable to run {refactor}: {e}\nNote that c2rust-refactor must be installed separately from c2rust and c2rust-transpile.")
        })?;
    if status.success() {
        Ok(())
    } else {
        Err(failure::format_err!(
            "Refactoring failed. Please fix errors above and re-run:\n    c2rust refactor {}",
            args.join(" "),
        ))
    }
}

fn reorganize_definitions(
    tcfg: &TranspilerConfig,
    build_dir: &Path,
    crate_file: Option<PathBuf>,
) -> Result<(), Error> {
    // We only run the reorganization refactoring if we emitted a fresh crate file
    if crate_file.is_none() || tcfg.disable_refactoring || !tcfg.reorganize_definitions {
        return Ok(());
    }

    invoke_refactor(build_dir)?;

    if !tcfg.disable_rustfmt {
        // fix the formatting of the output of `c2rust-refactor`
        let status = Command::new("cargo")
            .args(["fmt"])
            .current_dir(build_dir)
            .status()?;
        if !status.success() {
            warn!("cargo fmt failed, code may not be well-formatted");
        }
    }

    Ok(())
}

fn transpile_single(
    tcfg: &TranspilerConfig,
    input_path: &Path,
    ancestor_path: &Path,
    build_dir: &Path,
    cc_db: &Path,
    extra_clang_args: &[&str],
) -> TranspileResult {
    let output_path = get_output_path(tcfg, input_path, ancestor_path, build_dir);
    if output_path.exists() && !tcfg.overwrite_existing {
        warn!("Skipping existing file {}", output_path.display());
        return Err(());
    }

    let file = input_path.file_name().unwrap().to_str().unwrap();
    if !input_path.exists() {
        warn!(
            "Input C file {} does not exist, skipping!",
            input_path.display()
        );
        return Err(());
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
            warn!(
                "Error: {}. Skipping {}; is it well-formed C?",
                e,
                input_path.display()
            );
            return Err(());
        }
        Ok(cxt) => cxt,
    };

    println!("Transpiling {}", file);

    if tcfg.dump_untyped_context {
        println!("CBOR Clang AST");
        println!("{:#?}", untyped_context);
    }

    // Convert this into a typed AST
    let typed_context = {
        let conv = ConversionContext::new(input_path, &untyped_context);
        if conv.invalid_clang_ast && tcfg.fail_on_error {
            panic!("Clang AST was invalid");
        }
        conv.into_typed_context()
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
    let (translated_string, maybe_decl_map, pragmas, crates) =
        translator::translate(typed_context, tcfg, input_path);

    if let Some(decl_map) = maybe_decl_map {
        let decl_map_path = output_path.with_extension("c_decls.json");
        let file = match File::create(&decl_map_path) {
            Ok(file) => file,
            Err(e) => panic!(
                "Unable to open file {} for writing: {}",
                output_path.display(),
                e
            ),
        };

        match serde_json::ser::to_writer(file, &decl_map) {
            Ok(()) => (),
            Err(e) => panic!(
                "Unable to write C declaration map to file {}: {}",
                output_path.display(),
                e
            ),
        };
    }

    let mut file = match File::create(&output_path) {
        Ok(file) => file,
        Err(e) => panic!(
            "Unable to open file {} for writing: {}",
            output_path.display(),
            e
        ),
    };

    match file.write_all(translated_string.as_bytes()) {
        Ok(()) => (),
        Err(e) => panic!(
            "Unable to write translation to file {}: {}",
            output_path.display(),
            e
        ),
    };

    if !tcfg.disable_rustfmt {
        rustfmt(&output_path, build_dir);
    }

    Ok((output_path, pragmas, crates))
}

fn get_output_path(
    tcfg: &TranspilerConfig,
    input_path: &Path,
    ancestor_path: &Path,
    build_dir: &Path,
) -> PathBuf {
    // When an output file name is not explicitly specified, we should convert files
    // with dashes to underscores, as they are not allowed in rust file names.
    let file_name = input_path
        .file_name()
        .unwrap()
        .to_str()
        .unwrap()
        .replace('-', "_");

    let mut input_path = input_path.with_file_name(file_name);
    input_path.set_extension("rs");

    if tcfg.output_dir.is_some() {
        let path_buf = input_path
            .strip_prefix(ancestor_path)
            .expect("Couldn't strip common ancestor path");

        // Place the source files in build_dir/src/
        let mut output_path = build_dir.to_path_buf();
        output_path.push("src");
        for elem in path_buf.iter() {
            let path = Path::new(elem);
            let name = get_module_name(path, false, true, false).unwrap();
            output_path.push(name);
        }

        // Create the parent directory if it doesn't exist
        let parent = output_path.parent().unwrap();
        if !parent.exists() {
            fs::create_dir_all(parent).unwrap_or_else(|_| {
                panic!("couldn't create source directory: {}", parent.display())
            });
        }
        output_path
    } else {
        input_path
    }
}

fn rustfmt(output_path: &Path, build_dir: &Path) {
    let edition = "2021";

    let status = Command::new("rustfmt")
        .args(["--edition", edition])
        .arg(output_path)
        .current_dir(build_dir)
        .status();

    if !status.map_or(false, |status| status.success()) {
        warn!("rustfmt failed, code may not be well-formatted");
    }
}
