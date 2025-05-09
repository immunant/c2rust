use clap::{Parser, ValueEnum};
use log::LevelFilter;
use regex::Regex;
use std::{fs, path::PathBuf, collections::HashSet};

use c2rust_transpile::{Derive, Diagnostic, ReplaceMode, TranspilerConfig};

const DEFAULT_DERIVES: &[Derive] = &[Derive::Clone, Derive::Copy, Derive::BitfieldStruct];

#[derive(Debug, Parser)]
#[clap(
name = "transpile",
author = "- The C2Rust Project Developers <c2rust@immunant.com>
- Eric Mertens <emertens@galois.com>
- Alec Theriault <atheriault@galois.com>",
version,
about = "Translate C code to equivalent Rust code",
long_about = None,
trailing_var_arg = true)]
struct Args {
    /// Adds a prefix to all function names. Generally only useful for testing
    #[clap(long)]
    prefix_function_names: Option<String>,

    /// Prints out CBOR based Clang AST
    #[clap(long)]
    dump_untyped_clang_ast: bool,

    /// Prints out the parsed typed Clang AST
    #[clap(long)]
    dump_typed_clang_ast: bool,

    /// Pretty-prints out the parsed typed Clang AST
    #[clap(long)]
    pretty_typed_clang_ast: bool,

    /// Debug Clang AST exporter plugin
    #[clap(long)]
    debug_ast_exporter: bool,

    /// Verbose mode
    #[clap(short = 'v', long)]
    verbose: bool,

    /// Enable translation of some C macros into consts
    #[clap(long)]
    translate_const_macros: bool,

    /// Enable translation of some C function macros into invalid Rust code. WARNING: resulting code will not compile.
    #[clap(long)]
    translate_fn_macros: bool,

    /// Disable relooping function bodies incrementally
    #[clap(long)]
    no_incremental_relooper: bool,

    /// Do not run a pass to simplify structures
    #[clap(long)]
    no_simplify_structures: bool,

    /// Don't keep/use information about C loops
    #[clap(long)]
    ignore_c_loop_info: bool,

    /// Don't keep/use information about C branches
    #[clap(long)]
    ignore_c_multiple_info: bool,

    /// Dumps into files DOT visualizations of the CFGs of every function
    #[clap(long = "ddump-function-cfgs")]
    dump_function_cfgs: bool,

    /// Dumps into files JSON visualizations of the CFGs of every function
    #[clap(long)]
    json_function_cfgs: bool,

    /// Dump into the DOT file visualizations liveness information
    #[clap(long = "ddump-cfgs-liveness", requires = "dump-function-cfgs")]
    dump_cfgs_liveness: bool,

    /// Dumps out to STDERR the intermediate structures produced by relooper
    #[clap(long = "ddump-structures")]
    dump_structures: bool,

    /// Generate readable 'current_block' values in relooper
    #[clap(long = "ddebug-labels")]
    debug_labels: bool,

    /// Path to compile_commands.json, or a list of source files
    #[clap(parse(from_os_str), multiple_values = true)]
    compile_commands: Vec<PathBuf>,

    /// How to handle violated invariants or invalid code
    #[clap(long, value_enum, default_value_t = InvalidCodes::CompileError)]
    invalid_code: InvalidCodes,

    /// Emit .rs files as modules instead of crates, excluding the crate preambles
    #[clap(long)]
    emit_modules: bool,

    /// Emit Rust build files, i.e., Cargo.toml for a library (and one or more binaries if -b/--binary is given). Implies --emit-modules.
    #[clap(short = 'e', long)]
    emit_build_files: bool,

    /// Path to output directory. Rust sources will be emitted in DIR/src/ and build files will be emitted in DIR/.
    #[clap(short = 'o', long, value_name = "DIR")]
    output_dir: Option<PathBuf>,

    /// Only transpile files matching filter
    #[clap(short = 'f', long)]
    filter: Option<Regex>,

    /// Fail to translate a module when a portion is not able to be translated
    #[clap(long)]
    fail_on_error: bool,

    /// Emit Rust build files for a binary using the main function in the specified translation unit (implies -e/--emit-build-files)
    #[clap(short = 'b', long = "binary", multiple = true, number_of_values = 1)]
    binary: Option<Vec<String>>,

    /// Emit files even if it causes existing files to be overwritten
    #[clap(long)]
    overwrite_existing: bool,

    /// Reduces the number of explicit type annotations where it should be safe to do so
    #[clap(long)]
    reduce_type_annotations: bool,

    /// Output file in such a way that the refactoring tool can deduplicate code
    #[clap(short = 'r', long)]
    reorganize_definitions: bool,

    /// Extra arguments to pass to clang frontend during parsing the input C file
    #[clap(multiple = true, last(true))]
    extra_clang_args: Vec<String>,

    /// Enable the specified warning (all enables all warnings)
    #[clap(short = 'W')]
    warn: Option<Diagnostic>,

    /// Emit code using core rather than std
    #[clap(long)]
    emit_no_std: bool,

    /// Disable running refactoring tool after translation
    #[clap(long)]
    disable_refactoring: bool,

    /// Include static and inline functions in translation
    #[clap(long)]
    preserve_unused_functions: bool,

    /// Logging level
    #[clap(long, default_value_t = LevelFilter::Warn)]
    log_level: LevelFilter,

    /// Fail when the control-flow graph generates branching constructs
    #[clap(long)]
    fail_on_multiple: bool,

    /// Add extra derived traits to generated structs in addition to the default
    /// set of derives (Copy, Clone, BitfieldStruct). Specify multiple times to
    /// add more than one derive. A struct will derive all traits in the set for
    /// which it is eligible.
    ///
    /// For example, a struct containing a union cannot derive Debug, so
    /// `#[derive(Debug)]` will not be added to that struct regardless of
    /// whether `--derive Debug` is specified.
    #[clap(long = "derive", value_enum, value_name = "TRAIT")]
    extra_derives: Vec<ExtraDerive>,
}

#[derive(Debug, PartialEq, Eq, ValueEnum, Clone)]
#[clap(rename_all = "snake_case")]
enum InvalidCodes {
    Panic,
    CompileError,
}

#[derive(Clone, Copy, Debug, ValueEnum)]
#[clap(rename_all = "PascalCase")]
enum ExtraDerive {
    Debug,
}

impl ExtraDerive {
    fn to_transpiler_derive(&self) -> Derive {
        match self {
            Self::Debug => Derive::Debug,
        }
    }
}

fn main() {
    let args = Args::parse();

    // Build a TranspilerConfig from the command line
    let mut tcfg = TranspilerConfig {
        dump_untyped_context: args.dump_untyped_clang_ast,
        dump_typed_context: args.dump_typed_clang_ast,
        pretty_typed_context: args.pretty_typed_clang_ast,
        dump_function_cfgs: args.dump_function_cfgs,
        json_function_cfgs: args.json_function_cfgs,
        dump_cfg_liveness: args.dump_cfgs_liveness,
        dump_structures: args.dump_structures,
        debug_ast_exporter: args.debug_ast_exporter,
        verbose: args.verbose,

        incremental_relooper: !args.no_incremental_relooper,
        fail_on_error: args.fail_on_error,
        fail_on_multiple: args.fail_on_multiple,
        filter: args.filter,
        debug_relooper_labels: args.debug_labels,
        prefix_function_names: args.prefix_function_names,

        // We used to guard asm translation with a command-line
        // option. Defaulting to enabled now, can add an option to disable if
        // needed.
        translate_asm: true,

        // We used to guard varargs with a command-line option before nightly
        // support landed. We may still want to disable this option to target
        // stable rust output.
        translate_valist: true,

        translate_const_macros: args.translate_const_macros,
        translate_fn_macros: args.translate_fn_macros,
        disable_refactoring: args.disable_refactoring,
        preserve_unused_functions: args.preserve_unused_functions,

        use_c_loop_info: !args.ignore_c_loop_info,
        use_c_multiple_info: !args.ignore_c_multiple_info,
        simplify_structures: !args.no_simplify_structures,
        overwrite_existing: args.overwrite_existing,
        reduce_type_annotations: args.reduce_type_annotations,
        reorganize_definitions: args.reorganize_definitions,
        emit_modules: args.emit_modules,
        emit_build_files: args.emit_build_files,
        output_dir: args.output_dir,
        binaries: args.binary.unwrap_or_default(),
        panic_on_translator_failure: args.invalid_code == InvalidCodes::Panic,
        replace_unsupported_decls: ReplaceMode::Extern,
        emit_no_std: args.emit_no_std,
        enabled_warnings: args.warn.into_iter().collect(),
        log_level: args.log_level,
        derives: get_derives(&args.extra_derives),
    };
    // binaries imply emit-build-files
    if !tcfg.binaries.is_empty() {
        tcfg.emit_build_files = true
    };
    // emit-build-files implies emit-modules
    if tcfg.emit_build_files {
        tcfg.emit_modules = true
    };

    let mut created_temp_compile_commands = false;

    let compile_commands = if args.compile_commands.len() == 1
        && args.compile_commands[0].extension() == Some(std::ffi::OsStr::new("json"))
    {
        // Only one file provided and it's a JSON file
        match fs::canonicalize(&args.compile_commands[0]) {
            Ok(canonical_path) => canonical_path,
            Err(e) => panic!("Failed to canonicalize path: {:?}", e),
        }
    } else if args
        .compile_commands
        .iter()
        .any(|path| path.extension() == Some(std::ffi::OsStr::new("json")))
    {
        // More than one file provided and at least one is a JSON file
        panic!("Compile commands JSON and multiple sources provided.
                Exactly one compile_commands.json file should be provided, or a list of source files, but not both.");
    } else {
        // Handle as a list of source files
        created_temp_compile_commands = true;
        c2rust_transpile::create_temp_compile_commands(&args.compile_commands)
    };

    let extra_args = args
        .extra_clang_args
        .iter()
        .map(AsRef::as_ref)
        .collect::<Vec<_>>();

    c2rust_transpile::transpile(tcfg, &compile_commands, &extra_args);

    // Remove the temporary compile_commands.json if it was created
    if created_temp_compile_commands {
        std::fs::remove_file(&compile_commands)
            .expect("Failed to remove temporary compile_commands.json");
    }
}

fn get_derives(extra_derives: &[ExtraDerive]) -> Vec<Derive> {
    // Make sure there are no dupes and sort so the derives are always in the same order
    let derives_set: HashSet<_> = DEFAULT_DERIVES
        .iter()
        .cloned()
        .chain(extra_derives.iter().map(|d| d.to_transpiler_derive()))
        .collect();
    let mut derives: Vec<Derive> = derives_set.into_iter().collect();
    derives.sort();
    derives
}
