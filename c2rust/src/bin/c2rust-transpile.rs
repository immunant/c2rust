use clap::{load_yaml, App};
use clap::{Parser, ValueEnum};
use regex::Regex;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use c2rust_transpile::{Diagnostic, ReplaceMode, TranspilerConfig};

#[derive(Debug, Parser)]
#[clap(author, version, about, long_about = None, trailing_var_arg = true)]
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
    #[clap(long)]
    verbose: bool,

    /// Enable translation of some C macros into consts
    #[clap(long)]
    translate_const_macros: bool,

    /// "Enable translation of some C function macros into invalid Rust code. WARNING: resulting code will not compile."
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
    #[clap(long)]
    dump_function_cfgs: bool,

    /// Dumps into files JSON visualizations of the CFGs of every function
    #[clap(long)]
    json_function_cfgs: bool,

    /// Dump into the DOT file visualizations liveness information
    #[clap(long, requires = "dump-function-cfgs")]
    dump_cfgs_liveness: bool,

    /// Dumps out to STDERR the intermediate structures produced by relooper
    #[clap(long)]
    dump_structures: bool,

    /// Generate readable 'current_block' values in relooper
    #[clap(long)]
    debug_labels: bool,

    /// Input compile_commands.json file
    #[clap()]
    compile_commands: PathBuf,

    /// How to handle violated invariants or invalid code
    #[clap(long, value_enum, default_value = "compile_error")]
    invalid_code: InvalidCodes,

    /// Emit .rs files as modules instead of crates, excluding the crate preambles
    #[clap(long)]
    emit_modules: bool,

    /// Emit Rust build files, i.e., Cargo.toml for a library (and one or more binaries if -b/--binary is given). Implies --emit-modules.
    #[clap(short = 'e', long)]
    emit_build_files: bool,

    /// Path to output directory. Rust sources will be emitted in DIR/src/ and build files will be emitted in DIR/.
    #[clap(short = 'o', long, value_name = "DIR")]
    output_dir: Option<String>,

    /// Only transpile files matching filter
    #[clap(short = 'f', long)]
    filter: Option<String>,

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
    #[clap(long, multiple = true)]
    extra_clang_args: Option<Vec<String>>,

    /// Enable the specified warning (all enables all warnings)
    #[clap(short = 'W', long)]
    warn: Option<String>,

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
    #[clap(long = "log-level", value_enum, default_value = "warn")]
    log_level: LogLevel,
}

#[derive(Debug, ValueEnum, Clone)]
enum InvalidCodes {
    Panic,
    CompileError,
}

#[derive(Debug, ValueEnum, Clone)]
enum LogLevel {
    Off,
    Error,
    Warn,
    Info,
    Debug,
    Trace,
}

fn main() {
    let args = Args::parse();
    let yaml = load_yaml!("../transpile.yaml");
    let matches = App::from_yaml(yaml).get_matches();

    // Build a TranspilerConfig from the command line
    let cc_json_path = Path::new(&args.compile_commands);
    let cc_json_path = cc_json_path.canonicalize().unwrap_or_else(|_| {
        panic!(
            "Could not find compile_commands.json file at path: {}",
            cc_json_path.display()
        )
    });

    let extra_args = args.extra_clang_args.unwrap();
    let extra_args = extra_args.iter().map(AsRef::as_ref).collect::<Vec<&str>>();
    let extra_args = extra_args.as_slice();

    let enabled_warnings: HashSet<Diagnostic> = matches
        .values_of("warn")
        .unwrap_or_default()
        .map(|s| Diagnostic::from_str(s).unwrap())
        .collect();

    let log_level = match matches.value_of("log-level") {
        Some("off") => log::LevelFilter::Off,
        Some("error") => log::LevelFilter::Error,
        Some("warn") => log::LevelFilter::Warn,
        Some("info") => log::LevelFilter::Info,
        Some("debug") => log::LevelFilter::Debug,
        Some("trace") => log::LevelFilter::Trace,
        _ => panic!("Invalid log level"),
    };

    let mut tcfg = TranspilerConfig {
        dump_untyped_context: matches.is_present("dump-untyped-clang-ast"),
        dump_typed_context: matches.is_present("dump-typed-clang-ast"),
        pretty_typed_context: matches.is_present("pretty-typed-clang-ast"),
        dump_function_cfgs: matches.is_present("dump-function-cfgs"),
        json_function_cfgs: matches.is_present("json-function-cfgs"),
        dump_cfg_liveness: matches.is_present("dump-cfgs-liveness"),
        dump_structures: matches.is_present("dump-structures"),
        debug_ast_exporter: matches.is_present("debug-ast-exporter"),
        verbose: matches.is_present("verbose"),

        incremental_relooper: !matches.is_present("no-incremental-relooper"),
        fail_on_error: matches.is_present("fail-on-error"),
        fail_on_multiple: matches.is_present("fail-on-multiple"),
        filter: {
            if matches.is_present("filter") {
                let filter = matches.value_of("filter").unwrap();
                Some(Regex::new(filter).unwrap())
            } else {
                None
            }
        },
        debug_relooper_labels: matches.is_present("debug-labels"),
        prefix_function_names: matches.value_of("prefix-function-names").map(String::from),

        // We used to guard asm translation with a command-line
        // option. Defaulting to enabled now, can add an option to disable if
        // needed.
        translate_asm: true,

        // We used to guard varargs with a command-line option before nightly
        // support landed. We may still want to disable this option to target
        // stable rust output.
        translate_valist: true,

        translate_const_macros: matches.is_present("translate-const-macros"),
        translate_fn_macros: matches.is_present("translate-fn-macros"),
        disable_refactoring: matches.is_present("disable-refactoring"),
        preserve_unused_functions: matches.is_present("preserve-unused-functions"),

        use_c_loop_info: !matches.is_present("ignore-c-loop-info"),
        use_c_multiple_info: !matches.is_present("ignore-c-multiple-info"),
        simplify_structures: !matches.is_present("no-simplify-structures"),
        overwrite_existing: matches.is_present("overwrite-existing"),
        reduce_type_annotations: matches.is_present("reduce-type-annotations"),
        reorganize_definitions: matches.is_present("reorganize-definitions"),
        emit_modules: matches.is_present("emit-modules"),
        emit_build_files: matches.is_present("emit-build-files"),
        output_dir: matches.value_of("output-dir").map(PathBuf::from),
        binaries: matches
            .values_of("binary")
            .map(|values| values.map(String::from).collect())
            .unwrap_or_default(),
        panic_on_translator_failure: {
            match matches.value_of("invalid-code") {
                Some("panic") => true,
                Some("compile_error") => false,
                _ => panic!("Invalid option"),
            }
        },
        replace_unsupported_decls: ReplaceMode::Extern,
        emit_no_std: matches.is_present("emit-no-std"),
        enabled_warnings,
        log_level,
    };
    // binaries imply emit-build-files
    if !tcfg.binaries.is_empty() {
        tcfg.emit_build_files = true
    };
    // emit-build-files implies emit-modules
    if tcfg.emit_build_files {
        tcfg.emit_modules = true
    };

    c2rust_transpile::transpile(tcfg, &cc_json_path, extra_args);
}
