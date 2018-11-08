#[macro_use]
extern crate clap;
extern crate transpiler;
extern crate ast_exporter;

use std::io::stdout;
use std::io::prelude::*;
use std::fs::{File, canonicalize};
use std::path::{Path, PathBuf};
use transpiler::c_ast::*;
use transpiler::c_ast::Printer;
use transpiler::translator::{ReplaceMode,TranslationConfig};
use clap::{Arg, App};


fn main() {
    let matches = App::new("C2Rust Transpiler")
        .version("0.1.0")
        .author(crate_authors!())

        // Testing related
        .arg(Arg::with_name("prefix-function-names")
            .long("prefix-function-names")
            .help("Adds a prefix to all function names. Generally only useful for testing")
            .takes_value(true))
        .arg(Arg::with_name("translate-entry")
            .long("translate-entry")
            .help("Creates an entry point that calls the C main function")
            .takes_value(false))
        .arg(Arg::with_name("fail-on-multiple")
             .long("fail-on-multiple")
             .requires("reloop-cfgs")
             .help("Fail to translate if we ever need 'current_block'")
             .takes_value(false))

        // `AstContext` and `TypedAstContext` related
        .arg(Arg::with_name("dump-untyped-clang-ast")
            .long("ddump-untyped-clang-ast")
            .help("Prints out CBOR based Clang AST")
            .takes_value(false))
        .arg(Arg::with_name("dump-typed-clang-ast")
            .long("ddump-typed-clang-ast")
            .help("Prints out the parsed typed Clang AST")
            .takes_value(false))
        .arg(Arg::with_name("pretty-typed-clang-ast")
            .long("dpretty-typed-clang-ast")
            .help("Pretty-prints out the parsed typed Clang AST")
            .takes_value(false))
        .arg(Arg::with_name("translate-asm")
            .long("translate-asm")
            .help("Translate inline assembly without translating the assembly fragment")
            .takes_value(false))
        .arg(Arg::with_name("translate-valist")
            .long("translate-valist")
            .help("Translate uses of va_list, requires custom rustc")
            .takes_value(false))

        // CFG/Relooper related
        .arg(Arg::with_name("reloop-cfgs")
            .long("reloop-cfgs")
            .help("Translate ALL function bodies using a CFG/Relooper approach")
            .takes_value(false))
        .arg(Arg::with_name("no-simplify-structures")
            .long("no-simplify-structures")
            .help("Do not run a pass to simplify structures")
            .takes_value(false))
        .arg(Arg::with_name("ignore-c-loop-info")
            .long("ignore-c-loop-info")
            .help("Don't keep/use information about C loops")
            .takes_value(false))
        .arg(Arg::with_name("ignore-c-multiple-info")
            .long("ignore-c-multiple-info")
            .help("Don't keep/use information about C branches")
            .takes_value(false))
        .arg(Arg::with_name("dump-function-cfgs")
            .long("ddump-function-cfgs")
            .help("Dumps into files DOT visualizations of the CFGs of every function")
            .takes_value(false))
        .arg(Arg::with_name("json-function-cfgs")
            .long("json-function-cfgs")
            .help("Dumps into files JSON visualizations of the CFGs of every function")
            .takes_value(false))
        .arg(Arg::with_name("dump-cfgs-liveness")
            .requires("dump-function-cfgs")
            .long("ddump-cfgs-liveness")
            .help("Dump into the DOT file visualizations liveness information")
            .takes_value(false))
        .arg(Arg::with_name("dump-structures")
            .long("ddump-structures")
            .help("Dumps out to STDERR the intermediate structures produced by relooper")
            .takes_value(false))
        .arg(Arg::with_name("debug-labels")
            .long("ddebug-labels")
            .help("Generate readable 'current_block' values in relooper")
            .takes_value(false))

        // Cross-check related
        .arg(Arg::with_name("cross-checks")
             .long("cross-checks")
             .help("Enable cross-checks")
             .takes_value(false))
        .arg(Arg::with_name("cross-check-config")
             .long("cross-check-config")
             .help("Add the given configuration files to the top-level #[cross_check(...)] attribute")
             .requires("cross-checks")
             .multiple(true)
             .takes_value(true))

        // End-user
        .arg(Arg::with_name("INPUT")
            .help("Input C file")
            .required(true)
            .index(1))
        .arg(Arg::with_name("invalid-code")
            .long("invalid-code")
            .help("How to handle violated invariants or invalid code")
            .possible_values(&["panic", "compile_error"])
            .default_value("compile_error"))
        .arg(Arg::with_name("emit-module")
             .long("emit-module")
             .help("Emit the .rs file as a module instead of a crate, excluding the crate preamble")
             .takes_value(false))
        .arg(Arg::with_name("fail-on-error")
             .long("fail-on-error")
             .help("Fail to translate a module when a portion is not able to be translated")
             .takes_value(false))
        .arg(Arg::with_name("reduce-type-annotations")
             .long("reduce-type-annotations")
             .help("Reduces the number of explicit type annotations where it should be safe to do so")
             .takes_value(false))
        .arg(Arg::with_name("output-file")
             .long("output")
             .short("o")
             .help("Write the output to a specified file")
             .takes_value(true))
        .arg(Arg::with_name("reorganize-definitions")
             .long("reorganize-definitions")
             .help("Output file in such a way that the refactoring tool can deduplicate code")
             .takes_value(false))
        .get_matches();

    // Build a TranslationConfig from the command line
    let c_path = canonicalize(Path::new(matches.value_of("INPUT").unwrap())).unwrap();
    let tcfg = TranslationConfig {
        fail_on_error:          matches.is_present("fail-on-error"),
        reloop_cfgs:            matches.is_present("reloop-cfgs"),
        fail_on_multiple:       matches.is_present("fail-on-multiple"),
        dump_function_cfgs:     matches.is_present("dump-function-cfgs"),
        json_function_cfgs:     matches.is_present("json-function-cfgs"),
        dump_cfg_liveness:      matches.is_present("dump-cfgs-liveness"),
        dump_structures:        matches.is_present("dump-structures"),
        debug_relooper_labels:  matches.is_present("debug-labels"),
        cross_checks:           matches.is_present("cross-checks"),
        cross_check_configs:    matches.values_of("cross-check-config")
            .map(|vals| vals.map(String::from).collect::<Vec<_>>())
            .unwrap_or_default(),
        prefix_function_names:  matches.value_of("prefix-function-names")
            .map(String::from),
        translate_asm:          matches.is_present("translate-asm"),
        translate_entry:        matches.is_present("translate-entry"),
        translate_valist:       matches.is_present("translate-valist"),
        use_c_loop_info:        !matches.is_present("ignore-c-loop-info"),
        use_c_multiple_info:    !matches.is_present("ignore-c-multiple-info"),
        simplify_structures:    !matches.is_present("no-simplify-structures"),
        reduce_type_annotations:matches.is_present("reduce-type-annotations"),
        reorganize_definitions: matches.is_present("reorganize-definitions"),
        emit_module:            matches.is_present("emit-module"),
        main_file:              Some(c_path.with_extension("")),
        panic_on_translator_failure: {
            match matches.value_of("invalid-code") {
                Some("panic") => true,
                Some("compile_error") => false,
                _ => panic!("Invalid option"),
            }
        },
        replace_unsupported_decls: ReplaceMode::Extern,
    };

    let dump_untyped_context = matches.is_present("dump-untyped-clang-ast");
    let dump_typed_context = matches.is_present("dump-typed-clang-ast");
    let pretty_typed_context = matches.is_present("pretty-typed-clang-ast");

    // Extract the untyped AST from the CBOR file
    let untyped_context = match ast_exporter::get_untyped_ast(&c_path) {
        Err(e) => panic!("{:#?}", e),
        Ok(cxt) => cxt,
    };

    if dump_untyped_context {
        println!("CBOR Clang AST");
        println!("{:#?}", untyped_context);
    }

    // Convert this into a typed AST
    let typed_context = {
        let mut conv = ConversionContext::new(&untyped_context);
        conv.convert(&untyped_context);
        conv.typed_context
    };

    if dump_typed_context {
        println!("Clang AST");
        println!("{:#?}", typed_context);
    }

    if pretty_typed_context {
        println!("Pretty-printed Clang AST");
        println!("{:#?}", Printer::new(stdout()).print(&typed_context));
    }


//    use syn::parse;
//    use quote::ToTokens;
//    use quote::Tokens;
//    if let parse::IResult::Done(_, t) = parse::ty("[u32; 10]") {
//        let mut tokens = Tokens::new();
//        t.to_tokens(&mut tokens);
//        println!("{}", tokens.as_str());
//    }

    // Perform the translation

    let translated_string = transpiler::translator::translate(typed_context, tcfg);
    let output_path = get_output_path(&c_path, matches.value_of("output-file"));

    let mut file = match File::create(output_path) {
        Ok(file) => file,
        Err(e) => panic!("Unable to open file for writing: {}", e),
    };

    match file.write_all(translated_string.as_bytes()) {
        Ok(()) => (),
        Err(e) => panic!("Unable to write translation to file: {}", e),
    };
}

fn get_output_path(input_file: &Path, specified_path: Option<&str>) -> PathBuf {
    if let Some(output_file) = specified_path {
        return Path::new(output_file).to_path_buf();
    }

    // with_extension will clear the .cbor; set_extension will change .c to .rs
    // even if there is no extension for some reason, this will still work
    let mut path_buf = input_file.with_extension("");

    // When an output file name is not explictly specified, we should convert files
    // with dashes to underscores, as they are not allowed in rust file names.
    let file_name = path_buf.file_name().unwrap().to_str().unwrap().replace('-', "_");

    path_buf.set_file_name(file_name);
    path_buf.set_extension("rs");
    path_buf
}
