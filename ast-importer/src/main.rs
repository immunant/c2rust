#[macro_use]
extern crate clap;
extern crate cbor;
extern crate ast_importer;

use std::io::{Error, stdout, Cursor};
use std::io::prelude::*;
use std::fs::File;
use cbor::Decoder;
use ast_importer::clang_ast::process;
use ast_importer::c_ast::*;
use ast_importer::c_ast::Printer;
use ast_importer::clang_ast::AstContext;
use ast_importer::translator::TranslationConfig;
use clap::{Arg, App};

fn main() {

    let matches = App::new("AST Importer")
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

        // CFG/Relooper related
        .arg(Arg::with_name("reloop-cfgs")
            .long("reloop-cfgs")
            .help("Translate function bodies using a CFG/Relooper approach")
            .takes_value(false))
        .arg(Arg::with_name("no-simplify-structures")
            .requires("reloop-cfgs")
            .long("no-simplify-structures")
            .help("Do not run a pass to simplify structures")
            .takes_value(false))
        .arg(Arg::with_name("use-c-loop-info")
            .requires("reloop-cfgs")
            .long("use-c-loop-info")
            .help("Keep and use information about C loops")
            .takes_value(false))
        .arg(Arg::with_name("use-c-multiple-info")
            .requires("reloop-cfgs")
            .long("use-c-multiple-info")
            .help("Keep and use information about C branches")
            .takes_value(false))
        .arg(Arg::with_name("dump-function-cfgs")
            .requires("reloop-cfgs")
            .long("ddump-function-cfgs")
            .help("Dumps into files DOT visualizations of the CFGs of every function")
            .takes_value(false))
        .arg(Arg::with_name("dump-structures")
            .requires("reloop-cfgs")
            .long("ddump-structures")
            .help("Dumps out to STDERR the intermediate structures produced by relooper")
            .takes_value(false))
        .arg(Arg::with_name("debug-labels")
            .requires("reloop-cfgs")
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
            .help("Sets the input CBOR file to use")
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
        .get_matches();

    // Build a TranslationConfig from the command line
    let tcfg = TranslationConfig {
        reloop_cfgs:            matches.is_present("reloop-cfgs"),
        dump_function_cfgs:     matches.is_present("dump-function-cfgs"),
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
        use_c_loop_info:        matches.is_present("use-c-loop-info"),
        use_c_multiple_info:    matches.is_present("use-c-multiple-info"),
        simplify_structures:    !matches.is_present("no-simplify-structures"),
        emit_module:            matches.is_present("emit-module"),
        panic_on_translator_failure: {
            match matches.value_of("invalid-code") {
                Some("panic") => true,
                Some("compile_error") => false,
                _ => panic!("Invalid option"),
            }
        },
    };
    let file = matches.value_of("INPUT").unwrap();
    let dump_untyped_context = matches.is_present("dump-untyped-clang-ast");
    let dump_typed_context = matches.is_present("dump-typed-clang-ast");
    let pretty_typed_context = matches.is_present("pretty-typed-clang-ast");

    // Extract the untyped AST from the CBOR file 
    let untyped_context = match parse_untyped_ast(file) {
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
    use ast_importer::translator::translate;

    let mut conv = ConversionContext::new(&untyped_context);
    conv.convert(&untyped_context);

    println!("{}", translate(conv.typed_context, tcfg));
}

fn parse_untyped_ast(filename: &str) -> Result<AstContext, Error> {
    let mut f = File::open(filename)?;
    let mut buffer = vec![];
    f.read_to_end(&mut buffer)?;

    let mut cursor: Decoder<Cursor<Vec<u8>>> = Decoder::from_bytes(buffer);
    let items = cursor.items();

    match process(items) {
        Ok(cxt) => Ok(cxt),
        Err(e) => panic!("{:#?}", e),
    }
}


