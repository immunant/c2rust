#[macro_use]
extern crate clap;
extern crate c2rust_transpile;
extern crate c2rust_ast_exporter;

use std::io::stdout;
use std::io::prelude::*;
use std::fs::{File, canonicalize};
use std::path::{Path, PathBuf};
use std::process;
use c2rust_transpile::c_ast::*;
use c2rust_transpile::c_ast::Printer;
use c2rust_transpile::translator::{ReplaceMode,TranslationConfig};
use clap::App;


fn main() {
    let yaml = load_yaml!("args.yaml");
    let matches = App::from_yaml(yaml)
        .get_matches();

    // Build a TranslationConfig from the command line
    let c_path = canonicalize(Path::new(matches.value_of("INPUT").unwrap())).unwrap();
    let extra_args: Vec<&str> = match matches.values_of("extra-clang-args") {
        Some(args) => args.collect(),
        None => Vec::new(),
    };
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
    let untyped_context = match c2rust_ast_exporter::get_untyped_ast(&c_path, &extra_args) {
        Err(e) => {
            eprintln!("Error: {:}", e);
            process::exit(1);
        }
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

    let translated_string = c2rust_transpile::translator::translate(typed_context, tcfg);
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
