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
use clap::{Arg, App};

fn main() {

    let matches = App::new("AST Importer")
        .version("0.1.0")
        .author(crate_authors!())
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
        .arg(Arg::with_name("INPUT")
            .help("Sets the input CBOR file to use")
            .required(true)
            .index(1))
        .get_matches();

    let file = matches.value_of("INPUT").unwrap();
    let dump_untyped_context = matches.is_present("dump-untyped-clang-ast");
    let dump_typed_context = matches.is_present("dump-typed-clang-ast");
    let pretty_typed_context = matches.is_present("pretty-typed-clang-ast");

    // Extract from the CBOR file the untyped AST
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
    //use ast_importer::translator::translate;
    use ast_importer::translator::translate;

    let mut conv = ConversionContext::new(&untyped_context);
    conv.convert(&untyped_context);

    println!("{}", translate(&conv.typed_context));
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


