extern crate cbor;
extern crate clap;
extern crate syn;
extern crate ast_importer;

extern crate quote;

use std::io::Cursor;
use std::env;
use std::io::Error;
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
        .author("Eric Mertens <emertens@galois.com>")
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
        Ok(cxt) => {
            if dump_untyped_context {
                println!("{:#?}", cxt);
            }
            cxt
        },
    };

    // Conditionally convert this to a typed AST
    if dump_typed_context || pretty_typed_context {
        let mut conv = ConversionContext::new(&untyped_context);
        conv.convert(&untyped_context);

        if dump_typed_context {
            println!("Typed Clang AST");
            println!("{:#?}", conv.typed_context);
        }

        if pretty_typed_context {
            println!("Pretty-printed typed Clang AST");
            println!("{:#?}", Printer::new().print(&conv.typed_context));
        }
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
    println!("{}", translate(untyped_context));
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


