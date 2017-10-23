extern crate cbor;
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

fn main() {

    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Usage: ast-importer FILENAME");
        return;
    }

    let output = parse_and_dump(&args[1]);
    match output {
        Err(e) => println!("{:#?}", e),
        Ok(cxt) => {
            println!("{:#?}", cxt);

            let mut conv = ConversionContext::new(&cxt);
            conv.convert(&cxt);
            println!("{:#?}", Printer::new().print(&conv.typed_context));
        },
    }

    use syn::parse;
    use quote::ToTokens;
    use quote::Tokens;
    if let parse::IResult::Done(_, t) = parse::ty("[u32; 10]") {
        let mut tokens = Tokens::new();
        t.to_tokens(&mut tokens);
        println!("{}", tokens.as_str());
    }

    use ast_importer::translator::translate;
    let output = parse_and_dump(&args[1]);
    match output {
        Err(e) => println!("{:#?}", e),
        Ok(cxt) => {
            println!("{}", translate(cxt));
        },
    }

}

fn parse_and_dump(filename: &str) -> Result<AstContext, Error> {
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


