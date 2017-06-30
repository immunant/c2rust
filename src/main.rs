#![feature(
    i128_type,
    rustc_private,
    trace_macros,
)]
extern crate rustc;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_metadata;
extern crate rustc_resolve;
extern crate rustc_trans;
extern crate syntax;
extern crate syntax_ext;
extern crate syntax_pos;

use std::env;
use std::fs::File;
use std::io::Read;
use rustc_errors::{Diagnostic, ColorConfig};
use syntax::ast::{Expr, Crate};
use syntax::parse::{self, ParseSess};
use syntax::print::pprust;
use syntax::ptr::P;

use syntax::ast::{Pat, Stmt, Item};
use syntax::codemap::{CodeMap, Span, DUMMY_SP};
use syntax::visit::Visitor;

use subst::Subst;


mod util;
mod ast_equiv;
mod fold;
mod visit;
mod print_spans;

mod bindings;
mod driver;
mod span_fix;
mod matcher;
mod matcher_impls;
mod subst;
mod rewrite;
mod rewrite_impls;
mod file_rewrite;



fn read_file(path: &str) -> String {
    let mut f = File::open(path).unwrap();
    let mut buf = String::new();
    f.read_to_string(&mut buf).unwrap();
    buf
}

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let mode = &args[1];
    let pattern_file = &args[2];
    let repl_file = &args[3];
    let remaining_args = &args[4..];

    let (krate, sess) = driver::parse_crate(remaining_args);
    let krate = span_fix::fix_spans(&sess, krate);


    let pattern_src = read_file(&pattern_file);
    let repl_src = read_file(&repl_file);

    let krate2 = match mode as &str {
        "expr" => {
            let pattern = driver::parse_expr(&sess, &pattern_src).unwrap();
            let repl = driver::parse_expr(&sess, &repl_src).unwrap();
            matcher::fold_match(pattern, krate.clone(), |_, bnd| {
                repl.clone().subst(&bnd)
            })
        },

        "stmt" => {
            let pattern = driver::parse_stmts(&sess, &pattern_src).unwrap();
            let repl = driver::parse_stmts(&sess, &repl_src).unwrap();

            let mut init_mcx = matcher::MatchCtxt::new();
            // FIXME: hack for while loop pattern
            init_mcx.set_type("__i", bindings::Type::Ident);

            matcher::fold_match_with(init_mcx, pattern, krate.clone(), |_, bnd| {
                repl.clone().subst(&bnd)
            })
        },

        _ => panic!("unknown mode: {}", mode),
    };


    let rws = rewrite::rewrite(&sess, &krate, &krate2);
    if rws.len() == 0 {
        println!("(no files to rewrite)");
    } else {
        file_rewrite::rewrite_files(sess.codemap(), &rws);
    }
}
