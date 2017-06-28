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
use syntax::codemap::DUMMY_SP;
use syntax::parse::{self, ParseSess};
use syntax::print::pprust;
use syntax::ptr::P;

use subst::Subst;


mod bindings;
mod ast_equiv;
mod util;
mod fold;

mod driver;
mod matcher;
mod matcher_impls;
mod subst;
mod rewriter;
mod rewriter_impls;
mod file_rewrite;


enum NodeType {
    Item,
    Stmt,
    Expr,
}

fn read_file(path: &str) -> String {
    println!("reading {}", path);
    let mut f = File::open(path).unwrap();
    let mut buf = String::new();
    f.read_to_string(&mut buf).unwrap();
    println!("read data: {:?}", buf);
    buf
}

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let mode = &args[1];
    let pattern_file = &args[2];
    let repl_file = &args[3];
    let remaining_args = &args[4..];
    println!("remaining args = {:?}", remaining_args);

    let (krate, sess) = driver::parse_crate(remaining_args);
    println!("krate:\n ===\n{}\n ===\n",
             pprust::to_string(|s| s.print_mod(&krate.module, &[])));


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
            let pattern = driver::parse_stmt(&sess, &pattern_src).unwrap();
            let repl = driver::parse_stmt(&sess, &repl_src).unwrap();
            matcher::fold_match(pattern, krate.clone(), |_, bnd| {
                repl.clone().subst(&bnd)
            })
        },

        _ => panic!("unknown mode: {}", mode),
    };

    println!("krate2:\n ===\n{}\n ===\n",
             pprust::to_string(|s| s.print_mod(&krate2.module, &[])));



    let mut rw = rewriter::RewriteCtxt::new();
    rw.rewrite(&krate, &krate2);
    println!("rw = {:?}", rw);

    file_rewrite::rewrite_files(sess.codemap(), rw.rewrites());
}
