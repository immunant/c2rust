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


mod bindings;
mod ast_equiv;
mod util;
mod driver;
mod matcher;
mod matcher_impls;
mod replacer;
mod rewriter;
mod rewriter_impls;
mod file_rewrite;


enum NodeType {
    Item,
    Stmt,
    Expr,
}

fn mk_diagnostic(msg: &str) -> Diagnostic {
    let h = rustc_errors::Handler::with_tty_emitter(ColorConfig::Auto, true, false, None);
    let diag = h.struct_err(msg).into_diagnostic();
    diag
}

fn parse_crate(src: &str) -> Result<Crate, Diagnostic> {
    let sess = ParseSess::new();
    let name = "<string>".to_owned();
    let src = src.to_owned();
    let r = match parse::parse_crate_from_source_str(name, src, &sess) {
        Ok(krate) => Ok(krate),
        Err(e) => Err(e.into_diagnostic()),
    };
    r
}

fn parse_expr(src: &str) -> Result<P<Expr>, Diagnostic> {
    let sess = ParseSess::new();
    let name = "<string>".to_owned();
    let src = src.to_owned();
    let r = match parse::parse_expr_from_source_str(name, src, &sess) {
        Ok(expr) => Ok(expr),
        Err(e) => Err(e.into_diagnostic()),
    };
    r
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
    let pattern_file = &args[1];
    let repl_file = &args[2];
    let remaining_args = &args[3..];
    println!("remaining args = {:?}", remaining_args);

    let pattern_src = read_file(&pattern_file);
    let pattern = parse_expr(&pattern_src).unwrap();

    let repl_src = read_file(&repl_file);
    let repl = parse_expr(&repl_src).unwrap();

    let (krate, sess) = driver::parse_crate(remaining_args);
    println!("krate:\n ===\n{}\n ===\n",
             pprust::to_string(|s| s.print_mod(&krate.module, &[])));

    let krate2 = replacer::find_and_replace_expr(&pattern, &repl, &krate);
    println!("krate2:\n ===\n{}\n ===\n",
             pprust::to_string(|s| s.print_mod(&krate2.module, &[])));

    let mut rw = rewriter::RewriteCtxt::new();
    rw.rewrite(&krate, &krate2);
    println!("rw = {:?}", rw);

    file_rewrite::rewrite_files(sess.codemap(), rw.rewrites());
}
