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


mod bindings;
mod ast_equiv;
mod util;
mod fold;
mod visit;

mod driver;
mod matcher;
mod matcher_impls;
mod subst;
mod rewrite;
mod rewrite_impls;
mod file_rewrite;


struct PrintSpanVisitor<'a> {
    cm: &'a CodeMap,
}

impl<'a> PrintSpanVisitor<'a> {
    fn span_desc(&self, span: Span) -> String {
        if span == DUMMY_SP {
            return "DUMMY_SP".to_owned();
        }

        let lo = self.cm.lookup_byte_offset(span.lo);
        let hi = self.cm.lookup_byte_offset(span.hi);
        let mut s = format!("{}: {} .. {}", lo.fm.name, lo.pos.0, hi.pos.0);

        let span2 = span.source_callsite();
        if span2 != span {
            s.push_str(" < ");
            s.push_str(&self.span_desc(span2));
        }

        s
    }
}


impl<'a> Visitor<'a> for PrintSpanVisitor<'a> {
    fn visit_expr(&mut self, x: &'a Expr) {
        println!("[EXPR] {}: {}",
                 self.span_desc(x.span), pprust::expr_to_string(x));
        syntax::visit::walk_expr(self, x);
    }

    fn visit_pat(&mut self, x: &'a Pat) {
        println!("[PAT] {}: {}",
                 self.span_desc(x.span), pprust::pat_to_string(x));
        syntax::visit::walk_pat(self, x);
    }

    fn visit_stmt(&mut self, x: &'a Stmt) {
        println!("[STMT] {}: {}",
                 self.span_desc(x.span), pprust::stmt_to_string(x));
        syntax::visit::walk_stmt(self, x);
    }

    fn visit_item(&mut self, x: &'a Item) {
        println!("[ITEM] {}: {}",
                 self.span_desc(x.span), pprust::item_to_string(x));
        syntax::visit::walk_item(self, x);
    }
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

    let (krate, sess) = driver::parse_crate(remaining_args);
    println!("krate:\n ===\n{}\n ===\n",
             pprust::to_string(|s| s.print_mod(&krate.module, &[])));


    let pattern_src = read_file(&pattern_file);
    let repl_src = read_file(&repl_file);

    let krate2 = match mode as &str {
        "expr" => {
            let pattern = driver::parse_expr(&sess, &pattern_src).unwrap();
            let repl = driver::parse_expr(&sess, &repl_src).unwrap();
            (PrintSpanVisitor {
                cm: sess.codemap(),
            }).visit_expr(&repl);
            matcher::fold_match(pattern, krate.clone(), |_, bnd| {
                repl.clone().subst(&bnd)
            })
        },

        "stmt" => {
            let pattern = driver::parse_stmts(&sess, &pattern_src).unwrap();
            let repl = driver::parse_stmts(&sess, &repl_src).unwrap();
            println!("pat = {:?}", pattern);
            println!("repl = {:?}", repl);

            let mut init_mcx = matcher::MatchCtxt::new();
            init_mcx.set_type("__i", bindings::Type::Ident);

            matcher::fold_match_with(init_mcx, pattern, krate.clone(), |_, bnd| {
                repl.clone().subst(&bnd)
            })
        },

        _ => panic!("unknown mode: {}", mode),
    };

    println!("krate2:\n ===\n{}\n ===\n",
             pprust::to_string(|s| s.print_mod(&krate2.module, &[])));
    /*
    visit::walk_crate(&mut PrintSpanVisitor {
        cm: sess.codemap(),
    }, &krate2);
    */



    let rws = rewrite::rewrite(&sess, &krate, &krate2);
    println!("rws = {:?}", rws);

    file_rewrite::rewrite_files(sess.codemap(), &rws);
}
