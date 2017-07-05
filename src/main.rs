#![feature(
    i128_type,
    rustc_private,
    trace_macros,
)]
extern crate arena;
extern crate getopts;
extern crate rustc;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_metadata;
extern crate rustc_resolve;
extern crate rustc_trans;
extern crate syntax;
extern crate syntax_ext;
extern crate syntax_pos;

mod util;
mod ast_equiv;
mod fold;
mod visit;
mod print_spans;
mod remove_paren;
mod cursor;

mod bindings;
mod driver;
mod span_fix;
mod matcher;
mod matcher_impls;
mod seq_edit;
mod subst;
mod rewrite;
mod rewrite_impls;
mod file_rewrite;

mod api;
mod transform;


use std::mem;


struct Options {
    rewrite_mode: file_rewrite::RewriteMode,
    transform_name: String,
    transform_args: Vec<String>,
    rustc_args: Vec<String>,
}

fn find<T: PartialEq<U>, U: ?Sized>(xs: &[T], x: &U) -> Option<usize> {
    for i in 0 .. xs.len() {
        if &xs[i] == x {
            return Some(i);
        }
    }
    None
}

fn print_usage(prog: &str, opts: &[getopts::OptGroup]) {
    let brief = format!("Usage: {} [options] transform [args...] -- [rustc args...]", prog);
    print!("{}", getopts::usage(&brief, opts));
}

fn parse_opts(argv: Vec<String>) -> Option<Options> {
    use getopts::{opt, HasArg, Occur};
    let opts = &[
        opt("r", "rewrite-mode",
            "output rewritten code `inplace`, `alongside` the original, \
               or `print` to screen? (default: print)",
            "MODE", HasArg::Yes, Occur::Optional),
        opt("c", "cursor", 
            "a cursor position, used to filter some rewrite operations",
            "FILE:LINE:COL", HasArg::Yes, Occur::Multi),
        opt("h", "help", 
            "display usage information",
            "", HasArg::No, Occur::Optional),
    ];

    // Separate idiomize args from rustc args
    let (local_args, mut rustc_args) = match find(&argv, "--") {
        Some(idx) => {
            let mut argv = argv;
            let rest = argv.split_off(idx);
            (argv, rest)
        },
        None => {
            println!("Expected `--` followed by rustc arguments");
            print_usage(&argv[0], opts);
            return None;
        },
    };

    // Parse idiomize args
    let prog = &local_args[0];

    let m = match getopts::getopts(&local_args[1..], opts) {
        Ok(m) => m,
        Err(e) => {
            println!("{}", e.to_string());
            return None;
        },
    };

    if m.opt_present("h") {
        print_usage(prog, opts);
        return None;
    }

    let rewrite_mode = match m.opt_str("rewrite-mode") {
        Some(mode_str) => match &mode_str as &str {
            "inplace" => file_rewrite::RewriteMode::InPlace,
            "alongside" => file_rewrite::RewriteMode::Alongside,
            "print" => file_rewrite::RewriteMode::Print,
            _ => {
                println!("Unknown rewrite mode: {}", mode_str);
                return None;
            },
        },
        None => file_rewrite::RewriteMode::Print,
    };

    if m.free.len() < 1 {
        println!("Missing transform name");
        return None;
    }
    let mut iter = m.free.clone().into_iter();
    let transform_name = iter.next().unwrap();
    let transform_args = iter.collect();

    // Replace "--" with the program name
    rustc_args[0] = "rustc".to_owned();

    Some(Options {
        rewrite_mode,
        transform_name,
        transform_args,
        rustc_args,
    })
}

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let opts = match parse_opts(args) {
        Some(x) => x,
        None => return,
    };

    let transform = transform::get_transform(&opts.transform_name, &opts.transform_args);

    driver::with_crate_and_context(&opts.rustc_args, transform.min_phase(), |krate, cx| {
        let krate = span_fix::fix_spans(cx.session(), krate);
        let krate2 = transform.transform(krate.clone(), cx);

        let rws = rewrite::rewrite(cx.session(), &krate, &krate2);
        if rws.len() == 0 {
            println!("(no files to rewrite)");
        } else {
            file_rewrite::rewrite_files(cx.session().codemap(), &rws, opts.rewrite_mode);
        }
    });
}
