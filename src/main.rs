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

mod util;
mod ast_equiv;
mod fold;
mod visit;
mod print_spans;
mod remove_paren;

mod bindings;
mod driver;
mod span_fix;
mod matcher;
mod matcher_impls;
mod subst;
mod rewrite;
mod rewrite_impls;
mod file_rewrite;

mod api;
mod transform;


fn split_args<'a>(args: &'a [String]) -> (&'a [String], &'a [String]) {
    for i in 0 .. args.len() {
        if &args[i] == "--" {
            return (&args[..i], &args[i..]);
        }
    }
    (args, &[])
}

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let (my_args, rustc_args) = split_args(&args[1..]);
    let rewrite_mode_str = &my_args[0];
    let transform_name = &my_args[1];
    let transform_args = &my_args[2..];

    let rewrite_mode = match &rewrite_mode_str as &str {
        "inplace" => file_rewrite::RewriteMode::InPlace,
        "alongside" => file_rewrite::RewriteMode::Alongside,
        "print" => file_rewrite::RewriteMode::Print,
        _ => panic!("unknown rewrite mode {:?}", rewrite_mode_str),
    };

    let (krate, sess) = driver::parse_crate(rustc_args);
    let krate = span_fix::fix_spans(&sess, krate);

    let krate2 = transform::get_transform(transform_name, transform_args)
        .transform(krate.clone(), &sess);

    let rws = rewrite::rewrite(&sess, &krate, &krate2);
    if rws.len() == 0 {
        println!("(no files to rewrite)");
    } else {
        file_rewrite::rewrite_files(sess.codemap(), &rws, rewrite_mode);
    }
}
