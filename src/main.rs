#![feature(
    i128_type,
    rustc_private,
    trace_macros,
)]
extern crate arena;
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

    let transform = transform::get_transform(transform_name, transform_args);


    driver::with_crate_and_context(rustc_args, transform.min_phase(), |krate, cx| {
        let krate = span_fix::fix_spans(cx.session(), krate);
        let krate2 = transform.transform(krate.clone(), cx);

        let rws = rewrite::rewrite(cx.session(), &krate, &krate2);
        if rws.len() == 0 {
            println!("(no files to rewrite)");
        } else {
            file_rewrite::rewrite_files(cx.session().codemap(), &rws, rewrite_mode);
        }
    });
}
