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


fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let mode = &args[1];
    let remaining_args = &args[2..];

    let (krate, sess) = driver::parse_crate(remaining_args);
    let krate = span_fix::fix_spans(&sess, krate);

    let krate2 = transform::get_transform(mode).transform(krate.clone(), &sess);

    let rws = rewrite::rewrite(&sess, &krate, &krate2);
    if rws.len() == 0 {
        println!("(no files to rewrite)");
    } else {
        file_rewrite::rewrite_files(sess.codemap(), &rws);
    }
}
