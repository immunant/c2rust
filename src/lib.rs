#![feature(
    i128_type,
    rustc_private,
    trace_macros,
)]
extern crate arena;
extern crate diff;
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
mod get_node_id;
mod get_span;

mod bindings;
pub mod driver;
pub mod span_fix;
mod matcher;
mod matcher_impls;
mod seq_edit;
mod subst;
pub mod rewrite;
mod rewrite_impls;
pub mod file_rewrite;

mod api;
pub mod transform;
