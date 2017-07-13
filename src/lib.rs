#![feature(
    i128_type,
    rustc_private,
    trace_macros,
)]
extern crate arena;
extern crate libc;
extern crate diff;
#[macro_use] extern crate json;
#[macro_use] extern crate log;
extern crate rustc;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_metadata;
extern crate rustc_resolve;
extern crate rustc_trans;
extern crate syntax;
extern crate syntax_ext;
extern crate syntax_pos;

#[macro_use] mod macros;

pub mod util;
mod cursor;

// AST queries
mod ast_equiv;
mod visit;
pub mod visit_node;
pub mod print_spans;
mod get_node_id;
mod get_span;
mod contains_mark;
pub mod pick_node;

// AST edits
mod fold;
pub mod fold_node;
pub mod remove_paren;
pub mod span_fix;
mod seq_edit;
mod make_ast;

// Higher-level AST manipulation
mod fn_edit;
pub mod path_edit;
mod dataflow;
mod api;

// Pattern matching and substitution
mod bindings;
mod matcher;
mod matcher_impls;
mod subst;

// High-level interfaces
pub mod driver;
pub mod command;
pub mod interact;

// Command implementations
pub mod transform;
pub mod mark_adjust;

// Source rewriting
pub mod rewrite;
mod rewrite_impls;
pub mod file_rewrite;
