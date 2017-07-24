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
extern crate regex;
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
pub mod cursor;

// AST queries
pub mod ast_equiv;
pub mod visit;
pub mod visit_node;
pub mod print_spans;
pub mod get_node_id;
pub mod get_span;
pub mod contains_mark;
pub mod pick_node;

// AST edits
pub mod fold;
pub mod fold_node;
pub mod remove_paren;
pub mod span_fix;
pub mod seq_edit;
pub mod make_ast;

// Higher-level AST manipulation
pub mod fn_edit;
pub mod path_edit;
pub mod dataflow;
pub mod api;

// Pattern matching and substitution
pub mod bindings;
pub mod matcher;
pub mod matcher_impls;
pub mod subst;

// High-level interfaces
pub mod driver;
pub mod command;
pub mod interact;
pub mod plugin;

// Command implementations
pub mod transform;
pub mod mark_adjust;

// Source rewriting
pub mod rewrite;
pub mod rewrite_impls;
pub mod file_rewrite;
