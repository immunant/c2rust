//! `GetSpan` trait for obtaining the `Span` of a generic AST node.
use rustc_ast::*;
use rustc_ast::token::Token;
use rustc_ast::ptr::P;
use rustc_span::source_map::{Span, Spanned};

use crate::ast_manip::util::extend_span_attrs;

/// Trait for obtaining the `Span` of a generic AST node.
pub trait GetSpan {
    fn get_span(&self) -> Span;
}

impl<T> GetSpan for Spanned<T> {
    fn get_span(&self) -> Span {
        self.span
    }
}

impl<'a, T: GetSpan> GetSpan for &'a T {
    fn get_span(&self) -> Span {
        <T as GetSpan>::get_span(self)
    }
}

impl<T: GetSpan> GetSpan for P<T> {
    fn get_span(&self) -> Span {
        <T as GetSpan>::get_span(self)
    }
}

include!(concat!(env!("OUT_DIR"), "/get_span_gen.inc.rs"));
