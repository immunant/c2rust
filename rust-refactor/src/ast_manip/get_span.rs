//! `GetSpan` trait for obtaining the `Span` of a generic AST node.
use syntax::ast::*;
use syntax::ptr::P;
use syntax::source_map::{Span, Spanned};

use ast_manip::util::extended_span;


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
