use std::ops::Deref;
use std::rc::Rc;
use rustc::session::Session;
use syntax::ast::*;
use syntax::abi::Abi;
use syntax::codemap::{Span, Spanned, DUMMY_SP};
use syntax::ext::hygiene::SyntaxContext;
use syntax::print::pprust;
use syntax::ptr::P;
use syntax::tokenstream::{TokenStream, ThinTokenStream};
use syntax::util::parser::{AssocOp, Fixity};

use ast_equiv::AstEquiv;
use driver;
use rewrite::{Rewrite, RewriteCtxt, RewriteCtxtRef, VisitStep, NodeTable, TextAdjust};


pub trait GetSpan {
    fn get_span(&self) -> Span;
}

impl<T> GetSpan for Spanned<T> {
    fn get_span(&self) -> Span {
        self.span
    }
}

include!(concat!(env!("OUT_DIR"), "/get_span_gen.inc.rs"));
