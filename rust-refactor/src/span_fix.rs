//! This module contains some AST folds for fixing up span information.  There are a few sources of
//! bogus spans, which cause major confusion during rewriting.
//!
//!  - `format!`.  The expansion of `format!("...", x)` copies the span of the expression `x` to a
//!    number of other nodes, such as `&x`, the `__arg0` pattern used to match `x`, and the
//!    reference to `std::fmt::Display::fmt` used to format `x`.  We'd like to detect all of these
//!    bogus spans and reset them.

use rustc::session::Session;
use smallvec::SmallVec;
use syntax::ast::*;
use syntax::source_map::{Span, SourceMap};
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax_pos::FileName;

use ast_manip::{AstEquiv, Fold};
use ast_manip::util::{with_span_text, extended_span};
use driver;


/// Folder for fixing expansions of `format!` and related macros.  It remembers the span in use on
/// entry to the macro expansion, and applies that same span to all nodes inside, except for
/// `Expr`s copied from the macro arguments.
struct FixFormat<'a> {
    sess: &'a Session,
    codemap: &'a SourceMap,
    current_expansion: Option<Span>,
}

impl<'a> Folder for FixFormat<'a> {
    fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
        let old_ce = self.current_expansion;
        if self.current_expansion.is_none() {
            // Check if this is the top of a `format!` expansion.
            let lo = self.codemap.lookup_byte_offset(e.span.lo());
            if let &FileName::Macros(_) = &lo.fm.name {
                self.current_expansion = Some(e.span)
            }
        } else {
            // Check if we are exiting the current expansion, traversing into a copy of a macro
            // argument expression.
            let current_expansion = self.current_expansion.unwrap();
            if e.span.ctxt() != current_expansion.ctxt() &&
               e.span.ctxt() == current_expansion.source_callsite().ctxt() {
                // The current node at least *claims* to be a macro argument.  But `format!`
                // applies the argument's span to all kinds of random nodes, so we have to
                // validate the span info by parsing the indicated text and comparing `e` to
                // the resulting `Expr`.

                // TODO: Behavior is a litte weird if the argument is another `format!()`.  In
                // something like `format!("{}", format!("{}", 12345))`, we set `current_expansion`
                // on entry to the outer `format!`, and don't clear it until we get to the `12345`
                // argument.  The inner `format!` essentially gets treated as if it were part of
                // the outer one.  Not a big problem at the moment, but it is a little odd.
                let mut parsed = None;
                with_span_text(self.codemap, e.span, |s| {
                    parsed = Some(driver::parse_expr(self.sess, s));
                });
                if let Some(parsed) = parsed {
                    if parsed.ast_equiv(&e) {
                        self.current_expansion = None;
                    }
                }
            }
        }
        let result = e.map(|e| fold::noop_fold_expr(e, self));
        self.current_expansion = old_ce;
        result
    }

    fn new_span(&mut self, sp: Span) -> Span {
        if let Some(span) = self.current_expansion {
            span
        } else {
            sp
        }
    }

    fn fold_mac(&mut self, mac: Mac) -> Mac {
        fold::noop_fold_mac(mac, self)
    }
}


/// Folder for fixing up spans of items with attributes.  We set the span of the item to include
/// all its attrs, so that removing the item will also remove the attrs from the source text.
struct FixAttrs;

impl Folder for FixAttrs {
    fn fold_item(&mut self, i: P<Item>) -> SmallVec<[P<Item>; 1]> {
        let new_span = extended_span(i.span, &i.attrs);
        let i =
            if new_span != i.span {
                i.map(|i| Item { span: new_span, ..i })
            } else {
                i
            };
        fold::noop_fold_item(i, self)
    }

    fn fold_foreign_item(&mut self, fi: ForeignItem) -> SmallVec<[ForeignItem; 1]> {
        let new_span = extended_span(fi.span, &fi.attrs);
        let fi =
            if new_span != fi.span {
                ForeignItem { span: new_span, ..fi }
            } else {
                fi
            };
        fold::noop_fold_foreign_item(fi, self)
    }

    fn fold_mac(&mut self, mac: Mac) -> Mac {
        fold::noop_fold_mac(mac, self)
    }
}


pub fn fix_format<T: Fold>(sess: &Session, node: T) -> <T as Fold>::Result {
    let mut fix_format = FixFormat {
        sess: sess,
        codemap: sess.codemap(),
        current_expansion: None,
    };
    node.fold(&mut fix_format)
}

pub fn fix_attr_spans<T: Fold>(node: T) -> <T as Fold>::Result {
    node.fold(&mut FixAttrs)
}
