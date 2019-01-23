//! This module contains some AST folds for fixing up span information.  There are a few sources of
//! bogus spans, which cause major confusion during rewriting.
//!
//!  - `format!`.  The expansion of `format!("...", x)` copies the span of the expression `x` to a
//!    number of other nodes, such as `&x`, the `__arg0` pattern used to match `x`, and the
//!    reference to `std::fmt::Display::fmt` used to format `x`.  We'd like to detect all of these
//!    bogus spans and reset them.

use std::mem;
use smallvec::SmallVec;
use syntax::ast::*;
use syntax::source_map::{Span, DUMMY_SP};
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax_pos::hygiene::SyntaxContext;

use crate::ast_manip::Fold;
use crate::ast_manip::util::extended_span;


/// Folder for fixing expansions of `format!`.  `format!(..., foo)` generates an expression `&foo`,
/// and gives it the same span as `foo` itself (notably, *not* a macro generated span), which
/// causes problems for us later on.  This folder detects nodes like `&foo` and gives them a
/// macro-generated span to fix the problem.
struct FixFormat {
    /// The span of the most recent ancestor `Expr`.
    parent_span: Span,
    /// Are we currently inside (the macro-generated part of) a `format!` invocation?
    in_format: bool,
}

impl FixFormat {
    fn descend<F, R>(&mut self, in_format: bool, cur_span: Span, f: F) -> R
            where F: FnOnce(&mut Self) -> R {
        let old_in_format = mem::replace(&mut self.in_format, in_format);
        let old_parent_span = mem::replace(&mut self.parent_span, cur_span);
        let r = f(self);
        self.in_format = old_in_format;
        self.parent_span = old_parent_span;
        r
    }

    /// Check if we should set `in_format` when descending into this expr.  Note that this doesn't
    /// need to fire for *every* `format!`-generated expr - it just needs to fire somewhere above
    /// the spliced-in arguments (`foo`).
    fn is_format_entry(&self, e: &Expr) -> bool {
        // We're looking for the `match` that `format!` uses for unpacking its arguments.  We
        // recognize it by its span: it's macro-generated, but the "macro definition" actually
        // points to the format string, which lies inside the macro invocation itself.

        if !matches!([e.node] ExprKind::Match(..)) {
            return false;
        }

        if e.span.ctxt() == SyntaxContext::empty() {
            return false;
        }

        e.span.source_callsite().contains(e.span)
    }
}

impl Folder for FixFormat {
    fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
        if self.in_format &&
           e.span.ctxt() == SyntaxContext::empty() &&
           matches!([e.node] ExprKind::AddrOf(..)) {
            trace!("EXITING format! at {:?}", e);
            // Current node is the `&foo`.  We need to change its span.  On recursing into `foo`,
            // we are no longer inside a `format!` invocation.
            let new_span = self.parent_span;
            self.descend(false, e.span, |this| e.map(|e| {
                let mut e = fold::noop_fold_expr(e, this);
                e.span = new_span;
                e
            }))
        } else if !self.in_format && self.is_format_entry(&e) {
            trace!("ENTERING format! at {:?}", e);
            self.descend(true, e.span, |this| e.map(|e| fold::noop_fold_expr(e, this)))
        } else {
            let in_format = self.in_format;
            self.descend(in_format, e.span, |this| e.map(|e| fold::noop_fold_expr(e, this)))
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


pub fn fix_format<T: Fold>(node: T) -> <T as Fold>::Result {
    let mut fix_format = FixFormat {
        parent_span: DUMMY_SP,
        in_format: false,
    };
    node.fold(&mut fix_format)
}

pub fn fix_attr_spans<T: Fold>(node: T) -> <T as Fold>::Result {
    node.fold(&mut FixAttrs)
}
