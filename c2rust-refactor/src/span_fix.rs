//! This module contains some AST folds for fixing up span information.  There are a few sources of
//! bogus spans, which cause major confusion during rewriting.
//!
//!  - `format!`.  The expansion of `format!("...", x)` copies the span of the expression `x` to a
//!    number of other nodes, such as `&x`, the `__arg0` pattern used to match `x`, and the
//!    reference to `std::fmt::Display::fmt` used to format `x`.  We'd like to detect all of these
//!    bogus spans and reset them.

use smallvec::SmallVec;
use std::mem;
use syntax::ast::*;
use syntax::mut_visit::{self, MutVisitor};
use syntax::ptr::P;
use syntax::source_map::{Span, DUMMY_SP};
use syntax_pos::hygiene::SyntaxContext;

use crate::ast_manip::util::extend_span_attrs;
use crate::ast_manip::MutVisit;

/// MutVisitor for fixing expansions of `format!`.  `format!(..., foo)` generates an expression `&foo`,
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
    where
        F: FnOnce(&mut Self) -> R,
    {
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

impl MutVisitor for FixFormat {
    fn visit_expr(&mut self, e: &mut P<Expr>) {
        if self.in_format
            && e.span.ctxt() == SyntaxContext::empty()
            && matches!([e.node] ExprKind::AddrOf(..))
        {
            trace!("EXITING format! at {:?}", e);
            // Current node is the `&foo`.  We need to change its span.  On recursing into `foo`,
            // we are no longer inside a `format!` invocation.
            let new_span = self.parent_span;
            self.descend(false, e.span, |this| {
                mut_visit::noop_visit_expr(e, this);
                e.span = new_span;
            })
        } else if !self.in_format && self.is_format_entry(&e) {
            trace!("ENTERING format! at {:?}", e);
            self.descend(true, e.span, |this| mut_visit::noop_visit_expr(e, this))
        } else {
            let in_format = self.in_format;
            self.descend(in_format, e.span, |this| {
                mut_visit::noop_visit_expr(e, this)
            })
        }
    }

    fn visit_mac(&mut self, mac: &mut Mac) {
        mut_visit::noop_visit_mac(mac, self)
    }
}

/// MutVisitor for fixing up spans of items with attributes.  We set the span of the item to include
/// all its attrs, so that removing the item will also remove the attrs from the source text.
struct FixAttrs;

impl MutVisitor for FixAttrs {
    fn flat_map_item(&mut self, i: P<Item>) -> SmallVec<[P<Item>; 1]> {
        let new_span = extend_span_attrs(i.span, &i.attrs);
        let i = if new_span != i.span {
            i.map(|i| Item {
                span: new_span,
                ..i
            })
        } else {
            i
        };
        mut_visit::noop_flat_map_item(i, self)
    }

    fn flat_map_foreign_item(&mut self, fi: ForeignItem) -> SmallVec<[ForeignItem; 1]> {
        let new_span = extend_span_attrs(fi.span, &fi.attrs);
        let fi = if new_span != fi.span {
            ForeignItem {
                span: new_span,
                ..fi
            }
        } else {
            fi
        };
        mut_visit::noop_flat_map_foreign_item(fi, self)
    }

    fn visit_mac(&mut self, mac: &mut Mac) {
        mut_visit::noop_visit_mac(mac, self)
    }
}

#[cfg_attr(feature = "profile", flame)]
pub fn fix_format<T: MutVisit>(node: &mut T) {
    let mut fix_format = FixFormat {
        parent_span: DUMMY_SP,
        in_format: false,
    };
    node.visit(&mut fix_format)
}

#[cfg_attr(feature = "profile", flame)]
pub fn fix_attr_spans<T: MutVisit>(node: &mut T) {
    node.visit(&mut FixAttrs)
}
