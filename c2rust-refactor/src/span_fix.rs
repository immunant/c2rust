//! This module contains some AST folds for fixing up span information.  There are a few sources of
//! bogus spans, which cause major confusion during rewriting.
//!
//!  - `format!`.  The expansion of `format!("...", x)` copies the span of the expression `x` to a
//!    number of other nodes, such as `&x`, the `__arg0` pattern used to match `x`, and the
//!    reference to `std::fmt::Display::fmt` used to format `x`.  We'd like to detect all of these
//!    bogus spans and reset them.

use log::trace;
use rustc_ast::mut_visit::{self, MutVisitor};
use rustc_ast::ptr::P;
use rustc_ast::*;
use rustc_span::source_map::{Span, DUMMY_SP};
use smallvec::SmallVec;
use std::mem;

use crate::ast_manip::util::extend_span_attrs;
use crate::ast_manip::MutVisit;

/// MutVisitor for fixing expansions of `format!`.  `format!(..., foo)` generates an expression `&foo`,
/// and gives it the same span as `foo` itself (notably, *not* a macro generated span), which
/// causes problems for us later on.  This folder detects nodes like `&foo` and gives them a
/// macro-generated span to fix the problem.
struct FixFormat {
    ctxt: FormatCtxt,
}

#[derive(Clone)]
struct FormatCtxt {
    /// The span of the most recent ancestor `Expr`.
    parent_span: Span,
    /// Are we currently inside (the macro-generated part of) a `format!` invocation?
    in_format: bool,
    /// Are we currently inside the match in (the macro-generated part of) a
    /// `format!` invocation?
    in_match: bool,
}

impl FormatCtxt {
    fn new(span: Span) -> Self {
        FormatCtxt {
            parent_span: span,
            in_format: false,
            in_match: false,
        }
    }

    fn enter_span(&self, span: Span) -> Self {
        FormatCtxt {
            parent_span: span,
            ..*self
        }
    }

    fn enter_format(&self, span: Span) -> Self {
        FormatCtxt {
            parent_span: span,
            in_format: true,
            ..*self
        }
    }

    fn enter_match(&self, span: Span) -> Self {
        FormatCtxt {
            parent_span: span,
            in_match: true,
            ..*self
        }
    }
}

impl FixFormat {
    fn descend<F, R>(&mut self, new_ctxt: FormatCtxt, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let old_ctxt = mem::replace(&mut self.ctxt, new_ctxt);
        let r = f(self);
        self.ctxt = old_ctxt;
        r
    }

    /// Check if we should set `in_format` when descending into this expr.  Note that this doesn't
    /// need to fire for *every* `format!`-generated expr - it just needs to fire somewhere above
    /// the spliced-in arguments (`foo`).
    fn is_format_entry(&self, e: &Expr) -> bool {
        // We're looking for the `match` that `format!` uses for unpacking its arguments.  We
        // recognize it by its span: it's macro-generated, but the "macro definition" actually
        // points to the format string, which lies inside the macro invocation itself.

        if !e.span.from_expansion() {
            return false;
        }

        if let ExprKind::Call(callee, _) = &e.kind {
            if let ExprKind::Path(None, path) = &callee.kind {
                let matches_fmt_args = path.segments.len() == 4
                    && path.segments[1].ident.as_str() == "fmt"
                    && path.segments[2].ident.as_str() == "Arguments"
                    && (path.segments[3].ident.as_str() == "new_v1"
                        || path.segments[3].ident.as_str() == "new_v1_formatted");
                return matches_fmt_args;
            }
        }

        false
    }
}

impl MutVisitor for FixFormat {
    fn visit_expr(&mut self, e: &mut P<Expr>) {
        if !e.span.from_expansion()
            && self.ctxt.in_match
            && crate::matches!([e.kind] ExprKind::AddrOf(..))
        {
            trace!("EXITING format! at {:?}", e);
            // Current node is the `&foo`.  We need to change its span.  On
            // recursing into `foo`, we are no longer inside a `format!`
            // invocation.
            let mac_span = self.ctxt.parent_span;
            let leave_ctxt = FormatCtxt::new(e.span);
            self.descend(leave_ctxt, |this| {
                mut_visit::noop_visit_expr(e, this);
                e.span = mac_span;
            })
        } else if !e.span.from_expansion() && self.ctxt.in_format && !self.ctxt.in_match {
            trace!("Fixing format! string at {:?}", e);
            let mac_span = self.ctxt.parent_span;
            let new_ctxt = self.ctxt.enter_span(mac_span);
            self.descend(new_ctxt, |this| {
                mut_visit::noop_visit_expr(e, this);
                e.span = mac_span;
            })
        } else if self.ctxt.in_format && crate::matches!([e.kind] ExprKind::Match(..)) {
            let new_ctxt = self.ctxt.enter_match(e.span);
            self.descend(new_ctxt, |this| mut_visit::noop_visit_expr(e, this))
        } else if !self.ctxt.in_format && self.is_format_entry(&e) {
            trace!("ENTERING format! at {:?}", e);
            let new_ctxt = self.ctxt.enter_format(e.span);
            self.descend(new_ctxt, |this| mut_visit::noop_visit_expr(e, this))
        } else {
            let new_ctxt = self.ctxt.enter_span(e.span);
            self.descend(new_ctxt, |this| mut_visit::noop_visit_expr(e, this))
        }
    }

    fn visit_mac_call(&mut self, mac: &mut MacCall) {
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

    fn flat_map_foreign_item(&mut self, mut fi: P<ForeignItem>) -> SmallVec<[P<ForeignItem>; 1]> {
        let new_span = extend_span_attrs(fi.span, &fi.attrs);
        if new_span != fi.span {
            fi.span = new_span;
        }
        mut_visit::noop_flat_map_foreign_item(fi, self)
    }

    fn visit_mac_call(&mut self, mac: &mut MacCall) {
        mut_visit::noop_visit_mac(mac, self)
    }
}

#[cfg_attr(feature = "profile", flame)]
pub fn fix_format<T: MutVisit>(node: &mut T) {
    let mut fix_format = FixFormat {
        ctxt: FormatCtxt::new(DUMMY_SP),
    };
    node.visit(&mut fix_format)
}

#[cfg_attr(feature = "profile", flame)]
pub fn fix_attr_spans<T: MutVisit>(node: &mut T) {
    node.visit(&mut FixAttrs)
}
