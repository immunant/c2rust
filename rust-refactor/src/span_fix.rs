//! This module contains some AST folds for fixing up span information.  There are a few sources of
//! bogus spans, which cause major confusion during rewriting.
//!
//!  - Macro expansion.  The expansion of the macro has its span set to the macro definition.  It's
//!    much more convenient to have the span of the expansion point to the invocation instead.
//!  - `format!`.  The expansion of `format!("...", x)` copies the span of the expression `x` to a
//!    number of other nodes, such as `&x`, the `__arg0` pattern used to match `x`, and the
//!    reference to `std::fmt::Display::fmt` used to format `x`.  We'd like to detect all of these
//!    bogus spans and reset them.

use rustc::session::Session;
use syntax::ast::*;
use syntax::codemap::{Span, CodeMap};
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::util::small_vector::SmallVector;
use syntax_pos::FileName;
use syntax_pos::hygiene::SyntaxContext;

use ast_manip::{AstEquiv, Fold};
use ast_manip::util::{with_span_text, extended_span};
use driver;
use util::Lone;


/// Folder for fixing expansions of `format!` and related macros.  It remembers the span in use on
/// entry to the macro expansion, and applies that same span to all nodes inside, except for
/// `Expr`s copied from the macro arguments.
struct FixFormat<'a> {
    sess: &'a Session,
    codemap: &'a CodeMap,
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


/// Folder for fixing up spans of macro expansions.  When we enter a particular macro expansion, we
/// set the span of the topmost node in the expansion to the span of the macro invocation.
///
/// Note that this span adjustment discards the `SyntaxContext` for the topmost node in each macro
/// expansion.  In general, discarding `SyntaxContext`s is a bad idea - resolution relies on them
/// to interpret `$crate`, feature gate checking consults them to determine where unstable features
/// are allowed, and there are likely others.  Here, we are hoping that the "important" spans are
/// in some subtree of the macro expansion, not the very top-level node.  For `$crate`, this is
/// certainly the case (a macro can't expand to a lone `Ident`), and in practice, feature gate /
/// `#[allow_internal_unstable]` checks seem to work that way as well.
struct FixMacros {
    in_macro: bool,
}

fn invocation_span(span: Span) -> Span {
    let mut span = span;
    while span.ctxt() != SyntaxContext::empty() {
        let new_span = span.source_callsite();
        assert!(new_span != span);
        span = new_span;
    }
    span
}

impl Folder for FixMacros {
    fn fold_expr(&mut self, mut e: P<Expr>) -> P<Expr> {
        let was_in_macro = self.in_macro;
        self.in_macro = e.span.ctxt() != SyntaxContext::empty();

        let old_span = e.span;
        // Clear all macro spans in the node and its children.
        e = e.map(|e| fold::noop_fold_expr(e, self));

        if !was_in_macro && self.in_macro {
            // This is the topmost node in a macro expansion.  Set its span to the span of the
            // macro invocation.

            e = e.map(|e| Expr {
                span: invocation_span(old_span),
                .. e
            });
        }

        self.in_macro = was_in_macro;

        e
    }

    fn fold_stmt(&mut self, mut s: Stmt) -> SmallVector<Stmt> {
        let was_in_macro = self.in_macro;
        self.in_macro = s.span.ctxt() != SyntaxContext::empty();

        let old_span = s.span;
        // Clear all macro spans in the node and its children.
        s = fold::noop_fold_stmt(s, self).lone();

        if !was_in_macro && self.in_macro {
            // This is the topmost node in a macro expansion.  Set its span to the span of the
            // macro invocation.

            s = Stmt {
                span: invocation_span(old_span),
                .. s
            };
        }

        self.in_macro = was_in_macro;

        SmallVector::one(s)
    }

    fn fold_item(&mut self, mut i: P<Item>) -> SmallVector<P<Item>> {
        let was_in_macro = self.in_macro;
        self.in_macro = i.span.ctxt() != SyntaxContext::empty();

        let old_span = i.span;
        // Clear all macro spans in the node and its children.
        i = fold::noop_fold_item(i, self).lone();

        if !was_in_macro && self.in_macro {
            // This is the topmost node in a macro expansion.  Set its span to the span of the
            // macro invocation.

            i = i.map(|i| Item {
                span: invocation_span(old_span),
                .. i
            });
        }

        self.in_macro = was_in_macro;

        SmallVector::one(i)
    }

    // TODO: Eventually we should extend this to work on the remaining node types where macros can
    // appear (Pat, Ty, and the Item-likes).

    fn fold_mac(&mut self, mac: Mac) -> Mac {
        fold::noop_fold_mac(mac, self)
    }
}


/// Folder for fixing up spans of items with attributes.  We set the span of the item to include
/// all its attrs, so that removing the item will also remove the attrs from the source text.
struct FixAttrs;

impl Folder for FixAttrs {
    fn fold_item(&mut self, i: P<Item>) -> SmallVector<P<Item>> {
        let new_span = extended_span(i.span, &i.attrs);
        let i =
            if new_span != i.span {
                i.map(|i| Item { span: new_span, ..i })
            } else {
                i
            };
        fold::noop_fold_item(i, self)
    }

    fn fold_foreign_item(&mut self, fi: ForeignItem) -> SmallVector<ForeignItem> {
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


pub fn fix_attr_spans<T: Fold>(node: T) -> <T as Fold>::Result {
    node.fold(&mut FixAttrs)
}

pub fn fix_spans<T: Fold<Result=T>>(sess: &Session, node: T) -> T {
    let mut fix_format = FixFormat {
        sess: sess,
        codemap: sess.codemap(),
        current_expansion: None,
    };
    let node = node.fold(&mut fix_format);

    let mut fix_macros = FixMacros {
        in_macro: false,
    };
    let node = node.fold(&mut fix_macros);

    node
}
