use std::ops::Deref;
use std::rc::Rc;
use diff;
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
use get_node_id;
use get_span;
use rewrite::{Rewrite, RewriteCtxt, RewriteCtxtRef, VisitStep, NodeTable, TextAdjust};
use util;
use util::Lone;


fn describe(sess: &Session, span: Span) -> String {
    let cm = sess.codemap();
    let lo = cm.lookup_byte_offset(span.lo);
    let hi = cm.lookup_byte_offset(span.hi);
    let src = &lo.fm.src.as_ref().unwrap()[lo.pos.0 as usize .. hi.pos.0 as usize];

    if Rc::ptr_eq(&lo.fm, &hi.fm) {
        format!("{}: {} .. {} = {}", lo.fm.name, lo.pos.0, hi.pos.0, src)
    } else {
        format!("{}: {} .. {}: {} = {}", lo.fm.name, lo.pos.0, hi.fm.name, hi.pos.0, src)
    }
}


trait Splice: Rewrite+'static {
    fn span(&self) -> Span;
    fn id(&self) -> NodeId;

    fn to_string(&self) -> String;

    type Parsed: Deref<Target=Self>;
    fn parse(sess: &Session, src: &str) -> Self::Parsed;

    fn node_table<'a, 's>(rcx: &'a mut RewriteCtxt<'s>) -> &'a mut NodeTable<'s, Self>;

    fn get_adjustment(&self, _rcx: &RewriteCtxt) -> TextAdjust {
        TextAdjust::None
    }


    fn get_node<'a, 's>(mut rcx: RewriteCtxtRef<'s, 'a>, id: NodeId) -> Option<&'s Self> {
        Self::node_table(&mut rcx).get(id)
    }

    fn splice_recycled_span(new: &Self, old_span: Span, mut rcx: RewriteCtxtRef) {
        let printed = new.to_string();
        let reparsed = Self::parse(rcx.session(), &printed);

        if old_span.lo != old_span.hi {
            println!("REWRITE {}", describe(rcx.session(), old_span));
            println!("   INTO {}", describe(rcx.session(), reparsed.span()));
        } else {
            println!("INSERT AT {}", describe(rcx.session(), old_span));
            println!("     TEXT {}", describe(rcx.session(), reparsed.span()));
        }

        let mut rewrites = Vec::new();
        let old_fs = rcx.replace_fresh_start(new.span());
        Rewrite::rewrite_fresh(new, &reparsed, rcx.with_rewrites(&mut rewrites));
        rcx.replace_fresh_start(old_fs);

        let adj = new.get_adjustment(&rcx);
        rcx.record(old_span, reparsed.span(), rewrites, adj);
    }

    fn splice_recycled(new: &Self, old: &Self, rcx: RewriteCtxtRef) {
        Splice::splice_recycled_span(new, old.span(), rcx);
    }

    fn splice_fresh(new: &Self, reparsed: &Self, mut rcx: RewriteCtxtRef) -> bool {
        // Don't try to replace the entire fresh subtree with old text.   This breaks an infinite
        // recursion when a non-splice-point child differs between the old and new ASTs.  In such a
        // situation, `splice_recycled` wants to replace the old text with newly printed text
        // (because `old != new`), but `splice_fresh` wants to replace the printed text with the
        // old text (because `new` still has a source span covering the old text).  It's always
        // safe to use printed text instead of old text, so we bail out here if we detect this.
        if new.span() == rcx.fresh_start() {
            return false;
        }

        let old = match Self::get_node(rcx.borrow(), new.id()) {
            Some(x) => x,
            None => {
                return false;
            },
        };


        if old.span() == DUMMY_SP {
            return false;
        }

        let fm = rcx.session().codemap().lookup_byte_offset(old.span().lo).fm;
        if fm.abs_path.is_none() {
            return false;
        }

        println!("REVERT {}", describe(rcx.session(), reparsed.span()));
        println!("    TO {}", describe(rcx.session(), old.span()));

        let mut rewrites = Vec::new();
        let mark = rcx.mark();
        let failed = Rewrite::rewrite_recycled(new, old, rcx.with_rewrites(&mut rewrites));
        if failed {
            rcx.rewind(mark);
            return false;
        }

        let adj = new.get_adjustment(&rcx);
        rcx.record(reparsed.span(), old.span(), rewrites, adj);
        true
    }
}


struct SelfDeref<T>(pub T);
impl<T> Deref for SelfDeref<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.0
    }
}

impl Splice for Expr {
    fn span(&self) -> Span {
        util::extended_span(self.span, &self.attrs)
    }

    fn id(&self) -> NodeId {
        self.id
    }

    fn to_string(&self) -> String {
        pprust::expr_to_string(self)
    }

    type Parsed = P<Expr>;
    fn parse(sess: &Session, src: &str) -> Self::Parsed {
        driver::parse_expr(sess, src)
    }

    fn node_table<'a, 's>(rcx: &'a mut RewriteCtxt<'s>) -> &'a mut NodeTable<'s, Self> {
        rcx.old_exprs()
    }

    fn get_adjustment(&self, rcx: &RewriteCtxt) -> TextAdjust {
        // Check for cases where we can safely omit parentheses.
        let can_omit_parens =
            if let ExprKind::Block(_) = self.node {
                true
            } else if let Some(parent_step) = rcx.parent_step() {
                if let &VisitStep::StmtExpr = parent_step {
                    true
                } else if let Some(parent) = parent_step.get_expr_kind() {
                    let current = &self.node;
                    match (parent, current) {
                        (&ExprKind::Binary(parent_op, _, _),
                         &ExprKind::Binary(current_op, _, _)) => {
                            let parent_assoc = AssocOp::from_ast_binop(parent_op.node);
                            let current_assoc = AssocOp::from_ast_binop(current_op.node);
                            if current_assoc.precedence() > parent_assoc.precedence() {
                                true
                            } else if current_assoc.precedence() == parent_assoc.precedence() {
                                match parent_assoc.fixity() {
                                    Fixity::Left => parent_step.is_left(),
                                    Fixity::Right => parent_step.is_right(),
                                    _ => false,
                                }
                            } else {
                                false
                            }
                        },
                        (_, _) => false,
                    }
                } else {
                    false
                }
            } else {
                false
            };

        if can_omit_parens {
            TextAdjust::None
        } else {
            TextAdjust::Parenthesize
        }
    }
}

impl Splice for Pat {
    fn span(&self) -> Span {
        self.span
    }

    fn id(&self) -> NodeId {
        self.id
    }

    fn to_string(&self) -> String {
        pprust::pat_to_string(self)
    }

    type Parsed = P<Pat>;
    fn parse(sess: &Session, src: &str) -> Self::Parsed {
        driver::parse_pat(sess, src)
    }

    fn node_table<'a, 's>(rcx: &'a mut RewriteCtxt<'s>) -> &'a mut NodeTable<'s, Self> {
        rcx.old_pats()
    }
}

impl Splice for Ty {
    fn span(&self) -> Span {
        self.span
    }

    fn id(&self) -> NodeId {
        self.id
    }

    fn to_string(&self) -> String {
        pprust::ty_to_string(self)
    }

    type Parsed = P<Ty>;
    fn parse(sess: &Session, src: &str) -> Self::Parsed {
        driver::parse_ty(sess, src)
    }

    fn node_table<'a, 's>(rcx: &'a mut RewriteCtxt<'s>) -> &'a mut NodeTable<'s, Self> {
        rcx.old_tys()
    }
}

impl Splice for Stmt {
    fn span(&self) -> Span {
        self.span
    }

    fn id(&self) -> NodeId {
        self.id
    }

    fn to_string(&self) -> String {
        pprust::stmt_to_string(self)
    }

    type Parsed = SelfDeref<Stmt>;
    fn parse(sess: &Session, src: &str) -> Self::Parsed {
        let stmt = driver::parse_stmts(sess, src).lone();
        SelfDeref(stmt)
    }

    fn node_table<'a, 's>(rcx: &'a mut RewriteCtxt<'s>) -> &'a mut NodeTable<'s, Self> {
        rcx.old_stmts()
    }
}

impl Splice for Item {
    fn span(&self) -> Span {
        println!("checking span of item {:?}", self);
        util::extended_span(self.span, &self.attrs)
    }

    fn id(&self) -> NodeId {
        self.id
    }

    fn to_string(&self) -> String {
        pprust::item_to_string(self)
    }

    type Parsed = P<Item>;
    fn parse(sess: &Session, src: &str) -> Self::Parsed {
        driver::parse_items(sess, src).lone()
    }

    fn node_table<'a, 's>(rcx: &'a mut RewriteCtxt<'s>) -> &'a mut NodeTable<'s, Self> {
        rcx.old_items()
    }
}


trait SeqItem {
    #[inline]
    fn supported() -> bool { false }

    fn get_span(&self) -> Span { unimplemented!() }
    fn get_id(&self) -> NodeId { unimplemented!() }

    fn splice_recycled_span(new: &Self, old_span: Span, mut rcx: RewriteCtxtRef) {
        unimplemented!()
    }
}

impl<T: SeqItem> SeqItem for P<T> {
    #[inline]
    fn supported() -> bool { <T as SeqItem>::supported() }

    fn get_span(&self) -> Span {
        <T as SeqItem>::get_span(self)
    }

    fn get_id(&self) -> NodeId {
        <T as SeqItem>::get_id(self)
    }

    fn splice_recycled_span(new: &Self, old_span: Span, rcx: RewriteCtxtRef) {
        <T as SeqItem>::splice_recycled_span(new, old_span, rcx);
    }
}

impl<T: SeqItem> SeqItem for Rc<T> {
    #[inline]
    fn supported() -> bool { <T as SeqItem>::supported() }

    fn get_span(&self) -> Span {
        <T as SeqItem>::get_span(self)
    }

    fn get_id(&self) -> NodeId {
        <T as SeqItem>::get_id(self)
    }

    fn splice_recycled_span(new: &Self, old_span: Span, rcx: RewriteCtxtRef) {
        <T as SeqItem>::splice_recycled_span(new, old_span, rcx);
    }
}

// Stub impls
impl<T: SeqItem> SeqItem for Spanned<T> {}
impl<T: SeqItem> SeqItem for Option<T> {}
impl<A: SeqItem, B: SeqItem> SeqItem for (A, B) {}
impl<A: SeqItem, B: SeqItem, C: SeqItem> SeqItem for (A, B, C) {}


impl<T: Rewrite> Rewrite for P<T> {
    fn rewrite_recycled(&self, old: &Self, rcx: RewriteCtxtRef) -> bool {
        <T as Rewrite>::rewrite_recycled(self, old, rcx)
    }

    fn rewrite_fresh(&self, reparsed: &Self, rcx: RewriteCtxtRef) {
        <T as Rewrite>::rewrite_fresh(self, reparsed, rcx);
    }
}

impl<T: Rewrite> Rewrite for Rc<T> {
    fn rewrite_recycled(&self, old: &Self, rcx: RewriteCtxtRef) -> bool {
        <T as Rewrite>::rewrite_recycled(self, old, rcx)
    }

    fn rewrite_fresh(&self, reparsed: &Self, rcx: RewriteCtxtRef) {
        <T as Rewrite>::rewrite_fresh(self, reparsed, rcx);
    }
}

impl<T: Rewrite> Rewrite for Spanned<T> {
    fn rewrite_recycled(&self, old: &Self, rcx: RewriteCtxtRef) -> bool {
        <T as Rewrite>::rewrite_recycled(&self.node, &old.node, rcx)
    }

    fn rewrite_fresh(&self, reparsed: &Self, rcx: RewriteCtxtRef) {
        <T as Rewrite>::rewrite_fresh(&self.node, &reparsed.node, rcx);
    }
}

impl<T: Rewrite> Rewrite for Option<T> {
    fn rewrite_recycled(&self, old: &Self, rcx: RewriteCtxtRef) -> bool {
        match (self, old) {
            (&Some(ref x1),
             &Some(ref x2)) => {
                Rewrite::rewrite_recycled(x1, x2, rcx)
            }
            (&None, &None) => false,
            (_, _) => true,
        }
    }

    fn rewrite_fresh(&self, reparsed: &Self, rcx: RewriteCtxtRef) {
        match (self, reparsed) {
            (&Some(ref x1),
             &Some(ref x2)) => {
                Rewrite::rewrite_fresh(x1, x2, rcx);
            },
            (&None, &None) => {},
            (_, _) => panic!("new and reparsed ASTs differ"),
        }
    }
}

impl<A: Rewrite, B: Rewrite> Rewrite for (A, B) {
    fn rewrite_recycled(&self, old: &Self, mut rcx: RewriteCtxtRef) -> bool {
        <A as Rewrite>::rewrite_recycled(&self.0, &old.0, rcx.borrow()) ||
        <B as Rewrite>::rewrite_recycled(&self.1, &old.1, rcx.borrow()) ||
        false
    }

    fn rewrite_fresh(&self, reparsed: &Self, mut rcx: RewriteCtxtRef) {
        <A as Rewrite>::rewrite_fresh(&self.0, &reparsed.0, rcx.borrow());
        <B as Rewrite>::rewrite_fresh(&self.1, &reparsed.1, rcx.borrow());
    }
}

impl<A: Rewrite, B: Rewrite, C: Rewrite> Rewrite for (A, B, C) {
    fn rewrite_recycled(&self, old: &Self, mut rcx: RewriteCtxtRef) -> bool {
        <A as Rewrite>::rewrite_recycled(&self.0, &old.0, rcx.borrow()) ||
        <B as Rewrite>::rewrite_recycled(&self.1, &old.1, rcx.borrow()) ||
        <C as Rewrite>::rewrite_recycled(&self.2, &old.2, rcx.borrow()) ||
        false
    }

    fn rewrite_fresh(&self, reparsed: &Self, mut rcx: RewriteCtxtRef) {
        <A as Rewrite>::rewrite_fresh(&self.0, &reparsed.0, rcx.borrow());
        <B as Rewrite>::rewrite_fresh(&self.1, &reparsed.1, rcx.borrow());
        <C as Rewrite>::rewrite_fresh(&self.2, &reparsed.2, rcx.borrow());
    }
}


impl<T: Rewrite+SeqItem> Rewrite for [T] {
    fn rewrite_recycled(&self, old: &Self, mut rcx: RewriteCtxtRef) -> bool {
        if !<T as SeqItem>::supported() {
            if self.len() != old.len() {
                return true;
            }

            for i in 0 .. self.len() {
                if Rewrite::rewrite_recycled(&self[i], &old[i], rcx.borrow()) {
                    return true;
                }
            }
            false
        } else {
            if old.len() == 0 && self.len() != 0 {
                // We can't handle this case because it provides us with no span information about
                // the `old` side.  We need the spans so we know where to splice in any new items.
                return true;
            }

            let new_ids = self.iter().map(|x| x.get_id()).collect::<Vec<_>>();
            let old_ids = old.iter().map(|x| x.get_id()).collect::<Vec<_>>();

            let mut i = 0;
            let mut j = 0;

            for step in diff::slice(&old_ids, &new_ids) {
                match step {
                    diff::Result::Left(_) => {
                        // There's an item on the left corresponding to nothing on the right.
                        // Delete the item from the left.
                        println!("DELETE {}", describe(rcx.session(), old[i].get_span()));
                        rcx.record(old[i].get_span(), DUMMY_SP, vec![], TextAdjust::None);
                        i += 1;
                    },
                    diff::Result::Right(_) => {
                        // There's an item on the right corresponding to nothing on the left.
                        // Insert the item before the current item on the left, rewriting
                        // recursively.
                        let old_span =
                            if i > 0 {
                                let s = old[i - 1].get_span();
                                Span {
                                    lo: s.hi,
                                    hi: s.hi,
                                    ctxt: s.ctxt,
                                }
                            } else {
                                let s = old[0].get_span();
                                Span {
                                    lo: s.lo,
                                    hi: s.lo,
                                    ctxt: s.ctxt,
                                }
                            };
                        SeqItem::splice_recycled_span(&self[j], old_span, rcx.borrow());
                        j += 1;
                    },
                    diff::Result::Both(_, _) => {
                        if Rewrite::rewrite_recycled(&self[j], &old[i], rcx.borrow()) {
                            return true;
                        }
                        i += 1;
                        j += 1;
                    },
                }
            }

            false
        }
    }

    fn rewrite_fresh(&self, reparsed: &Self, mut rcx: RewriteCtxtRef) {
        assert!(self.len() == reparsed.len());

        for i in 0 .. self.len() {
            Rewrite::rewrite_fresh(&self[i], &reparsed[i], rcx.borrow());
        }
    }
}

impl<T: Rewrite+SeqItem> Rewrite for Vec<T> {
    fn rewrite_recycled(&self, old: &Self, rcx: RewriteCtxtRef) -> bool {
        <[T] as Rewrite>::rewrite_recycled(&self, &old, rcx)
    }

    fn rewrite_fresh(&self, reparsed: &Self, rcx: RewriteCtxtRef) {
        <[T] as Rewrite>::rewrite_fresh(&self, &reparsed, rcx);
    }
}

impl<T: Rewrite+SeqItem> Rewrite for ThinVec<T> {
    fn rewrite_recycled(&self, old: &Self, rcx: RewriteCtxtRef) -> bool {
        <[T] as Rewrite>::rewrite_recycled(&self, &old, rcx)
    }

    fn rewrite_fresh(&self, reparsed: &Self, rcx: RewriteCtxtRef) {
        <[T] as Rewrite>::rewrite_fresh(&self, &reparsed, rcx);
    }
}


include!(concat!(env!("OUT_DIR"), "/rewrite_impls_gen.inc.rs"));
