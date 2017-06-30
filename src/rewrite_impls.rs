use std::ops::Deref;
use std::rc::Rc;
use rustc::session::Session;
use syntax::ast::*;
use syntax::abi::Abi;
use syntax::ast::*;
use syntax::codemap::{Span, Spanned, DUMMY_SP};
use syntax::ext::hygiene::SyntaxContext;
use syntax::print::pprust;
use syntax::ptr::P;
use syntax::tokenstream::{TokenStream, ThinTokenStream};

use ast_equiv::AstEquiv;
use driver;
use rewrite::{self, Rewrite, RewriteCtxt, RewriteCtxtRef, NodeTable};


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


trait Splice: Rewrite+AstEquiv+::std::fmt::Debug+'static {
    fn span(&self) -> Span;
    fn id(&self) -> NodeId;

    fn to_string(&self) -> String;

    type Parsed: Deref<Target=Self>;
    fn parse(sess: &Session, src: &str) -> Self::Parsed;

    fn node_table<'a, 's>(rcx: &'a mut RewriteCtxt<'s>) -> &'a mut NodeTable<'s, Self>;


    /*
    fn get_valid<'a, 's>(mut rcx: RewriteCtxtRef<'s, 'a>, id: NodeId) -> Option<&'s Self> {
        let sess = rcx.session();
        let cm = sess.codemap();
        Self::node_table(&mut rcx).get_valid(id, |node| {
            // Extract the text under the span and check if it's equal to the actual node.
            let lo = cm.lookup_byte_offset(node.span().lo);
            let hi = cm.lookup_byte_offset(node.span().hi);
            let file_src = match lo.fm.src.as_ref() {
                Some(x) => x,
                None => return false,
            };
            let node_src = &file_src[lo.pos.0 as usize .. hi.pos.0 as usize];

            let parsed = Self::parse(sess, &node_src);
            println!("COMPARE {:?}", node);
            println!("   WITH {:?}", &parsed as &Self);
            let v = node.ast_equiv(&parsed);
            println!(" VALID? {}", if v { "yes" } else { " *** NO ***" });
            v
        })
    }
    */

    fn get_node<'a, 's>(mut rcx: RewriteCtxtRef<'s, 'a>, id: NodeId) -> Option<&'s Self> {
        Self::node_table(&mut rcx).get(id)
    }

    fn splice_recycled(new: &Self, old: &Self, mut rcx: RewriteCtxtRef) {
        let printed = new.to_string();
        let reparsed = Self::parse(rcx.session(), &printed);

        println!("REWRITE(R) {}", describe(rcx.session(), old.span()));
        println!("      INTO {}", describe(rcx.session(), reparsed.span()));

        let mut rewrites = Vec::new();
        let old_fs = rcx.replace_fresh_start(new.span());
        Rewrite::rewrite_fresh(new, &reparsed, rcx.with_rewrites(&mut rewrites));
        rcx.replace_fresh_start(old_fs);

        rcx.record(old.span(), reparsed.span(), rewrites);
    }

    fn splice_fresh(new: &Self, reparsed: &Self, mut rcx: RewriteCtxtRef) -> bool {
        // Don't try to replace the entire fresh subtree with old text.   This breaks an infinite
        // recursion when a non-splice-point child differs between the old and new ASTs.  In such a
        // situation, `splice_recycled` wants to replace the old text with newly printed text
        // (because `old != new`), but `splice_fresh` wants to replace the printed text with the
        // old text (because `new` still has a source span covering the old text).  It's always
        // safe to use printed text instead of old text, so we bail out here if we detect this.
        println!("looking at {} = {:?}", describe(rcx.session(), new.span()), new);
        if new.span() == rcx.fresh_start() {
            println!("   skipping: fresh start");
            return false;
        }

        let old = match Self::get_node(rcx.borrow(), new.id()) {
            Some(x) => x,
            None => {
                println!("   skipping: no valid node with id {:?}", new.id());
                return false;
            },
        };

        println!("    old is {} = {:?}", describe(rcx.session(), old.span()), old);

        if old.span() == DUMMY_SP {
            println!("   skipping: dummy_sp");
            return false;
        }

        let fm = rcx.session().codemap().lookup_byte_offset(old.span().lo).fm;
        if fm.abs_path.is_none() {
            println!("   skipping: file has no abs_path");
            return false;
        }

        println!("REWRITE(F) {}", describe(rcx.session(), reparsed.span()));
        println!("      INTO {}", describe(rcx.session(), old.span()));

        println!("new = {:?}", new);
        println!("rp = {:?}", reparsed);

        let mut rewrites = Vec::new();
        let mark = rcx.mark();
        let failed = Rewrite::rewrite_recycled(new, old, rcx.with_rewrites(&mut rewrites));
        if failed {
            rcx.rewind(mark);
            return false;
        }

        rcx.record(reparsed.span(), old.span(), rewrites);
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
        self.span
    }

    fn id(&self) -> NodeId {
        self.id
    }

    fn to_string(&self) -> String {
        pprust::expr_to_string(self)
    }

    type Parsed = P<Expr>;
    fn parse(sess: &Session, src: &str) -> Self::Parsed {
        driver::parse_expr(sess, src).unwrap()
    }

    fn node_table<'a, 's>(rcx: &'a mut RewriteCtxt<'s>) -> &'a mut NodeTable<'s, Self> {
        rcx.old_exprs()
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
        let mut stmts = driver::parse_stmts(sess, src).unwrap();
        assert!(stmts.len() == 1);
        SelfDeref(stmts.pop().unwrap())
    }

    fn node_table<'a, 's>(rcx: &'a mut RewriteCtxt<'s>) -> &'a mut NodeTable<'s, Self> {
        rcx.old_stmts()
    }
}

impl Splice for Item {
    fn span(&self) -> Span {
        self.span
    }

    fn id(&self) -> NodeId {
        self.id
    }

    fn to_string(&self) -> String {
        pprust::item_to_string(self)
    }

    type Parsed = P<Item>;
    fn parse(sess: &Session, src: &str) -> Self::Parsed {
        let mut items = driver::parse_items(sess, src).unwrap();
        assert!(items.len() == 1);
        items.pop().unwrap()
    }

    fn node_table<'a, 's>(rcx: &'a mut RewriteCtxt<'s>) -> &'a mut NodeTable<'s, Self> {
        rcx.old_items()
    }
}



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

impl<T: Rewrite> Rewrite for [T] {
    fn rewrite_recycled(&self, old: &Self, mut rcx: RewriteCtxtRef) -> bool {
        if self.len() != old.len() {
            return true;
        }

        for i in 0 .. self.len() {
            if Rewrite::rewrite_recycled(&self[i], &old[i], rcx.borrow()) {
                return true;
            }
        }
        false
    }

    fn rewrite_fresh(&self, reparsed: &Self, mut rcx: RewriteCtxtRef) {
        assert!(self.len() == reparsed.len());

        for i in 0 .. self.len() {
            Rewrite::rewrite_fresh(&self[i], &reparsed[i], rcx.borrow());
        }
    }
}

impl<T: Rewrite> Rewrite for Vec<T> {
    fn rewrite_recycled(&self, old: &Self, rcx: RewriteCtxtRef) -> bool {
        <[T] as Rewrite>::rewrite_recycled(&self, &old, rcx)
    }

    fn rewrite_fresh(&self, reparsed: &Self, rcx: RewriteCtxtRef) {
        <[T] as Rewrite>::rewrite_fresh(&self, &reparsed, rcx);
    }
}

impl<T: Rewrite> Rewrite for ThinVec<T> {
    fn rewrite_recycled(&self, old: &Self, rcx: RewriteCtxtRef) -> bool {
        <[T] as Rewrite>::rewrite_recycled(&self, &old, rcx)
    }

    fn rewrite_fresh(&self, reparsed: &Self, rcx: RewriteCtxtRef) {
        <[T] as Rewrite>::rewrite_fresh(&self, &reparsed, rcx);
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

include!("rewrite_impls_gen.inc.rs");
