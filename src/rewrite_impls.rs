use std::rc::Rc;
use rustc::session::Session;
use syntax::ast::*;
use syntax::abi::Abi;
use syntax::ast::*;
use syntax::codemap::{Span, Spanned};
use syntax::ext::hygiene::SyntaxContext;
use syntax::print::pprust;
use syntax::ptr::P;
use syntax::tokenstream::{TokenStream, ThinTokenStream};

use driver;
use rewrite::{self, Rewrite, RewriteCtxt, RewriteCtxtRef};


fn text(sess: &Session, span: Span) -> String {
    let cm = sess.codemap();
    let lo = cm.lookup_byte_offset(span.lo);
    let hi = cm.lookup_byte_offset(span.hi);
    lo.fm.src.as_ref().unwrap()[lo.pos.0 as usize .. hi.pos.0 as usize].to_owned()
}

fn splice_recycled_expr(new: &Expr, old: &Expr, mut rcx: RewriteCtxtRef) {
    //rcx.record(old.span, pprust::expr_to_string(new));
    let old_span = old.span;

    let printed = pprust::expr_to_string(new);
    let reparsed = driver::parse_expr(rcx.session(), &printed).unwrap();
    let new_span = reparsed.span;

    println!("REWRITE {:?} = {}", old_span, text(rcx.session(), old_span));
    println!("   INTO {:?} = {}", new_span, text(rcx.session(), new_span));

    let mut rewrites = Vec::new();
    Rewrite::rewrite_fresh(new, &reparsed, rcx.with_rewrites(&mut rewrites));

    rcx.record(old_span, new_span, rewrites);
}

fn splice_fresh_expr(new: &Expr, reparsed: &Expr, mut rcx: RewriteCtxtRef) -> bool {
    let fm = rcx.session().codemap().lookup_byte_offset(new.span.lo).fm;
    if fm.abs_path.is_none() {
        return false;
    }

    let old = match rcx.get_expr(new.span) {
        Some(x) => x,
        None => return false,
    };

    println!("REWRITE {:?} = {}", reparsed.span, text(rcx.session(), reparsed.span));
    println!("   INTO {:?} = {}", old.span, text(rcx.session(), old.span));

    let mut rewrites = Vec::new();
    Rewrite::rewrite_recycled(new, old, rcx.with_rewrites(&mut rewrites));

    rcx.record(reparsed.span, old.span, rewrites);
    true
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
