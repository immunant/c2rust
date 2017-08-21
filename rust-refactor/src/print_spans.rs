use syntax;
use syntax::ast::*;
use syntax::codemap::{CodeMap, Span, DUMMY_SP};
use syntax::print::pprust;
use syntax::visit::Visitor;

use visit::Visit;


struct PrintSpanVisitor<'a> {
    cm: &'a CodeMap,
}

impl<'a> PrintSpanVisitor<'a> {
    fn span_desc(&self, span: Span) -> String {
        if span == DUMMY_SP {
            return "DUMMY_SP".to_owned();
        }

        let lo = self.cm.lookup_byte_offset(span.lo);
        let hi = self.cm.lookup_byte_offset(span.hi);
        let mut s = format!("{}: {} .. {}", lo.fm.name, lo.pos.0, hi.pos.0);

        let span2 = span.source_callsite();
        if span2 != span {
            s.push_str(" < ");
            s.push_str(&self.span_desc(span2));
        }

        s
    }
}

impl<'a> Visitor<'a> for PrintSpanVisitor<'a> {
    fn visit_expr(&mut self, x: &'a Expr) {
        info!("[EXPR] {}: {}",
                 self.span_desc(x.span), pprust::expr_to_string(x));
        syntax::visit::walk_expr(self, x);
    }

    fn visit_pat(&mut self, x: &'a Pat) {
        info!("[PAT] {}: {}",
                 self.span_desc(x.span), pprust::pat_to_string(x));
        syntax::visit::walk_pat(self, x);
    }

    fn visit_ty(&mut self, x: &'a Ty) {
        info!("[TY] {}: {}",
                 self.span_desc(x.span), pprust::ty_to_string(x));
        syntax::visit::walk_ty(self, x);
    }

    fn visit_stmt(&mut self, x: &'a Stmt) {
        info!("[STMT] {}: {}",
                 self.span_desc(x.span), pprust::stmt_to_string(x));
        syntax::visit::walk_stmt(self, x);
    }

    fn visit_item(&mut self, x: &'a Item) {
        info!("[ITEM] {}: {}",
                 self.span_desc(x.span), pprust::item_to_string(x));
        syntax::visit::walk_item(self, x);
    }
}

#[allow(dead_code)] // Helper function for debugging
pub fn print_spans<T: Visit>(x: &T, cm: &CodeMap) {
    x.visit(&mut PrintSpanVisitor { cm: cm });
}
