//! Debug command for printing the span of every major AST node.
use syntax;
use syntax::ast::*;
use syntax::codemap::{CodeMap, Span, DUMMY_SP};
use syntax::print::pprust;
use syntax::visit::Visitor;

use ast_manip::{Visit, visit_nodes};
use command::{Registry, DriverCommand};
use driver::Phase;


struct PrintSpanVisitor<'a> {
    cm: &'a CodeMap,
}

impl<'a> PrintSpanVisitor<'a> {
    fn span_desc(&self, span: Span) -> String {
        span_desc(self.cm, span)
    }
}

pub fn span_desc(cm: &CodeMap, span: Span) -> String {
    if span == DUMMY_SP {
        return "DUMMY_SP".to_owned();
    }

    let lo = cm.lookup_byte_offset(span.lo());
    let hi = cm.lookup_byte_offset(span.hi());
    let mut s = format!("{}: {} .. {} (raw = {:?} .. {:?})",
                        lo.fm.name, lo.pos.0, hi.pos.0, span.lo(), span.hi());

    let span2 = span.source_callsite();
    if span2 != span {
        s.push_str(" < ");
        s.push_str(&span_desc(cm, span2));
    }

    s
}

impl<'a> Visitor<'a> for PrintSpanVisitor<'a> {
    fn visit_expr(&mut self, x: &'a Expr) {
        info!("[EXPR {:?}] {}: {}",
              x.id, self.span_desc(x.span), pprust::expr_to_string(x));
        syntax::visit::walk_expr(self, x);
    }

    fn visit_pat(&mut self, x: &'a Pat) {
        info!("[PAT {:?}] {}: {}",
              x.id, self.span_desc(x.span), pprust::pat_to_string(x));
        syntax::visit::walk_pat(self, x);
    }

    fn visit_ty(&mut self, x: &'a Ty) {
        info!("[TY {:?}] {}: {}",
              x.id, self.span_desc(x.span), pprust::ty_to_string(x));
        syntax::visit::walk_ty(self, x);
    }

    fn visit_stmt(&mut self, x: &'a Stmt) {
        info!("[STMT {:?}] {}: {}",
              x.id, self.span_desc(x.span), pprust::stmt_to_string(x));
        syntax::visit::walk_stmt(self, x);
    }

    fn visit_item(&mut self, x: &'a Item) {
        info!("[ITEM {:?}] {}: {}",
              x.id, self.span_desc(x.span), pprust::item_to_string(x));
        syntax::visit::walk_item(self, x);
    }

    fn visit_mac(&mut self, mac: &'a Mac) {
        syntax::visit::walk_mac(self, mac);
    }
}

/// Print the spans of all major nodes in `x`.
pub fn print_spans<T: Visit>(x: &T, cm: &CodeMap) {
    x.visit(&mut PrintSpanVisitor { cm: cm });
}


pub fn print_one_span<T: Visit>(id: usize, root: &T, cm: &CodeMap, msg: &str) {
    let mut found = false;
    visit_nodes(root, |i: &Item| {
        if i.id == NodeId::new(id) {
            info!("{:?}: {} ({})", i.id, span_desc(cm, i.span), msg);
            found = true;
        }
    });
    if !found {
        info!("no such node {} ({})", id, msg);
    }
}


pub fn register_commands(reg: &mut Registry) {
    reg.register("print_spans", |_args| {
        Box::new(DriverCommand::new(Phase::Phase2, move |st, cx| {
            print_spans(&st.krate() as &Crate, cx.session().codemap());
        }))
    });
}
