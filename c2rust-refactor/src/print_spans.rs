//! Debug command for printing the span of every major AST node.
use log::info;
use rustc_ast::visit::Visitor;
use rustc_ast::*;
use rustc_ast_pretty::pprust::{self, PrintState};
use rustc_span::source_map::{SourceMap, Span, DUMMY_SP};

use crate::ast_manip::{visit_nodes, Visit};
use crate::command::{DriverCommand, Registry};
use crate::driver::Phase;

struct PrintSpanVisitor<'a> {
    cm: &'a SourceMap,
}

impl<'a> PrintSpanVisitor<'a> {
    fn span_desc(&self, span: Span) -> String {
        span_desc(self.cm, span)
    }
}

pub fn span_desc(cm: &SourceMap, span: Span) -> String {
    if span == DUMMY_SP {
        return "DUMMY_SP".to_owned();
    }

    let lo = cm.lookup_byte_offset(span.lo());
    let hi = cm.lookup_byte_offset(span.hi());
    let mut s = format!(
        "{:?}: {} .. {} (raw = {:?} .. {:?})",
        lo.sf.name,
        lo.pos.0,
        hi.pos.0,
        span.lo(),
        span.hi()
    );

    let span2 = span.source_callsite();
    if span2 != span {
        s.push_str(" < ");
        s.push_str(&span_desc(cm, span2));
    }

    s
}

impl<'a> Visitor<'a> for PrintSpanVisitor<'a> {
    fn visit_expr(&mut self, x: &'a Expr) {
        info!(
            "[EXPR {:?}] {}: {}",
            x.id,
            self.span_desc(x.span),
            pprust::expr_to_string(x)
        );
        rustc_ast::visit::walk_expr(self, x);
    }

    fn visit_pat(&mut self, x: &'a Pat) {
        info!(
            "[PAT {:?}] {}: {}",
            x.id,
            self.span_desc(x.span),
            pprust::pat_to_string(x)
        );
        rustc_ast::visit::walk_pat(self, x);
    }

    fn visit_ty(&mut self, x: &'a Ty) {
        info!(
            "[TY {:?}] {}: {}",
            x.id,
            self.span_desc(x.span),
            pprust::ty_to_string(x)
        );
        rustc_ast::visit::walk_ty(self, x);
    }

    fn visit_stmt(&mut self, x: &'a Stmt) {
        info!(
            "[STMT {:?}] {}: {}",
            x.id,
            self.span_desc(x.span),
            pprust::to_string(|s| {
                s.stmt_to_string(x);
            })
        );
        rustc_ast::visit::walk_stmt(self, x);
    }

    fn visit_item(&mut self, x: &'a Item) {
        info!(
            "[ITEM {:?}] {}: {}",
            x.id,
            self.span_desc(x.span),
            pprust::item_to_string(x)
        );
        rustc_ast::visit::walk_item(self, x);
    }

    fn visit_mac_call(&mut self, mac: &'a MacCall) {
        rustc_ast::visit::walk_mac(self, mac);
    }
}

/// Print the spans of all major nodes in `x`.
pub fn print_spans<T: Visit>(x: &T, cm: &SourceMap) {
    x.visit(&mut PrintSpanVisitor { cm });
}

pub fn print_one_span<T: Visit>(id: usize, root: &T, cm: &SourceMap, msg: &str) {
    let mut found = false;
    visit_nodes(root, |i: &Item| {
        if i.id == NodeId::from_usize(id) {
            info!("{:?}: {} ({})", i.id, span_desc(cm, i.span), msg);
            found = true;
        }
    });
    if !found {
        info!("no such node {} ({})", id, msg);
    }
}

/// # `print_spans` Command
///
/// Test command - not intended for general use.
///
/// Usage: `print_spans`
///
/// Print IDs, spans, and pretty-printed source for all
/// exprs, pats, tys, stmts, and items.
fn register_print_spans(reg: &mut Registry) {
    reg.register("print_spans", |_args| {
        Box::new(DriverCommand::new(Phase::Phase2, move |st, cx| {
            print_spans(&st.krate() as &Crate, cx.session().source_map());
        }))
    });
}

pub fn register_commands(reg: &mut Registry) {
    register_print_spans(reg);
}
