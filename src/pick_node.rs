use std::str::FromStr;
use syntax::ast::*;
use syntax::codemap::{Span, BytePos};
use syntax::ext::hygiene::SyntaxContext;
use syntax::visit::{self, Visitor, FnKind};

use driver;
use visit::Visit;


#[derive(Debug)]
pub struct NodeInfo {
    pub id: NodeId,
    pub span: Span,
}

struct PickVisitor {
    node_info: Option<NodeInfo>,
    kind: NodeKind,
    target: Span,
}


impl<'a> Visitor<'a> for PickVisitor {
    fn visit_item(&mut self, x: &'a Item) {
        // Recurse first, so that the deepest node gets visited first.  This way we get
        // the function and not its containing module, for example.
        visit::walk_item(self, x);
        if self.node_info.is_none() &&
           self.kind.includes(NodeKind::Item) &&
           x.span.contains(self.target) {
            self.node_info = Some(NodeInfo { id: x.id, span: x.span });
        }
    }

    fn visit_stmt(&mut self, x: &'a Stmt) {
        visit::walk_stmt(self, x);
        if self.node_info.is_none() &&
           self.kind.includes(NodeKind::Stmt) &&
           x.span.contains(self.target) {
            self.node_info = Some(NodeInfo { id: x.id, span: x.span });
        }
    }

    fn visit_expr(&mut self, x: &'a Expr) {
        visit::walk_expr(self, x);
        if self.node_info.is_none() &&
           self.kind.includes(NodeKind::Expr) &&
           x.span.contains(self.target) {
            self.node_info = Some(NodeInfo { id: x.id, span: x.span });
        }
    }

    fn visit_pat(&mut self, x: &'a Pat) {
        visit::walk_pat(self, x);
        if self.node_info.is_none() &&
           self.kind.includes(NodeKind::Pat) &&
           x.span.contains(self.target) {
            self.node_info = Some(NodeInfo { id: x.id, span: x.span });
        }
    }

    fn visit_ty(&mut self, x: &'a Ty) {
        visit::walk_ty(self, x);
        if self.node_info.is_none() &&
           self.kind.includes(NodeKind::Ty) &&
           x.span.contains(self.target) {
            self.node_info = Some(NodeInfo { id: x.id, span: x.span });
        }
    }

    // There's no `visit_arg`, unfortunately, so we have to do this instead.
    fn visit_fn(&mut self, fk: FnKind<'a>, fd: &'a FnDecl, s: Span, id: NodeId) {
        visit::walk_fn(self, fk, fd, s);

        if self.node_info.is_none() &&
           self.kind.includes(NodeKind::Arg) {
            for arg in &fd.inputs {
                if arg.ty.span.contains(self.target) ||
                   arg.pat.span.contains(self.target) ||
                   (arg.ty.span.ctxt == arg.pat.span.ctxt &&
                    arg.ty.span.between(arg.pat.span).contains(self.target)) {
                    self.node_info = Some(NodeInfo {
                        id: arg.id,
                        span: arg.ty.span.to(arg.pat.span),
                    });
                }
            }
        }
    }

    fn visit_mac(&mut self, mac: &'a Mac) {
        visit::walk_mac(self, mac);
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum NodeKind {
    Any,
    Item,
    Stmt,
    Expr,
    Pat,
    Ty,
    Arg,
}

impl NodeKind {
    fn includes(self, other: NodeKind) -> bool {
        self == NodeKind::Any || other == self
    }
}

impl FromStr for NodeKind {
    type Err = ();
    fn from_str(s: &str) -> Result<NodeKind, ()> {
        let kind =
            match s {
                "any" => NodeKind::Any,
                "item" => NodeKind::Item,
                "stmt" => NodeKind::Stmt,
                "expr" => NodeKind::Expr,
                "pat" => NodeKind::Pat,
                "ty" => NodeKind::Ty,
                "arg" => NodeKind::Arg,
                s => return Err(()),
            };
        Ok(kind)
    }
}

pub fn pick_node(krate: &Crate, kind: NodeKind, pos: BytePos) -> Option<NodeInfo> {
    let mut v = PickVisitor {
        node_info: None,
        kind: kind,
        target: Span { lo: pos, hi: pos, ctxt: SyntaxContext::empty() },
    };
    krate.visit(&mut v);
    v.node_info
}

pub fn pick_node_at_loc(krate: &Crate,
                        cx: &driver::Ctxt,
                        kind: NodeKind,
                        file: &str,
                        line: u32,
                        col: u32) -> Option<NodeInfo> {
    let fm = match cx.session().codemap().get_filemap(file) {
        Some(x) => x,
        None => {
            panic!("target position lies in nonexistent file {:?}", file);
        },
    };

    if line == 0 || line as usize - 1 >= fm.lines.borrow().len() {
        panic!("line {} is outside the bounds of {}", line, file);
    };
    let (lo, hi) = fm.line_bounds(line as usize - 1);

    let line_len = hi.0 - lo.0;
    if col == 0 || col - 1 >= line_len {
        panic!("column {} is outside the bounds of {} line {}", col, file, line);
    }

    // TODO: make this work when the line contains multibyte characters
    let pos = lo + BytePos(col - 1);

    pick_node(krate, kind, pos)
}

pub fn pick_node_command(krate: &Crate, cx: &driver::Ctxt, args: &[String]) {
    let kind = NodeKind::from_str(&args[0]).unwrap();
    let file = &args[1];
    let line = u32::from_str(&args[2]).unwrap();
    let col = u32::from_str(&args[3]).unwrap();

    let result = pick_node_at_loc(krate, cx, kind, file, line, col);

    if let Some(ref result) = result {
        let lo_loc = cx.session().codemap().lookup_char_pos(result.span.lo);
        let hi_loc = cx.session().codemap().lookup_char_pos(result.span.hi - BytePos(1));
        println!("{{ \
            found: true, \
            node_id: {}, \
            span_lo: [{}, {}], \
            span_hi: [{}, {}] \
            }}", result.id, lo_loc.line, lo_loc.col.0 + 1, hi_loc.line, hi_loc.col.0 + 1);
    } else {
        println!("{{ found: false }}");
    }
}
