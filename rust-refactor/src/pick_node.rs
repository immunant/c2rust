//! Helper functions for picking a node by source location.
//!
//! This is used in various parts of the frontend to set marks at specific locations.
use std::path::PathBuf;
use std::str::FromStr;
use syntax::ast::*;
use syntax::source_map::{Span, BytePos};
use syntax::ext::hygiene::SyntaxContext;
use syntax::visit::{self, Visitor, FnKind};
use syntax_pos::FileName;

use ast_manip::Visit;
use command::{Registry, DriverCommand};
use driver::{self, Phase};


/// The ID and span of a selected node.
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
           self.kind.contains(NodeKind::Item) &&
           x.span.contains(self.target) {
            self.node_info = Some(NodeInfo { id: x.id, span: x.span });
        }

        // Special case for modules.  If the cursor lies within the inner span of a mod item
        // (meaning inside the included file), then we mark the mod item itself.  This is because
        // `Mod` nodes don't have their own IDs.
        if self.node_info.is_none() {
            if let ItemKind::Mod(ref m) = x.node {
                if m.inner.contains(self.target) {
                    self.node_info = Some(NodeInfo { id: x.id, span: x.span });
                }
            }
        }
    }

    fn visit_trait_item(&mut self, x: &'a TraitItem) {
        visit::walk_trait_item(self, x);
        if self.node_info.is_none() &&
           self.kind.contains(NodeKind::TraitItem) &&
           x.span.contains(self.target) {
            self.node_info = Some(NodeInfo { id: x.id, span: x.span });
        }
    }

    fn visit_impl_item(&mut self, x: &'a ImplItem) {
        visit::walk_impl_item(self, x);
        if self.node_info.is_none() &&
           self.kind.contains(NodeKind::ImplItem) &&
           x.span.contains(self.target) {
            self.node_info = Some(NodeInfo { id: x.id, span: x.span });
        }
    }

    fn visit_foreign_item(&mut self, x: &'a ForeignItem) {
        visit::walk_foreign_item(self, x);
        if self.node_info.is_none() &&
           self.kind.contains(NodeKind::ForeignItem) &&
           x.span.contains(self.target) {
            self.node_info = Some(NodeInfo { id: x.id, span: x.span });
        }
    }

    fn visit_stmt(&mut self, x: &'a Stmt) {
        visit::walk_stmt(self, x);
        if self.node_info.is_none() &&
           self.kind.contains(NodeKind::Stmt) &&
           x.span.contains(self.target) {
            self.node_info = Some(NodeInfo { id: x.id, span: x.span });
        }
    }

    fn visit_expr(&mut self, x: &'a Expr) {
        visit::walk_expr(self, x);
        if self.node_info.is_none() &&
           self.kind.contains(NodeKind::Expr) &&
           x.span.contains(self.target) {
            self.node_info = Some(NodeInfo { id: x.id, span: x.span });
        }
    }

    fn visit_pat(&mut self, x: &'a Pat) {
        visit::walk_pat(self, x);
        if self.node_info.is_none() &&
           self.kind.contains(NodeKind::Pat) &&
           x.span.contains(self.target) {
            self.node_info = Some(NodeInfo { id: x.id, span: x.span });
        }
    }

    fn visit_ty(&mut self, x: &'a Ty) {
        visit::walk_ty(self, x);
        if self.node_info.is_none() &&
           self.kind.contains(NodeKind::Ty) &&
           x.span.contains(self.target) {
            self.node_info = Some(NodeInfo { id: x.id, span: x.span });
        }
    }

    // There's no `visit_arg`, unfortunately, so we have to do this instead.
    fn visit_fn(&mut self, fk: FnKind<'a>, fd: &'a FnDecl, s: Span, _id: NodeId) {
        visit::walk_fn(self, fk, fd, s);

        if self.node_info.is_none() &&
           self.kind.contains(NodeKind::Arg) {
            for arg in &fd.inputs {
                if arg.ty.span.contains(self.target) ||
                   arg.pat.span.contains(self.target) ||
                   (arg.ty.span.ctxt() == arg.pat.span.ctxt() &&
                    arg.pat.span.between(arg.ty.span).contains(self.target)) {
                    self.node_info = Some(NodeInfo {
                        id: arg.id,
                        span: arg.pat.span.to(arg.ty.span),
                    });
                }
            }
        }
    }

    fn visit_struct_field(&mut self, x: &'a StructField) {
        visit::walk_struct_field(self, x);
        if self.node_info.is_none() &&
           self.kind.contains(NodeKind::Field) &&
           x.span.contains(self.target) {
            self.node_info = Some(NodeInfo { id: x.id, span: x.span });
        }
    }

    fn visit_mac(&mut self, mac: &'a Mac) {
        visit::walk_mac(self, mac);
    }
}


/// Enum of node kinds.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum NodeKind {
    /// Any kind of node.
    Any,
    /// Any item-like node.
    ItemLike,

    // The rest refer to specific node types.
    Item,
    TraitItem,
    ImplItem,
    ForeignItem,
    Stmt,
    Expr,
    Pat,
    Ty,
    Arg,
    Field,
}

impl NodeKind {
    /// Check if `self` contains kind `other`.  `other` is expected to be a specific node kind, not
    /// a category like `Any`.
    pub fn contains(self, other: NodeKind) -> bool {
        match self {
            NodeKind::Any => true,
            NodeKind::ItemLike => match other {
                NodeKind::Item |
                NodeKind::TraitItem |
                NodeKind::ImplItem |
                NodeKind::ForeignItem => true,
                _ => false,
            },
            _ => self == other,
        }
    }

    pub fn as_str(&self) -> &'static str {
        match *self {
            NodeKind::Any => "any",
            NodeKind::ItemLike => "itemlike",
            NodeKind::Item => "item",
            NodeKind::TraitItem => "trait_item",
            NodeKind::ImplItem => "impl_item",
            NodeKind::ForeignItem => "foreign_item",
            NodeKind::Stmt => "stmt",
            NodeKind::Expr => "expr",
            NodeKind::Pat => "pat",
            NodeKind::Ty => "ty",
            NodeKind::Arg => "arg",
            NodeKind::Field => "field",
        }
    }
}

impl FromStr for NodeKind {
    type Err = ();
    fn from_str(s: &str) -> Result<NodeKind, ()> {
        let kind =
            match s {
                "any" => NodeKind::Any,
                "itemlike" => NodeKind::ItemLike,

                "item" => NodeKind::Item,
                "trait_item" => NodeKind::TraitItem,
                "impl_item" => NodeKind::ImplItem,
                "foreign_item" => NodeKind::ForeignItem,
                "stmt" => NodeKind::Stmt,
                "expr" => NodeKind::Expr,
                "pat" => NodeKind::Pat,
                "ty" => NodeKind::Ty,
                "arg" => NodeKind::Arg,
                "field" => NodeKind::Field,

                _ => return Err(()),
            };
        Ok(kind)
    }
}

/// Select an AST node by its `BytePos` in the `SourceMap`.  Only nodes of the specified `kind` will
/// be selected.
pub fn pick_node(krate: &Crate, kind: NodeKind, pos: BytePos) -> Option<NodeInfo> {
    let mut v = PickVisitor {
        node_info: None,
        kind: kind,
        target: Span::new(pos, pos, SyntaxContext::empty()),
    };
    krate.visit(&mut v);

    // If the cursor falls inside the crate's module, then mark the crate itself.
    if v.node_info.is_none() {
        if krate.module.inner.contains(v.target) {
            v.node_info = Some(NodeInfo { id: CRATE_NODE_ID, span: krate.span });
        }
    }

    v.node_info
}

/// Select an AST node by its file, line, and column numbers.
pub fn pick_node_at_loc(krate: &Crate,
                        cx: &driver::Ctxt,
                        kind: NodeKind,
                        file: &str,
                        line: u32,
                        col: u32) -> Option<NodeInfo> {
    let fm = match cx.session().codemap().get_filemap(&FileName::Real(PathBuf::from(file))) {
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
    if col >= line_len {
        panic!("column {} is outside the bounds of {} line {}", col, file, line);
    }

    // TODO: This math is probably off when the line contains multibyte characters.  The
    // information to properly handle multibyte chars should be accessible through the `SourceFile`.
    let pos = lo + BytePos(col);

    pick_node(krate, kind, pos)
}

/// Implementation of the `pick_node` command.
pub fn pick_node_command(krate: &Crate, cx: &driver::Ctxt, args: &[String]) {
    let kind = NodeKind::from_str(&args[0]).unwrap();
    let file = &args[1];
    let line = u32::from_str(&args[2]).unwrap();
    let col = u32::from_str(&args[3]).unwrap();

    let result = pick_node_at_loc(krate, cx, kind, file, line, col);

    if let Some(ref result) = result {
        let lo_loc = cx.session().codemap().lookup_char_pos(result.span.lo());
        let hi_loc = cx.session().codemap().lookup_char_pos(result.span.hi() - BytePos(1));
        info!("{{ \
            found: true, \
            node_id: {}, \
            span_lo: [{}, {}], \
            span_hi: [{}, {}] \
            }}", result.id, lo_loc.line, lo_loc.col.0 + 1, hi_loc.line, hi_loc.col.0 + 1);
    } else {
        info!("{{ found: false }}");
    }
}

pub fn register_commands(reg: &mut Registry) {
    reg.register("pick_node", |args| {
        let args = args.to_owned();
        Box::new(DriverCommand::new(Phase::Phase2, move |st, cx| {
            pick_node_command(&st.krate(), &cx, &args);
        }))
    });
}
