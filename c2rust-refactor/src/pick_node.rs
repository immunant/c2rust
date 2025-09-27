//! Helper functions for picking a node by source location.
//!
//! This is used in various parts of the frontend to set marks at specific locations.
use log::info;
use rustc_ast::visit::{self, AssocCtxt, FnKind, Visitor};
use rustc_ast::*;
use rustc_session::Session;
use rustc_span::hygiene::SyntaxContext;
use rustc_span::source_map::{BytePos, Span};
use rustc_span::FileName;
use std::path::PathBuf;
use std::str::FromStr;

use crate::ast_manip::Visit;
use crate::command::{DriverCommand, Registry};
use crate::driver::Phase;
use crate::RefactorCtxt;

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
        if self.node_info.is_none()
            && self.kind.contains(NodeKind::Item)
            && x.span.contains(self.target)
        {
            self.node_info = Some(NodeInfo {
                id: x.id,
                span: x.span,
            });
        }

        // Special case for modules.  If the cursor lies within the inner span of a mod item
        // (meaning inside the included file), then we mark the mod item itself.  This is because
        // `Mod` nodes don't have their own IDs.
        if self.node_info.is_none() {
            if let ItemKind::Mod(_, ModKind::Loaded(_, _, ref m_spans)) = x.kind {
                if m_spans.inner_span.contains(self.target) {
                    self.node_info = Some(NodeInfo {
                        id: x.id,
                        span: x.span,
                    });
                }
            }
        }
    }

    fn visit_assoc_item(&mut self, x: &'a AssocItem, ctxt: AssocCtxt) {
        let kind = match ctxt {
            AssocCtxt::Trait => NodeKind::TraitItem,
            AssocCtxt::Impl => NodeKind::ImplItem,
        };

        visit::walk_assoc_item(self, x, ctxt);
        if self.node_info.is_none() && self.kind.contains(kind) && x.span.contains(self.target) {
            self.node_info = Some(NodeInfo {
                id: x.id,
                span: x.span,
            });
        }
    }

    fn visit_foreign_item(&mut self, x: &'a ForeignItem) {
        visit::walk_foreign_item(self, x);
        if self.node_info.is_none()
            && self.kind.contains(NodeKind::ForeignItem)
            && x.span.contains(self.target)
        {
            self.node_info = Some(NodeInfo {
                id: x.id,
                span: x.span,
            });
        }
    }

    fn visit_stmt(&mut self, x: &'a Stmt) {
        visit::walk_stmt(self, x);
        if self.node_info.is_none()
            && self.kind.contains(NodeKind::Stmt)
            && x.span.contains(self.target)
        {
            self.node_info = Some(NodeInfo {
                id: x.id,
                span: x.span,
            });
        }
    }

    fn visit_expr(&mut self, x: &'a Expr) {
        visit::walk_expr(self, x);
        if self.node_info.is_none()
            && self.kind.contains(NodeKind::Expr)
            && x.span.contains(self.target)
        {
            self.node_info = Some(NodeInfo {
                id: x.id,
                span: x.span,
            });
        }
    }

    fn visit_pat(&mut self, x: &'a Pat) {
        visit::walk_pat(self, x);
        if self.node_info.is_none()
            && self.kind.contains(NodeKind::Pat)
            && x.span.contains(self.target)
        {
            self.node_info = Some(NodeInfo {
                id: x.id,
                span: x.span,
            });
        }
    }

    fn visit_ty(&mut self, x: &'a Ty) {
        visit::walk_ty(self, x);
        if self.node_info.is_none()
            && self.kind.contains(NodeKind::Ty)
            && x.span.contains(self.target)
        {
            self.node_info = Some(NodeInfo {
                id: x.id,
                span: x.span,
            });
        }
    }

    // There's no `visit_arg`, unfortunately, so we have to do this instead.
    fn visit_fn(&mut self, fk: FnKind<'a>, s: Span, _id: NodeId) {
        visit::walk_fn(self, fk, s);

        if self.node_info.is_none() && self.kind.contains(NodeKind::Param) {
            for arg in &fk.decl().inputs {
                if arg.ty.span.contains(self.target)
                    || arg.pat.span.contains(self.target)
                    || (arg.ty.span.ctxt() == arg.pat.span.ctxt()
                        && arg.pat.span.between(arg.ty.span).contains(self.target))
                {
                    self.node_info = Some(NodeInfo {
                        id: arg.id,
                        span: arg.pat.span.to(arg.ty.span),
                    });
                }
            }
        }
    }

    fn visit_field_def(&mut self, x: &'a FieldDef) {
        visit::walk_field_def(self, x);
        if self.node_info.is_none()
            && self.kind.contains(NodeKind::Field)
            && x.span.contains(self.target)
        {
            self.node_info = Some(NodeInfo {
                id: x.id,
                span: x.span,
            });
        }
    }

    fn visit_mac_call(&mut self, mac: &'a MacCall) {
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
    Param,
    Field,
}

impl NodeKind {
    /// Check if `self` contains kind `other`.  `other` is expected to be a specific node kind, not
    /// a category like `Any`.
    pub fn contains(self, other: NodeKind) -> bool {
        match self {
            NodeKind::Any => true,
            NodeKind::ItemLike => match other {
                NodeKind::Item
                | NodeKind::TraitItem
                | NodeKind::ImplItem
                | NodeKind::ForeignItem => true,
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
            NodeKind::Param => "param",
            NodeKind::Field => "field",
        }
    }
}

impl FromStr for NodeKind {
    type Err = ();
    fn from_str(s: &str) -> Result<NodeKind, ()> {
        let kind = match s {
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
            "param" => NodeKind::Param,
            "arg" => NodeKind::Param, // arg is an alias for param
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
        kind,
        target: Span::new(pos, pos, SyntaxContext::root(), None),
    };
    krate.visit(&mut v);

    // If the cursor falls inside the crate's module, then mark the crate itself.
    if v.node_info.is_none() {
        if krate.spans.inner_span.contains(v.target) {
            v.node_info = Some(NodeInfo {
                id: CRATE_NODE_ID,
                span: krate.spans.inner_span,
            });
        }
    }

    v.node_info
}

/// Select an AST node by its file, line, and column numbers.
pub fn pick_node_at_loc(
    krate: &Crate,
    session: &Session,
    kind: NodeKind,
    file: &str,
    line: u32,
    col: u32,
) -> Option<NodeInfo> {
    let fm = match session
        .source_map()
        .get_source_file(&FileName::from(PathBuf::from(file)))
    {
        Some(x) => x,
        None => {
            panic!("target position lies in nonexistent file {:?}", file);
        }
    };

    if line == 0 || line as usize > fm.count_lines() {
        panic!("line {} is outside the bounds of {}", line, file);
    };
    let line_range = fm.line_bounds(line as usize - 1);

    let line_len = line_range.end.0 - line_range.start.0;
    if col >= line_len {
        panic!(
            "column {} is outside the bounds of {} line {}",
            col, file, line
        );
    }

    // TODO: This math is probably off when the line contains multibyte characters.  The
    // information to properly handle multibyte chars should be accessible through the `SourceFile`.
    let pos = line_range.start + BytePos(col);

    pick_node(krate, kind, pos)
}

/// # `pick_node` Command
///
/// Test command - not intended for general use.
///
/// Usage: `pick_node KIND FILE LINE COL`
///
/// Find a node of kind `KIND` at location `FILE:LINE:COL`.
/// If successful, logs the node's ID and span at level `info`.
pub fn pick_node_command(krate: &Crate, cx: &RefactorCtxt, args: &[String]) {
    let kind = NodeKind::from_str(&args[0]).unwrap();
    let file = &args[1];
    let line = u32::from_str(&args[2]).unwrap();
    let col = u32::from_str(&args[3]).unwrap();

    let result = pick_node_at_loc(krate, cx.session(), kind, file, line, col);

    if let Some(ref result) = result {
        let lo_loc = cx.session().source_map().lookup_char_pos(result.span.lo());
        let hi_loc = cx
            .session()
            .source_map()
            .lookup_char_pos(result.span.hi() - BytePos(1));
        info!(
            "{{ \
             found: true, \
             node_id: {}, \
             span_lo: [{}, {}], \
             span_hi: [{}, {}] \
             }}",
            result.id,
            lo_loc.line,
            lo_loc.col.0 + 1,
            hi_loc.line,
            hi_loc.col.0 + 1
        );
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
