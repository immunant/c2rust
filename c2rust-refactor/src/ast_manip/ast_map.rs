use std::collections::HashMap;
use syntax::ast::*;
use syntax::visit::{self, Visitor};

use crate::ast_manip::Visit;

/// A table of references to AST nodes of some type, indexed by NodeId.
#[derive(Clone, Debug)]
pub struct NodeTable<'s, T: ?Sized + 's> {
    nodes: HashMap<NodeId, &'s T>,
}

impl<'s, T> Default for NodeTable<'s, T> {
    fn default() -> NodeTable<'s, T> {
        NodeTable {
            nodes: HashMap::new(),
        }
    }
}

impl<'s, T: ?Sized> NodeTable<'s, T> {
    pub fn new() -> NodeTable<'s, T> {
        NodeTable {
            nodes: HashMap::new(),
        }
    }

    pub fn insert(&mut self, id: NodeId, node: &'s T) {
        if id == DUMMY_NODE_ID {
            return;
        }
        assert!(!self.nodes.contains_key(&id));
        self.nodes.insert(id, node);
    }

    pub fn get(&self, id: NodeId) -> Option<&'s T> {
        self.nodes.get(&id).map(|&x| x)
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }
}

/// A lookup table for finding nodes within an AST or AST fragment.
#[derive(Clone, Default)]
pub struct AstMap<'s> {
    pub exprs: NodeTable<'s, Expr>,
    pub pats: NodeTable<'s, Pat>,
    pub tys: NodeTable<'s, Ty>,
    pub stmts: NodeTable<'s, Stmt>,
    pub items: NodeTable<'s, Item>,
    pub foreign_items: NodeTable<'s, ForeignItem>,
    pub blocks: NodeTable<'s, Block>,
}

impl<'s> AstMap<'s> {
    pub fn new() -> AstMap<'s> {
        AstMap {
            exprs: NodeTable::new(),
            pats: NodeTable::new(),
            tys: NodeTable::new(),
            stmts: NodeTable::new(),
            items: NodeTable::new(),
            foreign_items: NodeTable::new(),
            blocks: NodeTable::new(),
        }
    }
}

struct MapAstInto<'a, 's: 'a> {
    map: &'a mut AstMap<'s>,
}

impl<'a, 's> Visitor<'s> for MapAstInto<'a, 's> {
    fn visit_expr(&mut self, x: &'s Expr) {
        if let ExprKind::Paren(_) = x.node {
            // Ignore.  `Paren` nodes cause problems because they have the same NodeId as the inner
            // expression.
        } else {
            self.map.exprs.insert(x.id, x);
        }
        visit::walk_expr(self, x);
    }

    fn visit_pat(&mut self, x: &'s Pat) {
        self.map.pats.insert(x.id, x);
        visit::walk_pat(self, x);
    }

    fn visit_ty(&mut self, x: &'s Ty) {
        self.map.tys.insert(x.id, x);
        visit::walk_ty(self, x);
    }

    fn visit_stmt(&mut self, x: &'s Stmt) {
        self.map.stmts.insert(x.id, x);
        visit::walk_stmt(self, x);
    }

    fn visit_item(&mut self, x: &'s Item) {
        self.map.items.insert(x.id, x);
        visit::walk_item(self, x);
    }

    fn visit_foreign_item(&mut self, x: &'s ForeignItem) {
        self.map.foreign_items.insert(x.id, x);
        visit::walk_foreign_item(self, x);
    }

    fn visit_block(&mut self, x: &'s Block) {
        self.map.blocks.insert(x.id, x);
        visit::walk_block(self, x);
    }

    fn visit_mac(&mut self, mac: &'s Mac) {
        visit::walk_mac(self, mac);
    }
}

pub fn map_ast_into<'s, T: Visit>(x: &'s T, map: &mut AstMap<'s>) {
    x.visit(&mut MapAstInto { map })
}

pub fn map_ast<'s, T: Visit>(x: &'s T) -> AstMap<'s> {
    let mut m = AstMap::new();
    map_ast_into(x, &mut m);
    m
}
