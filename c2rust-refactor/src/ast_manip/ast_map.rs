use rustc_ast::visit::{self, Visitor};
use rustc_ast::*;
use std::collections::HashMap;
use std::convert::{TryFrom, TryInto};
use std::ops::{Deref, DerefMut};

use super::{AstNodeRef, Visit};

pub fn map_ast_into<'s, T: Visit>(x: &'s T, map: &mut AstMap<'s>) {
    x.visit(&mut MapAstInto { map })
}

pub fn map_ast<'s, T: Visit>(x: &'s T) -> AstMap<'s> {
    let mut m = AstMap::new();
    map_ast_into(x, &mut m);
    m
}

pub fn map_ast_into_unified<'s, T: Visit>(x: &'s T, map: &mut UnifiedAstMap<'s>) {
    x.visit(&mut MapAstIntoUnified { map })
}

pub fn map_ast_unified<'s, T: Visit>(x: &'s T) -> UnifiedAstMap<'s> {
    let mut m = UnifiedAstMap::new();
    map_ast_into_unified(x, &mut m);
    m
}

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
        if let ExprKind::Paren(_) = x.kind {
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

    fn visit_mac_call(&mut self, mac: &'s MacCall) {
        visit::walk_mac(self, mac);
    }
}

/// A lookup table for finding nodes within an AST or AST fragment.
pub struct UnifiedAstMap<'s>(HashMap<NodeId, AstNodeRef<'s>>);

impl<'s> UnifiedAstMap<'s> {
    fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn get_ast<T>(&self, id: &NodeId) -> Option<&'s T>
    where
        &'s T: TryFrom<AstNodeRef<'s>>,
    {
        self.0.get(id).and_then(|x| x.clone().try_into().ok())
    }
}

impl<'s> Deref for UnifiedAstMap<'s> {
    type Target = HashMap<NodeId, AstNodeRef<'s>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'s> DerefMut for UnifiedAstMap<'s> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

struct MapAstIntoUnified<'a, 's: 'a> {
    map: &'a mut UnifiedAstMap<'s>,
}

impl<'a, 's> Visitor<'s> for MapAstIntoUnified<'a, 's> {
    fn visit_expr(&mut self, x: &'s Expr) {
        if let ExprKind::Paren(_) = x.kind {
            // Ignore.  `Paren` nodes cause problems because they have the same NodeId as the inner
            // expression.
        } else {
            self.map.insert(x.id, x.try_into().unwrap());
        }
        visit::walk_expr(self, x);
    }

    fn visit_pat(&mut self, x: &'s Pat) {
        self.map.insert(x.id, x.try_into().unwrap());
        visit::walk_pat(self, x);
    }

    fn visit_ty(&mut self, x: &'s Ty) {
        self.map.insert(x.id, x.try_into().unwrap());
        visit::walk_ty(self, x);
    }

    fn visit_stmt(&mut self, x: &'s Stmt) {
        self.map.insert(x.id, x.try_into().unwrap());
        visit::walk_stmt(self, x);
    }

    fn visit_item(&mut self, x: &'s Item) {
        self.map.insert(x.id, x.try_into().unwrap());
        visit::walk_item(self, x);
    }

    fn visit_foreign_item(&mut self, x: &'s ForeignItem) {
        self.map.insert(x.id, x.try_into().unwrap());
        visit::walk_foreign_item(self, x);
    }

    fn visit_block(&mut self, x: &'s Block) {
        self.map.insert(x.id, x.try_into().unwrap());
        visit::walk_block(self, x);
    }

    fn visit_mac_call(&mut self, mac: &'s MacCall) {
        visit::walk_mac(self, mac);
    }
}
