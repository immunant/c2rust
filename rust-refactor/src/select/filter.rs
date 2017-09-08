use std::str::FromStr;
use syntax::ast::*;
use syntax::attr;
use syntax::codemap::Span;
use syntax::symbol::Symbol;
use syntax::visit::{self, Visitor, FnKind};

use ast_manip::AstEquiv;
use command::CommandState;
use driver;
use matcher::MatchCtxt;
use pick_node::NodeKind;
use reflect;
use select::{Filter, AnyPattern};


#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum AnyNode<'ast> {
    Item(&'ast Item),
    TraitItem(&'ast TraitItem),
    ImplItem(&'ast ImplItem),
    ForeignItem(&'ast ForeignItem),
    Stmt(&'ast Stmt),
    Expr(&'ast Expr),
    Pat(&'ast Pat),
    Ty(&'ast Ty),
    Arg(&'ast Arg),
    Field(&'ast StructField),
}

impl<'ast> AnyNode<'ast> {
    pub fn kind(&self) -> NodeKind {
        match *self {
            AnyNode::Item(_) => NodeKind::Item,
            AnyNode::TraitItem(_) => NodeKind::TraitItem,
            AnyNode::ImplItem(_) => NodeKind::ImplItem,
            AnyNode::ForeignItem(_) => NodeKind::ForeignItem,
            AnyNode::Stmt(_) => NodeKind::Stmt,
            AnyNode::Expr(_) => NodeKind::Expr,
            AnyNode::Pat(_) => NodeKind::Pat,
            AnyNode::Ty(_) => NodeKind::Ty,
            AnyNode::Arg(_) => NodeKind::Arg,
            AnyNode::Field(_) => NodeKind::Field,
        }
    }

    pub fn id(&self) -> NodeId {
        match *self {
            AnyNode::Item(x) => x.id,
            AnyNode::TraitItem(x) => x.id,
            AnyNode::ImplItem(x) => x.id,
            AnyNode::ForeignItem(x) => x.id,
            AnyNode::Stmt(x) => x.id,
            AnyNode::Expr(x) => x.id,
            AnyNode::Pat(x) => x.id,
            AnyNode::Ty(x) => x.id,
            AnyNode::Arg(x) => x.id,
            AnyNode::Field(x) => x.id,
        }
    }

    pub fn vis(&self) -> Option<&'ast Visibility> {
        match *self {
            AnyNode::Item(i) => Some(&i.vis),
            AnyNode::ImplItem(i) => Some(&i.vis),
            AnyNode::ForeignItem(i) => Some(&i.vis),
            _ => None,
        }
    }

    pub fn name(&self) -> Option<Symbol> {
        match *self {
            AnyNode::Item(i) => Some(i.ident.name),
            AnyNode::TraitItem(i) => Some(i.ident.name),
            AnyNode::ImplItem(i) => Some(i.ident.name),
            AnyNode::ForeignItem(i) => Some(i.ident.name),
            AnyNode::Arg(a) => match a.pat.node {
                PatKind::Ident(_, ref i, _) => Some(i.node.name),
                _ => None,
            },
            AnyNode::Field(f) => f.ident.map(|i| i.name),
            _ => None,
        }
    }

    pub fn attrs(&self) -> Option<&'ast [Attribute]> {
        match *self {
            AnyNode::Item(i) => Some(&i.attrs),
            AnyNode::TraitItem(i) => Some(&i.attrs),
            AnyNode::ImplItem(i) => Some(&i.attrs),
            AnyNode::ForeignItem(i) => Some(&i.attrs),
            AnyNode::Expr(i) => Some(&i.attrs),
            AnyNode::Field(i) => Some(&i.attrs),
            _ => None,
        }
    }

    pub fn itemlike_kind(&self) -> Option<ItemLikeKind> {
        match *self {
            AnyNode::Item(i) => Some(ItemLikeKind::from_item(i)),
            AnyNode::TraitItem(i) => Some(ItemLikeKind::from_trait_item(i)),
            AnyNode::ImplItem(i) => Some(ItemLikeKind::from_impl_item(i)),
            AnyNode::ForeignItem(i) => Some(ItemLikeKind::from_foreign_item(i)),
            _ => None,
        }
    }
}


#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ItemLikeKind {
    ExternCrate,
    Use,
    Static,
    Const,
    Fn,
    Mod,
    ForeignMod,
    GlobalAsm,
    Ty,
    Enum,
    Struct,
    Union,
    Trait,
    DefaultImpl,
    Impl,
    Mac,
    MacroDef,
}

impl FromStr for ItemLikeKind {
    type Err = ();

    fn from_str(s: &str) -> Result<ItemLikeKind, ()> {
        match s {
            "extern_crate" => Ok(ItemLikeKind::ExternCrate),
            "use" => Ok(ItemLikeKind::Use),
            "static" => Ok(ItemLikeKind::Static),
            "const" => Ok(ItemLikeKind::Const),
            "fn" => Ok(ItemLikeKind::Fn),
            "mod" => Ok(ItemLikeKind::Mod),
            "foreign_mod" => Ok(ItemLikeKind::ForeignMod),
            "global_asm" => Ok(ItemLikeKind::GlobalAsm),
            "ty" => Ok(ItemLikeKind::Ty),
            "enum" => Ok(ItemLikeKind::Enum),
            "struct" => Ok(ItemLikeKind::Struct),
            "union" => Ok(ItemLikeKind::Union),
            "trait" => Ok(ItemLikeKind::Trait),
            "default_impl" => Ok(ItemLikeKind::DefaultImpl),
            "impl" => Ok(ItemLikeKind::Impl),
            "mac" => Ok(ItemLikeKind::Mac),
            "macro_def" => Ok(ItemLikeKind::MacroDef),

            _ => Err(()),
        }
    }
}

impl ItemLikeKind {
    pub fn from_item(i: &Item) -> ItemLikeKind {
        match i.node {
            ItemKind::ExternCrate(..) => ItemLikeKind::ExternCrate,
            ItemKind::Use(..) => ItemLikeKind::Use,
            ItemKind::Static(..) => ItemLikeKind::Static,
            ItemKind::Const(..) => ItemLikeKind::Const,
            ItemKind::Fn(..) => ItemLikeKind::Fn,
            ItemKind::Mod(..) => ItemLikeKind::Mod,
            ItemKind::ForeignMod(..) => ItemLikeKind::ForeignMod,
            ItemKind::GlobalAsm(..) => ItemLikeKind::GlobalAsm,
            ItemKind::Ty(..) => ItemLikeKind::Ty,
            ItemKind::Enum(..) => ItemLikeKind::Enum,
            ItemKind::Struct(..) => ItemLikeKind::Struct,
            ItemKind::Union(..) => ItemLikeKind::Union,
            ItemKind::Trait(..) => ItemLikeKind::Trait,
            ItemKind::DefaultImpl(..) => ItemLikeKind::DefaultImpl,
            ItemKind::Impl(..) => ItemLikeKind::Impl,
            ItemKind::Mac(..) => ItemLikeKind::Mac,
            ItemKind::MacroDef(..) => ItemLikeKind::MacroDef,
        }
    }

    pub fn from_trait_item(i: &TraitItem) -> ItemLikeKind {
        match i.node {
            TraitItemKind::Const(..) => ItemLikeKind::Const,
            TraitItemKind::Method(..) => ItemLikeKind::Fn,
            TraitItemKind::Type(..) => ItemLikeKind::Ty,
            TraitItemKind::Macro(..) => ItemLikeKind::Mac,
        }
    }

    pub fn from_impl_item(i: &ImplItem) -> ItemLikeKind {
        match i.node {
            ImplItemKind::Const(..) => ItemLikeKind::Const,
            ImplItemKind::Method(..) => ItemLikeKind::Fn,
            ImplItemKind::Type(..) => ItemLikeKind::Ty,
            ImplItemKind::Macro(..) => ItemLikeKind::Mac,
        }
    }

    pub fn from_foreign_item(i: &ForeignItem) -> ItemLikeKind {
        match i.node {
            ForeignItemKind::Fn(..) => ItemLikeKind::Fn,
            ForeignItemKind::Static(..) => ItemLikeKind::Static,
        }
    }
}


pub fn matches_filter(st: &CommandState,
                      cx: &driver::Ctxt,
                      node: AnyNode,
                      filter: &Filter) -> bool {
    match *filter {
        Filter::Kind(k) => k.contains(node.kind()),
        Filter::ItemKind(k) => node.itemlike_kind().map_or(false, |nk| nk == k),
        Filter::Public => node.vis().map_or(false, |v| v == &Visibility::Public),
        Filter::Name(ref re) => node.name().map_or(false, |n| re.is_match(&n.as_str())),
        Filter::PathPrefix(drop_segs, ref expect_path) => {
            if !reflect::can_reflect_path(cx.hir_map(), node.id()) {
                return false;
            }
            let def_id = match cx.hir_map().opt_local_def_id(node.id()) {
                Some(id) => id,
                None => return false,
            };
            let path = reflect::reflect_path(cx.ty_ctxt(), def_id).1;   // TODO: handle qself
            if path.segments.len() != expect_path.segments.len() + drop_segs {
                return false;
            }
            AstEquiv::ast_equiv(&expect_path.segments as &[_],
                                &path.segments[.. path.segments.len() - drop_segs])
        },
        Filter::HasAttr(name) => node.attrs().map_or(false, |attrs| {
            attr::contains_name(attrs, &name.as_str())
        }),
        Filter::Matches(ref pat) => match (node, pat) {
            (AnyNode::Expr(target), &AnyPattern::Expr(ref pattern)) =>
                MatchCtxt::from_match(st, cx, &**pattern, target).is_ok(),
            (AnyNode::Pat(target), &AnyPattern::Pat(ref pattern)) =>
                MatchCtxt::from_match(st, cx, &**pattern, target).is_ok(),
            (AnyNode::Ty(target), &AnyPattern::Ty(ref pattern)) =>
                MatchCtxt::from_match(st, cx, &**pattern, target).is_ok(),
            (AnyNode::Stmt(target), &AnyPattern::Stmt(ref pattern)) =>
                MatchCtxt::from_match(st, cx, pattern, target).is_ok(),
            _ => false,
        },
        Filter::Marked(label) => st.marked(node.id(), label),

        Filter::AnyChild(ref filt) => {
            let mut result = false;
            iter_children(node, |child| {
                if !result && matches_filter(st, cx, child, filt) {
                    result = true;
                }
            });
            result
        },
        Filter::AllChild(ref filt) => {
            let mut result = true;
            iter_children(node, |child| {
                if result && !matches_filter(st, cx, child, filt) {
                    result = false;
                }
            });
            result
        },

        Filter::AnyDesc(ref filt) => {
            let mut result = false;
            iter_descendants(node, |child| {
                if !result && matches_filter(st, cx, child, filt) {
                    result = true;
                }
            });
            result
        },
        Filter::AllDesc(ref filt) => {
            let mut result = true;
            iter_descendants(node, |child| {
                if result && !matches_filter(st, cx, child, filt) {
                    result = false;
                }
            });
            result
        },

        Filter::And(ref filts) => {
            for filt in filts.iter() {
                if !matches_filter(st, cx, node, filt) {
                    return false;
                }
            }
            true
        },
        Filter::Or(ref filts) => {
            for filt in filts.iter() {
                if matches_filter(st, cx, node, filt) {
                    return true;
                }
            }
            false
        },
        Filter::Not(ref filt) => !matches_filter(st, cx, node, filt),
    }
}


struct ChildVisitor<F: FnMut(AnyNode)> {
    func: F,
}

impl<'ast, F: FnMut(AnyNode)> Visitor<'ast> for ChildVisitor<F> {
    fn visit_item(&mut self, x: &'ast Item) {
        (self.func)(AnyNode::Item(x));
    }

    fn visit_trait_item(&mut self, x: &'ast TraitItem) {
        (self.func)(AnyNode::TraitItem(x));
    }

    fn visit_impl_item(&mut self, x: &'ast ImplItem) {
        (self.func)(AnyNode::ImplItem(x));
    }

    fn visit_foreign_item(&mut self, x: &'ast ForeignItem) {
        (self.func)(AnyNode::ForeignItem(x));
    }

    fn visit_stmt(&mut self, x: &'ast Stmt) {
        (self.func)(AnyNode::Stmt(x));
    }

    fn visit_expr(&mut self, x: &'ast Expr) {
        (self.func)(AnyNode::Expr(x));
    }

    fn visit_pat(&mut self, x: &'ast Pat) {
        (self.func)(AnyNode::Pat(x));
    }

    fn visit_ty(&mut self, x: &'ast Ty) {
        (self.func)(AnyNode::Ty(x));
    }

    fn visit_fn(&mut self, _: FnKind<'ast>, fd: &'ast FnDecl, _: Span, _: NodeId) {
        for arg in &fd.inputs {
            (self.func)(AnyNode::Arg(arg));
        }
        match fd.output {
            FunctionRetTy::Default(_) => {},
            FunctionRetTy::Ty(ref t) => {
                (self.func)(AnyNode::Ty(t));
            },
        }
    }

    fn visit_struct_field(&mut self, x: &'ast StructField) {
        (self.func)(AnyNode::Field(x));
    }
}

pub fn iter_children<F: FnMut(AnyNode)>(node: AnyNode, func: F) {
    let mut v = ChildVisitor { func: func };
    match node {
        AnyNode::Item(x) => visit::walk_item(&mut v, x),
        AnyNode::TraitItem(x) => visit::walk_trait_item(&mut v, x),
        AnyNode::ImplItem(x) => visit::walk_impl_item(&mut v, x),
        AnyNode::ForeignItem(x) => visit::walk_foreign_item(&mut v, x),
        AnyNode::Stmt(x) => visit::walk_stmt(&mut v, x),
        AnyNode::Expr(x) => visit::walk_expr(&mut v, x),
        AnyNode::Pat(x) => visit::walk_pat(&mut v, x),
        AnyNode::Ty(x) => visit::walk_ty(&mut v, x),
        AnyNode::Arg(x) => {
            v.visit_pat(&x.pat);
            v.visit_ty(&x.ty);
        },
        AnyNode::Field(x) => visit::walk_struct_field(&mut v, x),
    }
}


struct DescendantVisitor<F: FnMut(AnyNode)> {
    func: F,
}

impl<'ast, F: FnMut(AnyNode)> Visitor<'ast> for DescendantVisitor<F> {
    fn visit_item(&mut self, x: &'ast Item) {
        (self.func)(AnyNode::Item(x));
        visit::walk_item(self, x);
    }

    fn visit_trait_item(&mut self, x: &'ast TraitItem) {
        (self.func)(AnyNode::TraitItem(x));
        visit::walk_trait_item(self, x);
    }

    fn visit_impl_item(&mut self, x: &'ast ImplItem) {
        (self.func)(AnyNode::ImplItem(x));
        visit::walk_impl_item(self, x);
    }

    fn visit_foreign_item(&mut self, x: &'ast ForeignItem) {
        (self.func)(AnyNode::ForeignItem(x));
        visit::walk_foreign_item(self, x);
    }

    fn visit_stmt(&mut self, x: &'ast Stmt) {
        (self.func)(AnyNode::Stmt(x));
        visit::walk_stmt(self, x);
    }

    fn visit_expr(&mut self, x: &'ast Expr) {
        (self.func)(AnyNode::Expr(x));
        visit::walk_expr(self, x);
    }

    fn visit_pat(&mut self, x: &'ast Pat) {
        (self.func)(AnyNode::Pat(x));
        visit::walk_pat(self, x);
    }

    fn visit_ty(&mut self, x: &'ast Ty) {
        (self.func)(AnyNode::Ty(x));
        visit::walk_ty(self, x);
    }

    fn visit_fn(&mut self, kind: FnKind<'ast>, fd: &'ast FnDecl, span: Span, _id: NodeId) {
        for arg in &fd.inputs {
            (self.func)(AnyNode::Arg(arg));
        }
        // `walk` call handles the return type.
        visit::walk_fn(self, kind, fd, span);
    }

    fn visit_struct_field(&mut self, x: &'ast StructField) {
        (self.func)(AnyNode::Field(x));
        visit::walk_struct_field(self, x);
    }
}

pub fn iter_descendants<F: FnMut(AnyNode)>(node: AnyNode, func: F) {
    let mut v = DescendantVisitor { func: func };
    match node {
        AnyNode::Item(x) => visit::walk_item(&mut v, x),
        AnyNode::TraitItem(x) => visit::walk_trait_item(&mut v, x),
        AnyNode::ImplItem(x) => visit::walk_impl_item(&mut v, x),
        AnyNode::ForeignItem(x) => visit::walk_foreign_item(&mut v, x),
        AnyNode::Stmt(x) => visit::walk_stmt(&mut v, x),
        AnyNode::Expr(x) => visit::walk_expr(&mut v, x),
        AnyNode::Pat(x) => visit::walk_pat(&mut v, x),
        AnyNode::Ty(x) => visit::walk_ty(&mut v, x),
        AnyNode::Arg(x) => {
            v.visit_pat(&x.pat);
            v.visit_ty(&x.ty);
        },
        AnyNode::Field(x) => visit::walk_struct_field(&mut v, x),
    }
}

