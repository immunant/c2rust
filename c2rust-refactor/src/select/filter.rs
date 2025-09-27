use rustc_ast::visit::{self, AssocCtxt, FnKind, Visitor};
use rustc_ast::*;
use rustc_span::source_map::Span;
use rustc_span::symbol::Symbol;
use std::str::FromStr;

use crate::ast_manip::AstEquiv;
use crate::command::CommandState;
use crate::matcher::MatchCtxt;
use crate::pick_node::NodeKind;
use crate::reflect;
use crate::select::{AnyPattern, Filter};
use crate::RefactorCtxt;

#[derive(Clone, Copy, Debug)]
pub enum AnyNode<'ast> {
    Item(&'ast Item),
    TraitItem(&'ast AssocItem),
    ImplItem(&'ast AssocItem),
    ForeignItem(&'ast ForeignItem),
    Stmt(&'ast Stmt),
    Expr(&'ast Expr),
    Pat(&'ast Pat),
    Ty(&'ast Ty),
    Param(&'ast Param),
    Field(&'ast FieldDef),
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
            AnyNode::Param(_) => NodeKind::Param,
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
            AnyNode::Param(x) => x.id,
            AnyNode::Field(x) => x.id,
        }
    }

    pub fn vis(&self) -> Option<&'ast Visibility> {
        match *self {
            AnyNode::Item(i) => Some(&i.vis),
            AnyNode::TraitItem(i) => Some(&i.vis),
            AnyNode::ImplItem(i) => Some(&i.vis),
            AnyNode::ForeignItem(i) => Some(&i.vis),
            _ => None,
        }
    }

    pub fn mutbl(&self) -> Option<Mutability> {
        match *self {
            AnyNode::Item(i) => match i.kind {
                ItemKind::Static(_, mutbl, _) => Some(mutbl),
                _ => None,
            },
            AnyNode::ForeignItem(fi) => match fi.kind {
                ForeignItemKind::Static(_, mutability, _) => Some(mutability),
                _ => None,
            },
            AnyNode::Pat(p) => match p.kind {
                PatKind::Ident(mode, _, _) => match mode {
                    BindingMode::ByRef(mutbl) => Some(mutbl),
                    BindingMode::ByValue(mutbl) => Some(mutbl),
                },
                _ => None,
            },
            _ => None,
        }
    }

    pub fn name(&self) -> Option<Symbol> {
        match *self {
            AnyNode::Item(i) => Some(i.ident.name),
            AnyNode::TraitItem(i) => Some(i.ident.name),
            AnyNode::ImplItem(i) => Some(i.ident.name),
            AnyNode::ForeignItem(i) => Some(i.ident.name),
            AnyNode::Param(a) => match a.pat.kind {
                PatKind::Ident(_, ref i, _) => Some(i.name),
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
            AnyNode::TraitItem(i) => Some(ItemLikeKind::from_assoc_item(i)),
            AnyNode::ImplItem(i) => Some(ItemLikeKind::from_assoc_item(i)),
            AnyNode::ForeignItem(i) => Some(ItemLikeKind::from_foreign_item(i)),
            _ => None,
        }
    }

    pub fn from_assoc_item(i: &'ast AssocItem, ctxt: AssocCtxt) -> Self {
        match ctxt {
            AssocCtxt::Trait => AnyNode::TraitItem(i),
            AssocCtxt::Impl => AnyNode::ImplItem(i),
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
    InlineAsm,
    Ty,
    OpaqueTy,
    Enum,
    Struct,
    Union,
    Trait,
    TraitAlias,
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
            "inline_asm" => Ok(ItemLikeKind::InlineAsm),
            "ty" => Ok(ItemLikeKind::Ty),
            "opaquety" => Ok(ItemLikeKind::OpaqueTy),
            "enum" => Ok(ItemLikeKind::Enum),
            "struct" => Ok(ItemLikeKind::Struct),
            "union" => Ok(ItemLikeKind::Union),
            "trait" => Ok(ItemLikeKind::Trait),
            "trait_alias" => Ok(ItemLikeKind::TraitAlias),
            "impl" => Ok(ItemLikeKind::Impl),
            "mac" => Ok(ItemLikeKind::Mac),
            "macro_def" => Ok(ItemLikeKind::MacroDef),

            _ => Err(()),
        }
    }
}

impl ItemLikeKind {
    pub fn from_item(i: &Item) -> ItemLikeKind {
        match i.kind {
            ItemKind::ExternCrate(..) => ItemLikeKind::ExternCrate,
            ItemKind::Use(..) => ItemLikeKind::Use,
            ItemKind::Static(..) => ItemLikeKind::Static,
            ItemKind::Const(..) => ItemLikeKind::Const,
            ItemKind::Fn(..) => ItemLikeKind::Fn,
            ItemKind::Mod(..) => ItemLikeKind::Mod,
            ItemKind::ForeignMod(..) => ItemLikeKind::ForeignMod,
            ItemKind::GlobalAsm(..) => ItemLikeKind::InlineAsm,
            ItemKind::TyAlias(..) => ItemLikeKind::Ty,
            ItemKind::Enum(..) => ItemLikeKind::Enum,
            ItemKind::Struct(..) => ItemLikeKind::Struct,
            ItemKind::Union(..) => ItemLikeKind::Union,
            ItemKind::Trait(..) => ItemLikeKind::Trait,
            ItemKind::TraitAlias(..) => ItemLikeKind::TraitAlias,
            ItemKind::Impl(..) => ItemLikeKind::Impl,
            ItemKind::MacCall(..) => ItemLikeKind::Mac,
            ItemKind::MacroDef(..) => ItemLikeKind::MacroDef,
        }
    }

    pub fn from_assoc_item(i: &AssocItem) -> ItemLikeKind {
        match i.kind {
            AssocItemKind::Const(..) => ItemLikeKind::Const,
            AssocItemKind::Fn(..) => ItemLikeKind::Fn,
            AssocItemKind::TyAlias(..) => ItemLikeKind::Ty,
            AssocItemKind::MacCall(..) => ItemLikeKind::Mac,
        }
    }

    pub fn from_foreign_item(i: &ForeignItem) -> ItemLikeKind {
        match i.kind {
            ForeignItemKind::Fn(..) => ItemLikeKind::Fn,
            ForeignItemKind::Static(..) => ItemLikeKind::Static,
            ForeignItemKind::TyAlias(..) => ItemLikeKind::Ty,
            ForeignItemKind::MacCall(..) => ItemLikeKind::Mac,
        }
    }
}

pub fn matches_filter(
    st: &CommandState,
    cx: &RefactorCtxt,
    node: AnyNode,
    filter: &Filter,
) -> bool {
    match *filter {
        Filter::Kind(k) => k.contains(node.kind()),
        Filter::ItemKind(k) => node.itemlike_kind().map_or(false, |nk| nk == k),
        Filter::Public => node
            .vis()
            .map_or(false, |v| crate::matches!([v.kind] VisibilityKind::Public)),
        Filter::Mutable => node.mutbl().map_or(false, |m| m == Mutability::Mut),
        Filter::Name(ref re) => node.name().map_or(false, |n| re.is_match(&n.as_str())),
        Filter::PathPrefix(drop_segs, ref expect_path) => {
            if !reflect::can_reflect_path(cx, node.id()) {
                return false;
            }
            let def_id = match cx.hir_map().opt_local_def_id_from_node_id(node.id()) {
                Some(ldid) => ldid.to_def_id(),
                None => return false,
            };
            let path = reflect::reflect_def_path(cx.ty_ctxt(), def_id).1; // TODO: handle qself
            if path.segments.len() != expect_path.segments.len() + drop_segs {
                return false;
            }
            AstEquiv::ast_equiv(
                &expect_path.segments as &[_],
                &path.segments[..path.segments.len() - drop_segs],
            )
        }
        Filter::HasAttr(name) => node
            .attrs()
            .map_or(false, |attrs| crate::util::contains_name(attrs, name)),
        Filter::Matches(ref pat) => match (node, pat) {
            (AnyNode::Expr(target), &AnyPattern::Expr(ref pattern)) => {
                MatchCtxt::from_match(st, cx, &**pattern, target).is_ok()
            }
            (AnyNode::Pat(target), &AnyPattern::Pat(ref pattern)) => {
                MatchCtxt::from_match(st, cx, &**pattern, target).is_ok()
            }
            (AnyNode::Ty(target), &AnyPattern::Ty(ref pattern)) => {
                MatchCtxt::from_match(st, cx, &**pattern, target).is_ok()
            }
            (AnyNode::Stmt(target), &AnyPattern::Stmt(ref pattern)) => {
                MatchCtxt::from_match(st, cx, pattern, target).is_ok()
            }
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
        }
        Filter::AllChild(ref filt) => {
            let mut result = true;
            iter_children(node, |child| {
                if result && !matches_filter(st, cx, child, filt) {
                    result = false;
                }
            });
            result
        }

        Filter::AnyDesc(ref filt) => {
            let mut result = false;
            iter_descendants(node, |child| {
                if !result && matches_filter(st, cx, child, filt) {
                    result = true;
                }
            });
            result
        }
        Filter::AllDesc(ref filt) => {
            let mut result = true;
            iter_descendants(node, |child| {
                if result && !matches_filter(st, cx, child, filt) {
                    result = false;
                }
            });
            result
        }

        Filter::And(ref filts) => {
            for filt in filts.iter() {
                if !matches_filter(st, cx, node, filt) {
                    return false;
                }
            }
            true
        }
        Filter::Or(ref filts) => {
            for filt in filts.iter() {
                if matches_filter(st, cx, node, filt) {
                    return true;
                }
            }
            false
        }
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

    fn visit_assoc_item(&mut self, x: &'ast AssocItem, ctxt: AssocCtxt) {
        let node = match ctxt {
            AssocCtxt::Trait => AnyNode::TraitItem(x),
            AssocCtxt::Impl => AnyNode::ImplItem(x),
        };
        (self.func)(node);
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

    fn visit_fn(&mut self, kind: FnKind<'ast>, _: Span, _: NodeId) {
        for arg in &kind.decl().inputs {
            (self.func)(AnyNode::Param(arg));
        }
        match kind.decl().output {
            FnRetTy::Default(_) => {}
            FnRetTy::Ty(ref t) => {
                (self.func)(AnyNode::Ty(t));
            }
        }
    }

    fn visit_field_def(&mut self, x: &'ast FieldDef) {
        (self.func)(AnyNode::Field(x));
    }
}

pub fn iter_children<F: FnMut(AnyNode)>(node: AnyNode, func: F) {
    let mut v = ChildVisitor { func };
    match node {
        AnyNode::Item(x) => visit::walk_item(&mut v, x),
        AnyNode::TraitItem(x) => visit::walk_assoc_item(&mut v, x, AssocCtxt::Trait),
        AnyNode::ImplItem(x) => visit::walk_assoc_item(&mut v, x, AssocCtxt::Impl),
        AnyNode::ForeignItem(x) => visit::walk_foreign_item(&mut v, x),
        AnyNode::Stmt(x) => visit::walk_stmt(&mut v, x),
        AnyNode::Expr(x) => visit::walk_expr(&mut v, x),
        AnyNode::Pat(x) => visit::walk_pat(&mut v, x),
        AnyNode::Ty(x) => visit::walk_ty(&mut v, x),
        AnyNode::Param(x) => {
            v.visit_pat(&x.pat);
            v.visit_ty(&x.ty);
        }
        AnyNode::Field(x) => visit::walk_field_def(&mut v, x),
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

    fn visit_assoc_item(&mut self, x: &'ast AssocItem, ctxt: AssocCtxt) {
        let node = match ctxt {
            AssocCtxt::Trait => AnyNode::TraitItem(x),
            AssocCtxt::Impl => AnyNode::ImplItem(x),
        };
        (self.func)(node);
        visit::walk_assoc_item(self, x, ctxt);
    }

    fn visit_foreign_item(&mut self, x: &'ast ForeignItem) {
        // Make sure we visit foreign function args as Param
        if let ForeignItemKind::Fn(box Fn { ref sig, .. }) = x.kind {
            for arg in &sig.decl.inputs {
                (self.func)(AnyNode::Param(arg));
            }
        }
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

    fn visit_fn(&mut self, kind: FnKind<'ast>, span: Span, _id: NodeId) {
        for arg in &kind.decl().inputs {
            (self.func)(AnyNode::Param(arg));
        }
        // `walk` call handles the return type.
        visit::walk_fn(self, kind, span);
    }

    fn visit_field_def(&mut self, x: &'ast FieldDef) {
        (self.func)(AnyNode::Field(x));
        visit::walk_field_def(self, x);
    }
}

pub fn iter_descendants<F: FnMut(AnyNode)>(node: AnyNode, func: F) {
    let mut v = DescendantVisitor { func };
    match node {
        AnyNode::Item(x) => visit::walk_item(&mut v, x),
        AnyNode::TraitItem(x) => visit::walk_assoc_item(&mut v, x, AssocCtxt::Trait),
        AnyNode::ImplItem(x) => visit::walk_assoc_item(&mut v, x, AssocCtxt::Impl),
        AnyNode::ForeignItem(x) => visit::walk_foreign_item(&mut v, x),
        AnyNode::Stmt(x) => visit::walk_stmt(&mut v, x),
        AnyNode::Expr(x) => visit::walk_expr(&mut v, x),
        AnyNode::Pat(x) => visit::walk_pat(&mut v, x),
        AnyNode::Ty(x) => visit::walk_ty(&mut v, x),
        AnyNode::Param(x) => {
            v.visit_pat(&x.pat);
            v.visit_ty(&x.ty);
        }
        AnyNode::Field(x) => visit::walk_field_def(&mut v, x),
    }
}
