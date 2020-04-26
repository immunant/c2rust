use json::{self, JsonValue};
use std::collections::{HashMap, HashSet};
use syntax::ast::*;
use syntax::source_map::{SourceMap, Span};
use syntax::symbol::Symbol;
use syntax::visit::{self, FnKind, Visitor};

use crate::rewrite::{TextAdjust, TextRewrite};

fn encode_span(sm: &SourceMap, sp: Span) -> JsonValue {
    let lo = sm.lookup_byte_offset(sp.lo());
    let hi = sm.lookup_byte_offset(sp.hi());
    let src = &lo.sf.src.as_ref().unwrap()[lo.pos.0 as usize..hi.pos.0 as usize];

    object! {
        "file" => lo.sf.name.to_string(),
        "lo" => lo.pos.0,
        "hi" => hi.pos.0,
        "src" => src,
    }
}

struct Encoder<'a> {
    sm: &'a SourceMap,
}

impl<'a> Encoder<'a> {
    fn encode_rewrite(&self, r: &TextRewrite) -> JsonValue {
        object! {
            "old_span" => self.encode_span(r.old_span),
            "new_span" => self.encode_span(r.new_span),
            "rewrites" => JsonValue::Array(
                r.rewrites.iter().map(|r| self.encode_rewrite(r)).collect()),
            "nodes" => JsonValue::Array(
                r.nodes.iter().map(|&(span, id)| self.encode_node(span, id)).collect()),
            "adjust" => self.encode_adjust(r.adjust),
        }
    }

    fn encode_span(&self, sp: Span) -> JsonValue {
        encode_span(self.sm, sp)
    }

    fn encode_node(&self, sp: Span, id: NodeId) -> JsonValue {
        object! {
            "span" => self.encode_span(sp),
            "id" => id.as_usize(),
        }
    }

    fn encode_adjust(&self, adj: TextAdjust) -> JsonValue {
        match adj {
            TextAdjust::None => JsonValue::Null,
            TextAdjust::Parenthesize => JsonValue::String("parenthesize".to_owned()),
        }
    }
}

pub fn encode_rewrite(sm: &SourceMap, r: &TextRewrite) -> JsonValue {
    Encoder { sm }.encode_rewrite(r)
}

pub fn encode_rewrites(sm: &SourceMap, rs: &[TextRewrite]) -> JsonValue {
    let enc = Encoder { sm };
    JsonValue::Array(rs.iter().map(|r| enc.encode_rewrite(r)).collect())
}

pub fn stringify_rewrite(sm: &SourceMap, r: &TextRewrite) -> String {
    json::stringify_pretty(encode_rewrite(sm, r), 2)
}

pub fn stringify_rewrites(sm: &SourceMap, rs: &[TextRewrite]) -> String {
    json::stringify_pretty(encode_rewrites(sm, rs), 2)
}

struct MarkVisitor<'a> {
    node_id_map: &'a HashMap<NodeId, NodeId>,
    marks: HashMap<NodeId, Vec<Symbol>>,
    j: Vec<JsonValue>,
}

impl<'a> MarkVisitor<'a> {
    fn encode(&mut self, kind: &'static str, id: NodeId) {
        self.encode_inner(kind, id, None)
    }

    fn encode_inner(&mut self, kind: &'static str, id: NodeId, name: Option<Symbol>) {
        let marks = match self.marks.get(&id) {
            Some(x) => x,
            None => return,
        };
        self.j.push(object! {
            "id" => id.as_usize(),
            "orig_id" => self.node_id_map.get(&id).map(|&id| id.as_usize()),
            "kind" => kind,
            "name" => if let Some(name) = name {
                (&name.as_str() as &str).into()
            } else {
                json::Null
            },
            "labels" => JsonValue::Array(
                marks.iter().map(|&x| (&x.as_str() as &str).into()).collect()),
        });
    }

    fn encode_named(&mut self, kind: &'static str, id: NodeId, ident: Ident) {
        self.encode_inner(kind, id, Some(ident.name))
    }
}

impl<'a, 'ast> Visitor<'ast> for MarkVisitor<'a> {
    fn visit_item(&mut self, x: &'ast Item) {
        self.encode_named("item", x.id, x.ident);
        visit::walk_item(self, x);
    }

    fn visit_impl_item(&mut self, x: &'ast ImplItem) {
        self.encode_named("impl item", x.id, x.ident);
        visit::walk_impl_item(self, x);
    }

    fn visit_trait_item(&mut self, x: &'ast TraitItem) {
        self.encode_named("trait item", x.id, x.ident);
        visit::walk_trait_item(self, x);
    }

    fn visit_foreign_item(&mut self, x: &'ast ForeignItem) {
        self.encode_named("foreign item", x.id, x.ident);
        visit::walk_foreign_item(self, x);
    }

    fn visit_stmt(&mut self, x: &'ast Stmt) {
        self.encode("stmt", x.id);
        visit::walk_stmt(self, x);
    }

    fn visit_expr(&mut self, x: &'ast Expr) {
        self.encode("expr", x.id);
        visit::walk_expr(self, x);
    }

    fn visit_pat(&mut self, x: &'ast Pat) {
        self.encode("pat", x.id);
        visit::walk_pat(self, x);
    }

    fn visit_ty(&mut self, x: &'ast Ty) {
        self.encode("ty", x.id);
        visit::walk_ty(self, x);
    }

    fn visit_fn(&mut self, kind: FnKind<'ast>, fd: &'ast FnDecl, span: Span, _id: NodeId) {
        for arg in &fd.inputs {
            let name = match arg.pat.kind {
                PatKind::Ident(_, ident, _) => Some(ident.name),
                _ => None,
            };
            self.encode_inner("arg", arg.id, name);
        }
        visit::walk_fn(self, kind, fd, span);
    }

    fn visit_struct_field(&mut self, x: &'ast StructField) {
        self.encode_inner("field", x.id, x.ident.map(|i| i.name));
        visit::walk_struct_field(self, x);
    }

    fn visit_mac(&mut self, x: &'ast Mac) {
        visit::walk_mac(self, x);
    }
}

pub fn encode_marks(
    krate: &Crate,
    node_id_map: &HashMap<NodeId, NodeId>,
    marks: &HashSet<(NodeId, Symbol)>,
) -> JsonValue {
    let mut mark_map = HashMap::new();
    for &(id, label) in marks {
        mark_map.entry(id).or_insert_with(Vec::new).push(label);
    }
    for v in mark_map.values_mut() {
        if v.len() > 1 {
            v.sort();
        }
    }

    let mut v = MarkVisitor {
        node_id_map,
        marks: mark_map,
        j: Vec::new(),
    };
    visit::walk_crate(&mut v, krate);
    v.encode("crate", CRATE_NODE_ID);
    JsonValue::Array(v.j)
}

pub fn stringify_marks(
    krate: &Crate,
    node_id_map: &HashMap<NodeId, NodeId>,
    marks: &HashSet<(NodeId, Symbol)>,
) -> String {
    json::stringify_pretty(encode_marks(krate, node_id_map, marks), 2)
}
