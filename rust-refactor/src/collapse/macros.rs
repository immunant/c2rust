use std::collections::{HashMap, HashSet};
use syntax::ast::*;
use syntax::codemap::Span;
use syntax::codemap::hygiene::SyntaxContext;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::parse::token::{Token, Nonterminal};
use syntax::tokenstream::{TokenStream, ThinTokenStream, TokenTree};
use syntax::util::small_vector::SmallVector;
use syntax::visit::{self, Visitor};

use super::mac_table::{MacTable, InvocId};
use super::nt_match::{self, NtMatch};

use ast_manip::{Fold, Visit};
use ast_manip::make_ast::mk;



struct CollapseMacros<'a> {
    mac_table: &'a MacTable<'a>,

    /// Used to keep track of which invocations we've seen, inside contexts where a macro might
    /// expand to multiple nodes.  The first node for each invocation gets replaced with the macro;
    /// the rest get discarded.
    seen_invocs: HashSet<InvocId>,

    token_rewrites: Vec<(InvocId, Span, Nonterminal)>,
}

impl<'a> CollapseMacros<'a> {
    fn collect_token_rewrites<T: NtMatch + ::std::fmt::Debug>(&mut self, invoc_id: InvocId, old: &T, new: &T) {
        info!("(invoc {:?}) record nts for {:?} -> {:?}", invoc_id, old, new);
        for (sp, nt) in nt_match::match_nonterminals(old, new) {
            info!("  got {} at {:?}",
                  ::syntax::print::pprust::token_to_string(
                      &Token::interpolated(nt.clone())),
                      sp);
            self.token_rewrites.push((invoc_id, sp, nt));
        }
    }
}

impl<'a> Folder for CollapseMacros<'a> {
    fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
        if let Some(mac) = self.mac_table.get(e.id) {
            let old = mac.expanded.as_expr()
                .expect("replaced a node with one of a different type?");
            self.collect_token_rewrites(mac.id, old, &e as &Expr);
            let new_e = mk().id(e.id).mac_expr(mac.mac);
            info!("collapse: {:?} -> {:?}", e, new_e);
            new_e
        } else {
            e.map(|e| fold::noop_fold_expr(e, self))
        }
    }

    fn fold_pat(&mut self, p: P<Pat>) -> P<Pat> {
        if let Some(mac) = self.mac_table.get(p.id) {
            let old = mac.expanded.as_pat()
                .expect("replaced a node with one of a different type?");
            self.collect_token_rewrites(mac.id, old, &p as &Pat);
            let new_p = mk().id(p.id).mac_pat(mac.mac);
            info!("collapse: {:?} -> {:?}", p, new_p);
            new_p
        } else {
            fold::noop_fold_pat(p, self)
        }
    }

    fn fold_ty(&mut self, t: P<Ty>) -> P<Ty> {
        if let Some(mac) = self.mac_table.get(t.id) {
            let old = mac.expanded.as_ty()
                .expect("replaced a node with one of a different type?");
            self.collect_token_rewrites(mac.id, old, &t as &Ty);
            let new_t = mk().id(t.id).mac_ty(mac.mac);
            info!("collapse: {:?} -> {:?}", t, new_t);
            new_t
        } else {
            fold::noop_fold_ty(t, self)
        }
    }

    fn fold_stmt(&mut self, s: Stmt) -> SmallVector<Stmt> {
        if let Some(mac) = self.mac_table.get(s.id) {
            let old = mac.expanded.as_stmt()
                .expect("replaced a node with one of a different type?");
            self.collect_token_rewrites(mac.id, old, &s as &Stmt);
            let new_s = mk().id(s.id).mac_stmt(mac.mac);
            info!("collapse: {:?} -> {:?}", s, new_s);
            SmallVector::one(new_s)
        } else {
            fold::noop_fold_stmt(s, self)
        }
    }
}


fn convert_token_rewrites(rewrite_vec: Vec<(InvocId, Span, Nonterminal)>,
                          mac_table: &MacTable) -> HashMap<InvocId, ThinTokenStream> {
    let mut new_tokens: HashMap<InvocId, TokenStream> = HashMap::new();

    for (id, sp, nt) in rewrite_vec {
        let ts = new_tokens.entry(id)
            .or_insert_with(|| {
                mac_table.get_invoc(id)
                    .expect("recorded token rewrites for nonexistent invocation?")
                    .node.tts.clone().into()
            });

        let mut started = false;
        let mut ended = false;
        let mut nt = Some(nt);
        *ts = ts.trees().filter_map(|tt| {
            let tt_span = tt.span();
            let mut r = Some(tt);
            if !started && tt_span.lo() == sp.lo() {
                started = true;
            }
            if started && !ended {
                r = None;
            }
            if started && !ended && tt_span.hi() == sp.hi() {
                ended = true;
                let nt = nt.take().expect("duplicate spans in tokenstream?");
                r = Some(TokenTree::Token(sp, Token::interpolated(nt)));
            }
            r
        }).collect::<TokenStream>();
    }

    new_tokens.into_iter().map(|(k, v)| (k, v.into())).collect()
}


struct ReplaceTokens<'a> {
    mac_table: &'a MacTable<'a>,
    new_tokens: HashMap<InvocId, ThinTokenStream>,
}

impl<'a> Folder for ReplaceTokens<'a> {
    fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
        if let Some(invoc_id) = self.mac_table.get(e.id).map(|m| m.id) {
            if let Some(new_tts) = self.new_tokens.get(&invoc_id).cloned() {
                return e.map(|mut e| {
                    expect!([e.node] ExprKind::Mac(ref mut mac) => mac.node.tts = new_tts);
                    e
                });
            }
        }
        e.map(|e| fold::noop_fold_expr(e, self))
    }

    fn fold_pat(&mut self, p: P<Pat>) -> P<Pat> {
        if let Some(invoc_id) = self.mac_table.get(p.id).map(|m| m.id) {
            if let Some(new_tts) = self.new_tokens.get(&invoc_id).cloned() {
                return p.map(|mut p| {
                    expect!([p.node] PatKind::Mac(ref mut mac) => mac.node.tts = new_tts);
                    p
                });
            }
        }
        fold::noop_fold_pat(p, self)
    }

    fn fold_ty(&mut self, t: P<Ty>) -> P<Ty> {
        if let Some(invoc_id) = self.mac_table.get(t.id).map(|m| m.id) {
            if let Some(new_tts) = self.new_tokens.get(&invoc_id).cloned() {
                return t.map(|mut t| {
                    expect!([t.node] TyKind::Mac(ref mut mac) => mac.node.tts = new_tts);
                    t
                });
            }
        }
        fold::noop_fold_ty(t, self)
    }

    fn fold_stmt(&mut self, s: Stmt) -> SmallVector<Stmt> {
        if let Some(invoc_id) = self.mac_table.get(s.id).map(|m| m.id) {
            if let Some(new_tts) = self.new_tokens.get(&invoc_id).cloned() {
                let mut s = s;
                unpack!([s.node] StmtKind::Mac(mac));
                let mac = mac.map(|(mut mac, style, attrs)| {
                    mac.node.tts = new_tts;
                    (mac, style, attrs)
                });
                return SmallVector::one(Stmt { node: StmtKind::Mac(mac), ..s });
            }
        }
        fold::noop_fold_stmt(s, self)
    }

    fn fold_mac(&mut self, mac: Mac) -> Mac {
        info!("encountered mac: {:?} {}",
              mac.node.path, ::syntax::print::pprust::tokens_to_string(mac.node.tts.clone().into()));
        fold::noop_fold_mac(mac, self)
    }
}


pub fn collapse_macros(krate: Crate,
                       mac_table: &MacTable) -> Crate {
    let mut collapse_macros = CollapseMacros {
        mac_table,
        seen_invocs: HashSet::new(),
        token_rewrites: Vec::new(),
    };
    let krate = krate.fold(&mut collapse_macros);
    let new_tokens = convert_token_rewrites(collapse_macros.token_rewrites, mac_table);
    for (k, v) in &new_tokens {
        info!("new tokens for {:?} = {:?}", k, ::syntax::print::pprust::tokens_to_string(v.clone().into()));
    }
    krate.fold(&mut ReplaceTokens { mac_table, new_tokens })
}
