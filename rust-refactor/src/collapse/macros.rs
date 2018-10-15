use std::collections::{HashMap, HashSet, BTreeMap};
use syntax::ast::*;
use syntax::codemap::{Span, BytePos};
use syntax::codemap::hygiene::SyntaxContext;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::parse::token::{Token, Nonterminal};
use syntax::tokenstream::{self, TokenStream, ThinTokenStream, TokenTree, Delimited};
use syntax::util::small_vector::SmallVector;
use syntax::visit::{self, Visitor};

use super::mac_table::{MacTable, InvocId};
use super::nt_match::{self, NtMatch};

use ast_manip::{Fold, Visit};
use ast_manip::make_ast::mk;
use ast_manip::AstEquiv;



#[derive(Clone, Debug)]
struct RewriteItem {
    invoc_id: InvocId,
    span: Span,
    nt: Nonterminal,
}


struct CollapseMacros<'a> {
    mac_table: &'a MacTable<'a>,

    /// Used to keep track of which invocations we've seen, inside contexts where a macro might
    /// expand to multiple nodes.  The first node for each invocation gets replaced with the macro;
    /// the rest get discarded.
    seen_invocs: HashSet<InvocId>,

    token_rewrites: Vec<RewriteItem>,
}

impl<'a> CollapseMacros<'a> {
    fn collect_token_rewrites<T: NtMatch + ::std::fmt::Debug>(&mut self, invoc_id: InvocId, old: &T, new: &T) {
        info!("(invoc {:?}) record nts for {:?} -> {:?}", invoc_id, old, new);
        for (span, nt) in nt_match::match_nonterminals(old, new) {
            info!("  got {} at {:?}",
                  ::syntax::print::pprust::token_to_string(
                      &Token::interpolated(nt.clone())),
                      span);
            self.token_rewrites.push(RewriteItem { invoc_id, span, nt });
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

            if !self.seen_invocs.contains(&mac.id) {
                self.seen_invocs.insert(mac.id);
                let new_s = mk().id(s.id).mac_stmt(mac.mac);
                info!("collapse: {:?} -> {:?}", s, new_s);
                SmallVector::one(new_s)
            } else {
                info!("collapse (duplicate): {:?} -> /**/", s);
                SmallVector::new()
            }
        } else {
            fold::noop_fold_stmt(s, self)
        }
    }
}


fn spans_overlap(sp1: Span, sp2: Span) -> bool {
    sp1.lo() < sp2.hi() && sp2.lo() < sp1.hi()
}

fn token_rewrite_map(vec: Vec<RewriteItem>) -> BTreeMap<BytePos, RewriteItem> {
    // Map from `span.lo()` to the full RewriteItem.  Invariant: all spans in `map` are
    // nonoverlapping and nonempty.  This means we can check if a new span overlaps any old span by
    // examining only the entries just before and just after the new span's `lo()` position.
    let mut map: BTreeMap<BytePos, RewriteItem> = BTreeMap::new();

    for item in vec {
        if item.span.lo() == item.span.hi() {
            warn!("macro rewrite has empty span {:?}", item.span);
            continue;
        }

        let key = item.span.lo();
        if let Some((_, before)) = map.range(..key).next_back() {
            if spans_overlap(item.span, before.span) {
                warn!("macro rewrite at {:?} overlaps rewrite at {:?}", item.span, before.span);
                continue;
            }
        }
        if let Some((_, after)) = map.range(key..).next() {
            if item.span == after.span && AstEquiv::ast_equiv(&item.nt, &after.nt) {
                // Both rewrites replace the same old tokens with the same new AST, so we can just
                // drop the second one.
                continue;
            } else if spans_overlap(item.span, after.span) {
                warn!("macro rewrite at {:?} overlaps rewrite at {:?}", item.span, after.span);
                continue;
            }
        }

        map.insert(key, item);
    }

    map
}

fn rewrite_tokens(invoc_id: InvocId,
                  tts: tokenstream::Cursor,
                  rewrites: &mut BTreeMap<BytePos, RewriteItem>) -> TokenStream {
    let mut new_tts = Vec::new();
    let mut ignore_until = None;

    for tt in tts {
        if let Some(end) = ignore_until {
            if tt.span().lo() >= end {
                // Previous ignore is now over.
                ignore_until = None;
            } else {
                continue;
            }
        }

        if let Some(item) = rewrites.remove(&tt.span().lo()) {
            assert!(item.invoc_id == invoc_id);
            new_tts.push(TokenTree::Token(item.span, Token::interpolated(item.nt)));
            ignore_until = Some(item.span.hi());
            continue;
        }

        match tt {
            TokenTree::Token(sp, t) => {
                new_tts.push(TokenTree::Token(sp, t));
            },
            TokenTree::Delimited(sp, d) => {
                let d_tts: TokenStream = d.tts.into();
                new_tts.push(TokenTree::Delimited(sp, Delimited {
                    tts: rewrite_tokens(invoc_id, d_tts.into_trees(), rewrites).into(),
                    ..d
                }));
            },
        }
    }

    new_tts.into_iter().collect()
}

fn convert_token_rewrites(rewrite_vec: Vec<RewriteItem>,
                          mac_table: &MacTable) -> HashMap<InvocId, ThinTokenStream> {
    let mut rewrite_map = token_rewrite_map(rewrite_vec);
    info!("rewrite map:");
    for (k,v) in &rewrite_map {
        info!("  {:?}: {:?}", k, v);
    }
    let invoc_ids = rewrite_map.values().map(|r| r.invoc_id).collect::<HashSet<_>>();
    invoc_ids.into_iter().map(|invoc_id| {
        let old_tts: TokenStream = mac_table.get_invoc(invoc_id)
            .expect("recorded token rewrites for nonexistent invocation?")
            .node.tts.clone().into();
        let new_tts = rewrite_tokens(invoc_id, old_tts.into_trees(), &mut rewrite_map);
        (invoc_id, new_tts.into())
    }).collect()
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
