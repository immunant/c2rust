use std::collections::hash_map::{HashMap, Entry};
use std::collections::HashSet;
use std::mem;
use rustc::hir::def_id::{DefId, LOCAL_CRATE};
use rustc::ty::TypeVariants;
use syntax::ast::*;
use syntax::ptr::P;
use syntax::visit::{self, Visitor};

use api::*;
use command::{CommandState, Registry};
use driver::{self, Phase};
use transform::Transform;
use rustc::middle::cstore::CrateStore;


pub struct LetXUninitialized;

impl Transform for LetXUninitialized {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let krate = replace_stmts(st, cx, krate,
                                  "let __pat;",
                                  "let __pat = ::std::mem::uninitialized();");
        let krate = replace_stmts(st, cx, krate,
                                  "let __pat: __ty;",
                                  "let __pat: __ty = ::std::mem::uninitialized();");
        krate
    }
}


/// Move declarations of the form `let x;` or `let x = uninitialized();` into the deepest enclosing
/// block where `x` is used.  The restriction on the RHS ensures that we don't reorder side
/// effects.
pub struct SinkLets;

impl Transform for SinkLets {
    fn transform(&self, krate: Crate, _st: &CommandState, cx: &driver::Ctxt) -> Crate {
        // (1) Collect info on every local that might be worth moving.

        struct LocalInfo {
            local: P<Local>,
            old_node_id: NodeId,
        }

        let mut locals: HashMap<DefId, LocalInfo> = HashMap::new();
        visit_nodes(&krate, |l: &Local| {
            if let PatKind::Ident(BindingMode::ByValue(_), _, None) = l.pat.node {
                if l.init.is_none() || is_uninit_call(cx, l.init.as_ref().unwrap()) {
                    let def_id = cx.node_def_id(l.pat.id);
                    locals.insert(def_id, LocalInfo {
                        local: P(Local {
                            // Later on, e're going to copy this `Local` to create the newly
                            // generated bindings.  Afterward, we want to delete the old bindings.
                            // To distinguish the old and new bindings, we give the new one a dummy
                            // `NodeId`.
                            id: DUMMY_NODE_ID,
                            .. l.clone()
                        }),
                        old_node_id: l.id,
                    });
                }
            }
        });

        // (2) Build the set of used locals for every block.

        /// The only two cases we care about: the local is referenced only from inside a single
        /// nested block, or it's referenced some other way (multiple times, or from multiple
        /// blocks, or from outside any block).
        #[derive(Clone, Copy, PartialEq, Eq, Debug)]
        enum UseKind {
            InsideOneBlock,
            Other,
        }

        struct BlockLocalsVisitor<'a, 'tcx: 'a> {
            cur: HashMap<DefId, UseKind>,
            block_locals: HashMap<NodeId, HashMap<DefId, UseKind>>,

            cx: &'a driver::Ctxt<'a, 'tcx>,
            locals: &'a HashMap<DefId, LocalInfo>,
        }

        impl<'a, 'tcx> BlockLocalsVisitor<'a, 'tcx> {
            fn record_use_inside_block(&mut self, id: DefId) {
                match self.cur.entry(id) {
                    Entry::Occupied(e) => { *e.into_mut() = UseKind::Other; },
                    Entry::Vacant(e) => { e.insert(UseKind::InsideOneBlock); },
                }
            }
        }

        impl<'a, 'tcx, 'ast> Visitor<'ast> for BlockLocalsVisitor<'a, 'tcx> {
            fn visit_expr(&mut self, e: &'ast Expr) {
                if let Some(def_id) = self.cx.try_resolve_expr(e) {
                    if self.locals.contains_key(&def_id) {
                        self.cur.insert(def_id, UseKind::Other);
                    }
                }
                visit::walk_expr(self, e);
            }

            fn visit_block(&mut self, b: &'ast Block) {
                let old_cur = mem::replace(&mut self.cur, HashMap::new());
                visit::walk_block(self, b);
                let uses = mem::replace(&mut self.cur, old_cur);
                for &id in uses.keys() {
                    self.record_use_inside_block(id);
                }
                info!("record uses {:?} for {:?}", uses, b);
                self.block_locals.insert(b.id, uses);
            }

            fn visit_item(&mut self, i: &'ast Item) {
                let old_cur = mem::replace(&mut self.cur, HashMap::new());
                visit::walk_item(self, i);
                // Discard collected uses.  They aren't meaningful outside the item body.
                self.cur = old_cur;
            }
        }

        let block_locals = {
            let mut v = BlockLocalsVisitor {
                cur: HashMap::new(),
                block_locals: HashMap::new(),
                cx: cx,
                locals: &locals,
            };
            visit::walk_crate(&mut v, &krate);
            v.block_locals
        };

        // (3) Compute where to place every local.

        // Map from block NodeId to DefIds of locals to place in the block.
        let mut local_placement = HashMap::new();
        let mut placed_locals = HashSet::new();

        // This is separate from the actual rewrite because we need to do a preorder traversal, but
        // folds are always postorder to avoid infinite recursion.
        visit_nodes(&krate, |b: &Block| {
            let used_locals = &block_locals[&b.id];

            // Check if there are any locals we should place in this block.  We place a local here
            // if its use kind is `Other` and it hasn't been placed already.  A use kind of
            // `InsideOneBlock` means the local can be placed somewhere deeper, so this strategy
            // ensures we place the local in the deepest legal position.  We rely on `fold_nodes`
            // doing a preorder traversal to avoid placing them too deep.
            let mut place_here = used_locals.iter()
                .filter(|&(&id, &kind)| kind == UseKind::Other && !placed_locals.contains(&id))
                .map(|(&id, _)| id)
                .collect::<Vec<_>>();
            // Put the new locals in the same order as they appeared in the original source.
            place_here.sort_by_key(|&id| locals[&id].old_node_id);

            for &id in &place_here {
                placed_locals.insert(id);
            }

            if place_here.len() > 0 {
                local_placement.insert(b.id, place_here);
            }
        });

        // (4) Place new locals in the appropriate locations.

        let krate = fold_nodes(krate, |b: P<Block>| {
            let place_here = match_or!([local_placement.get(&b.id)]
                                       Some(x) => x; return b);

            b.map(|mut b| {
                let mut new_stmts = place_here.iter()
                    .map(|&id| mk().local_stmt(&locals[&id].local))
                    .collect::<Vec<_>>();
                new_stmts.append(&mut b.stmts);
                Block {
                    stmts: new_stmts,
                    .. b
                }
            })
        });

        // (5) Remove old locals

        // Note that we don't check for locals that we failed to place.  The only way we can fail
        // to place a local is if it is never used anywhere.  Otherwise we would, at worst, place
        // it back in its original block.  The result is that this pass has the additional effect
        // of removing unused locals.  
        let remove_local_ids = locals.iter()
            .map(|(_, info)| info.old_node_id)
            .collect::<HashSet<_>>();

        let krate = fold_nodes(krate, |b: P<Block>| {
            b.map(|mut b| {
                b.stmts.retain(|s| {
                    match s.node {
                        StmtKind::Local(ref l) => !remove_local_ids.contains(&l.id),
                        _ => true,
                    }
                });
                b
            })
        });

        krate
    }
}

fn is_uninit_call(cx: &driver::Ctxt, e: &Expr) -> bool {
    let func = match_or!([e.node] ExprKind::Call(ref func, _) => func; return false);
    let def_id = cx.resolve_expr(func);
    if def_id.krate == LOCAL_CRATE {
        return false;
    }
    let crate_name = cx.cstore().crate_name_untracked(def_id.krate);
    let path = cx.cstore().def_path(def_id);

    (crate_name.as_str() == "std" || crate_name.as_str() == "core") &&
    path.data.len() == 2 &&
    path.data[0].data.get_opt_name().map_or(false, |sym| sym == "mem") &&
    path.data[1].data.get_opt_name().map_or(false, |sym| sym == "uninitialized")
}



/// Fold `let x; ...; x = 10;` into `...; let x = 10;`, if there are no intervening uses of `x`.
///
/// This pass only takes effect when the `let` and the assignment are in the same block.  Running
/// `SinkLets` first will move `let`s into the correct block if it is legal to do so.
pub struct FoldLetAssign;

impl Transform for FoldLetAssign {
    fn transform(&self, krate: Crate, _st: &CommandState, cx: &driver::Ctxt) -> Crate {
        // (1) Find all locals that might be foldable.

        let mut locals: HashMap<DefId, P<Local>> = HashMap::new();
        visit_nodes(&krate, |l: &Local| {
            if let PatKind::Ident(BindingMode::ByValue(_), _, None) = l.pat.node {
                if l.init.is_none() || is_uninit_call(cx, l.init.as_ref().unwrap()) {
                    let def_id = cx.node_def_id(l.pat.id);
                    locals.insert(def_id, P(l.clone()));
                }
            }
        });

        // (2) Compute the set of foldable locals that are used in each statement.

        struct StmtLocalsVisitor<'a, 'tcx: 'a> {
            cur: HashSet<DefId>,
            stmt_locals: HashMap<NodeId, HashSet<DefId>>,

            cx: &'a driver::Ctxt<'a, 'tcx>,
            locals: &'a HashMap<DefId, P<Local>>,
        }

        impl<'a, 'tcx, 'ast> Visitor<'ast> for StmtLocalsVisitor<'a, 'tcx> {
            fn visit_expr(&mut self, e: &'ast Expr) {
                if let Some(def_id) = self.cx.try_resolve_expr(e) {
                    if self.locals.contains_key(&def_id) {
                        self.cur.insert(def_id);
                    }
                }
                visit::walk_expr(self, e);
            }

            fn visit_stmt(&mut self, s: &'ast Stmt) {
                let old_cur = mem::replace(&mut self.cur, HashSet::new());
                visit::walk_stmt(self, s);
                let uses = mem::replace(&mut self.cur, old_cur);
                for &id in &uses {
                    self.cur.insert(id);
                }
                info!("record uses {:?} for {:?}", uses, s);
                self.stmt_locals.insert(s.id, uses);
            }

            fn visit_item(&mut self, i: &'ast Item) {
                let old_cur = mem::replace(&mut self.cur, HashSet::new());
                visit::walk_item(self, i);
                // Discard collected uses.  They aren't meaningful outside the item body.
                self.cur = old_cur;
            }
        }

        let stmt_locals = {
            let mut v = StmtLocalsVisitor {
                cur: HashSet::new(),
                stmt_locals: HashMap::new(),
                cx: cx,
                locals: &locals,
            };
            visit::walk_crate(&mut v, &krate);
            v.stmt_locals
        };


        // (3) Walk through blocks, looking for non-initializing `let`s and assignment statements,
        // and rewriting them when found.

        // Map from node ID to def ID, for known locals.
        let local_node_def = locals.iter().map(|(did, l)| (l.id, did)).collect::<HashMap<_, _>>();

        // Map from def ID to a Mark, giving the position of the local so we can delete it later.
        // If a local gets used before we reach the assignment, we delete it from this map.
        let mut local_pos = HashMap::new();

        fold_blocks(krate, |mut curs| {
            while !curs.eof() {
                // Is it a local declaration?  If so, mark it.
                let mark_did = match curs.next().node {
                    StmtKind::Local(ref l) => {
                        if let Some(&did) = local_node_def.get(&l.id) {
                            Some(did)
                        } else {
                            None
                        }
                    },
                    _ => None,
                };
                if let Some(did) = mark_did {
                    local_pos.insert(did, curs.mark());
                }

                // Is it an assignment to a local?
                let assign_info = match curs.next().node {
                    StmtKind::Semi(ref e) => {
                        match e.node {
                            ExprKind::Assign(ref lhs, ref rhs) => {
                                if let Some(def_id) = cx.try_resolve_expr(&lhs) {
                                    if local_pos.contains_key(&def_id) {
                                        Some((def_id, rhs.clone()))
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            },
                            _ => None,
                        }
                    },
                    _ => None,
                };
                if let Some((def_id, init)) = assign_info {
                    let local = &locals[&def_id];
                    let local_mark = local_pos.remove(&def_id).unwrap();
                    let l = local.clone().map(|l| {
                        Local {
                            init: Some(init),
                            .. l
                        }
                    });
                    curs.replace(|_| mk().local_stmt(l));

                    let here = curs.mark();
                    curs.seek(local_mark);
                    curs.remove();
                    curs.seek(here);
                }

                // Does it access some locals?
                if let Some(locals) = stmt_locals.get(&curs.next().id) {
                    for &def_id in locals {
                        // This local is being accessed before its first recognized assignment.
                        // That means we can't fold the `let` with the later assignment.
                        local_pos.remove(&def_id);
                    }
                }


                curs.advance();
            }
        })
    }
}


/// Replace `let x = uninitialized()` with `let x = 0` or a similarly appropriate default value.
pub struct UninitToDefault;

impl Transform for UninitToDefault {
    fn transform(&self, krate: Crate, _st: &CommandState, cx: &driver::Ctxt) -> Crate {
        fold_nodes(krate, |l: P<Local>| {
            if !l.init.as_ref().map_or(false, |e| is_uninit_call(cx, e)) {
                return l;
            }

            let init = l.init.as_ref().unwrap().clone();
            let ty = cx.node_type(init.id);
            let new_init_lit = match ty.sty {
                TypeVariants::TyBool => mk().bool_lit(false),
                TypeVariants::TyChar => mk().char_lit('\0'),
                TypeVariants::TyInt(ity) => mk().int_lit(0, ity),
                TypeVariants::TyUint(uty) => mk().int_lit(0, uty),
                TypeVariants::TyFloat(fty) => mk().float_lit("0", fty),
                _ => return l,
            };
            l.map(|l| {
                Local {
                    init: Some(mk().lit_expr(new_init_lit)),
                    .. l
                }
            })
        })
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}



pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("let_x_uninitialized", |_args| mk(LetXUninitialized));
    reg.register("sink_lets", |_args| mk(SinkLets));
    reg.register("fold_let_assign", |_args| mk(FoldLetAssign));
    reg.register("uninit_to_default", |_args| mk(UninitToDefault));
}
