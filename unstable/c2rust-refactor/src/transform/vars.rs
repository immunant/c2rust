use log::info;
use std::collections::hash_map::{HashMap, Entry};
use std::collections::HashSet;
use std::mem;
use rustc_hir::def_id::LOCAL_CRATE;
use rustc_hir::HirId;
use rustc_middle::ty::{TyKind, ParamEnv};
use rustc_ast::*;
use rustc_ast::mut_visit::{self, MutVisitor};
use rustc_ast::ptr::P;
use rustc_ast::visit::{self, Visitor};

use crate::ast_builder::mk;
use crate::ast_manip::{MutVisit, MutVisitNodes, fold_blocks, visit_nodes};
use crate::command::{CommandState, DriverCommand, Registry};
use crate::driver::{Phase};
use crate::match_or;
use crate::matcher::{MatchCtxt, Subst, mut_visit_match_with, replace_stmts};
use crate::reflect::reflect_tcx_ty;
use crate::transform::Transform;
use crate::RefactorCtxt;


/// # `let_x_uninitialized` Command
///
/// Obsolete - the translator now does this automatically.
///
/// Usage: `let_x_uninitialized`
///
/// For each local variable that is uninitialized (`let x;`), add
/// `mem::uninitialized()` as an initializer expression.
pub struct LetXUninitialized;

impl Transform for LetXUninitialized {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        replace_stmts(st, cx, krate,
                                  "let __pat;",
                                  "let __pat = ::std::mem::uninitialized();");
        replace_stmts(st, cx, krate,
                                  "let __pat: __ty;",
                                  "let __pat: __ty = ::std::mem::uninitialized();");
    }
}


/// # `sink_lets` Command
///
/// Usage: `sink_lets`
///
/// For each local variable with a trivial initializer, move the local's
/// declaration to the innermost block containing all its uses.
///
/// "Trivial" is currently defined as no initializer (`let x;`) or an initializer
/// without any side effects.  This transform requires trivial assignments to avoid
/// reordering side effects.
pub struct SinkLets;

impl Transform for SinkLets {
    fn transform(&self, krate: &mut Crate, _st: &CommandState, cx: &RefactorCtxt) {
        // (1) Collect info on every local that might be worth moving.

        struct LocalInfo {
            local: P<Local>,
            old_node_id: NodeId,
        }

        let mut locals: HashMap<HirId, LocalInfo> = HashMap::new();
        visit_nodes(krate, |l: &Local| {
            if let PatKind::Ident(BindingMode::ByValue(_), _, None) = l.pat.kind {
                let l_init = get_local_init(&l.kind);
                if l_init.is_none() || !expr_has_side_effects(cx, l_init.unwrap()) {
                    let hir_id = cx.hir_map().node_to_hir_id(l.pat.id);
                    locals.insert(hir_id, LocalInfo {
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
            cur: HashMap<HirId, UseKind>,
            block_locals: HashMap<NodeId, HashMap<HirId, UseKind>>,

            cx: &'a RefactorCtxt<'a, 'tcx>,
            locals: &'a HashMap<HirId, LocalInfo>,
        }

        impl<'a, 'tcx> BlockLocalsVisitor<'a, 'tcx> {
            fn record_use_inside_block(&mut self, id: HirId) {
                match self.cur.entry(id) {
                    Entry::Occupied(e) => { *e.into_mut() = UseKind::Other; },
                    Entry::Vacant(e) => { e.insert(UseKind::InsideOneBlock); },
                }
            }
        }

        impl<'a, 'tcx, 'ast> Visitor<'ast> for BlockLocalsVisitor<'a, 'tcx> {
            fn visit_expr(&mut self, e: &'ast Expr) {
                if let Some(hir_id) = self.cx.try_resolve_expr_to_hid(e) {
                    if self.locals.contains_key(&hir_id) {
                        self.cur.insert(hir_id, UseKind::Other);
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

        // Map from block NodeId to HirIds of locals to place in the block.
        let mut local_placement = HashMap::new();
        let mut placed_locals = HashSet::new();

        // This is separate from the actual rewrite because we need to do a preorder traversal, but
        // folds are always postorder to avoid infinite recursion.
        visit_nodes(krate, |b: &Block| {
            let used_locals = &block_locals[&b.id];

            // Check if there are any locals we should place in this block.  We place a local here
            // if its use kind is `Other` and it hasn't been placed already.  A use kind of
            // `InsideOneBlock` means the local can be placed somewhere deeper, so this strategy
            // ensures we place the local in the deepest legal position.  We rely on `mut_visit_nodes`
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

            if !place_here.is_empty() {
                local_placement.insert(b.id, place_here);
            }
        });

        // (4) Place new locals in the appropriate locations.

        MutVisitNodes::visit(krate, |b: &mut P<Block>| {
            let place_here = match_or!([local_placement.get(&b.id)]
                                       Some(x) => x; return);

            let mut new_stmts = place_here.iter()
                .map(|&id| mk().local_stmt(&locals[&id].local))
                .collect::<Vec<_>>();
            new_stmts.append(&mut b.stmts);
            b.stmts = new_stmts;
        });

        // (5) Remove old locals

        // Note that we don't check for locals that we failed to place.  The only way we can fail
        // to place a local is if it is never used anywhere.  Otherwise we would, at worst, place
        // it back in its original block.  The result is that this pass has the additional effect
        // of removing unused locals.
        let remove_local_ids = locals.iter()
            .map(|(_, info)| info.old_node_id)
            .collect::<HashSet<_>>();

        MutVisitNodes::visit(krate, |b: &mut P<Block>| {
            b.stmts.retain(|s| {
                match s.kind {
                    StmtKind::Local(ref l) => !remove_local_ids.contains(&l.id),
                    _ => true,
                }
            });
        });
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


/// Helper function that returns the initializer expression
/// as a `P<Expr>` in contrast to a `LocalKind::init()` which gives a `&Expr`.
fn get_local_init(local: &LocalKind) -> Option<&P<Expr>> {
    match local {
        LocalKind::Decl => None,
        LocalKind::Init(i) | LocalKind::InitElse(i, _) => Some(i),
    }
}

fn expr_has_side_effects(cx: &RefactorCtxt, e: &P<Expr>) -> bool {
    match e.kind {
        // Literals never have side effects
        ExprKind::Lit(_) => false,
        ExprKind::Array(ref elems) => elems.iter().any(|e| expr_has_side_effects(cx, e)),
        ExprKind::Call(ref func, ref args) => {
            let func_is_const_fn = cx.try_resolve_expr(func)
                .map_or(false, |func_id| cx.ty_ctxt().is_const_fn(func_id));
            !func_is_const_fn ||
                args.iter().any(|e| expr_has_side_effects(cx, e))
        },
        ExprKind::Tup(ref elems) => elems.iter().any(|e| expr_has_side_effects(cx, e)),
        ExprKind::Cast(ref expr, _) => expr_has_side_effects(cx, expr),
        ExprKind::Type(ref expr, _) => expr_has_side_effects(cx, expr),
        // TODO: ExprKind::Path safe???
        ExprKind::Struct(ref se) => {
            if se.fields.iter().any(|f| expr_has_side_effects(cx, &f.expr)) {
                return true;
            }

            if let StructRest::Base(ref e) = se.rest {
                return expr_has_side_effects(cx, e);
            }

            false
        }
        ExprKind::Repeat(ref expr, _) => expr_has_side_effects(cx, expr),
        ExprKind::Paren(ref expr) => expr_has_side_effects(cx, expr),

        // We conservatively assume that all others have side effects
        _ => true,
    }
}


fn is_uninit_call(cx: &RefactorCtxt, e: &Expr) -> bool {
    let func = match_or!([e.kind] ExprKind::Call(ref func, _) => func; return false);
    let def_id = cx.resolve_expr(func);
    if def_id.krate == LOCAL_CRATE {
        return false;
    }
    let crate_name = cx.ty_ctxt().crate_name(def_id.krate);
    let path = cx.ty_ctxt().def_path(def_id);

    (crate_name.as_str() == "std" || crate_name.as_str() == "core") &&
    path.data.len() == 2 &&
    path.data[0].data.get_opt_name().map_or(false, |sym| sym.as_str() == "mem") &&
    path.data[1].data.get_opt_name().map_or(false, |sym| sym.as_str() == "uninitialized")
}



/// # `fold_let_assign` Command
///
/// Usage: `fold_let_assign`
///
/// Fold together `let`s with no initializer or a trivial one, and subsequent assignments.
/// For example, replace `let x; x = 10;` with `let x = 10;`.
pub struct FoldLetAssign;

impl Transform for FoldLetAssign {
    fn transform(&self, krate: &mut Crate, _st: &CommandState, cx: &RefactorCtxt) {
        // (1) Find all locals that might be foldable.

        let mut locals: HashMap<HirId, P<Local>> = HashMap::new();
        visit_nodes(krate, |l: &Local| {
            if let PatKind::Ident(BindingMode::ByValue(_), _, None) = l.pat.kind {
                let l_init = get_local_init(&l.kind);
                if l_init.is_none() || !expr_has_side_effects(cx, l_init.unwrap()) {
                    let hir_id = cx.hir_map().node_to_hir_id(l.pat.id);
                    locals.insert(hir_id, P(l.clone()));
                }
            }
        });

        // (2) Compute the set of foldable locals that are used in each statement.

        struct StmtLocalsVisitor<'a, 'tcx: 'a> {
            cur: HashSet<HirId>,
            stmt_locals: HashMap<NodeId, HashSet<HirId>>,

            cx: &'a RefactorCtxt<'a, 'tcx>,
            locals: &'a HashMap<HirId, P<Local>>,
        }

        impl<'a, 'tcx, 'ast> Visitor<'ast> for StmtLocalsVisitor<'a, 'tcx> {
            fn visit_expr(&mut self, e: &'ast Expr) {
                if let Some(hir_id) = self.cx.try_resolve_expr_to_hid(e) {
                    if self.locals.contains_key(&hir_id) {
                        self.cur.insert(hir_id);
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
            visit::walk_crate(&mut v, krate);
            v.stmt_locals
        };


        // (3) Walk through blocks, looking for non-initializing `let`s and assignment statements,
        // and rewriting them when found.

        // Map from node ID to def ID, for known locals.
        let local_node_def = locals.iter().map(|(did, l)| (l.id, did)).collect::<HashMap<_, _>>();

        // Map from def ID to a Mark, giving the position of the local so we can delete it later.
        // If a local gets used before we reach the assignment, we delete it from this map.
        let mut local_pos = HashMap::new();

        fold_blocks(krate, |curs| {
            while !curs.eof() {
                // Is it a local declaration?  If so, mark it.
                let mark_did = match curs.next().kind {
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
                let assign_info = match curs.next().kind {
                    StmtKind::Semi(ref e) => {
                        match e.kind {
                            ExprKind::Assign(ref lhs, ref rhs, _) => {
                                if let Some(hir_id) = cx.try_resolve_expr_to_hid(&lhs) {
                                    if local_pos.contains_key(&hir_id) {
                                        if is_self_ref(cx, hir_id, rhs) {
                                            None
                                        } else {
                                            Some((hir_id, rhs.clone()))
                                        }
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
                if let Some((hir_id, init)) = assign_info {
                    let local = &locals[&hir_id];
                    let local_mark = local_pos.remove(&hir_id).unwrap();
                    let mut l = local.clone();
                    l.kind = LocalKind::Init(init);
                    curs.replace(|_| mk().local_stmt(l));

                    let here = curs.mark();
                    curs.seek(local_mark);
                    curs.remove();
                    curs.seek(here);
                }

                // Does it access some locals?
                if let Some(locals) = stmt_locals.get(&curs.next().id) {
                    for &hir_id in locals {
                        // This local is being accessed before its first recognized assignment.
                        // That means we can't fold the `let` with the later assignment.
                        local_pos.remove(&hir_id);
                    }
                }


                curs.advance();
            }
        })
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}

fn is_self_ref(cx: &RefactorCtxt, lhs: HirId, rhs: &Expr) -> bool {
    let mut is_self_ref = false;
    visit_nodes(rhs, |e: &Expr| {
        if let Some(hir_id) = cx.try_resolve_expr_to_hid(&e) {
            if hir_id == lhs {
                is_self_ref = true;
            }
        }
    });
    is_self_ref
}


/// # `uninit_to_default` Command
///
/// Obsolete - works around translator problems that no longer exist.
///
/// Usage: `uninit_to_default`
///
/// In local variable initializers, replace `mem::uninitialized()` with an
/// appropriate default value of the variable's type.
pub struct UninitToDefault;

impl Transform for UninitToDefault {
    fn transform(&self, krate: &mut Crate, _st: &CommandState, cx: &RefactorCtxt) {
        MutVisitNodes::visit(krate, |l: &mut P<Local>| {
            if !l.kind.init().as_ref().map_or(false, |e| is_uninit_call(cx, e)) {
                return;
            }

            let init = l.kind.init().unwrap();
            let ty = cx.node_type(init.id);
            let new_init = match ty.kind() {
                TyKind::Bool => mk().lit_expr(mk().bool_lit(false)),
                TyKind::Char => mk().lit_expr('\0'),
                TyKind::Int(ity) => mk().lit_expr(mk().int_lit(0, *ity)),
                TyKind::Uint(uty) => mk().lit_expr(mk().int_lit(0, *uty)),
                TyKind::Float(fty) => mk().lit_expr(mk().float_lit("0", *fty)),
                _ => return,
            };

            match l.kind {
                LocalKind::Decl => {},
                LocalKind::Init(ref mut init) => *init = new_init,
                LocalKind::InitElse(ref mut init, _) => *init = new_init,
            };
        })
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


/// # `remove_redundant_let_types` Command
///
/// Usage: `remove_redundant_let_types`
///
/// Removes types from all `let` statements where the initializer's type matches the declared one,
/// so the latter can be omitted and inferred.
/// For example, replace `let x: u32 = 1u32;` with `let x = 1u32;`
pub struct RemoveRedundantLetTypes;

impl Transform for RemoveRedundantLetTypes {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        let tcx = cx.ty_ctxt();
        let mut mcx = MatchCtxt::new(st, cx);
        let pat = mcx.parse_stmts("let $pat:Pat : $ty:Ty = $init:Expr;");
        let repl = mcx.parse_stmts("let $pat = $init;");
        mut_visit_match_with(mcx, pat, krate, |ast, mcx| {
            let e = mcx.bindings.get::<_, P<Expr>>("$init").unwrap();
            let e_ty = cx.adjusted_node_type(e.id);
            let e_ty = tcx.normalize_erasing_regions(ParamEnv::empty(), e_ty);

            let t = mcx.bindings.get::<_, P<Ty>>("$ty").unwrap();
            let t_ty = cx.adjusted_node_type(t.id);
            let t_ty = tcx.normalize_erasing_regions(ParamEnv::empty(), t_ty);
            if e_ty == t_ty {
                *ast = repl.clone().subst(st, cx, &mcx.bindings);
            }
        })
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}

/// # `expand_local_ptr_tys` Command
///
/// Usage: `expand_local_ptr_tys`
///
/// Ownership analysis now supports locals and therefore it may be necessary to
/// add explicit type annotations to locals which are missing them. This is because
/// ownership analysis marks types
fn expand_local_ptr_tys(st: &CommandState, cx: &RefactorCtxt) {
    struct LocalVisitor<'a, 'tctx: 'a> {
        cx: &'a RefactorCtxt<'a, 'tctx>,
    }

    impl<'a, 'tctx> MutVisitor for LocalVisitor<'a, 'tctx> {
        fn visit_local(&mut self, local: &mut P<Local>) {
            // If it already has a ty, skip
            if local.ty.is_some() {
                return mut_visit::noop_visit_local(local, self);
            }

            // Get the type
            let rty = self.cx.node_type(local.id);
            let ty = reflect_tcx_ty(self.cx.ty_ctxt(), rty);

            // Assign ty if a raw ptr
            if let rustc_ast::ast::TyKind::Ptr(_) = ty.kind {
                local.ty = Some(ty);
            }

            mut_visit::noop_visit_local(local, self)
        }
    }

    st.map_krate(|krate| { krate.visit(&mut LocalVisitor { cx }) });
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("let_x_uninitialized", |_args| mk(LetXUninitialized));
    reg.register("sink_lets", |_args| mk(SinkLets));
    reg.register("fold_let_assign", |_args| mk(FoldLetAssign));
    reg.register("uninit_to_default", |_args| mk(UninitToDefault));
    reg.register("remove_redundant_let_types", |_args| mk(RemoveRedundantLetTypes));
    reg.register("expand_local_ptr_tys", |_args| {
        Box::new(DriverCommand::new(Phase::Phase3, move |st, cx| {
            expand_local_ptr_tys(st, cx);
        }))
    });
}
