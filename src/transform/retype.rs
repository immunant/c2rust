use std::collections::{HashMap, HashSet};
use rustc::hir::def_id::DefId;
use rustc::ty::TypeVariants;
use syntax::abi::Abi;
use syntax::ast::*;
use syntax::codemap::Spanned;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::util::small_vector::SmallVector;

use api::*;
use bindings::Bindings;
use command::CommandState;
use driver::{self, Phase};
use fold::Fold;
use transform::Transform;


/// Change the type of function arguments.  All `target` args will have their types changed to
/// `new_ty`.  Values passed for those arguments will be converted with `wrap`, and uses of those
/// arguments inside the modified functions will be converted with `unwrap`.
pub struct RetypeArgument {
    pub new_ty: String,
    pub wrap: String,
    pub unwrap: String,
}

impl Transform for RetypeArgument {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        // (1) Change argument types and rewrite function bodies.

        let new_ty = parse_ty(cx.session(), &self.new_ty);
        let wrap = parse_expr(cx.session(), &self.wrap);
        let unwrap = parse_expr(cx.session(), &self.unwrap);

        // Modified functions, by DefId.  For each one, we track the argument indices that were
        // modified.
        let mut mod_fns: HashMap<DefId, HashSet<usize>> = HashMap::new();

        let krate = fold_fns(krate, |mut fl| {
            let fn_id = fl.id;

            // Def IDs of changed arguments.
            let mut changed_args = HashSet::new();
            fl.decl = fl.decl.map(|mut decl| {
                for (i, arg) in decl.inputs.iter_mut().enumerate() {
                    if st.marked(arg.id, "target") {
                        arg.ty = new_ty.clone();
                        mod_fns.entry(cx.node_def_id(fn_id)).or_insert_with(HashSet::new).insert(i);

                        if let Some(def_id) = cx.hir_map().opt_local_def_id(arg.pat.id) {
                            changed_args.insert(def_id);
                        } else {
                            warn!("can't find DefId for arg pattern {:?} (for type {:?})",
                                  arg.pat, arg.ty);
                        }
                    }
                }
                decl
            });

            if changed_args.len() == 0 {
                return fl;
            }

            info!("changed args: {:?}", changed_args);

            // An argument was changed, so we need to rewrite uses of that argument inside the
            // function body.

            // `fold_nodes` does a preorder traversal, so if we replace `x` with `wrap(x)`, we will
            // see `x` again in the recursive call.  We keep track of which nodes have already been
            // rewritten so that we don't end up with a stack overflow.
            let mut rewritten_nodes = HashSet::new();
            fl.block = fold_nodes(fl.block.take(), |e: P<Expr>| {
                if let Some(def_id) = cx.try_resolve_expr(&e) {
                    if changed_args.contains(&def_id) && !rewritten_nodes.contains(&e.id) {
                        rewritten_nodes.insert(e.id);
                        let mut bnd = Bindings::new();
                        bnd.add_expr("__new", e.clone());
                        return unwrap.clone().subst(&bnd);
                    }
                }
                e
            });

            fl
        });

        info!("modified fns: {:?}", mod_fns);

        // (2) Rewrite callsites of modified functions.

        // We don't need any protection against infinite recursion here, because it doesn't make
        // sense for `wrap` to call the function whose args we're changing.
        let krate = fold_nodes(krate, |e: P<Expr>| {
            info!("look: {:?}", e);
            let callee = match_or!([cx.opt_callee(&e)] Some(x) => x; return e);
            info!(" ** found call to {:?}", callee);
            let mod_args = match_or!([mod_fns.get(&callee)] Some(x) => x; return e);
            info!(" ** ... with mod_args {:?}", mod_args);
            e.map(|mut e| {
                {
                    let args: &mut [P<Expr>] =
                        match e.node {
                            ExprKind::Call(_, ref mut args) => args,
                            ExprKind::MethodCall(_, ref mut args) => args,
                            _ => panic!("expected Call or MethodCall"),
                        };
                    for &idx in mod_args {
                        let mut bnd = Bindings::new();
                        bnd.add_expr("__old", args[idx].clone());
                        args[idx] = wrap.clone().subst(&bnd);
                    }
                }
                e
            })
        });

        krate
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}

