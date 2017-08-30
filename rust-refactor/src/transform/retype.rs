use std::collections::{HashMap, HashSet};
use std::mem;
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
use command::{CommandState, Registry};
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
                        return unwrap.clone().subst(st, cx, &bnd);
                    }
                }
                e
            });

            fl
        });

        // (2) Rewrite callsites of modified functions.

        // We don't need any protection against infinite recursion here, because it doesn't make
        // sense for `wrap` to call the function whose args we're changing.
        let krate = fold_nodes(krate, |e: P<Expr>| {
            let callee = match_or!([cx.opt_callee(&e)] Some(x) => x; return e);
            let mod_args = match_or!([mod_fns.get(&callee)] Some(x) => x; return e);
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
                        args[idx] = wrap.clone().subst(st, cx, &bnd);
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


pub fn bitcast_retype<F>(krate: Crate, retype: F) -> Crate
        where F: FnMut(&P<Ty>) -> Option<P<Ty>> {
    // (1) Walk over all supported nodes, replacing type annotations.  Also record which nodes had
    // type annotations replaced, for future reference.

    struct ChangeTypeFolder<F> {
        retype: F,
        changed_inputs: HashMap<(NodeId, usize), (P<Ty>, P<Ty>)>,
        changed_outputs: HashMap<NodeId, (P<Ty>, P<Ty>)>,
        changed_defs: HashMap<NodeId, (P<Ty>, P<Ty>)>,
    }

    impl<F> Folder for ChangeTypeFolder<F>
            where F: FnMut(&P<Ty>) -> Option<P<Ty>> {
        fn fold_item(&mut self, i: P<Item>) -> SmallVector<P<Item>> {
            eprintln!("at item {:?}", i);
            let i = if matches!([i.node] ItemKind::Fn(..)) {
                i.map(|mut i| {
                    let mut fd = expect!([i.node]
                                         ItemKind::Fn(ref fd, _, _ ,_ ,_ ,_) =>
                                         fd.clone().unwrap());
                    eprintln!("handling fn decl {:?}", fd);

                    for (j, arg) in fd.inputs.iter_mut().enumerate() {
                        if let Some(new_ty) = (self.retype)(&arg.ty) {
                            let old_ty = mem::replace(&mut arg.ty, new_ty.clone());
                            self.changed_inputs.insert((i.id, j), (old_ty, new_ty));
                        }
                    }

                    if let FunctionRetTy::Ty(ref mut ty) = fd.output {
                        if let Some(new_ty) = (self.retype)(ty) {
                            let old_ty = mem::replace(ty, new_ty.clone());
                            self.changed_outputs.insert(i.id, (old_ty, new_ty));
                        }
                    }

                    match i.node {
                        ItemKind::Fn(ref mut fd_ptr, _, _, _, _, _) => {
                            *fd_ptr = P(fd);
                        },
                        _ => panic!("expected ItemKind::Fn"),
                    }

                    i
                })

            } else if matches!([i.node] ItemKind::Static(..)) {
                i.map(|mut i| {
                    {
                        let ty = expect!([i.node] ItemKind::Static(ref mut ty, _, _) => ty);
                        if let Some(new_ty) = (self.retype)(ty) {
                            let old_ty = mem::replace(ty, new_ty.clone());
                            self.changed_defs.insert(i.id, (old_ty, new_ty));
                        }
                    }
                    i
                })

            } else if matches!([i.node] ItemKind::Const(..)) {
                i.map(|mut i| {
                    {
                        let ty = expect!([i.node] ItemKind::Const(ref mut ty, _) => ty);
                        if let Some(new_ty) = (self.retype)(ty) {
                            let old_ty = mem::replace(ty, new_ty.clone());
                            self.changed_defs.insert(i.id, (old_ty, new_ty));
                        }
                    }
                    i
                })

            } else {
                i
            };

            fold::noop_fold_item(i, self)
        }

        fn fold_struct_field(&mut self, mut sf: StructField) -> StructField {
            eprintln!("at struct field {:?}", sf);
            if let Some(new_ty) = (self.retype)(&sf.ty) {
                let old_ty = mem::replace(&mut sf.ty, new_ty.clone());
                self.changed_defs.insert(sf.id, (old_ty, new_ty));
            }
            fold::noop_fold_struct_field(sf, self)
        }
    }

    let mut f = ChangeTypeFolder {
        retype: retype,
        changed_inputs: HashMap::new(),
        changed_outputs: HashMap::new(),
        changed_defs: HashMap::new(),
    };
    eprintln!("folding krate...");
    let krate = krate.fold(&mut f);
    eprintln!("fold done!");
    let ChangeTypeFolder { changed_inputs, changed_outputs, changed_defs, .. } = f;

    // (2) Look for exprs referencing the changed items, and wrap them in transmutes.


    krate
}


/// Replace types in signatures, fields, and globals.  The new types must be identical in
/// representation to the old types, as all conversions between old and new types will be done with
/// `transmute`.
pub struct BitcastRetype {
    pub pat: String,
    pub repl: String,
}

impl Transform for BitcastRetype {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let pat = parse_ty(cx.session(), &self.pat);
        let repl = parse_ty(cx.session(), &self.repl);

        eprintln!("running! {:?} -> {:?}", pat, repl);

        bitcast_retype(krate, |ty| {
            // Doing a "deep" rewrite here is based on the assumption that if `T` and `U` are
            // transmute-compatible, then so are `&T` and `&U`, `(T, T)` and `(U, U)`, `S<T>` and
            // `S<U>`, etc.  This might not be true when associated types are involved (`T::SomeTy`
            // and `U::SomeTy` could be totally unrelated).

            let mut matched = false;
            let new_ty = fold_match(st, cx, pat.clone(), ty.clone(), |_, bnd| {
                matched = true;
                repl.clone().subst(st, cx, &bnd)
            });
            if matched {
                Some(new_ty)
            } else {
                None
            }
        })
    }
}


pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("retype_argument", |args| mk(RetypeArgument {
        new_ty: args[0].clone(),
        wrap: args[1].clone(),
        unwrap: args[2].clone(),
    }));

    reg.register("bitcast_retype", |args| mk(BitcastRetype {
        pat: args[0].clone(),
        repl: args[1].clone(),
    }));
}
