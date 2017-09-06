use std::collections::{HashMap, HashSet};
use std::mem;
use rustc::hir::def_id::DefId;
use rustc::ty::TypeVariants;
use syntax::abi::Abi;
use syntax::ast::*;
use syntax::codemap::Spanned;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::util::move_map::MoveMap;
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


/// Rewrite types in the crate to types that are transmute-compatible with the original.
/// Automatically inserts `transmute` calls as needed to make the types line up after rewriting.
///
/// This function currently handles only direct function calls.  Creation and use of function
/// pointers is not handled correctly yet.
pub fn bitcast_retype<F>(st: &CommandState, cx: &driver::Ctxt, krate: Crate, retype: F) -> Crate
        where F: FnMut(&P<Ty>) -> Option<P<Ty>> {
    // (1) Walk over all supported nodes, replacing type annotations.  Also record which nodes had
    // type annotations replaced, for future reference.

    struct ChangeTypeFolder<F> {
        retype: F,
        changed_inputs: HashMap<(NodeId, usize), (P<Ty>, P<Ty>)>,
        changed_outputs: HashMap<NodeId, (P<Ty>, P<Ty>)>,
        // Functions where at least one input or output changed
        changed_funcs: HashSet<NodeId>,
        changed_defs: HashMap<NodeId, (P<Ty>, P<Ty>)>,
    }

    impl<F> Folder for ChangeTypeFolder<F>
            where F: FnMut(&P<Ty>) -> Option<P<Ty>> {
        fn fold_item(&mut self, i: P<Item>) -> SmallVector<P<Item>> {
            let i = if matches!([i.node] ItemKind::Fn(..)) {
                i.map(|mut i| {
                    let mut fd = expect!([i.node]
                                         ItemKind::Fn(ref fd, _, _ ,_ ,_ ,_) =>
                                         fd.clone().unwrap());

                    for (j, arg) in fd.inputs.iter_mut().enumerate() {
                        if let Some(new_ty) = (self.retype)(&arg.ty) {
                            let old_ty = mem::replace(&mut arg.ty, new_ty.clone());
                            self.changed_inputs.insert((i.id, j),
                                                       (old_ty.clone(), new_ty.clone()));
                            self.changed_funcs.insert(i.id);

                            // Also record that the type of the variable declared here has changed.
                            if matches!([arg.pat.node] PatKind::Ident(..)) {
                                // Note that `PatKind::Ident` doesn't guarantee that this is a
                                // variable binding.  But if it's not, then no name will ever
                                // resolve to `arg.pat`'s DefId, so it doesn't matter.
                                self.changed_defs.insert(arg.pat.id, (old_ty, new_ty));
                            } else {
                                // TODO: Would be nice to warn the user (or skip rewriting) if a
                                // nontrivial pattern gets its type changed, as we'll likely miss
                                // adding some required `transmute`s.
                            }
                        }
                    }

                    if let FunctionRetTy::Ty(ref mut ty) = fd.output {
                        if let Some(new_ty) = (self.retype)(ty) {
                            let old_ty = mem::replace(ty, new_ty.clone());
                            self.changed_outputs.insert(i.id, (old_ty, new_ty));
                            self.changed_funcs.insert(i.id);
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
        changed_funcs: HashSet::new(),
        changed_defs: HashMap::new(),
    };
    let krate = krate.fold(&mut f);
    let ChangeTypeFolder { changed_inputs, changed_outputs, changed_funcs,
                           changed_defs, .. } = f;


    // (2) Look for exprs referencing the changed items, and wrap them in transmutes.

    let rvalue_repl = parse_expr(cx.session(),
            "::std::mem::transmute::<__old_ty, __new_ty>(__e)");
    let lvalue_repl = parse_expr(cx.session(),
            "*::std::mem::transmute::<&__old_ty, &__new_ty>(&__e)");
    let lvalue_mut_repl = parse_expr(cx.session(),
            "*::std::mem::transmute::<&mut __old_ty, &mut __new_ty>(&mut __e)");

    // Folder for rewriting top-level exprs only
    struct ExprFolder<F> {
        callback: F,
    }

    impl<F: FnMut(P<Expr>) -> P<Expr>> Folder for ExprFolder<F> {
        fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
            (self.callback)(e)
        }
    }

    fn fold_top_exprs<T, F>(x: T, callback: F) -> <T as Fold>::Result
            where T: Fold, F: FnMut(P<Expr>) -> P<Expr> {
        let mut f = ExprFolder { callback: callback };
        x.fold(&mut f)
    }

    let transmute = |e, context, old_ty: &P<Ty>, new_ty: &P<Ty>| {
        let mut bnd = Bindings::new();
        bnd.add_expr("__e", e);
        bnd.add_ty("__old_ty", (*old_ty).clone());
        bnd.add_ty("__new_ty", (*new_ty).clone());

        let repl = match context {
            lr_expr::Context::Rvalue => rvalue_repl.clone(),
            lr_expr::Context::Lvalue => lvalue_repl.clone(),
            lr_expr::Context::LvalueMut => lvalue_mut_repl.clone(),
        };
        repl.subst(st, cx, &bnd)
    };

    let krate = fold_top_exprs(krate, |e: P<Expr>| {
        fold_expr_with_context(e, lr_expr::Context::Rvalue, |e, context| {
            match e.node {
                ExprKind::Path(..) => {
                    if let Some(&(ref old_ty, ref new_ty)) = cx.try_resolve_expr(&e)
                            .and_then(|did| cx.hir_map().as_local_node_id(did))
                            .and_then(|id| changed_defs.get(&id)) {
                        return transmute(e.clone(), context, new_ty, old_ty);
                    }
                },

                ExprKind::Field(ref obj, ref name) => {
                    let ty = cx.adjusted_node_type(obj.id);
                    match ty.sty {
                        TypeVariants::TyAdt(adt, _) => {
                            let did = adt.struct_variant().field_named(name.node.name).did;
                            if let Some(&(ref old_ty, ref new_ty)) = cx.hir_map()
                                    .as_local_node_id(did)
                                    .and_then(|id| changed_defs.get(&id)) {
                                return transmute(e.clone(), context, new_ty, old_ty);
                            }
                        },
                        _ => panic!("field access on non-adt"),
                    }
                },

                ExprKind::Call(ref func, ref args) => {
                    if let Some(func_id) = cx.opt_callee(&e)
                            .and_then(|did| cx.hir_map().as_local_node_id(did)) {
                        if changed_funcs.contains(&func_id) {
                            let mut e = e.clone();

                            let new_args = args.iter().enumerate().map(|(i, a)| {
                                if let Some(&(ref old_ty, ref new_ty)) =
                                        changed_inputs.get(&(func_id, i)) {
                                    transmute(a.clone(),
                                              lr_expr::Context::Rvalue,
                                              old_ty,
                                              new_ty)
                                } else {
                                    a.clone()
                                }
                            }).collect();
                            e = e.map(move |mut e| {
                                expect!([e.node]
                                        ExprKind::Call(_, ref mut args) => *args = new_args);
                                e
                            });

                            if let Some(&(ref old_ty, ref new_ty)) =
                                    changed_outputs.get(&func_id) {
                                e = transmute(e, context, new_ty, old_ty);
                            }

                            return e;
                        }
                    }
                },

                // TODO: Handle MethodCall.  In theory we should also deal with method calls from
                // operator overloads, but I doubt anybody wants to rewrite those definitions.

                _ => {},
            };

            e
        })
    });


    // (3) Wrap output expressions from functions whose return types were modified.

    let krate = fold_fns(krate, |mut fl| {
        if let Some(&(ref old_ty, ref new_ty)) = changed_outputs.get(&fl.id) {
            fl.block = fl.block.map(|b| fold_output_exprs(b, true, |e| {
                transmute(e, lr_expr::Context::Rvalue, old_ty, new_ty)
            }));
        }

        fl
    });


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

        bitcast_retype(st, cx, krate, |ty| {
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

    fn min_phase(&self) -> Phase {
        Phase::Phase3
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
