use std::collections::{HashMap, HashSet};
use rustc::hir::def_id::DefId;
use syntax::ast::*;
use syntax::ptr::P;
use syntax::symbol::Symbol;
use smallvec::SmallVec;

use api::*;
use command::{CommandState, Registry};
use driver;
use transform::Transform;
use util::IntoSymbol;
use util::Lone;
use util::dataflow;
use util::HirDefExt;


pub struct CollectToStruct {
    pub struct_name: String,
    pub instance_name: String,
}

impl Transform for CollectToStruct {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        let static_pat: P<Item> = parse_items(cx.session(), "static __x: __t = __init;").lone();


        // Map from Symbol (the name) to the DefId of the old `static`.
        let mut old_statics = HashMap::new();

        let krate = fold_modules(krate, |curs| {
            let mut matches = Vec::new();
            let mut insert_point = None;

            while let Some(bnd) = curs.advance_until_match(
                    |i| MatchCtxt::from_match(st, cx, &static_pat, &i)
                            .ok().map(|mcx| mcx.bindings)) {
                if !st.marked(curs.next().id, "target") {
                    curs.advance();
                    continue;
                }
                info!("found {:?}: {:?}", bnd.ident("__x"), bnd.ty("__t"));

                // Record this static
                old_statics.insert(bnd.ident("__x").name,
                                   cx.node_def_id(curs.next().id));

                if insert_point.is_none() {
                    insert_point = Some(curs.mark());
                }
                curs.remove();
                matches.push(bnd);
            }

            info!("collected {} matching statics", matches.len());

            if let Some(insert_point) = insert_point {
                curs.seek(insert_point);
                curs.insert(build_collected_struct(&self.struct_name, &matches));
                curs.insert(build_struct_instance(&self.struct_name,
                                                  &self.instance_name,
                                                  &matches));
            }
        });

        let ident_pat = parse_expr(cx.session(), "__x");
        let ident_repl = parse_expr(cx.session(), "__s.__x");
        let mut init_mcx = MatchCtxt::new(st, cx);
        init_mcx.set_type("__x", BindingType::Ident);
        init_mcx.bindings.add_ident(
            "__s", Ident::with_empty_ctxt((&self.instance_name as &str).into_symbol()));

        let krate = fold_match_with(init_mcx, ident_pat, krate, |orig, bnd| {
            let static_id = match old_statics.get(&bnd.ident("__x").name) {
                Some(&x) => x,
                None => return orig,
            };

            if cx.resolve_expr(&orig) != static_id {
                return orig;
            }

            // This really is a reference to one of the collected statics.  Replace it with a
            // reference to the generated struct.
            ident_repl.clone().subst(st, cx, &bnd)
        });

        krate
    }
}

fn build_collected_struct(name: &str, matches: &[Bindings]) -> P<Item> {
    let fields = matches.iter().map(
        |bnd| mk().struct_field(bnd.ident("__x"), bnd.ty("__t"))).collect::<Vec<_>>();
    mk().struct_item(name, fields)
}

fn build_struct_instance(struct_name: &str,
                         instance_name: &str,
                         matches: &[Bindings]) -> P<Item> {
    let fields = matches.iter().map(
        |bnd| mk().field(bnd.ident("__x"), bnd.expr("__init"))).collect::<Vec<_>>();
    mk().mutbl()
        .static_item(instance_name,
                     mk().path_ty(vec![struct_name]),
                     mk().struct_expr(vec![struct_name], fields))
}


pub struct Localize;

impl Transform for Localize {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        // (1) Collect all marked statics.

        struct StaticInfo {
            name: Ident,
            arg_name: Symbol,
            ty: P<Ty>,
            mutbl: Mutability,
        }
        let mut statics = HashMap::new();

        let krate = fold_nodes(krate, |i: P<Item>| {
            if !st.marked(i.id, "target") {
                return SmallVector::one(i);
            }

            match i.node {
                ItemKind::Static(ref ty, mutbl, _) => {
                    let def_id = cx.node_def_id(i.id);
                    let arg_name_str = format!("{}_", i.ident.name.as_str());
                    let arg_name = (&arg_name_str as &str).into_symbol();
                    statics.insert(def_id, StaticInfo {
                        name: i.ident.clone(),
                        arg_name: arg_name,
                        ty: ty.clone(),
                        mutbl: mutbl,
                    });
                },
                _ => {},
            }

            SmallVector::one(i)
        });


        // (2) Collect all marked functions, and figure out which statics are used in each.

        // Collect all outgoing references from marked functions.
        let mut fn_refs = HashMap::new();
        let krate = fold_fns(krate, |mut fl| {
            if !st.marked(fl.id, "user") {
                return fl;
            }

            let fn_def_id = cx.node_def_id(fl.id);

            let mut refs = HashSet::new();
            let block = fold_resolved_paths(fl.block, cx, |qself, path, def| {
                if let Some(def_id) = def.opt_def_id() {
                    refs.insert(def_id);
                }
                (qself, path)
            });
            fn_refs.insert(fn_def_id, refs);

            fl.block = block;
            fl
        });

        // Sort the references, collecting those that point to other marked functions and those
        // that point to statics.
        struct FnInfo {
            fn_refs: HashSet<DefId>,
            static_refs: HashSet<DefId>,
        }

        let fn_ids = fn_refs.keys().map(|&x| x).collect::<HashSet<_>>();
        let mut fns = fn_refs.into_iter().map(|(k, v)| {
            let fn_refs = v.iter().filter(|id| fn_ids.contains(id))
                .map(|&x| x).collect();
            let static_refs = v.iter().filter(|id| statics.contains_key(id))
                .map(|&x| x).collect();
            (k, FnInfo { fn_refs, static_refs })
        }).collect::<HashMap<_, _>>();

        // Propagate statics backward through the (partial) callgraph.
        dataflow::iterate(&mut fns, |cur_id, cur, data| {
            let mut changed = false;
            for &other_id in &cur.fn_refs {
                if other_id == cur_id {
                    continue;
                }
                for &static_id in &data[other_id].static_refs {
                    if !cur.static_refs.contains(&static_id) {
                        cur.static_refs.insert(static_id);
                        changed = true;
                    }
                }
            }
            changed
        });

        // Build the final map of static usage, sorted to ensure deterministic ordering.
        let fn_statics = fns.into_iter().map(|(k, v)| {
            let mut statics = v.static_refs.into_iter().collect::<Vec<_>>();
            statics.sort();
            (k, statics)
        }).collect::<HashMap<_, _>>();


        // (3) Do the actual rewrite.  Update calls to marked functions, passing any statics they
        // require as arguments.  Add arguments to marked functions' signatures, corresponding to
        // the statics they reference.  Replace uses of statics in the bodies of marked functions
        // with the corresponding parameter. 

        let krate = fold_fns(krate, |mut fl| {
            let fn_def_id = cx.node_def_id(fl.id);
            if let Some(static_ids) = fn_statics.get(&fn_def_id) {

                // Add new argument to function signature.
                fl.decl = fl.decl.map(|mut decl| {
                    for &static_id in static_ids {
                        let info = &statics[&static_id];
                        decl.inputs.push(mk().arg(
                                mk().set_mutbl(info.mutbl).ref_ty(&info.ty),
                                mk().ident_pat(info.arg_name)));
                    }
                    decl
                });

                // Update uses of statics.
                fl.block = fold_nodes(fl.block, |e: P<Expr>| {
                    if let Some(def_id) = cx.try_resolve_expr(&e) {
                        if let Some(info) = statics.get(&def_id) {
                            return mk().unary_expr("*", mk().ident_expr(info.arg_name));
                        }
                    }
                    e
                });

                // Update calls to other marked functions.
                fl.block = fold_nodes(fl.block, |e: P<Expr>| {
                    match e.node {
                        ExprKind::Call(_, _) => {},
                        _ => return e,
                    }

                    e.map(|e| {
                        unpack!([e.node] ExprKind::Call(func, args));
                        let mut args = args;

                        if let Some(func_id) = cx.try_resolve_expr(&func) {
                            if let Some(func_static_ids) = fn_statics.get(&func_id) {
                                for &static_id in func_static_ids {
                                    args.push(mk().ident_expr(statics[&static_id].arg_name));
                                }
                            }
                        }

                        Expr {
                            node: ExprKind::Call(func, args),
                            .. e
                        }
                    })
                });

            } else {
                // Update calls only.
                fl.block = fold_nodes(fl.block, |e: P<Expr>| {
                    match e.node {
                        ExprKind::Call(_, _) => {},
                        _ => return e,
                    }

                    e.map(|e| {
                        unpack!([e.node] ExprKind::Call(func, args));
                        let mut args = args;

                        if let Some(func_id) = cx.try_resolve_expr(&func) {
                            if let Some(func_static_ids) = fn_statics.get(&func_id) {
                                for &static_id in func_static_ids {
                                    let info = &statics[&static_id];
                                    args.push(mk().set_mutbl(info.mutbl).addr_of_expr(
                                            mk().ident_expr(info.name)));
                                }
                            }
                        }

                        Expr {
                            node: ExprKind::Call(func, args),
                            .. e
                        }
                    })
                });
            }

            fl
        });

        krate

    }
}




pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("static_collect_to_struct", |args| mk(CollectToStruct {
        struct_name: args[0].clone(),
        instance_name: args[1].clone(),
    }));
    reg.register("static_to_local_ref", |_args| mk(Localize));
}
