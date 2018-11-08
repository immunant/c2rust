use std::collections::{HashMap, HashSet};
use rustc::hir::def_id::DefId;
use syntax::ast::*;
use syntax::ptr::P;
use syntax::symbol::Symbol;

use api::*;
use command::{CommandState, Registry};
use driver;
use transform::Transform;
use c2rust_ast_builder::IntoSymbol;
use util::dataflow;
use util::HirDefExt;


/// # `static_collect_to_struct` Command
/// 
/// Usage: `static_collect_to_struct STRUCT VAR`
/// 
/// Marks: `target`
/// 
/// Collect marked statics into a single static struct.
/// 
/// Specifically:
/// 
///  1. Find all statics marked `target`.  For each one, record its name, type, and
///     initializer expression, then delete it.
///  2. Generate a new struct definition named `STRUCT`.  For each marked static,
///     include a field of `STRUCT` with the same name and type as the static.
///  3. Generate a new `static mut` named `VAR` whose type is `STRUCT`.  Initialize
///     it using the initializer expressions for the marked statics.
///  4. For each marked static `foo`, replace uses of `foo` with `VAR.foo`.
/// 
/// Example:
/// 
///     static mut FOO: i32 = 100;
///     static mut BAR: bool = true;
/// 
///     unsafe fn f() -> i32 {
///         FOO
///     }
/// 
/// 
/// After running `static_collect_to_struct Globals G`, with both statics marked:
/// 
///     struct Globals {
///         FOO: i32,
///         BAR: bool,
///     }
/// 
///     static mut G: Globals = Globals {
///         FOO: 100,
///         BAR: true,
///     };
/// 
///     unsafe fn f() -> i32 {
///         G.FOO
///     }
pub struct CollectToStruct {
    pub struct_name: String,
    pub instance_name: String,
}

impl Transform for CollectToStruct {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        // Map from Symbol (the name) to the DefId of the old `static`.
        let mut old_statics = HashMap::new();

        let krate = fold_modules(krate, |curs| {
            let mut matches = Vec::new();
            let mut insert_point = None;

            while let Some((ident, ty, init)) = curs.advance_until_match(
                    |i| match_or!([i.node] ItemKind::Static(ref ty, _, ref init) =>
                                  Some((i.ident, ty.clone(), init.clone())); None)) {
                if !st.marked(curs.next().id, "target") {
                    curs.advance();
                    continue;
                }
                info!("found {:?}: {:?}", ident, ty);

                // Record this static
                old_statics.insert(ident.name,
                                   cx.node_def_id(curs.next().id));

                if insert_point.is_none() {
                    insert_point = Some(curs.mark());
                }
                curs.remove();

                let mut bnd = Bindings::new();
                bnd.add_ident("__x", ident);
                bnd.add_ty("__t", ty);
                bnd.add_expr("__init", init);
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


/// # `static_to_local_ref` Command
/// 
/// Usage: `static_to_local_ref`
/// 
/// Marks: `target`, `user`
/// 
/// For each function marked `user`, replace uses of statics marked `target` with
/// uses of newly-introduced reference arguments.  Afterward, no `user` function
/// directly accesses any `target` static.  At call sites of `user` functions, a
/// reference to the original static is passed in for each new argument if the
/// caller is not itself a `user` function; otherwise, the caller's own reference
/// argument is passed through.  Note this sometimes results in functions gaining
/// arguments corresponding to statics that the function itself does not use, but
/// that its callees do.
/// 
/// Example:
/// 
///     static mut FOO: i32 = 100;  // FOO: target
/// 
///     unsafe fn f() -> i32 {  // f: user
///         FOO
///     }
/// 
///     unsafe fn g() -> i32 {  // g: user
///         f()
///     }
/// 
///     unsafe fn h() -> i32 {
///         g()
///     }
/// 
/// After running `static_to_local_ref`:
/// 
///     static mut FOO: i32 = 100;
/// 
///     // `f` is a `user` that references `FOO`, so it
///     // gains a new argument `FOO_`.
///     unsafe fn f(FOO_: &mut i32) -> i32 {
///         // References to `FOO` are replaced with `*FOO_`
///         *FOO_
///     }
/// 
///     // `g` is a `user` that references `FOO` indirectly,
///     // via fellow `user` `f`.
///     unsafe fn g(FOO_: &mut i32) -> i32 {
///         // `g` passes through its own `FOO_` reference
///         // when calling `f`.
///         f(FOO_)
///     }
/// 
///     // `h` is not a `user`, so its signature is unchanged.
///     unsafe fn h() -> i32 {
///         // `h` passes in a reference to the original
///         // static `FOO`.
///         g(&mut FOO)
///     }
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
                return smallvec![i];
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

            smallvec![i]
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


/// # `static_to_local` Command
/// 
/// Usage: `static_to_local`
/// 
/// Marks: `target`
/// 
/// Delete each static marked `target`.  For each function that uses a marked static, insert a new
/// local variable definition replicating the marked static.
/// 
/// Example:
/// 
///     static mut FOO: i32 = 100;  // FOO: target
/// 
///     unsafe fn f() -> i32 {
///         FOO
///     }
/// 
///     unsafe fn g() -> i32 {
///         FOO + 1
///     }
/// 
/// After running `static_to_local`:
/// 
///     // `FOO` deleted
/// 
///     // `f` gains a new local, replicating `FOO`.
///     unsafe fn f() -> i32 {
///         let FOO: i32 = 100;
///         FOO
///     }
/// 
///     // If multiple functions use `FOO`, each one gets its own copy.
///     unsafe fn g() -> i32 {
///         let FOO: i32 = 100;
///         FOO + 1
///     }
struct StaticToLocal;

impl Transform for StaticToLocal {
    fn transform(&self, krate: Crate, st: &CommandState, cx: &driver::Ctxt) -> Crate {
        // (1) Collect all marked statics.

        struct StaticInfo {
            name: Ident,
            ty: P<Ty>,
            mutbl: Mutability,
            expr: P<Expr>,
        }
        let mut statics = HashMap::new();

        let krate = fold_nodes(krate, |i: P<Item>| {
            if !st.marked(i.id, "target") {
                return smallvec![i];
            }

            match i.node {
                ItemKind::Static(ref ty, mutbl, ref expr) => {
                    let def_id = cx.node_def_id(i.id);
                    statics.insert(def_id, StaticInfo {
                        name: i.ident.clone(),
                        ty: ty.clone(),
                        mutbl: mutbl,
                        expr: expr.clone(),
                    });
                    return smallvec![];
                },
                _ => {},
            }

            smallvec![i]
        });


        // (2) Add a new local to every function that uses a marked static.

        let krate = fold_fns(krate, |mut fl| {
            // Figure out which statics (if any) this function uses.
            let mut ref_ids = HashSet::new();
            let mut refs = Vec::new();
            fl.block = fold_resolved_paths(fl.block, cx, |qself, path, def| {
                if let Some(def_id) = def.opt_def_id() {
                    if ref_ids.insert(def_id) {
                        if let Some(info) = statics.get(&def_id) {
                            refs.push(info);
                        }
                    }
                }
                (qself, path)
            });

            if refs.len() == 0 {
                return fl;
            }

            refs.sort_by_key(|info| info.name.name);

            fl.block = fl.block.map(|b| b.map(|mut b| {
                let mut new_stmts = Vec::with_capacity(refs.len() + b.stmts.len());

                for &info in &refs {
                    let pat = mk().set_mutbl(info.mutbl).ident_pat(info.name);
                    let local = mk().local(pat, Some(info.ty.clone()), Some(info.expr.clone()));
                    let stmt = mk().local_stmt(P(local));
                    new_stmts.push(stmt);
                }

                new_stmts.extend(b.stmts.into_iter());
                b.stmts = new_stmts;
                b
            }));

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
    reg.register("static_to_local", |_args| mk(StaticToLocal));
}
