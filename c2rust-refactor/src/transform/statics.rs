use log::info;
use std::collections::{HashMap, HashSet};
use std::mem;
use rustc_hir::def_id::DefId;
use rustc_ast::*;
use rustc_ast::ptr::P;
use rustc_span::symbol::{Ident, Symbol};
use smallvec::smallvec;

use crate::ast_manip::{FlatMapNodes, MutVisitNodes, fold_modules};
use crate::ast_manip::fn_edit::mut_visit_fns;
use crate::command::{CommandState, Registry};
use crate::driver::{parse_expr};
use crate::match_or;
use crate::matcher::{Bindings, BindingType, MatchCtxt, Subst, mut_visit_match_with};
use crate::path_edit::fold_resolved_paths;
use crate::transform::Transform;
use crate::ast_builder::{mk, IntoSymbol};
use crate::util::dataflow;
use crate::RefactorCtxt;


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
/// ```ignore
///     static mut FOO: i32 = 100;
///     static mut BAR: bool = true;
///
///     unsafe fn f() -> i32 {
///         FOO
///     }
/// ```
///
///
/// After running `static_collect_to_struct Globals G`, with both statics marked:
///
/// ```ignore
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
/// ```
pub struct CollectToStruct {
    pub struct_name: String,
    pub instance_name: String,
}

impl Transform for CollectToStruct {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        // Map from Symbol (the name) to the DefId of the old `static`.
        let mut old_statics = HashMap::new();

        fold_modules(krate, |curs| {
            let mut matches = Vec::new();
            let mut insert_point = None;

            while let Some((ident, ty, init)) = curs.advance_until_match(
                    |i| match_or!([i.kind] ItemKind::Static(ref ty, _, ref init) =>
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
                bnd.add("__x", ident);
                bnd.add("__t", ty);
                bnd.add("__init", init);
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
        init_mcx.bindings.add(
            "__s", Ident::with_dummy_span((&self.instance_name as &str).into_symbol()));

        mut_visit_match_with(init_mcx, ident_pat, krate, |orig, mcx| {
            let static_id = match old_statics.get(&mcx.bindings.get::<_, Ident>("__x").unwrap().name) {
                Some(&x) => x,
                None => return,
            };

            if cx.resolve_expr(&orig) != static_id {
                return;
            }

            // This really is a reference to one of the collected statics.  Replace it with a
            // reference to the generated struct.
            *orig = ident_repl.clone().subst(st, cx, &mcx.bindings)
        });
    }
}

fn build_collected_struct(name: &str, matches: &[Bindings]) -> P<Item> {
    let fields = matches.iter().map(
        |bnd| mk().field_def(bnd.get::<_, Ident>("__x").unwrap(), bnd.get::<_, P<Ty>>("__t").unwrap())).collect::<Vec<_>>();
    mk().struct_item(name, fields, false)
}

fn build_struct_instance(struct_name: &str,
                         instance_name: &str,
                         matches: &[Bindings]) -> P<Item> {
    let fields = matches.iter().map(
        |bnd| mk().field(bnd.get::<_, Ident>("__x").unwrap(), bnd.get::<_, P<Expr>>("__init").unwrap())).collect::<Vec<_>>();
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
/// ```ignore
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
/// ```
///
/// After running `static_to_local_ref`:
///
/// ```ignore
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
/// ```
pub struct Localize;

impl Transform for Localize {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        // (1) Collect all marked statics.

        struct StaticInfo {
            name: Ident,
            arg_name: Symbol,
            ty: P<Ty>,
            mutbl: Mutability,
        }
        let mut statics = HashMap::new();

        FlatMapNodes::visit(krate, |i: P<Item>| {
            if !st.marked(i.id, "target") {
                return smallvec![i];
            }

            match i.kind {
                ItemKind::Static(ref ty, mutbl, _) => {
                    let def_id = cx.node_def_id(i.id);
                    let arg_name_str = format!("{}_", i.ident.name.as_str());
                    let arg_name = (&arg_name_str as &str).into_symbol();
                    statics.insert(def_id, StaticInfo {
                        name: i.ident,
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
        mut_visit_fns(krate, |fl| {
            if !st.marked(fl.id, "user") {
                return;
            }

            let fn_def_id = cx.node_def_id(fl.id);

            let mut refs = HashSet::new();
            fold_resolved_paths(&mut fl.body, cx, |qself, path, def| {
                if let Some(def_id) = def[0].opt_def_id() {
                    refs.insert(def_id);
                }
                (qself, path)
            });
            fn_refs.insert(fn_def_id, refs);
        });

        // Sort the references, collecting those that point to other marked functions and those
        // that point to statics.
        struct FnInfo {
            fn_refs: HashSet<DefId>,
            static_refs: HashSet<DefId>,
        }

        let fn_ids = fn_refs.keys().copied().collect::<HashSet<_>>();
        let mut fns = fn_refs.into_iter().map(|(k, v)| {
            let fn_refs = v.iter().filter(|id| fn_ids.contains(id))
                .copied().collect();
            let static_refs = v.iter().filter(|id| statics.contains_key(id))
                .copied().collect();
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

        mut_visit_fns(krate, |fl| {
            let fn_def_id = cx.node_def_id(fl.id);
            if let Some(static_ids) = fn_statics.get(&fn_def_id) {

                // Add new argument to function signature.
                for &static_id in static_ids {
                    let info = &statics[&static_id];
                    fl.decl.inputs.push(mk().arg(
                        mk().set_mutbl(info.mutbl).ref_ty(&info.ty),
                        mk().ident_pat(info.arg_name)));
                }

                // Update uses of statics.
                MutVisitNodes::visit(&mut fl.body, |e: &mut P<Expr>| {
                    if let Some(def_id) = cx.try_resolve_expr(&e) {
                        if let Some(info) = statics.get(&def_id) {
                            *e = mk().unary_expr("*", mk().ident_expr(info.arg_name));
                            return;
                        }
                    }
                });

                // Update calls to other marked functions.
                MutVisitNodes::visit(&mut fl.body, |e: &mut P<Expr>| {
                    if let ExprKind::Call(func, args) = &mut e.kind {
                        if let Some(func_id) = cx.try_resolve_expr(&func) {
                            if let Some(func_static_ids) = fn_statics.get(&func_id) {
                                for &static_id in func_static_ids {
                                    args.push(mk().ident_expr(statics[&static_id].arg_name));
                                }
                            }
                        }
                    }
                });

            } else {
                // Update calls only.
                MutVisitNodes::visit(&mut fl.body, |e: &mut P<Expr>| {
                    if let ExprKind::Call(func, args) = &mut e.kind {
                        if let Some(func_id) = cx.try_resolve_expr(&func) {
                            if let Some(func_static_ids) = fn_statics.get(&func_id) {
                                for &static_id in func_static_ids {
                                    let info = &statics[&static_id];
                                    args.push(mk().set_mutbl(info.mutbl).addr_of_expr(
                                            mk().ident_expr(info.name)));
                                }
                            }
                        }
                    }
                });
            }
        });
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
/// ```ignore
///     static mut FOO: i32 = 100;  // FOO: target
///
///     unsafe fn f() -> i32 {
///         FOO
///     }
///
///     unsafe fn g() -> i32 {
///         FOO + 1
///     }
/// ```
///
/// After running `static_to_local`:
///
/// ```ignore
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
/// ```
struct StaticToLocal;

impl Transform for StaticToLocal {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        // (1) Collect all marked statics.

        struct StaticInfo {
            name: Ident,
            ty: P<Ty>,
            mutbl: Mutability,
            expr: Option<P<Expr>>,
        }
        let mut statics = HashMap::new();

        FlatMapNodes::visit(krate, |i: P<Item>| {
            if !st.marked(i.id, "target") {
                return smallvec![i];
            }

            match i.kind {
                ItemKind::Static(ref ty, mutbl, ref expr) => {
                    let def_id = cx.node_def_id(i.id);
                    statics.insert(def_id, StaticInfo {
                        name: i.ident,
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

        mut_visit_fns(krate, |fl| {
            // Figure out which statics (if any) this function uses.
            let mut ref_ids = HashSet::new();
            let mut refs = Vec::new();
            fold_resolved_paths(&mut fl.body, cx, |qself, path, def| {
                if let Some(def_id) = def[0].opt_def_id() {
                    if ref_ids.insert(def_id) {
                        if let Some(info) = statics.get(&def_id) {
                            refs.push(info);
                        }
                    }
                }
                (qself, path)
            });

            if refs.is_empty() {
                return;
            }

            refs.sort_by_key(|info| info.name.name);

            if let Some(block) = &mut fl.body {
                let new_stmts = Vec::with_capacity(refs.len() + block.stmts.len());
                let old_stmts = mem::replace(&mut block.stmts, new_stmts);

                for &info in &refs {
                    let pat = mk().set_mutbl(info.mutbl).ident_pat(info.name);
                    let local = mk().local(pat, Some(info.ty.clone()), info.expr.clone());
                    let stmt = mk().local_stmt(P(local));
                    block.stmts.push(stmt);
                }

                block.stmts.extend(old_stmts.into_iter());
            }
        });
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
