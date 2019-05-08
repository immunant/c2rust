use std::collections::{HashMap, HashSet};
use rustc::hir::def_id::DefId;
use rustc::ty::TyKind;
use rustc_target::spec::abi::Abi;
use syntax::ast;
use syntax::ast::*;
use syntax::attr;
use syntax::mut_visit::{self, MutVisitor};
use syntax::ptr::P;
use smallvec::SmallVec;

use c2rust_ast_builder::{mk, IntoSymbol};
use crate::ast_manip::{FlatMapNodes, MutVisitNodes, fold_modules, visit_nodes, MutVisit};
use crate::command::{CommandState, Registry};
use crate::driver::{Phase, parse_expr};
use crate::matcher::{BindingType, MatchCtxt, Subst, mut_visit_match_with};
use crate::path_edit::{fold_resolved_paths, fold_resolved_paths_with_id};
use crate::transform::Transform;
use crate::util::Lone;
use crate::RefactorCtxt;


/// # `func_to_method` Command
/// 
/// Usage: `func_to_method`
/// 
/// Marks: `target`, `dest`
/// 
/// Turn functions marked `target` into static methods (no `self`) in the `impl`
/// block marked `dest`.
/// Turn functions that have an argument marked `target` into methods, replacing
/// the named argument with `self`.
/// Rewrite all uses of marked functions to call the new method versions.
/// 
/// Marked arguments of type `T`, `&T`, and `&mut T` (where `T` is the `Self` type
/// of the `dest` `impl`) will be converted to `self`, `&self`, and `&mut self`
/// respectively.
pub struct ToMethod;

impl Transform for ToMethod {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        // (1) Find the impl we're inserting into.

        let mut dest = None;

        FlatMapNodes::visit(krate, |i: P<Item>| {
            // We're looking for an inherent impl (no `TraitRef`) marked with a cursor.
            if !st.marked(i.id, "dest") ||
               !matches!([i.node] ItemKind::Impl(_, _, _, _, None, _, _)) {
                return smallvec![i];
            }

            if dest.is_none() {
                dest = Some(i.clone());
            }

            smallvec![i]
        });

        if dest.is_none() {
            return;
        }
        let dest = dest.unwrap();


        // (2) Collect all marked functions, removing them from the AST.  Note that we collect only
        // free functions, not trait or impl methods.

        struct FnInfo {
            item: P<Item>,

            decl: P<FnDecl>,
            header: FnHeader,
            generics: Generics,
            block: P<Block>,

            /// Index of the argument that will be replaced with `self`, or `None` if this function
            /// is being turned into a static method.
            arg_idx: Option<usize>,
        }
        let mut fns = Vec::new();

        fold_modules(krate, |curs| {
            while let Some(arg_idx) = curs.advance_until_match(|i| {
                // Find the argument under the cursor.
                let decl = match_or!([i.node] ItemKind::Fn(ref decl, ..) => decl; return None);
                for (idx, arg) in decl.inputs.iter().enumerate() {
                    if st.marked(arg.id, "target") {
                        return Some(Some(idx));
                    }
                }
                if st.marked(i.id, "target") {
                    return Some(None);
                }
                None
            }) {
                let i = curs.remove();
                unpack!([i.node.clone()]
                        ItemKind::Fn(decl, header, generics, block));
                fns.push(FnInfo {
                    item: i,
                    decl, header, generics, block,
                    arg_idx,
                });
            }
        });

        // Build a hash table with info needed to rewrite references to marked functions.
        struct FnRefInfo {
            ident: Ident,
            arg_idx: Option<usize>,
        }
        let fn_ref_info = fns.iter().map(|f| {
            (cx.node_def_id(f.item.id),
             FnRefInfo {
                 ident: f.item.ident.clone(),
                 arg_idx: f.arg_idx,
             })
        }).collect::<HashMap<_, _>>();


        // (3) Rewrite function signatures and bodies, replacing the marked arg with `self`.
        for f in &mut fns {
            // Functions that are being turned into static methods don't need any changes.
            let arg_idx = match_or!([f.arg_idx] Some(x) => x; continue);
            let mut inputs = f.decl.inputs.clone();

            // Remove the marked arg and inspect it.
            let arg = inputs.remove(arg_idx);

            let mode = match arg.pat.node {
                PatKind::Ident(mode, _, _) => mode,
                _ => panic!("unsupported argument pattern (expected ident): {:?}", arg.pat),
            };

            let pat_ty = cx.node_type(arg.pat.id);
            let self_ty = cx.def_type(cx.node_def_id(dest.id));
            let arg_hir_id = cx.hir_map().node_to_hir_id(arg.pat.id);

            // Build the new `self` argument and insert it.
            let self_kind = {
                if pat_ty == self_ty {
                    match mode {
                        BindingMode::ByValue(mutbl) => Some(SelfKind::Value(mutbl)),
                        BindingMode::ByRef(mutbl) => Some(SelfKind::Region(None, mutbl)),
                    }
                } else {
                    match pat_ty.sty {
                        TyKind::Ref(_, ty, _) if ty == self_ty => {
                            match arg.ty.node {
                                ast::TyKind::Rptr(ref lt, ref mty) =>
                                    Some(SelfKind::Region(lt.clone(), mty.mutbl)),
                                _ => None,
                            }
                        },
                        _ => None,
                    }
                }
            };
            let self_kind = match self_kind {
                Some(x) => x,
                None => panic!("unsupported argument type (expected {:?} or a ref): {:?}",
                               self_ty, pat_ty),
            };

            inputs.insert(0, mk().self_arg(self_kind));

            // Update `decl`
            f.decl = f.decl.clone().map(|fd| FnDecl { inputs: inputs, .. fd });

            // Rewrite references to the marked argument within the function body.

            // FIXME: rustc changed how locals args are represented, and we
            // don't have a Def for locals any more, and thus no def_id. We need
            // to fix this in path_edit.rs
            fold_resolved_paths(&mut f.block, cx, |qself, path, def| {
                match cx.def_to_hir_id(&def) {
                    Some(hir_id) =>
                        if hir_id == arg_hir_id {
                            assert!(qself.is_none());
                            return (None, mk().path(vec!["self"]));
                        } else {
                            (qself, path)
                        },
                    None => (qself, path)
                }
            });
        }


        // (4) Find the destination impl again, and fill it in with the new methods.

        let mut fns = Some(fns);

        FlatMapNodes::visit(krate, |i: P<Item>| {
            if i.id != dest.id || fns.is_none() {
                return smallvec![i];
            }

            smallvec![i.map(|i| {
                unpack!([i.node] ItemKind::Impl(
                        unsafety, polarity, generics, defaultness, trait_ref, ty, items));
                let mut items = items;
                let fns = fns.take().unwrap();
                items.extend(fns.into_iter().map(|f| {
                    let sig = MethodSig {
                        header: f.header,
                        decl: f.decl,
                    };
                    ImplItem {
                        id: DUMMY_NODE_ID,
                        ident: f.item.ident.clone(),
                        vis: f.item.vis.clone(),
                        defaultness: Defaultness::Final,
                        attrs: f.item.attrs.clone(),
                        generics: f.generics,
                        node: ImplItemKind::Method(sig, f.block),
                        span: f.item.span,
                        tokens: None,
                    }
                }));
                Item {
                    node: ItemKind::Impl(
                              unsafety, polarity, generics, defaultness, trait_ref, ty, items),
                    .. i
                }
            })]
        });


        // (5) Find all uses of marked functions, and rewrite them into method calls.

        MutVisitNodes::visit(krate, |e: &mut P<Expr>| {
            if !matches!([e.node] ExprKind::Call(..)) {
                return;
            }

            unpack!([e.node.clone()] ExprKind::Call(func, args));
            let def_id = match_or!([cx.try_resolve_expr(&func)] Some(x) => x; return);
            let info = match_or!([fn_ref_info.get(&def_id)] Some(x) => x; return);

            // At this point, we know `func` is a reference to a marked function, and we have the
            // function's `FnRefInfo`.

            if let Some(arg_idx) = info.arg_idx {
                // Move the `self` argument into the first position.
                let mut args = args;
                let self_arg = args.remove(arg_idx);
                args.insert(0, self_arg);

                e.node = ExprKind::MethodCall(
                    mk().path_segment(&info.ident),
                    args
                );
            } else {
                // There is no `self` argument, but change the function reference to the new path.
                let mut new_path = cx.def_path(cx.node_def_id(dest.id));
                new_path.segments.push(mk().path_segment(&info.ident));

                e.node = ExprKind::Call(mk().path_expr(new_path), args);
            }
        });
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


/// # `fix_unused_unsafe` Command
/// 
/// Usage: `fix_unused_unsafe`
/// 
/// Find unused `unsafe` blocks and turn them into ordinary blocks.
pub struct FixUnusedUnsafe;

impl Transform for FixUnusedUnsafe {
    fn transform(&self, krate: &mut Crate, _st: &CommandState, cx: &RefactorCtxt) {
        MutVisitNodes::visit(krate, |b: &mut P<Block>| {
            if let BlockCheckMode::Unsafe(UnsafeSource::UserProvided) = b.rules {
                let parent = cx.hir_map().get_parent_did(b.id);
                let result = cx.ty_ctxt().unsafety_check_result(parent);
                let unused = result.unsafe_blocks.iter().any(|&(id, used)| {
                    id == cx.hir_map().node_to_hir_id(b.id) && !used
                });
                if unused {
                    b.rules = BlockCheckMode::Default;
                }
            }
        });
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


/// # `sink_unsafe` Command
/// 
/// Usage: `sink_unsafe`
/// 
/// Marks: `target`
/// 
/// For functions marked `target`, convert `unsafe fn f() { ... }` into `fn () {
/// unsafe { ... } }`.  Useful once unsafe argument handling has been eliminated
/// from the function.
pub struct SinkUnsafe;

struct SinkUnsafeFolder<'a> {
    st: &'a CommandState,
}

impl<'a> MutVisitor for SinkUnsafeFolder<'a> {
    fn flat_map_item(&mut self, i: P<Item>) -> SmallVec<[P<Item>; 1]> {
        let i = if self.st.marked(i.id, "target") {
            i.map(|mut i| {
                match i.node {
                    ItemKind::Fn(_, ref mut header, _, ref mut block) => {
                        sink_unsafe(&mut header.unsafety, block);
                    },
                    _ => {},
                }
                i
            })
        } else {
            i
        };


        mut_visit::noop_flat_map_item(i, self)
    }

    fn flat_map_impl_item(&mut self, mut i: ImplItem) -> SmallVec<[ImplItem; 1]> {
        if self.st.marked(i.id, "target") {
            match i.node {
                ImplItemKind::Method(MethodSig { ref mut header, .. }, ref mut block) => {
                    sink_unsafe(&mut header.unsafety, block);
                },
                _ => {},
            }
        }

        mut_visit::noop_flat_map_impl_item(i, self)
    }
}

fn sink_unsafe(unsafety: &mut Unsafety, block: &mut P<Block>) {
    if *unsafety == Unsafety::Unsafe {
        *unsafety = Unsafety::Normal;
        *block = mk().block(vec![
            mk().expr_stmt(mk().block_expr(mk().unsafe_().block(
                        block.stmts.clone())))]);
    }
}

impl Transform for SinkUnsafe {
    fn transform(&self, krate: &mut Crate, st: &CommandState, _cx: &RefactorCtxt) {
        krate.visit(&mut SinkUnsafeFolder { st })
    }
}


/// # `wrap_extern` Command
/// 
/// Usage: `wrap_extern`
/// 
/// Marks: `target`, `dest`
/// 
/// For each foreign function marked `target`, generate a wrapper function in the
/// module marked `dest`, and rewrite all uses of the function to call the wrapper
/// instead.
/// 
/// 
/// Example:
/// 
///     extern "C" {
///         fn foo(x: i32) -> i32;
///     }
/// 
///     mod wrappers {
///         // empty
///     }
/// 
///     fn main() {
///         let x = unsafe { foo(123) };
///     }
/// 
/// After transformation, with `fn foo` marked `target` and `mod wrappers` marked
/// `dest`:
/// 
///     extern "C" {
///         fn foo(x: i32) -> i32;
///     }
/// 
///     mod wrappers {
///         unsafe fn foo(x: i32) -> i32 {
///             ::foo(x)
///         }
///     }
/// 
///     fn main() {
///         let x = unsafe { ::wrappers::foo(123) };
///     }
/// 
/// Note that this also replaces the function in expressions that take its address,
/// which may cause problem as the wrapper function has a different type that the
/// original (it lacks the `extern "C"` ABI qualifier).
pub struct WrapExtern;

impl Transform for WrapExtern {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        // (1) Collect the marked externs.
        #[derive(Debug)]
        struct FuncInfo {
            id: NodeId,
            def_id: DefId,
            ident: Ident,
            decl: P<FnDecl>,
        }
        let mut fns = Vec::new();

        visit_nodes(krate, |fi: &ForeignItem| {
            if !st.marked(fi.id, "target") {
                return;
            }

            match fi.node {
                ForeignItemKind::Fn(ref decl, _) => {
                    fns.push(FuncInfo {
                        id: fi.id,
                        def_id: cx.node_def_id(fi.id),
                        ident: fi.ident.clone(),
                        decl: decl.clone(),
                    });
                },

                _ => {},
            }
        });

        info!("found {} fns", fns.len());
        for i in &fns {
            info!("  {:?}", i);
        }

        // (2) Generate wrappers in the destination module.
        let mut dest_path = None;
        FlatMapNodes::visit(krate, |i: P<Item>| {
            if !st.marked(i.id, "dest") {
                return smallvec![i];
            }

            if dest_path.is_some() {
                info!("warning: found multiple \"dest\" marks");
                return smallvec![i];
            }
            dest_path = Some(cx.def_path(cx.node_def_id(i.id)));

            smallvec![i.map(|i| {
                unpack!([i.node] ItemKind::Mod(m));
                let mut m = m;

                for f in &fns {
                    let func_path = cx.def_path(cx.node_def_id(f.id));
                    let arg_names = f.decl.inputs.iter().enumerate().map(|(idx, arg)| {
                        // TODO: match_arg("__i: __t", arg).ident("__i")
                        match arg.pat.node {
                            PatKind::Ident(BindingMode::ByValue(Mutability::Immutable),
                                           ident,
                                           None) => {
                                ident
                            },
                            _ => {
                                mk().ident(format!("arg{}", idx))
                            },
                        }
                    }).collect::<Vec<_>>();
                    let wrapper_args = f.decl.inputs.iter()
                        .zip(arg_names.iter())
                        .map(|(old, name)| {
                            Arg {
                                pat: mk().ident_pat(name.clone()),
                                ..old.clone()
                            }
                        }).collect::<Vec<_>>();
                    let arg_exprs = arg_names.iter().map(|name| {
                        mk().ident_expr(name)
                    }).collect::<Vec<_>>();
                    let decl = P(FnDecl {
                        inputs: wrapper_args,
                        output: f.decl.output.clone(),
                        c_variadic: false,
                    });
                    let body = mk().block(vec![
                            mk().expr_stmt(mk().call_expr(
                                    mk().path_expr(func_path),
                                    arg_exprs))]);
                    m.items.push(mk().pub_().unsafe_().fn_item(&f.ident, decl, body));

                }

                Item {
                    node: ItemKind::Mod(m),
                    .. i
                }
            })]
        });

        if dest_path.is_none() {
            info!("warning: found no \"dest\" mark");
            return;
        }
        let dest_path = dest_path.unwrap();

        // (3) Rewrite call sites to use the new wrappers.
        let ident_map = fns.iter().map(|f| (f.def_id, f.ident)).collect::<HashMap<_, _>>();
        fold_resolved_paths(krate, cx, |qself, path, def| {
            match def.opt_def_id() {
                Some(def_id) if ident_map.contains_key(&def_id) => {
                    let ident = ident_map.get(&def_id).unwrap();
                    let mut new_path = dest_path.clone();
                    new_path.segments.push(mk().path_segment(ident));
                    (qself, new_path)
                },
                _ => (qself, path),
            }
        });
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


/// # `wrap_api` Command
/// 
/// Usage: `wrap_api`
/// 
/// Marks: `target`
/// 
/// For each function `foo` marked `target`:
/// 
///  1. Reset the function's ABI to `"Rust"` (the default)
///  2. Remove any `#[no_mangle]` or `#[export_name]` attributes
///  3. Generate a new wrapper function called `foo_wrapper` with `foo`'s old ABI
///     and an `#[export_name="foo"]` attribute.
/// 
/// Calls to `foo` are left unchanged.  The result is that callers from C use the
/// wrapper function, while internal calls use `foo` directly, and the signature of
/// `foo` can be changed freely without affecting external callers.
pub struct WrapApi;

impl Transform for WrapApi {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        // Map from original function HirId to new function name
        let mut wrapper_map = HashMap::new();

        // Add wrapper functions
        FlatMapNodes::visit(krate, |i: P<Item>| {
            if !st.marked(i.id, "target") {
                return smallvec![i];
            }

            if !matches!([i.node] ItemKind::Fn(..)) {
                return smallvec![i];
            }

            let (decl, old_abi) = expect!([i.node]
                ItemKind::Fn(ref decl, ref header, _, _) => (decl.clone(), header.abi));

            // Get the exported symbol name of the function
            let symbol =
                if let Some(sym) = attr::first_attr_value_str_by_name(&i.attrs, "export_name") {
                    sym
                } else if attr::contains_name(&i.attrs, "no_mangle") {
                    i.ident.name
                } else {
                    warn!("marked function `{:?}` does not have a stable symbol", i.ident.name);
                    return smallvec![i];
                };

            // Remove export-related attrs from the original function, and set it to Abi::Rust.
            let i = i.map(|mut i| {
                i.attrs.retain(|attr| {
                    attr.path != "no_mangle" &&
                    attr.path != "export_name"
                });

                match i.node {
                    ItemKind::Fn(_, ref mut header, _, _) => header.abi = Abi::Rust,
                    _ => unreachable!(),
                }

                i
            });

            // Pick distinct names for the arguments in the wrapper.
            let mut used_names = HashSet::new();

            let arg_names = decl.inputs.iter().enumerate().map(|(idx, arg)| {
                let base = match arg.pat.node {
                    // Use the name from the original function, if there is one.  Otherwise, fall
                    // back on `arg0`, `arg1`, ...
                    PatKind::Ident(_, ref ident, _) => ident.name,
                    _ => format!("arg{}", idx).into_symbol(),
                };

                let name;
                if !used_names.contains(&base) {
                    name = base;
                } else {
                    let mut i = 0;
                    loop {
                        let gen_name = format!("{}_{}", base.as_str(), i).into_symbol();
                        if !used_names.contains(&gen_name) {
                            name = gen_name;
                            break;
                        }
                        i += 1;
                    }
                }

                used_names.insert(name);
                name
            }).collect::<Vec<_>>();

            // Generate the wrapper.  It gets an `#[export_name]`  attr and the original function's
            // old ABI.
            let wrapper_decl = decl.clone().map(|decl| {
                let new_inputs = decl.inputs.iter().zip(arg_names.iter()).map(|(arg, &name)| {
                    mk().arg(&arg.ty, mk().ident_pat(name))
                }).collect();
                FnDecl {
                    inputs: new_inputs,
                    .. decl
                }
            });

            let wrapper_args = arg_names.iter().map(|&name| mk().ident_expr(name)).collect();

            let wrapper_name = format!("{}_wrapper", symbol.as_str());
            let wrapper =
                mk().vis(i.vis.clone()).unsafe_().abi(old_abi)
                        .str_attr("export_name", symbol).fn_item(
                    &wrapper_name,
                    wrapper_decl,
                    mk().block(vec![
                        mk().expr_stmt(mk().call_expr(
                                mk().path_expr(vec![i.ident.name]),
                                wrapper_args,
                        ))
                    ])
                );


            let item_hir_id = cx.hir_map().node_to_hir_id(i.id);
            wrapper_map.insert(item_hir_id, wrapper_name);

            let mut v = smallvec![];
            v.push(i);
            v.push(wrapper);
            v
        });

        // Now ne need to find places where the old function was used as a function pointer.  We do
        // this by looking for uses outside a call expr's callee position.  If we find any of
        // these, we edit them to refer to the wrapper, which has the same type (specifically, the
        // same ABI) as the old function.
        let mut callees = HashSet::new();
        visit_nodes(krate, |e: &Expr| {
            if let ExprKind::Call(ref callee, _) = e.node {
                callees.insert(callee.id);
            }
        });

        fold_resolved_paths_with_id(krate, cx, |id, q, p, d| {
            if callees.contains(&id) || q.is_some() {
                return (q, p);
            }
            let hir_id = match_or!([cx.def_to_hir_id(d)] Some(x) => x; return (q, p));
            let name = match_or!([wrapper_map.get(&hir_id)] Some(x) => x; return (q, p));

            let mut new_path = p.clone();
            new_path.segments.pop();
            new_path.segments.push(mk().path_segment(name));
            (q, new_path)
        });
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


/// # `abstract` Command
/// 
/// Usage: `abstract SIG PAT [BODY]`
/// 
/// Replace all instances of `pat` with calls to a new function whose name and signature is given
/// by `sig`.  Example:
/// 
/// Input:
/// 
///     1 + 2
/// 
/// After running `abstract 'add(x: u32, y: u32) -> u32' 'x + y'`:
/// 
///     add(1, 2)
/// 
///     // Elsewhere:
///     fn add(x: u32, y: u32) -> u32 { x + y }
/// 
/// All type and value parameter names in `sig` act as bindings when matching `pat`.  The captured
/// exprs and types are passed as parameters when building the new call expression.  The body of
/// the function is `body`, if provided, otherwise `pat` itself.
/// 
/// Non-ident patterns in `sig` are not supported.  It is also an error for any type parameter's
/// name to collide with any value parameter.
/// 
/// If matching with `pat` fails to capture expressions for any of the value parameters of `sig`,
/// it is an error.  If it fails to capture for a type parameter, the parameter is filled in with
/// `_` (infer).
struct Abstract {
    sig: String,
    pat: String,
    body: Option<String>,
}

impl Transform for Abstract {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        let pat = parse_expr(cx.session(), &self.pat);

        let func_src = format!("unsafe fn {} {{\n    {}\n}}",
                               self.sig, self.body.as_ref().unwrap_or(&self.pat));
        let func: P<Item> = st.parse_items(cx, &func_src).lone();
        st.add_mark(func.id, "new");

        // Build the call expression template

        let mut value_args = Vec::new();
        let mut type_args = Vec::new();
        {
            let (decl, generics) = expect!([func.node]
                    ItemKind::Fn(ref decl, _, ref gen, _) => (decl, gen));
            for arg in &decl.inputs {
                let name = expect!([arg.pat.node] PatKind::Ident(_, ident, _) => ident);
                value_args.push(name);
            }
            for param in &generics.params {
                if let GenericParamKind::Type { .. } = param.kind {
                    type_args.push(param.ident);
                }
            }
        }

        let aba = mk().angle_bracketed_args(
            type_args.iter().map(|name| mk().ident_ty(name)).collect());
        let seg = mk().path_segment_with_args(func.ident, aba);
        let call_expr = mk().call_expr(
            mk().path_expr(vec![seg]),
            value_args.iter().map(|name| mk().ident_expr(name)).collect());

        // Search and replace

        let mut init_mcx = MatchCtxt::new(st, cx);
        for name in &value_args {
            init_mcx.set_type(name.name, BindingType::Expr);
        }
        for name in &type_args {
            init_mcx.set_type(name.name, BindingType::Ty);
        }

        mut_visit_match_with(init_mcx, pat, krate, |ast, mut mcx| {
            for name in &type_args {
                if mcx.bindings.get::<_, P<Ty>>(name.name).is_none() {
                    mcx.bindings.add(name.name, mk().infer_ty());
                }
            }
            *ast = call_expr.clone().subst(st, cx, &mcx.bindings);
        });

        // Add the function definition to the crate

        krate.module.items.push(func);
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("func_to_method", |_args| mk(ToMethod));
    reg.register("fix_unused_unsafe", |_args| mk(FixUnusedUnsafe));
    reg.register("sink_unsafe", |_args| mk(SinkUnsafe));
    reg.register("wrap_extern", |_args| mk(WrapExtern));
    reg.register("wrap_api", |_args| mk(WrapApi));
    reg.register("abstract", |args| mk(Abstract {
        sig: args[0].clone(),
        pat: args[1].clone(),
        body: args.get(2).cloned(),
    }));
}
