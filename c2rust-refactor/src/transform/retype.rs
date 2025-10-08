use log::{debug, info, trace};
use std::collections::{HashMap, HashSet};
use std::ops::DerefMut;
use rustc_hir as hir;
use rustc_hir::def_id::{DefId, LocalDefId};
use rustc_middle::ty::{self, TyKind, TyCtxt, ParamEnv};
use rustc_ast::*;
use rustc_ast::mut_visit::{self, MutVisitor};
use rustc_errors::PResult;
use rustc_parse::parser::Parser;
use rustc_ast::token::{TokenKind, BinOpToken};
use rustc_ast_pretty::pprust;
use rustc_ast::ptr::P;
use rustc_span::Span;
use smallvec::{smallvec, SmallVec};

use crate::ast_builder::{mk, IntoSymbol};
use crate::ast_manip::{FlatMapNodes, MutVisit, MutVisitNodes, fold_output_exprs};
use crate::ast_manip::fn_edit::{mut_visit_fns, visit_fns};
use crate::ast_manip::lr_expr::{self, fold_expr_with_context, fold_exprs_with_context};
use crate::command::{Command, CommandState, RefactorState, Registry, TypeckLoopResult};
use crate::driver::{self, Phase, parse_ty, parse_expr};
use crate::{expect, match_or};
use crate::illtyped::{IlltypedFolder, fold_illtyped};
use crate::matcher::{Bindings, MatchCtxt, Subst, mut_visit_match};
use crate::reflect::{self, reflect_tcx_ty};
use crate::transform::Transform;
use crate::RefactorCtxt;

/// # `retype_argument` Command
///
/// Usage: `retype_argument NEW_TY WRAP UNWRAP`
///
/// Marks: `target`
///
/// For each argument marked `target`, change the type of the argument to `NEW_TY`,
/// and use `WRAP` and `UNWRAP` to convert values to and from the original type of
/// the argument at call sites and within the function body.
///
/// `WRAP` should contain an expression placeholder `__old`, and should convert
/// `__old` from the argument's original type to `NEW_TY`.
/// `UNWRAP` should contain an expression placeholder `__new`, and should perform
/// the opposite conversion.
pub struct RetypeArgument {
    pub new_ty: String,
    pub wrap: String,
    pub unwrap: String,
}

impl Transform for RetypeArgument {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        // (1) Change argument types and rewrite function bodies.

        let new_ty = parse_ty(cx.session(), &self.new_ty);
        let wrap = parse_expr(cx.session(), &self.wrap);
        let unwrap = parse_expr(cx.session(), &self.unwrap);

        // Modified functions, by DefId.  For each one, we track the argument indices that were
        // modified.
        let mut mod_fns: HashMap<DefId, HashSet<usize>> = HashMap::new();

        mut_visit_fns(krate, |fl| {
            let fn_id = fl.id;

            // Def IDs of changed arguments.
            let mut changed_args = HashSet::new();
            for (i, arg) in fl.decl.inputs.iter_mut().enumerate() {
                if st.marked(arg.id, "target") {
                    arg.ty = new_ty.clone();
                    mod_fns.entry(cx.node_def_id(fn_id)).or_insert_with(HashSet::new).insert(i);

                    changed_args.insert(cx.hir_map().node_to_hir_id(arg.pat.id));
                }
            }

            if changed_args.is_empty() {
                return;
            }

            // An argument was changed, so we need to rewrite uses of that argument inside the
            // function body.

            // `mut_visit_nodes` does a preorder traversal, so if we replace `x` with `wrap(x)`, we will
            // see `x` again in the recursive call.  We keep track of which nodes have already been
            // rewritten so that we don't end up with a stack overflow.
            let mut rewritten_nodes = HashSet::new();
            fl.body.as_mut().map(|b| MutVisitNodes::visit(b, |e: &mut P<Expr>| {
                if let Some(hir_id) = cx.try_resolve_expr_to_hid(&e) {
                    if changed_args.contains(&hir_id) && !rewritten_nodes.contains(&e.id) {
                        rewritten_nodes.insert(e.id);
                        let mut bnd = Bindings::new();
                        bnd.add("__new", e.clone());
                        *e = unwrap.clone().subst(st, cx, &bnd);
                    }
                }
            }));
        });

        // (2) Rewrite callsites of modified functions.

        // We don't need any protection against infinite recursion here, because it doesn't make
        // sense for `wrap` to call the function whose args we're changing.
        MutVisitNodes::visit(krate, |e: &mut P<Expr>| {
            let callee = match_or!([cx.opt_callee(&e)] Some(x) => x; return);
            let mod_args = match_or!([mod_fns.get(&callee)] Some(x) => x; return);
            let args: &mut [P<Expr>] = match e.kind {
                ExprKind::Call(_, ref mut args) => args,
                ExprKind::MethodCall(_, ref mut args, _) => args,
                _ => panic!("expected Call or MethodCall"),
            };
            for &idx in mod_args {
                let mut bnd = Bindings::new();
                bnd.add("__old", args[idx].clone());
                args[idx] = wrap.clone().subst(st, cx, &bnd);
            }
        });
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


/// # `retype_return` Command
///
/// Usage: `retype_return NEW_TY WRAP UNWRAP`
///
/// Marks: `target`
///
/// For each function marked `target`, change the return type of the function to
/// `NEW_TY`, and use `WRAP` and `UNWRAP` to convert values to and from the
/// original type of the argument at call sites and within the function body.
///
/// `WRAP` should contain an expression placeholder `__old`, and should convert
/// `__old` from the function's original return type to `NEW_TY`.
/// `UNWRAP` should contain an expression placeholder `__new`, and should perform
/// the opposite conversion.
pub struct RetypeReturn {
    pub new_ty: String,
    pub wrap: String,
    pub unwrap: String,
}

impl Transform for RetypeReturn {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        // (1) Change argument types and rewrite function bodies.

        let new_ty = parse_ty(cx.session(), &self.new_ty);
        let wrap = parse_expr(cx.session(), &self.wrap);
        let unwrap = parse_expr(cx.session(), &self.unwrap);

        // Modified functions, by DefId.
        let mut mod_fns: HashSet<DefId> = HashSet::new();

        mut_visit_fns(krate, |fl| {
            if !st.marked(fl.id, "target") {
                return;
            }

            // Change the return type annotation
            fl.decl.output = FnRetTy::Ty(new_ty.clone());

            // Rewrite output expressions using `wrap`.
            fl.body.as_mut().map(|b| fold_output_exprs(b, true, |e| {
                let mut bnd = Bindings::new();
                bnd.add("__old", e.clone());
                *e = wrap.clone().subst(st, cx, &bnd);
            }));

            mod_fns.insert(cx.node_def_id(fl.id));
        });

        // (2) Rewrite callsites of modified functions.

        // We don't need any protection against infinite recursion here, because it doesn't make
        // sense for `unwrap` to call the function whose args we're changing.
        MutVisitNodes::visit(krate, |e: &mut P<Expr>| {
            let callee = match_or!([cx.opt_callee(&e)] Some(x) => x; return);
            if !mod_fns.contains(&callee) {
                return;
            }
            let mut bnd = Bindings::new();
            bnd.add("__new", e.clone());
            *e = unwrap.clone().subst(st, cx, &bnd)
        });
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


/// # `retype_static` Command
///
/// Usage: `retype_static NEW_TY REV_CONV_ASSIGN CONV_RVAL CONV_LVAL [CONV_LVAL_MUT]`
///
/// Marks: `target`
///
/// For each static marked `target`, change the type of the static to `NEW_TY`,
/// using the remaining arguments (which are all all expression templates) to
/// convert between the old and new types at the definition and use sites.
///
/// The expression arguments are used as follows:
///
///  * `REV_CONV_ASSIGN`: In direct assignments to the static and in its
///    initializer expression, the original assigned value is wrapped (as `__old`)
///    in `REV_CONV_ASSIGN` to produce a value of type `NEW_TY`.
///  * `CONV_RVAL`: In rvalue contexts, the static is wrapped (as `__new`) in
///    `CONV_RVAL` to produce a value of the static's old type.
///  * `CONV_LVAL` and `CONV_LVAL_MUT` are similar to `CONV_RVAL`, but for
///    immutable and mutable lvalue contexts respectively.  Especially for
///    `CONV_LVAL_MUT`, the result of wrapping should be an lvalue expression (such
///    as a dereference or field access), not a temporary, as otherwise updates to
///    the static could be lost.  `CONV_LVAL_MUT` is not required for immutable
///    statics, which cannot appear in mutable lvalue contexts.
pub struct RetypeStatic {
    pub new_ty: String,
    pub rev_conv_assign: String,
    pub conv_rval: String,
    pub conv_lval: String,
    pub conv_lval_mut: Option<String>,
}

impl Transform for RetypeStatic {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        // (1) Change the types of marked statics, and update their initializer expressions.

        let new_ty = parse_ty(cx.session(), &self.new_ty);
        let rev_conv_assign = st.parse_expr(cx, &self.rev_conv_assign);
        let conv_rval = st.parse_expr(cx, &self.conv_rval);
        let conv_lval = st.parse_expr(cx, &self.conv_lval);
        let conv_lval_mut = self.conv_lval_mut.as_ref().map(|src| st.parse_expr(cx, src));

        // Modified statics, by DefId.
        let mut mod_statics: HashSet<DefId> = HashSet::new();

        FlatMapNodes::visit(krate, |i: P<Item>| {
            if !st.marked(i.id, "target") {
                return smallvec![i];
            }

            smallvec![i.map(|mut i| {
                match i.kind {
                    ItemKind::Static(ref mut ty, _, ref mut init) => {
                        *ty = new_ty.clone();
                        if let Some(init) = init {
                            let mut bnd = Bindings::new();
                            bnd.add("__old", init.clone());
                            *init = rev_conv_assign.clone().subst(st, cx, &bnd);
                        }
                        mod_statics.insert(cx.node_def_id(i.id));
                    },
                    _ => {},
                }
                i
            })]
        });

        FlatMapNodes::visit(krate, |mut fi: P<ForeignItem>| {
            if !st.marked(fi.id, "target") {
                return smallvec![fi];
            }

            match fi.kind {
                ForeignItemKind::Static(ref mut ty, _, _) => {
                    *ty = new_ty.clone();
                    mod_statics.insert(cx.node_def_id(fi.id));
                },
                _ => {},
            }
            smallvec![fi]
        });

        // (2) Handle assignments into modified statics.  This is its own step because it's hard to
        // do inside `fold_exprs_with_context`.

        // Track IDs of exprs that were handled by this step, so the next step doesn't try to do
        // its own thing with them.  Note we assume the input AST is properly numbered.
        let mut handled_ids: HashSet<NodeId> = HashSet::new();

        MutVisitNodes::visit(krate, |e: &mut P<Expr>| {
            if !crate::matches!([e.kind] ExprKind::Assign(..), ExprKind::AssignOp(..)) {
                return;
            }

            match e.kind {
                ExprKind::Assign(ref lhs, ref mut rhs, _) |
                ExprKind::AssignOp(_, ref lhs, ref mut rhs) => {
                    if cx.try_resolve_expr(lhs)
                        .map_or(false, |did| mod_statics.contains(&did)) {
                            let mut bnd = Bindings::new();
                            bnd.add("__old", rhs.clone());
                            *rhs = rev_conv_assign.clone().subst(st, cx, &bnd);
                            handled_ids.insert(lhs.id);
                        }
                },
                _ => {},
            }
        });

        // (3) Rewrite use sites of modified statics.

        fold_exprs_with_context(krate, |e, ectx| {
            if !crate::matches!([e.kind] ExprKind::Path(..)) ||
               handled_ids.contains(&e.id) ||
               !cx.try_resolve_expr(&e).map_or(false, |did| mod_statics.contains(&did)) {
                return;
            }

            let mut bnd = Bindings::new();
            bnd.add("__new", e.clone());
            *e = match ectx {
                lr_expr::Context::Rvalue => conv_rval.clone().subst(st, cx, &bnd),
                lr_expr::Context::Lvalue => conv_lval.clone().subst(st, cx, &bnd),
                lr_expr::Context::LvalueMut => {
                    conv_lval_mut.clone().unwrap_or_else(
                        || panic!("need conv_lval_mut to handle LvalueMut expression `{}`",
                                  pprust::expr_to_string(&e)))
                        .subst(st, cx, &bnd)
                }
            };
        });
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
pub fn bitcast_retype<F>(st: &CommandState, cx: &RefactorCtxt, krate: &mut Crate, retype: F)
    where F: FnMut(&mut P<Ty>) -> bool {
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

    impl<F> MutVisitor for ChangeTypeFolder<F>
            where F: FnMut(&mut P<Ty>) -> bool {
        fn flat_map_item(&mut self, i: P<Item>) -> SmallVec<[P<Item>; 1]> {
            let i = if crate::matches!([i.kind] ItemKind::Fn(..)) {
                i.map(|mut i| {
                    let mut fd = expect!([i.kind]
                                         ItemKind::Fn(box Fn { ref sig, .. }) =>
                                         sig.decl.clone().into_inner());

                    for (j, arg) in fd.inputs.iter_mut().enumerate() {
                        let old_ty = arg.ty.clone();
                        if (self.retype)(&mut arg.ty) {
                            self.changed_inputs.insert((i.id, j),
                                                       (old_ty.clone(), arg.ty.clone()));
                            self.changed_funcs.insert(i.id);

                            // Also record that the type of the variable declared here has changed.
                            if crate::matches!([arg.pat.kind] PatKind::Ident(..)) {
                                // Note that `PatKind::Ident` doesn't guarantee that this is a
                                // variable binding.  But if it's not, then no name will ever
                                // resolve to `arg.pat`'s DefId, so it doesn't matter.
                                self.changed_defs.insert(arg.pat.id, (old_ty, arg.ty.clone()));
                            } else {
                                // TODO: Would be nice to warn the user (or skip rewriting) if a
                                // nontrivial pattern gets its type changed, as we'll likely miss
                                // adding some required `transmute`s.
                            }
                        }
                    }

                    if let FnRetTy::Ty(ref mut ty) = fd.output {
                        let old_ty = ty.clone();
                        if (self.retype)(ty) {
                            self.changed_outputs.insert(i.id, (old_ty, ty.clone()));
                            self.changed_funcs.insert(i.id);
                        }
                    }

                    match i.kind {
                        ItemKind::Fn(box Fn { ref mut sig, .. }) => {
                            *sig.decl = fd;
                        },
                        _ => panic!("expected ItemKind::Fn"),
                    }

                    i
                })

            } else if crate::matches!([i.kind] ItemKind::Static(..)) {
                i.map(|mut i| {
                    {
                        let ty = expect!([i.kind] ItemKind::Static(ref mut ty, _, _) => ty);
                        let old_ty = ty.clone();
                        if (self.retype)(ty) {
                            self.changed_defs.insert(i.id, (old_ty, ty.clone()));
                        }
                    }
                    i
                })

            } else if crate::matches!([i.kind] ItemKind::Const(..)) {
                i.map(|mut i| {
                    {
                        let ty = expect!([i.kind] ItemKind::Const(_, ref mut ty, _) => ty);
                        let old_ty = ty.clone();
                        if (self.retype)(ty) {
                            self.changed_defs.insert(i.id, (old_ty, ty.clone()));
                        }
                    }
                    i
                })

            } else {
                i
            };

            mut_visit::noop_flat_map_item(i, self)
        }

        fn flat_map_field_def(&mut self, mut fd: FieldDef) -> SmallVec<[FieldDef; 1]> {
            let old_ty = fd.ty.clone();
            if (self.retype)(&mut fd.ty) {
                self.changed_defs.insert(fd.id, (old_ty, fd.ty.clone()));
            }
            mut_visit::noop_flat_map_field_def(fd, self)
        }
    }

    let mut f = ChangeTypeFolder {
        retype: retype,
        changed_inputs: HashMap::new(),
        changed_outputs: HashMap::new(),
        changed_funcs: HashSet::new(),
        changed_defs: HashMap::new(),
    };
    krate.visit(&mut f);
    let ChangeTypeFolder { changed_inputs, changed_outputs, changed_funcs,
                           changed_defs, .. } = f;


    // (2) Look for exprs referencing the changed items, and wrap them in transmutes.

    let rvalue_repl = parse_expr(cx.session(),
            "::std::mem::transmute::<__old_ty, __new_ty>(__e)");
    let lvalue_repl = parse_expr(cx.session(),
            "*::std::mem::transmute::<&__old_ty, &__new_ty>(&__e)");
    let lvalue_mut_repl = parse_expr(cx.session(),
            "*::std::mem::transmute::<&mut __old_ty, &mut __new_ty>(&mut __e)");

    // MutVisitor for rewriting top-level exprs only
    struct ExprFolder<F> {
        callback: F,
    }

    impl<F: FnMut(&mut P<Expr>)> MutVisitor for ExprFolder<F> {
        fn visit_expr(&mut self, e: &mut P<Expr>) {
            (self.callback)(e)
        }
    }

    fn fold_top_exprs<T, F>(x: &mut T, callback: F)
            where T: MutVisit, F: FnMut(&mut P<Expr>) {
        let mut f = ExprFolder { callback: callback };
        x.visit(&mut f)
    }

    let transmute = |e, context, old_ty: &P<Ty>, new_ty: &P<Ty>| {
        let mut bnd = Bindings::new();
        bnd.add("__e", e);
        bnd.add("__old_ty", (*old_ty).clone());
        bnd.add("__new_ty", (*new_ty).clone());

        let repl = match context {
            lr_expr::Context::Rvalue => rvalue_repl.clone(),
            lr_expr::Context::Lvalue => lvalue_repl.clone(),
            lr_expr::Context::LvalueMut => lvalue_mut_repl.clone(),
        };
        repl.subst(st, cx, &bnd)
    };

    fold_top_exprs(krate, |e: &mut P<Expr>| {
        fold_expr_with_context(e, lr_expr::Context::Rvalue, |e, context| {
            match e.kind {
                ExprKind::Path(..) => {
                    if let Some(&(ref old_ty, ref new_ty)) = cx.try_resolve_expr_to_hid(&e)
                            .and_then(|hid| changed_defs.get(&cx.hir_map().hir_to_node_id(hid))) {
                        *e = transmute(e.clone(), context, new_ty, old_ty);
                    }
                },

                ExprKind::Field(ref obj, ref name) => {
                    let ty = cx.adjusted_node_type(obj.id);
                    match ty.kind() {
                        TyKind::Adt(adt, _) => {
                            let did = adt.non_enum_variant().fields
                              .iter()
                              .find(|f| f.ident(cx.ty_ctxt()) == *name)
                              .expect(&format!("Couldn't find struct field {}", name)).did;
                            if let Some(&(ref old_ty, ref new_ty)) = cx.hir_map()
                                    .as_local_node_id(did)
                                    .and_then(|id| changed_defs.get(&id)) {
                                *e = transmute(e.clone(), context, new_ty, old_ty);
                            }
                        },
                        _ => panic!("field access on non-adt"),
                    }
                },

                ExprKind::Call(_, ref args) => {
                    if let Some(func_id) = cx.opt_callee(&e)
                            .and_then(|did| cx.hir_map().as_local_node_id(did)) {
                        if changed_funcs.contains(&func_id) {
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
                            expect!([e.kind]
                                    ExprKind::Call(_, ref mut args) => *args = new_args);

                            if let Some(&(ref old_ty, ref new_ty)) = changed_outputs.get(&func_id) {
                                *e = transmute(e.clone(), context, new_ty, old_ty);
                            }
                        }
                    }
                },

                // TODO: Handle MethodCall.  In theory we should also deal with method calls from
                // operator overloads, but I doubt anybody wants to rewrite those definitions.

                _ => {},
            };
        })
    });


    // (3) Wrap output expressions from functions whose return types were modified.

    mut_visit_fns(krate, |fl| {
        if let Some(&(ref old_ty, ref new_ty)) = changed_outputs.get(&fl.id) {
            fl.body.as_mut().map(|b| fold_output_exprs(b, true, |e| {
                *e = transmute(e.clone(), lr_expr::Context::Rvalue, old_ty, new_ty);
            }));
        }
    });
}


/// # `bitcast_retype` Command
///
/// Usage: `bitcast_retype PAT REPL`
///
/// Marks: may read marks depending on `PAT`
///
/// For every type in the crate matching `PAT`, change the type to `REPL`.  `PAT`
/// and `REPL` are types, and can use placeholders in the manner of `rewrite_ty`.
/// For each definitions whose type has changed, it also inserts `mem::transmute`
/// calls at each use of the definition to fix discrepancies between the old and
/// new types.  (This implies that the original type and its replacement must be
/// transmutable to each other.)
pub struct BitcastRetype {
    pub pat: String,
    pub repl: String,
}

impl Transform for BitcastRetype {
    fn transform(&self, krate: &mut Crate, st: &CommandState, cx: &RefactorCtxt) {
        let pat = parse_ty(cx.session(), &self.pat);
        let repl = parse_ty(cx.session(), &self.repl);

        bitcast_retype(st, cx, krate, |ty| {
            // Doing a "deep" rewrite here is based on the assumption that if `T` and `U` are
            // transmute-compatible, then so are `&T` and `&U`, `(T, T)` and `(U, U)`, `S<T>` and
            // `S<U>`, etc.  This might not be true when associated types are involved (`T::SomeTy`
            // and `U::SomeTy` could be totally unrelated).

            let mut matched = false;
            mut_visit_match(st, cx, pat.clone(), ty, |ty, mcx| {
                matched = true;
                *ty = repl.clone().subst(st, cx, &mcx.bindings);
            });
            matched
        })
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}


/// # `type_fix_rules` Command
///
/// Usage: `type_fix_rules RULE...`
///
/// Attempts to fix type errors in the crate using the provided rules.  Each rule
/// has the form `"ectx, actual_ty, expected_ty => cast_expr"`.
///
///  - `ectx` is one of `rval`, `lval`, `lval_mut`, or `*`, and determines in what kinds of
///    expression contexts the rule applies.
///  - `actual_ty` is a pattern to be matched against the (reflected) actual expression type.
///  - `expected_ty` is a pattern to be matched against the (reflected) expected expression
///    type.
///  - `cast_expr` is a template for generating a cast expression.
///
/// For expressions in context `ectx`, whose actual type matches `actual_ty` and whose
/// expected type matches `expected_ty` (and where actual != expected), the expr is substituted
/// into `cast_expr` to replace the original expr with one of the expected type.  During
/// substitution, `cast_expr` has access to variables captured from both `actual_ty` and
/// `expected_ty`, as well as `__old` containing the original (ill-typed) expression.
pub struct TypeFixRules {
    pub rules: Vec<String>,
}

struct Rule {
    #[allow(unused)]
    ectx: Option<lr_expr::Context>,
    actual_ty: P<Ty>,
    expected_ty: P<Ty>,
    cast_expr: P<Expr>,
}

fn parse_rule<'a>(p: &mut Parser<'a>) -> PResult<'a, Rule> {
    let ectx = if p.eat(&TokenKind::Ident("rval".into_symbol(), false)) {
        Some(lr_expr::Context::Rvalue)
    } else if p.eat(&TokenKind::Ident("lval".into_symbol(), false)) {
        Some(lr_expr::Context::Lvalue)
    } else if p.eat(&TokenKind::Ident("lval_mut".into_symbol(), false)) {
        Some(lr_expr::Context::LvalueMut)
    } else {
        p.expect(&TokenKind::BinOp(BinOpToken::Star))?;
        None
    };
    p.expect(&TokenKind::Comma)?;

    let actual_ty = p.parse_ty()?;
    p.expect(&TokenKind::Comma)?;

    let expected_ty = p.parse_ty()?;

    p.expect(&TokenKind::FatArrow)?;

    let cast_expr = p.parse_expr()?;

    p.expect(&TokenKind::Eof)?;

    Ok(Rule { ectx, actual_ty, expected_ty, cast_expr })
}

impl Command for TypeFixRules {
    fn run(&mut self, state: &mut RefactorState) {
        let rules = self.rules.iter()
            .map(|s| driver::run_parser(state.session(), s, parse_rule))
            .collect::<Vec<_>>();

        state.run_typeck_loop(|krate, st, cx| {
            info!("Starting retyping iteration");

            let mut lr_map = HashMap::new();
            lr_expr::fold_exprs_with_context(krate, |e, ectx| {
                // This crate was just expanded (inside run_typeck_loop), so all nodes should be
                // numbered.
                assert!(e.id != DUMMY_NODE_ID);
                if ectx != lr_expr::Context::Rvalue {
                    lr_map.insert(e.id, ectx);
                }
            });

            let mut inserted = 0;
            fold_illtyped(cx, krate, TypeFixRulesFolder {
                st, cx,
                rules: &rules,
                num_inserted_casts: &mut inserted,
                lr_map: &lr_map,
            });
            if inserted > 0 {
                TypeckLoopResult::Iterate
            } else {
                TypeckLoopResult::Finished
            }
        }).expect("Could not retype crate!");
    }
}

struct TypeFixRulesFolder<'a, 'tcx: 'a> {
    st: &'a CommandState,
    cx: &'a RefactorCtxt<'a, 'tcx>,
    rules: &'a [Rule],

    num_inserted_casts: &'a mut u32,
    lr_map: &'a HashMap<NodeId, lr_expr::Context>,
}

impl<'a, 'tcx> IlltypedFolder<'tcx> for TypeFixRulesFolder<'a, 'tcx> {
    fn fix_expr(&mut self,
                e: &mut P<Expr>,
                actual: ty::Ty<'tcx>,
                expected: ty::Ty<'tcx>) {
        let ectx = self.lr_map.get(&e.id).cloned().unwrap_or(lr_expr::Context::Rvalue);
        let actual_ty_ast = reflect::reflect_tcx_ty(self.cx.ty_ctxt(), actual);
        let expected_ty_ast = reflect::reflect_tcx_ty(self.cx.ty_ctxt(), expected);
        debug!("looking for rule matching {:?}, {:?}, {:?}", ectx, actual_ty_ast, expected_ty_ast);


        for r in self.rules {
            if !r.ectx.map_or(true, |rule_ectx| rule_ectx == ectx) {
                trace!("wrong ectx: {:?} != {:?}", r.ectx, ectx);
                continue;
            }

            let mut mcx = MatchCtxt::new(self.st, self.cx);
            if let Err(e) = mcx.try_match(&r.actual_ty, &actual_ty_ast) {
                trace!("error matching actual {:?} with {:?}: {:?}",
                      r.actual_ty, actual_ty_ast, e);
                continue;
            }
            if let Err(e) = mcx.try_match(&r.expected_ty, &expected_ty_ast) {
                trace!("error matching expected {:?} with {:?}: {:?}",
                      r.expected_ty, expected_ty_ast, e);
                continue;
            }

            let mut bnd = mcx.bindings;
            bnd.add("__old", e.clone());
            info!("rewriting with bindings {:?}", bnd);
            *self.num_inserted_casts += 1;
            *e = r.cast_expr.clone().subst(self.st, self.cx, &bnd);
            return;
        }
    }
}


/// # `autoretype` Command
///
/// Usage: `autoretype 'A: T'...`
///
/// Marks: `A`... (specified in command)
///
/// Change the type of nodes with mark `A` to the new type `T`, propagating
/// changes and inserting casts when possible to satisfy type checking. Multiple
/// simultaneous retypings can be specified in this command as separate
/// arguments. Each argument should be of the form: `label: type` where `label`
/// is a mark label and `type` can be parsed as a valid rust type.
pub struct AutoRetype {
    /// Mapping from mark label to string representation of a rust type
    pub mark_types: HashMap<String, String>,
}

impl AutoRetype {
    fn new(args: &[String]) -> Self {
        let mut mark_types = HashMap::new();
        for arg in args {
            let words: Vec<&str> = arg.splitn(2, ':').collect();
            if words.len() != 2 {
                // TODO: It would be nice to return a proper error to the user
                // here rather than having to panic.
                panic!("retype command requires each argument to be of the form \"LABEL: TYPE\"");
            }
            mark_types.insert(words[0].to_string(), words[1].to_string());
        }

        AutoRetype {
            mark_types
        }
    }
}

impl Command for AutoRetype {
    fn run(&mut self, state: &mut RefactorState) {
        let type_annotations = state.transform_crate(Phase::Phase3, |st, cx| {
            let mut retype_prep = RetypePrepFolder::new(st, cx, &self.mark_types);
            st.map_krate(|krate| {
                krate.visit(&mut retype_prep)
            });
            retype_prep.type_annotations
        }).expect("Failed to run compiler");
        state.run_typeck_loop(|krate, _st, cx| {
            info!("Starting retyping iteration");
            RetypeIteration::new(cx, &type_annotations).run(krate)

            // TODO: Proper error handling showing type checking errors
        }).expect("Could not retype crate!");

        // We successfully retyped the crate, now restore type annotations we
        // removed in the process.
        state.transform_crate(Phase::Phase3, |st, cx| {
            st.map_krate(|krate| {
                let mut folder = RestoreAnnotationsFolder::new(cx, type_annotations);
                krate.visit(&mut folder)
            });
        }).expect("Failed to run compiler");
    }
}

/// This folder changes type annotations according to the given mapping and
/// erases type annotations from local variables. Type inference may infer new
/// types for these locals based on the changes to argument and field
/// types. These local variable annotations can be replaced after type
/// inference, or if type inference cannot disambiguate a local variable type.
struct RetypePrepFolder<'a> {
    st: &'a CommandState,

    mark_types: HashMap<String, P<Ty>>,

    type_annotations: HashMap<Span, P<Ty>>,
}

impl<'a> RetypePrepFolder<'a> {
    fn new<'tcx: 'a>(st: &'a CommandState, cx: &'a RefactorCtxt<'a, 'tcx>,
                     mark_types: &HashMap<String, String>) -> Self {
        let mark_types = mark_types
            .iter()
            .map(|(label, ty)| (label.clone(), parse_ty(cx.session(), ty)))
            .collect();

        RetypePrepFolder {
            st,
            mark_types,
            type_annotations: HashMap::new(),
       }
    }

    /// Check type node for marks and return the new type if found in
    /// `mark_types`, otherwise return the original type.
    fn map_type(&self, ty: &mut P<Ty>) {
        for (label, new_ty) in self.mark_types.iter() {
            if self.st.marked(ty.id, label) {
                *ty = new_ty.clone();
                return;
            }
        }
    }
}

impl<'a> MutVisitor for RetypePrepFolder<'a> {
    /// Replace marked argument types with their new types
    fn visit_fn_decl(&mut self, decl: &mut P<FnDecl>) {
        let FnDecl { inputs, output } = decl.deref_mut();
        for arg in inputs {
            self.map_type(&mut arg.ty);
        }
        match output {
            FnRetTy::Ty(ty) => self.map_type(ty),
            _ => {}
        }
    }

    /// Replace marked struct field types with their new types
    fn flat_map_field_def(&mut self, mut field: FieldDef) -> SmallVec<[FieldDef; 1]> {
        self.map_type(&mut field.ty);
        return mut_visit::noop_flat_map_field_def(field, self)
    }

    /// Remove all local variable types forcing type inference to update their
    /// types. We will replace these types if needed.
    fn visit_local(&mut self, local: &mut P<Local>) {
        if let Some(ty) = &local.ty {
            self.type_annotations.insert(local.span, ty.clone());
        }
        local.ty = None;

        match local.kind {
            LocalKind::Decl => {}
            LocalKind::Init(ref mut i) => {
                self.visit_expr(i);
            }
            LocalKind::InitElse(ref mut i, ref mut els) => {
                self.visit_expr(i);
                self.visit_block(els);
            }
        }
    }
}

/// This folder restores type annotations for the locals that don't currently
/// have annotations but did before we started. AST type annotations are
/// currently reflected from compiler derived internal types, so will be
/// semantically equivalent to the original but may not match textually
/// (i.e. typedef aliases are replaced by their aliasee)
struct RestoreAnnotationsFolder<'a, 'tcx: 'a> {
    cx: &'a RefactorCtxt<'a, 'tcx>,

    type_annotations: HashMap<Span, P<Ty>>,
}

impl<'a, 'tcx> RestoreAnnotationsFolder<'a, 'tcx> {
    fn new(cx: &'a RefactorCtxt<'a, 'tcx>, type_annotations: HashMap<Span, P<Ty>>) -> Self {
        RestoreAnnotationsFolder {
            cx,
            type_annotations,
        }
    }
}

impl<'a, 'tcx> MutVisitor for RestoreAnnotationsFolder<'a, 'tcx> {
    fn visit_local(&mut self, local: &mut P<Local>) {
        if local.ty.is_none() {
            if self.type_annotations.contains_key(&local.span) {
                let new_ty = self.cx.node_type(local.id);
                // Reflect the type back to an AST type. Since
                // we don't (yet) have a way to determine if an
                // AST Ty is equivalent to a TyCtxt Ty, we just
                // drop the old type and recreate it. Ideally we
                // would only change the old AST Ty if it was
                // changed in retyping.
                let new_ast_ty = reflect_tcx_ty(self.cx.ty_ctxt(), new_ty);
                local.ty = Some(new_ast_ty);
            }
        }
    }
}

/// Perform one iteration of fixing up type errors. We rely on the rustc type
/// checker to propagate types and change trait resolution based on the current
/// state of the retyped crate. This folder then recurses through the crate,
/// looking for child expressions that are not the expected type, which
/// indicates a type mismatch error. If a cast is possible, insert it at this
/// point, or bail out with an error indicating the expression that is blocking
/// retyping.
struct RetypeIteration<'a, 'tcx: 'a, 'b> {
    cx: &'a RefactorCtxt<'a, 'tcx>,

    num_inserted_casts: u32,

    type_annotations: &'b HashMap<Span, P<Ty>>,
}

impl<'a, 'tcx, 'b> RetypeIteration<'a, 'tcx, 'b> {
    fn new(cx: &'a RefactorCtxt<'a, 'tcx>, type_annotations: &'b HashMap<Span, P<Ty>>) -> Self {
        RetypeIteration {
            cx,
            num_inserted_casts: 0,
            type_annotations,
        }
    }

    fn run(&mut self, krate: &mut Crate) -> TypeckLoopResult {
        fold_illtyped(self.cx, krate, RetypeIterationFolder { iteration: self });
        if self.num_inserted_casts > 0 {
            return TypeckLoopResult::Iterate;
        }

        // If we find any remaining type errors, restore the explicit type
        // annotation to see if that will fix the error.
        let mut local_type_restored = false;
        MutVisitNodes::visit(krate, |local: &mut P<Local>| {
            let ty = self.cx.node_type(local.id);
            if let TyKind::Error(_) = ty.kind() {
                if let Some(old_ty) = self.type_annotations.get(&local.span) {
                    local_type_restored = true;
                    local.ty = Some(old_ty.clone());
                }
            }
        });

        if local_type_restored {
            return TypeckLoopResult::Iterate;
        }

        let mut errors = false;

        visit_fns(krate, |func| {
            if func.body.is_some() {
                let def_id = self.cx.hir_map().local_def_id_from_node_id(func.id);
                let tables = self.cx.ty_ctxt().typeck(def_id);
                if tables.tainted_by_errors.is_some() {
                    errors = true
                }
            }
        });

        if errors {
            debug!("{:#?}", krate);
            TypeckLoopResult::Err("Typechecking failed")
        } else {
            TypeckLoopResult::Finished
        }
    }
}

struct RetypeIterationFolder<'a, 'b, 'tcx, 'c> {
    iteration: &'b mut RetypeIteration<'a, 'tcx, 'c>,
}

impl<'a, 'b, 'tcx, 'c> IlltypedFolder<'tcx> for RetypeIterationFolder<'a, 'b, 'tcx, 'c> {
    fn fix_expr(
        &mut self,
        e: &mut P<Expr>,
        actual: ty::Ty<'tcx>,
        expected: ty::Ty<'tcx>
    ) {
        info!("Retyping {:?} into type {:?}", e, expected);
        if let TyKind::Error(_) = actual.kind() {
            return;
        }
        if self.iteration.try_retype(e, TypeExpectation::new(expected)) {
            info!("Retyped into {:?} with type {:?}", e, expected);
        } else {
            // With a bottom-up retyping, I'm not sure we want to panic
            // here. We may be able to retype a parent and eliminate the
            // need to retype the child.
            panic!("Could not transform expression {:?} from type {:?} into type {:?}", e, actual, expected)
        }
    }
}

#[derive(Clone, Debug)]
struct TypeExpectation<'tcx> {
    pub ty: ty::Ty<'tcx>,
    pub mutability: Option<hir::Mutability>,
    pub negated: bool,
}

impl<'tcx> TypeExpectation<'tcx> {
    fn new(ty: ty::Ty<'tcx>) -> Self {
        TypeExpectation {
            ty,
            mutability: None,
            negated: false,
        }
    }
}

impl<'a, 'tcx, 'b> RetypeIteration<'a, 'tcx, 'b> {
    /// Determine if `from` can cast be cast to `to` according to rust-rfc 0401.
    fn can_cast(&self, from: ty::Ty<'tcx>, to: ty::Ty<'tcx>, parent: LocalDefId) -> bool {
        use rustc_type_ir::sty::TyKind::*;
        use rustc_middle::ty::TypeAndMut;

        // coercion-cast
        if can_coerce(from, to, self.cx.ty_ctxt()) {
            return true
        }

        match (&from.kind(), &to.kind()) {
            (Ref(_, from, Mutability::Mut), Ref(_, to, _))
            // We ignore regions here because references from command-line args
            // won't have a valid region.
                => self.can_cast(*from, *to, parent),
            (Ref(_, from, Mutability::Not), Ref(_, to, Mutability::Not))
            // We ignore regions here because references from command-line args
            // won't have a valid region.
                => self.can_cast(*from, *to, parent),

            // ptr-ptr-cast
            (&RawPtr(TypeAndMut{ty: ref _from_ty, mutbl: from_mut}),
             &RawPtr(TypeAndMut{ty: ref _to_ty, mutbl: to_mut})) => match (from_mut, to_mut) {
                // Immutable -> Mutable is an allowed cast, but we shouldn't
                // introduce these as they may break semantics.
                (Mutability::Not, Mutability::Mut) => false,

                _ => {
                    let param_env_ty = self.cx.ty_ctxt().param_env(parent.to_def_id()).and(to);

                    // All pointer casts to sized types are allowed
                    self.cx.ty_ctxt().is_sized_raw(param_env_ty)

                    // Pointer casts to unsized types are also allowed if the
                    // from and to type have the same unsize info. TODO: Handle
                    // this case?
                },
            },

            // TODO: ptr-addr-cast
            // TODO: addr-ptr-cast

            // Semantics preserving numeric-casts
            (&Int(ref from_int), &Int(ref to_int)) => {
                match (from_int.bit_width(), to_int.bit_width()) {
                    (Some(from), Some(to)) if from > to => {
                        // Truncating
                        false
                    },
                    (None, _) | (_, None) => {
                        // Depends on the width of isize
                        false
                    },
                    _ => {
                        // Sign-extend
                        true
                    },
                }
            },
            (&Uint(ref from_int), &Uint(ref to_int)) => {
                match (from_int.bit_width(), to_int.bit_width()) {
                    (Some(from), Some(to)) if from > to => {
                        // Truncating
                        false
                    },
                    (None, _) | (_, None) => {
                        // Depends on the width of isize
                        false
                    },
                    _ => {
                        // Zero-extend
                        true
                    },
                }
            },
            (&Uint(ref from_int), &Int(ref to_int)) => {
                match (from_int.bit_width(), to_int.bit_width()) {
                    (Some(from), Some(to)) if from > to - 1 => {
                        // Truncating
                        false
                    },
                    (None, None) => {
                        // Truncating
                        false
                    },
                    (None, _) | (_, None) => {
                        // Depends on the width of usize/isize
                        false
                    },
                    _ => {
                        // Zero-extend
                        true
                    },
                }
            },
            (&Int(ref from_int), &Uint(ref to_int)) => {
                match (from_int.bit_width(), to_int.bit_width()) {
                    (Some(from), Some(to)) if from - 1 > to => {
                        // Truncating
                        false
                    },
                    (None, None) => {
                        // Sign-extend
                        true
                    },
                    (None, _) | (_, None) => {
                        // Depends on the width of usize/isize
                        false
                    },
                    _ => {
                        // Sign-extend
                        true
                    },
                }
            },
            (&Float(ref from_float), &Float(ref to_float)) => {
                // Can cast from smaller to larger float
                from_float.bit_width() < to_float.bit_width()
            },

            // prim-int-cast
            (Bool, Int(_)) => true,
            (Bool, Uint(_)) => true,
            (Char, Int(_)) => true,
            (Char, Uint(_)) => true,

            // u8-char-cast
            (Uint(ty::UintTy::U8), Char) => true,

            // TODO: enum-cast
            // TODO: array-ptr-cast? (might already be handled implicitly)
            // TODO: ftpr-ptr-cast
            // TODO: ftpr-addr-cast

            _ => false,
        }
    }

    /// Change the type of an integer literal if it fits into the expected
    /// type's range.
    fn retype_int_lit(&self, lit: Lit, expected: TypeExpectation<'tcx>) -> Option<P<Expr>> {
        // from librust_lint::TypeLimits
        // for isize & usize, be conservative with the warnings, so that the
        // warnings are consistent between 32- and 64-bit platforms
        fn int_ty_range(int_ty: ty::IntTy) -> (i128, i128) {
            match int_ty {
                ty::IntTy::Isize => (i64::min_value() as i128, i64::max_value() as i128),
                ty::IntTy::I8 => (i8::min_value() as i64 as i128, i8::max_value() as i128),
                ty::IntTy::I16 => (i16::min_value() as i64 as i128, i16::max_value() as i128),
                ty::IntTy::I32 => (i32::min_value() as i64 as i128, i32::max_value() as i128),
                ty::IntTy::I64 => (i64::min_value() as i128, i64::max_value() as i128),
                ty::IntTy::I128 =>(i128::min_value() as i128, i128::max_value()),
            }
        }

        fn uint_ty_range(uint_ty: ty::UintTy) -> (u128, u128) {
            match uint_ty {
                ty::UintTy::Usize => (u64::min_value() as u128, u64::max_value() as u128),
                ty::UintTy::U8 => (u8::min_value() as u128, u8::max_value() as u128),
                ty::UintTy::U16 => (u16::min_value() as u128, u16::max_value() as u128),
                ty::UintTy::U32 => (u32::min_value() as u128, u32::max_value() as u128),
                ty::UintTy::U64 => (u64::min_value() as u128, u64::max_value() as u128),
                ty::UintTy::U128 => (u128::min_value(), u128::max_value()),
            }
        }

        match (&expected.ty.kind(), &lit.kind) {
            (TyKind::Int(t), LitKind::Int(v, _)) => {
                let int_type = t.normalize(self.cx.session().target.pointer_width);
                let (_, max) = int_ty_range(int_type);
                let max = max as u128;

                // Detect literal value out of range [min, max] inclusive
                // avoiding use of -min to prevent overflow/panic
                if (expected.negated && *v <= max + 1) || (!expected.negated && *v <= max) {
                    Some(mk().lit_expr(mk().int_lit(*v, int_type)))
                } else {
                    None
                }
            }
            (TyKind::Uint(t), LitKind::Int(v, _)) => {
                let uint_type = t.normalize(self.cx.session().target.pointer_width);
                let (min, max) = uint_ty_range(uint_type);
                if *v >= min && *v <= max {
                    Some(mk().lit_expr(mk().int_lit(*v, uint_type)))
                } else {
                    None
                }
            }
            // TODO: Handle LitKind::Byte
            _ => None,
        }
    }

    /// Attempt to remove transmutes, optionally with as_ptr and as_mut_ptr
    /// calls. This is exclusively for readability, not correctness.
    fn try_transmute_fix(&mut self, expr: &mut P<Expr>, expected: TypeExpectation<'tcx>) -> bool {
        match (&mut expr.kind, &expected.ty.kind()) {
            (ExprKind::Call(ref callee, ref arguments), _) => {
                let callee_did = self.cx.try_resolve_expr(callee);
                if let Some(callee_did) = callee_did {
                    let callee_str = self.cx.ty_ctxt().def_path_str(callee_did);
                    // intrinsics are in an anonymous namespace, so the full
                    // path is actually core::intrinsics::<anon>::transmute
                    if callee_str == "std::intrinsics::transmute" ||
                        callee_str == "core::intrinsics::transmute"
                    {
                        let mut e = arguments[0].clone();
                        if self.try_retype(&mut e, expected) {
                            *expr = e;
                            return true;
                        }
                    }
                }
            }
            (
                ExprKind::MethodCall(ref path, ref arguments, _),
                TyKind::RawPtr(ty::TypeAndMut{ty: ref inner_ty, ref mutbl}),
            ) if (path.ident.name.as_str() == "as_mut_ptr"
                  || path.ident.name.as_str() == "as_ptr") => {
                let new_method_name = if *mutbl == hir::Mutability::Mut {
                    "as_mut_ptr"
                } else {
                    "as_ptr"
                };
                let mut sub_expected = expected;
                sub_expected.ty = self.cx.ty_ctxt().mk_slice(*inner_ty);
                sub_expected.mutability = Some(*mutbl);
                let mut e = arguments[0].clone();
                if self.try_retype(&mut e, sub_expected.clone()) {
                    *expr = mk().method_call_expr(e, new_method_name, Vec::<P<Expr>>::new());
                    return true;
                }
                sub_expected.ty = self.cx.ty_ctxt().mk_ref(
                    self.cx.ty_ctxt().lifetimes.re_root_empty,
                    ty::TypeAndMut{ty: sub_expected.ty, mutbl: *mutbl},
                );
                if self.try_retype(&mut e, sub_expected) {
                    *expr = mk().method_call_expr(e, new_method_name, Vec::<P<Expr>>::new());
                    return true;
                }
            }
            (ExprKind::Unary(UnOp::Deref, e), _) => {
                let mut sub_expected = expected.clone();
                let old_subtype = self.cx.node_type(e.id);
                sub_expected.ty = match old_subtype.kind() {
                    TyKind::RawPtr(ty::TypeAndMut{mutbl: subtype_mutbl, ..}) => {
                        let mutbl = expected.mutability.unwrap_or(*subtype_mutbl);
                        self.cx.ty_ctxt().mk_ptr(ty::TypeAndMut{
                            ty: expected.ty,
                            mutbl
                        })
                    }
                    TyKind::Ref(_, _, subtype_mutbl) => {
                        let mutbl = expected.mutability.unwrap_or(*subtype_mutbl);
                        self.cx.ty_ctxt().mk_ref(
                            self.cx.ty_ctxt().lifetimes.re_root_empty,
                            ty::TypeAndMut { ty: expected.ty, mutbl },
                        )
                    }
                    _ => panic!("Unsupported type for dereference"),
                };
                return self.try_retype(e, sub_expected);
            }
            (ExprKind::AddrOf(_, expr_mut, e), TyKind::Ref(_, subty, expected_mut)) => {
                let mutbl = match (&expr_mut, expected_mut) {
                    (Mutability::Mut, _) |
                    (Mutability::Not, hir::Mutability::Not) => expected_mut,
                    _ => return false,
                };
                let mut sub_expected = expected;
                sub_expected.ty = *subty;
                sub_expected.mutability = Some(*mutbl);
                if self.try_retype(e, sub_expected) {
                    *expr_mut = *mutbl;
                    return true;
                }
            }
            _ => (),
        };
        false
    }

    /// Attempt to coerce or cast an expression into the expected type
    fn try_retype(
        &mut self,
        expr: &mut P<Expr>,
        expected: TypeExpectation<'tcx>,
    ) -> bool {
        let cur_ty = self.cx.node_type(expr.id);
        debug!("Attempting to retype {:?} from {:?} to {:?}", expr, cur_ty, expected);
        if can_coerce(cur_ty, expected.ty, self.cx.ty_ctxt()) {
            return true;
        }

        match &mut expr.kind {
            ExprKind::Cast(expr, _) => {
                return self.try_retype(expr, expected);
            }
            ExprKind::Lit(lit) => {
                if let Some(e) = self.retype_int_lit(lit.clone(), expected.clone()) {
                    *expr = e;
                    return true;
                }
            }
            _ => (),
        };

        if self.try_transmute_fix(expr, expected.clone()) {
            return true;
        }

        let hir_id = self.cx.hir_map().node_to_hir_id(expr.id);
        if self.can_cast(cur_ty, expected.ty, self.cx.hir_map().get_parent_item(hir_id)) {
            self.num_inserted_casts += 1;
            *expr = mk().cast_expr(expr.clone(), reflect_tcx_ty(self.cx.ty_ctxt(), expected.ty));
            return true;
        }

        false
    }
}

/// Will `from_ty` coerce to `to_ty`?
/// Based on rules described in https://doc.rust-lang.org/nomicon/coercions.html
fn can_coerce<'a, 'tcx>(
    from_ty: ty::Ty<'tcx>,
    to_ty: ty::Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
) -> bool {
    use rustc_type_ir::sty::TyKind::*;

    // We won't necessarily have matching regions if we created new expressions
    // during retyping, so we should strip those. This also handles arrays with
    // length expressions that aren't yet evaluated. See types_approx_equal() in
    // illtyped.rs for more details.
    let from_ty = tcx.normalize_erasing_regions(ParamEnv::empty(), from_ty);
    let to_ty = tcx.normalize_erasing_regions(ParamEnv::empty(), to_ty);

    if from_ty == to_ty {
        return true;
    }
    match (from_ty.kind(), to_ty.kind()) {
        // Unsize for Array
        (Array(from_ty, _), Slice(to_ty)) => can_coerce(*from_ty, *to_ty, tcx),

        // Lifetime coercion. This is more permissive than the language allows
        // (should only be longer -> shorter lifetimes). However, if we assume
        // that we aren't changing lifetimes then we can be overly permissive,
        // since we couldn't fix the lifetimes if they did not match.
        (Ref(_, from_ty, mut1), Ref(_, to_ty, mut2)) => {
            mut1 == mut2 && can_coerce(*from_ty, *to_ty, tcx)
        }

        // TODO other unsizing

        // TODO Pointer Weakening:
        // &mut T to &T
        // *mut T to *const T
        // &T to *const T
        // &mut T to *mut T

        // TODO Deref coercion: Expression &x of type &T to &*x of type &U if T
        // derefs to U (i.e. T: Deref<Target=U>)

        (FnDef(..), FnPtr(sig)) => {
            from_ty.fn_sig(tcx) == *sig
        },

        _ => false
    }
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;

    reg.register("retype_argument", |args| mk(RetypeArgument {
        new_ty: args[0].clone(),
        wrap: args[1].clone(),
        unwrap: args[2].clone(),
    }));

    reg.register("retype_return", |args| mk(RetypeReturn {
        new_ty: args[0].clone(),
        wrap: args[1].clone(),
        unwrap: args[2].clone(),
    }));

    reg.register("retype_static", |args| mk(RetypeStatic {
        new_ty: args[0].clone(),
        rev_conv_assign: args[1].clone(),
        conv_rval: args[2].clone(),
        conv_lval: args[3].clone(),
        conv_lval_mut: args.get(4).cloned(),
    }));

    reg.register("bitcast_retype", |args| mk(BitcastRetype {
        pat: args[0].clone(),
        repl: args[1].clone(),
    }));

    reg.register("type_fix_rules", |args| Box::new(TypeFixRules { rules: args.to_owned() }));

    reg.register("autoretype", |args| Box::new(AutoRetype::new(args)));
}
