use std::collections::HashMap;
use std::collections::HashSet;

use arena::SyncDroplessArena;
use rustc::hir::def_id::DefId;
use rustc_index::vec::IndexVec;
use syntax::ast::*;
use syntax::source_map::DUMMY_SP;
use syntax::mut_visit::{self, MutVisitor};
use syntax::parse::token::{self, Token, TokenKind, DelimToken};
use syntax::ptr::P;
use syntax::symbol::Symbol;
use syntax::tokenstream::{TokenTree, TokenStream, DelimSpan};
use smallvec::SmallVec;

use crate::ast_manip::{MutVisitNodes, MutVisit};
use crate::ast_manip::fn_edit::flat_map_fns;
use crate::analysis::labeled_ty::LabeledTyCtxt;
use crate::analysis::ownership::{self, ConcretePerm, Var, PTy};
use crate::analysis::ownership::constraint::{ConstraintSet, Perm};
use crate::command::{CommandState, Registry, DriverCommand};
use crate::context::HirMap;
use crate::driver::{Phase};
use crate::type_map;
use crate::RefactorCtxt;
use c2rust_ast_builder::{mk, IntoSymbol};

pub fn register_commands(reg: &mut Registry) {
    reg.register("ownership_annotate", |args| {
        let label = args.get(0).map_or("target", |x| x).into_symbol();

        Box::new(DriverCommand::new(Phase::Phase3, move |st, cx| {
            do_annotate(st, cx, label);
        }))
    });

    reg.register("ownership_split_variants", |args| {
        let label = args.get(0).map_or("target", |x| x).into_symbol();

        Box::new(DriverCommand::new(Phase::Phase3, move |st, cx| {
            do_split_variants(st, cx, label);
        }))
    });

    reg.register("ownership_mark_pointers", |_args| {
        Box::new(DriverCommand::new(Phase::Phase3, move |st, cx| {
            do_mark_pointers(st, cx);
        }))
    });
}

/// # `ownership_annotate` Command
///
/// Usage: `ownership_annotate [MARK]`
///
/// Marks: `MARK`/`target`
///
/// Run ownership analysis on functions bearing `MARK` (default: `target`),
/// and add attributes to each function describing its inferred
/// ownership properties.
/// See `analysis/ownership/README.md` for details on ownership inference.
fn do_annotate(st: &CommandState,
               cx: &RefactorCtxt,
               label: Symbol) {
    let arena = SyncDroplessArena::default();
    let analysis = ownership::analyze(&st, &cx, &arena);

    struct AnnotateFolder<'a, 'tcx: 'a> {
        label: Symbol,
        ana: ownership::AnalysisResult<'tcx, 'tcx>,
        hir_map: HirMap<'a, 'tcx>,
        st: &'a CommandState,
    }

    impl<'lty, 'a, 'tcx> AnnotateFolder<'a, 'tcx> {
        fn static_attr_for(&self, id: NodeId) -> Option<Attribute> {
            self.hir_map.opt_local_def_id_from_node_id(id)
                .and_then(|def_id| self.ana.statics.get(&def_id))
                .and_then(|&ty| build_static_attr(ty))
        }

        fn constraints_attr_for(&self, id: NodeId) -> Option<Attribute> {
            self.hir_map.opt_local_def_id_from_node_id(id)
                .and_then(|def_id| self.ana.funcs.get(&def_id))
                .map(|fr| build_constraints_attr(&fr.cset))
        }

        fn push_mono_attrs_for(&self, id: NodeId, dest: &mut Vec<Attribute>) {
            if let Some((def_id, (fr, vr))) = self.hir_map.opt_local_def_id_from_node_id(id)
                    .map(|def_id| (def_id, self.ana.fn_results(def_id))) {
                if fr.num_sig_vars == 0 {
                    return;
                }

                if fr.variants.is_none() {
                    for idx in 0 .. fr.num_monos {
                        let mr = &self.ana.monos[&(def_id, idx)];
                        dest.push(build_mono_attr(&mr.suffix, &mr.assign));
                    }
                } else {
                    let mr = &self.ana.monos[&(def_id, vr.index)];
                    dest.push(build_mono_attr(&mr.suffix, &mr.assign));
                }
            }
        }

        fn clean_attrs(&self, attrs: &mut Vec<Attribute>) {
            attrs.retain(|a| {
                match &a.path.to_string() as &str {
                    "ownership_mono" |
                    "ownership_constraints" |
                    "ownership_static" => false,
                    _ => true,
                }
            });
        }
    }

    impl<'lty, 'a, 'tcx> MutVisitor for AnnotateFolder<'a, 'tcx> {
        fn flat_map_item(&mut self, i: P<Item>) -> SmallVec<[P<Item>; 1]> {
            if !self.st.marked(i.id, self.label) {
                return mut_visit::noop_flat_map_item(i, self);
            }

            mut_visit::noop_flat_map_item(i.map(|mut i| {
                match i.kind {
                    ItemKind::Static(..) | ItemKind::Const(..) => {
                        self.clean_attrs(&mut i.attrs);
                        if let Some(attr) = self.static_attr_for(i.id) {
                            i.attrs.push(attr);
                        }
                    },

                    ItemKind::Fn(..) => {
                        self.clean_attrs(&mut i.attrs);
                        if let Some(attr) = self.constraints_attr_for(i.id) {
                            i.attrs.push(attr);
                        }
                        self.push_mono_attrs_for(i.id, &mut i.attrs);
                    },

                    _ => {},
                }

                i
            }), self)
        }

        fn flat_map_impl_item(&mut self, i: ImplItem) -> SmallVec<[ImplItem; 1]> {
            if !self.st.marked(i.id, self.label) {
                return mut_visit::noop_flat_map_impl_item(i, self);
            }

            mut_visit::noop_flat_map_impl_item(i, self)
        }

        fn flat_map_struct_field(&mut self, mut sf: StructField) -> SmallVec<[StructField; 1]> {
            if !self.st.marked(sf.id, self.label) {
                return mut_visit::noop_flat_map_struct_field(sf, self);
            }

            self.clean_attrs(&mut sf.attrs);
            if let Some(attr) = self.static_attr_for(sf.id) {
                sf.attrs.push(attr);
            }

            mut_visit::noop_flat_map_struct_field(sf, self)
        }
    }

    st.map_krate(|krate| {
        krate.visit(&mut AnnotateFolder {
            label: label,
            ana: analysis,
            hir_map: cx.hir_map(),
            st: st,
        })
    });
}

fn build_static_attr(ty: PTy) -> Option<Attribute> {
    let mut args = Vec::new();
    ty.for_each_label(&mut |p| {
        if let Some(p) = *p {
            args.push(perm_token(p));
        }
    });
    let tokens = parens(args).into();
    Some(make_attr("ownership_static", tokens))
}

fn build_constraints_attr(cset: &ConstraintSet) -> Attribute {
    let mut args = Vec::new();

    fn push_perm_tokens(p: Perm, dest: &mut Vec<TokenTree>) {
        match p {
            Perm::Concrete(p) => dest.push(perm_token(p)),
            Perm::SigVar(v) => dest.push(ident_token(&format!("_{}", v.0))),
            Perm::Min(ps) => {
                let mut ts = Vec::new();
                for (i, &p) in ps.iter().enumerate() {
                    if i > 0 {
                        ts.push(token(TokenKind::Comma));
                    }
                    push_perm_tokens(p, &mut ts);
                }
                dest.push(ident_token("min"));
                dest.push(parens(ts));
            },
            _ => panic!("unexpected var kind in fn constraints"),
        }
    }

    for (i, &(a, b)) in cset.iter().enumerate() {
        if i > 0 {
            args.push(token(TokenKind::Comma));
        }
        args.push(ident_token("le"));

        let mut le_args = Vec::new();
        push_perm_tokens(a, &mut le_args);
        le_args.push(token(TokenKind::Comma));
        push_perm_tokens(b, &mut le_args);

        args.push(parens(le_args));
    }

    let tokens = parens(args).into();
    make_attr("ownership_constraints", tokens)
}

fn build_mono_attr(suffix: &str, assign: &IndexVec<Var, ConcretePerm>) -> Attribute {
    let mut args = Vec::new();
    args.push(str_token(suffix));

    for &p in assign.iter() {
        args.push(token(TokenKind::Comma));
        args.push(perm_token(p));
    }

    let tokens = parens(args).into();
    make_attr("ownership_mono", tokens)
}

fn perm_token(p: ConcretePerm) -> TokenTree {
    let name = match p {
        ConcretePerm::Read => "READ",
        ConcretePerm::Write => "WRITE",
        ConcretePerm::Move => "MOVE",
    };
    ident_token(name)
}

fn ident_token(name: &str) -> TokenTree {
    token(TokenKind::Ident(Symbol::intern(name), false))
}

fn str_token(s: &str) -> TokenTree {
    token(TokenKind::Literal(token::Lit {
        kind: token::LitKind::Str,
        symbol: s.into_symbol(),
        suffix: None,
    }))
}

fn token(kind: TokenKind) -> TokenTree {
    TokenTree::Token(Token{kind, span: DUMMY_SP})
}

fn parens(ts: Vec<TokenTree>) -> TokenTree {
    TokenTree::Delimited(
        DelimSpan::dummy(),
        DelimToken::Paren,
        ts.into_iter().collect::<TokenStream>().into(),
    )
}

fn make_attr(name: &str, tokens: TokenStream) -> Attribute {
    Attribute {
        id: AttrId(0),
        style: AttrStyle::Outer,
        item: AttrItem {
            path: mk().path(vec![name]),
            tokens: tokens,
        },
        is_sugared_doc: false,
        span: DUMMY_SP,
    }
}

fn build_variant_attr(group: &str) -> Attribute {
    let tokens = parens(vec![str_token(group)]).into();
    make_attr("ownership_variant_of", tokens)
}



/// # `ownership_split_variants` Command
///
/// Usage: `ownership_split_variants [MARK]`
///
/// Marks: `MARK`/`target`
///
/// Run ownership analysis on functions bearing `MARK` (default: `target`),
/// and split each ownership-polymorphic functions into multiple
/// monomorphic variants.
/// See `analysis/ownership/README.md` for details on ownership inference.
fn do_split_variants(st: &CommandState,
                     cx: &RefactorCtxt,
                     label: Symbol) {
    let arena = SyncDroplessArena::default();
    let ana = ownership::analyze(&st, &cx, &arena);

    // Map from ExprPath/ExprMethodCall span to function ref idx within the caller.
    let mut span_fref_idx = HashMap::new();
    for vr in ana.variants.values() {
        for (idx, fref) in vr.func_refs.iter().enumerate() {
            if let Some(span) = fref.span {
                span_fref_idx.insert(span, idx);
            }
        }
    }

    let mut handled_spans = HashSet::new();

    st.map_krate(|krate| {
        // (1) Duplicate marked fns with `mono` attrs to produce multiple variants.  We rewrite
        // references to other fns during this process, since afterward it would be difficult to
        // distinguish the different copies - their bodies have identical spans and `NodeId`s.
        flat_map_fns(krate, |fl| {
            if !st.marked(fl.id, label) {
                return smallvec![fl];
            }
            debug!("looking at {:?}", fl.ident);

            let def_id = match_or!([cx.hir_map().opt_local_def_id_from_node_id(fl.id)]
                                   Some(x) => x; return smallvec![fl]);
            if !ana.variants.contains_key(&def_id) {
                return smallvec![fl];
            }
            let (fr, vr) = ana.fn_results(def_id);

            if fr.variants.is_some() {
                // Func has already been split.  No work to do at this point.
                return smallvec![fl];
            }

            let path_str = cx.ty_ctxt().def_path(def_id).to_string_no_crate();


            // For consistency, we run the split logic even for funcs with only one mono.  This way
            // the "1 variant, N monos" case is handled here, and the "N variants, N monos" case is
            // handled below.
            let mut fls = SmallVec::with_capacity(fr.num_monos);
            for mono_idx in 0 .. fr.num_monos {
                let mr = &ana.monos[&(vr.func_id, mono_idx)];
                let mut fl = fl.clone();

                if mr.suffix.len() > 0 {
                    fl.ident = mk().ident(format!("{}_{}", fl.ident.name, mr.suffix));
                }

                // Apply ownership annotations
                fl.attrs.retain(|a| {
                    // If a `variant_of` annotation was present, then this fn should be part of a
                    // variant set, and we should have bailed out of the split logic already.
                    assert!(!a.check_name("ownership_variant_of".into_symbol()));

                    // Remove all `ownership_mono` (we add a new one below) and also remove
                    // `ownership_constraints` from all but the first split fn.
                    !a.check_name("ownership_mono".into_symbol()) &&
                    (!a.check_name("ownership_constraints".into_symbol()) || mono_idx == 0)
                });

                fl.attrs.push(build_mono_attr(&mr.suffix, &mr.assign));
                fl.attrs.push(build_variant_attr(&path_str));

                fl.block.as_mut().map(|b| MutVisitNodes::visit(b, |e: &mut P<Expr>| {
                    let fref_idx = match_or!([span_fref_idx.get(&e.span)]
                                             Some(&x) => x; return);
                    handled_spans.insert(e.span);

                    let dest = vr.func_refs[fref_idx].def_id;
                    let dest_fr = &ana.funcs[&dest];
                    let dest_marked = cx.hir_map().as_local_node_id(dest)
                        .map_or(false, |id| st.marked(id, label));
                    // Two conditions under which we adjust the call site.  First, if the fn is
                    // marked, then it's going to be split during this pass.  Second, if the callee
                    // func has variants (i.e, it was previously split), then we retarget the call
                    // to the appropriate variant.
                    if !dest_marked && dest_fr.variants.is_none() {
                        // A call from a split function to a non-split function.  Leave the call
                        // unchanged.
                        return;
                    }
                    let dest_mono_idx = mr.callee_mono_idxs[fref_idx];

                    let new_name = callee_new_name(cx, &ana, dest, dest_mono_idx);
                    rename_callee(e, &new_name);
                }));

                fls.push(fl);
            }
            fls
        });

        // (2) Find calls from other functions into functions being split.  Retarget those calls to
        // an appropriate monomorphization.
        MutVisitNodes::visit(krate, |e: &mut P<Expr>| {
            let fref_idx = match_or!([span_fref_idx.get(&e.span)]
                                     Some(&x) => x; return);
            if handled_spans.contains(&e.span) {
                // This span was handled while splitting a function into variants.
                return;
            }

            // Figure out where we are.
            let hir_id = cx.hir_map().node_to_hir_id(e.id);
            let src = cx.hir_map().get_parent_item(hir_id);
            let src = cx.hir_map().hir_to_node_id(src);
            let src_def_id = cx.node_def_id(src);
            let (src_fr, src_vr) = ana.fn_results(src_def_id);

            // Figure out what we're calling.
            let dest = src_vr.func_refs[fref_idx].def_id;
            let dest_fr = &ana.funcs[&dest];
            let dest_marked = cx.hir_map().as_local_node_id(dest)
                .map_or(false, |id| st.marked(id, label));
            if !dest_marked && dest_fr.variants.is_none() {
                return;
            }

            // Pick a monomorphization.

            // There are two cases here.  First, we might be in a non-split fn.  In that case, we
            // arbitrarily pretend that we're in monomorphization #0 of the src function.  Second,
            // we might be in a pre-split fn (a variant).  Then we use the variant index as the src
            // mono idx.

            let src_mono_idx =
                if src_fr.variants.is_none() { 0 }
                else { src_vr.index };
            let src_mr = &ana.monos[&(src_vr.func_id, src_mono_idx)];
            let dest_mono_idx = src_mr.callee_mono_idxs[fref_idx];

            let new_name = callee_new_name(cx, &ana, dest, dest_mono_idx);
            rename_callee(e, &new_name)
        });
    });
}

fn rename_callee(e: &mut P<Expr>, new_name: &str) {
    match &mut e.kind {
        ExprKind::Path(_, ref mut path) => {
            // Change the last path segment.
            let seg = path.segments.last_mut().unwrap();
            seg.ident = mk().ident(new_name);
        },

        ExprKind::MethodCall(ref mut seg, _) => {
            seg.ident = mk().ident(new_name);
        },

        _ => panic!("rename_callee: unexpected expr kind: {:?}", e),
    }
}

fn callee_new_name(cx: &RefactorCtxt,
                   ana: &ownership::AnalysisResult,
                   dest: DefId,
                   dest_mono_idx: usize) -> String {
    let fr = &ana.funcs[&dest];
    if let Some(ref var_ids) = fr.variants {
        // Function has variants.  Take the name of the indicated variant.
        let var_id = var_ids[dest_mono_idx];
        cx.ty_ctxt().def_path(var_id).data
           .last().unwrap().data.to_string()
    } else {
        // No variants.  Presumably the function is getting split.  Add the suffix for the selected
        // mono.
        let base_name = cx.ty_ctxt().def_path(dest).data
           .last().unwrap().data.get_opt_name().unwrap();
        let suffix = &ana.monos[&(dest, dest_mono_idx)].suffix;
        if suffix.len() > 0 {
            format!("{}_{}", base_name, suffix)
        } else {
            format!("{}", base_name)
        }
    }
}


/// # `ownership_mark_pointers` Command
///
/// Usage: `ownership_mark_pointers [MARK]`
///
/// Marks: reads `MARK`/`target`; sets `ref`, `mut`, and `box`
///
/// Run ownership analysis on functions bearing `MARK` (default: `target`),
/// then for pointer type appearing in their argument and return types,
/// apply one of the marks `ref`, `mut`, or `box`, reflecting the results
/// of the ownership analysis.
/// See `analysis/ownership/README.md` for details on ownership inference.
fn do_mark_pointers(st: &CommandState, cx: &RefactorCtxt) {
    let arena = SyncDroplessArena::default();
    let ana = ownership::analyze(&st, &cx, &arena);

    struct AnalysisTypeSource<'lty, 'tcx: 'lty> {
        ana: &'lty ownership::AnalysisResult<'lty, 'tcx>,
        hir_map: HirMap<'lty, 'tcx>,
    }

    impl<'lty, 'tcx> type_map::TypeSource for AnalysisTypeSource<'lty, 'tcx> {
        type Type = ownership::PTy<'lty, 'tcx>;
        type Signature = ownership::PFnSig<'lty, 'tcx>;

        fn def_type(&mut self, did: DefId) -> Option<Self::Type> {
            self.ana.statics.get(&did).cloned()
        }

        fn fn_sig(&mut self, did: DefId) -> Option<Self::Signature> {
            let (fr, vr) = self.ana.fn_results(did);
            // Only provide signatures for monomorphic fns.
            if fr.variants.is_none() && fr.num_monos > 1 {
                return None;
            }

            // Only one variant?  Use mono #0 (which is the only one, by the check above).
            // Multiple variants?  Use the mono for the current variant.
            let mono_idx =
                if fr.variants.is_none() { 0 }
                else { vr.index };

            let mr = &self.ana.monos[&(vr.func_id, mono_idx)];

            let lcx = LabeledTyCtxt::new(self.ana.arena());

            let sig = {
                let mut f = |l: &Option<_>| {
                    if let Some(v) = *l {
                        Some(mr.assign[v])
                    } else {
                        None
                    }
                };
                ownership::FnSig {
                    inputs: lcx.relabel_slice(fr.sig.inputs, &mut f),
                    output: lcx.relabel(fr.sig.output, &mut f),
                }
            };

            Some(sig)
        }

        fn pat_type(&mut self, p: &Pat) -> Option<Self::Type> {
            let hir_id = self.hir_map.opt_node_to_hir_id(p.id)?;
            let fn_def_id = self.hir_map.get_parent_did(hir_id);
            let f = self.ana.funcs.get(&fn_def_id)?;
            let local_var = f.locals.get(&p.span)?;

            // VTy -> PTy
            let mut map_fn = |opt_var: &Option<Var>| -> Option<ConcretePerm> {
                opt_var.map(|var| f.local_assign[var])
            };

            let lcx = LabeledTyCtxt::new(self.ana.arena());

            Some(lcx.relabel(local_var, &mut map_fn))
        }

        fn closure_sig(&mut self, _did: DefId) -> Option<Self::Signature> { None }
    }

    let source = AnalysisTypeSource {
        ana: &ana,
        hir_map: cx.hir_map(),
    };

    let s_ref = "ref".into_symbol();
    let s_mut = "mut".into_symbol();
    let s_box = "box".into_symbol();

    type_map::map_types(&cx.hir_map(), source, &st.krate(), |_source, ast_ty, lty| {
        dbg!((&ast_ty, &ast_ty.id, &lty.label));
        let p = match lty.label {
            Some(x) => x,
            None => return,
        };

        let label = match p {
            ConcretePerm::Read => s_ref,
            ConcretePerm::Write => s_mut,
            ConcretePerm::Move => s_box,
        };

        st.add_mark(ast_ty.id, label);
    });
}
