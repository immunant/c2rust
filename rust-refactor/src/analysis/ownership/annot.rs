//! Read user annotations, in the form of marks on type nodes, and use them to populate parts of
//! the analysis context.

use std::cmp;
use std::str::FromStr;

use arena::DroplessArena;
use rustc::hir::def_id::DefId;
use rustc::ty::TyCtxt;
use syntax::ast;
use syntax::visit::{self, Visitor};

use command::CommandState;
use driver;
use type_map::{self, TypeSource};
use visit::Visit;

use super::{LTy, LFnSig, ConcretePerm, Perm, Var};
use super::constraint::ConstraintSet;
use super::context::Ctxt;


struct LTySource<'a, 'gcx: 'tcx, 'tcx: 'a> {
    cx: &'a mut Ctxt<'tcx>,
    tcx: TyCtxt<'a, 'gcx, 'tcx>,

    // XXX - bit of a hack.  We keep the def id of the last call to `fn_sig`, and refer to that
    // inside the map_types callback to figure out the right scope for any SigVars in the type.
    // This relies on the fact that map_types invokes the next TypeSource method only once all
    // callback invocations resulting for the previous TypeSource call have been made.
    last_sig_did: Option<DefId>,
}

impl<'a, 'gcx, 'tcx> TypeSource for LTySource<'a, 'gcx, 'tcx> {
    type Type = LTy<'tcx>;
    type Signature = LFnSig<'tcx>;

    fn expr_type(&mut self, e: &ast::Expr) -> Option<Self::Type> {
        self.last_sig_did = None;
        None
    }

    fn pat_type(&mut self, p: &ast::Pat) -> Option<Self::Type> {
        self.last_sig_did = None;
        None
    }

    fn def_type(&mut self, did: DefId) -> Option<Self::Type> {
        self.last_sig_did = None;
        Some(self.cx.static_ty(did, self.tcx))
    }

    fn fn_sig(&mut self, did: DefId) -> Option<Self::Signature> {
        self.last_sig_did = Some(did);
        Some(self.cx.fn_sig(did, self.tcx))
    }

    fn closure_sig(&mut self, did: DefId) -> Option<Self::Signature> {
        self.last_sig_did = None;
        // TODO - should probably support this
        None
    }
}

impl<'tcx> type_map::Signature<LTy<'tcx>> for LFnSig<'tcx> {
    fn num_inputs(&self) -> usize {
        self.inputs.len()
    }

    fn input(&self, idx: usize) -> LTy<'tcx> {
        self.inputs[idx]
    }

    fn output(&self) -> LTy<'tcx> {
        self.output
    }
}

pub fn handle_marks<'a, 'hir, 'gcx, 'tcx>(cx: &mut Ctxt<'tcx>,
                                          st: &CommandState,
                                          dcx: &driver::Ctxt<'a, 'hir, 'gcx, 'tcx>) {
    let mut fixed_vars = Vec::new();
    {
        let source = LTySource {
            cx: cx,
            tcx: dcx.ty_ctxt(),
            last_sig_did: None,
        };

        type_map::map_types(dcx.hir_map(), source, &st.krate(), |source, ast_ty, lty| {
            eprintln!("match {:?} ({:?}) with {:?}", ast_ty, ast_ty.id, lty);
            if st.marked(ast_ty.id, "box") {
                if let Some(p) = lty.label {
                    fixed_vars.push((p, source.last_sig_did, ConcretePerm::Move));
                }
            }

            if st.marked(ast_ty.id, "mut") {
                if let Some(p) = lty.label {
                    fixed_vars.push((p, source.last_sig_did, ConcretePerm::Write));
                }
            }

            if st.marked(ast_ty.id, "ref") {
                if let Some(p) = lty.label {
                    fixed_vars.push((p, source.last_sig_did, ConcretePerm::Read));
                }
            }
        });
    }

    // For any marked types that are in signatures, add constraints to the parent function's cset.
    for (p, did, min_perm) in fixed_vars {
        eprintln!("FIXED VAR: {:?} = {:?} (in {:?})", p, min_perm, did);
        match p {
            Perm::StaticVar(v) => {
                let new_perm = cmp::max(min_perm, cx.static_assign[v]);
                cx.static_assign[v] = new_perm;
            },
            Perm::SigVar(_) => {
                let did = did.expect("expected DefId for SigVar");
                cx.fn_summ(did, dcx.ty_ctxt()).cset.add(Perm::Concrete(min_perm), p);
            }
            _ => panic!("expected StaticVar or SigVar, but got {:?}", p),
        }
    }
}



struct AttrVisitor<'ast> {
    fn_attrs: Vec<(ast::NodeId, &'ast [ast::Attribute])>,
}

impl<'ast> Visitor<'ast> for AttrVisitor<'ast> {
    fn visit_item(&mut self, i: &'ast ast::Item) {
        match i.node {
            ast::ItemKind::Fn(..) => {
                self.fn_attrs.push((i.id, &i.attrs));
            },
            _ => {},
        }

        visit::walk_item(self, i);
    }
    // TODO: impl items, statics, consts, struct fields
}

pub fn handle_attrs<'a, 'hir, 'gcx, 'tcx>(cx: &mut Ctxt<'tcx>,
                                          st: &CommandState,
                                          dcx: &driver::Ctxt<'a, 'hir, 'gcx, 'tcx>) {

    let krate = st.krate();
    let mut v = AttrVisitor {
        fn_attrs: Vec::new(),
    };
    krate.visit(&mut v);

    eprintln!("HANDLE_ATTRS: found {} funcs", v.fn_attrs.len());


    for (node_id, attrs) in v.fn_attrs {
        let def_id = match_or!([dcx.hir_map().opt_local_def_id(node_id)] Some(x) => x; continue);

        let summ = cx.fn_summ(def_id, dcx.ty_ctxt());
        for attr in attrs {
            let meta = match_or!([attr.meta()] Some(x) => x; continue);
            match &meta.name.as_str() as &str {
                "ownership_constraints" => {
                    let cset = parse_ownership_constraints(&meta, dcx.ty_arena())
                        .unwrap_or_else(|e| panic!("bad #[ownership_constraints] for {:?}: {}",
                                                   def_id, e));

                    eprintln!("found constraints for {:?}:", def_id);
                    for &(a, b) in cset.iter() {
                        eprintln!("  {:?} <= {:?}", a, b);
                    }

                    summ.attr_cset = Some(cset);
                },

                _ => {},
            }
        }
    }
}



fn meta_item_list(meta: &ast::MetaItem) -> Result<&[ast::NestedMetaItem], &'static str> {
    match meta.node {
        ast::MetaItemKind::List(ref xs) => Ok(xs),
        _ => Err("expected MetaItemKind::List"),
    }
}

fn nested_meta_item(nmeta: &ast::NestedMetaItem) -> Result<&ast::MetaItem, &'static str> {
    match nmeta.node {
        ast::NestedMetaItemKind::MetaItem(ref m) => Ok(m),
        _ => Err("expected NestedMetaItemKind::MetaItem"),
    }
}

fn parse_ownership_constraints<'tcx>(meta: &ast::MetaItem,
                                     arena: &'tcx DroplessArena)
                                     -> Result<ConstraintSet<'tcx>, &'static str> {
    use syntax::ast::*;
    let args = meta_item_list(meta)?;

    let mut cset = ConstraintSet::new();
    for arg in args {
        let arg = nested_meta_item(arg)?;
        if !arg.check_name("le") {
            return Err("expected `le(a, b)` in `ownership_constraints`");
        }

        let perms = meta_item_list(arg)?;
        if perms.len() != 2 {
            return Err("expected exactly two arguments in `le`");
        }

        let a = parse_perm(nested_meta_item(&perms[0])?, arena)?;
        let b = parse_perm(nested_meta_item(&perms[1])?, arena)?;
        cset.add(a, b);
    }

    Ok(cset)
}

fn parse_perm<'tcx>(meta: &ast::MetaItem,
                    arena: &'tcx DroplessArena)
                    -> Result<Perm<'tcx>, &'static str> {
    use syntax::ast::*;
    if meta.check_name("min") {
        let args = meta_item_list(meta)?;
        if args.len() == 0 {
            return Err("`min` requires at least one argument");
        }

        let mut perms = Vec::with_capacity(args.len());
        for arg in args {
            let arg_meta = nested_meta_item(arg)?;
            let perm = parse_perm(arg_meta, arena)?;
            perms.push(perm);
        }

        let perms = arena.alloc_slice(&perms);
        Ok(Perm::Min(perms))
    } else {
        match meta.node {
            MetaItemKind::Word => {},
            _ => return Err("permission values should not have arguments"),
        }

        let name = meta.name.as_str();
        match &name as &str {
            "READ" => return Ok(Perm::read()),
            "WRITE" => return Ok(Perm::write()),
            "MOVE" => return Ok(Perm::move_()),
            _ => {},
        }

        if !name.starts_with("_") {
            return Err("invalid permission variable");
        }
        let idx = FromStr::from_str(&name[1..]).map_err(|_| "invalid permission variable")?;
        Ok(Perm::SigVar(Var(idx)))
    }
}
