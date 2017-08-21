//! Read user annotations, in the form of marks on type nodes, and use them to populate parts of
//! the analysis context.

use std::cmp;

use rustc::hir::def_id::DefId;
use rustc::ty::TyCtxt;
use syntax::ast;

use command::CommandState;
use driver;
use type_map::{self, TypeSource};

use super::{LTy, LFnSig, ConcretePerm, Perm};
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

