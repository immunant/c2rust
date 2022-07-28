#![feature(rustc_private)]
extern crate either;
extern crate rustc_arena;
extern crate rustc_ast;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;

use crate::context::{
    AnalysisCtxt, FlagSet, GlobalAnalysisCtxt, GlobalAssignment, LTy, LocalAssignment,
    PermissionSet, PointerId,
};
use rustc_index::vec::IndexVec;
use rustc_middle::mir::{BindingForm, Body, LocalDecl, LocalInfo};
use rustc_middle::ty::query::{ExternProviders, Providers};
use rustc_middle::ty::{Ty, TyCtxt, TyKind, WithOptConstParam};
use rustc_session::Session;
use rustc_span::def_id::LocalDefId;
use rustc_span::Span;
use std::env;

mod borrowck;
mod context;
mod dataflow;
mod equiv;
mod expr_rewrite;
mod labeled_ty;
mod pointer_id;
mod type_desc;
mod util;

fn inspect_mir<'tcx>(
    gacx: &mut GlobalAnalysisCtxt<'tcx>,
    gasn: &mut GlobalAssignment,
    def: WithOptConstParam<LocalDefId>,
    mir: &Body<'tcx>,
) {
    let tcx = gacx.tcx;
    let name = tcx.item_name(def.to_global().did);
    eprintln!("\nprocessing function {:?}", name);

    let mut acx = gacx.enter_function(mir);

    // Label all pointers in local variables.
    // TODO: also label pointers in Rvalue::Cast (and ShallowInitBox?)
    assert!(acx.local_tys.is_empty());
    acx.local_tys = IndexVec::with_capacity(mir.local_decls.len());
    for (local, decl) in mir.local_decls.iter_enumerated() {
        let lty = assign_pointer_ids(&acx, decl.ty);
        let l = acx.local_tys.push(lty);
        assert_eq!(local, l);

        let ptr = acx.new_pointer();
        let l = acx.addr_of_local.push(ptr);
        assert_eq!(local, l);
    }

    let (dataflow, equiv_constraints) = self::dataflow::generate_constraints(&acx, mir);

    let mut lasn =
        LocalAssignment::new(acx.num_pointers(), PermissionSet::UNIQUE, FlagSet::empty());
    let mut asn = gasn.and(&mut lasn);

    {
        let mut equiv = asn.equiv_mut();
        for (a, b) in equiv_constraints {
            equiv.unify(a, b);
        }
    }

    dataflow.propagate(&mut asn.perms_mut());

    borrowck::borrowck_mir(&acx, &dataflow, &mut asn.perms_mut(), name.as_str(), mir);

    dataflow.propagate_cell(&mut asn);

    eprintln!("final labeling for {:?}:", name);
    let lcx1 = crate::labeled_ty::LabeledTyCtxt::new(tcx);
    let lcx2 = crate::labeled_ty::LabeledTyCtxt::new(tcx);
    let lcx3 = crate::labeled_ty::LabeledTyCtxt::new(tcx);
    for (local, decl) in mir.local_decls.iter_enumerated() {
        let addr_of1 = asn.perms()[acx.addr_of_local[local]];
        let ty1 = lcx1.relabel(acx.local_tys[local], &mut |lty| {
            if lty.label == PointerId::NONE {
                PermissionSet::empty()
            } else {
                asn.perms()[lty.label]
            }
        });
        eprintln!(
            "{:?} ({}): addr_of = {:?}, type = {:?}",
            local,
            describe_local(tcx, decl),
            addr_of1,
            ty1,
        );

        let addr_of2 = asn.flags()[acx.addr_of_local[local]];
        let ty2 = lcx2.relabel(acx.local_tys[local], &mut |lty| {
            if lty.label == PointerId::NONE {
                FlagSet::empty()
            } else {
                asn.flags()[lty.label]
            }
        });
        eprintln!(
            "{:?} ({}): addr_of flags = {:?}, type flags = {:?}",
            local,
            describe_local(tcx, decl),
            addr_of2,
            ty2,
        );

        let addr_of3 = asn.equiv_mut().rep(acx.addr_of_local[local]);
        let ty3 = lcx3.relabel(acx.local_tys[local], &mut |lty| {
            if lty.label == PointerId::NONE {
                PointerId::NONE
            } else {
                asn.equiv_mut().rep(lty.label)
            }
        });
        eprintln!(
            "{:?} ({}): addr_of = {:?}, type = {:?}",
            local,
            describe_local(tcx, decl),
            addr_of3,
            ty3,
        );
    }

    eprintln!("\ntype assignment for {:?}:", name);
    for (local, decl) in mir.local_decls.iter_enumerated() {
        // TODO: apply `Cell` if `addr_of_local` indicates it's needed
        let ty = type_desc::convert_type(&acx, acx.local_tys[local], &asn);
        eprintln!("{:?} ({}): {:?}", local, describe_local(tcx, decl), ty,);
    }

    eprintln!();
    let rewrites = expr_rewrite::gen_expr_rewrites(&acx, &asn, mir);
    for rw in &rewrites {
        eprintln!(
            "at {:?} ({}, {:?}):",
            rw.loc.stmt,
            describe_span(tcx, rw.loc.span),
            rw.loc.sub,
        );
        for kind in &rw.kinds {
            eprintln!("  {:?}", kind);
        }
    }
}

fn assign_pointer_ids<'tcx>(acx: &AnalysisCtxt<'_, 'tcx>, ty: Ty<'tcx>) -> LTy<'tcx> {
    acx.lcx().label(ty, &mut |ty| match ty.kind() {
        TyKind::Ref(_, _, _) | TyKind::RawPtr(_) => acx.new_pointer(),
        _ => PointerId::NONE,
    })
}

fn describe_local(tcx: TyCtxt, decl: &LocalDecl) -> String {
    let mut span = decl.source_info.span;
    if let Some(ref info) = decl.local_info {
        if let LocalInfo::User(ref binding_form) = **info {
            let binding_form = binding_form.as_ref().assert_crate_local();
            if let BindingForm::Var(ref v) = *binding_form {
                span = v.pat_span;
            }
        }
    }
    describe_span(tcx, span)
}

fn describe_span(tcx: TyCtxt, span: Span) -> String {
    let s = tcx.sess.source_map().span_to_snippet(span).unwrap();
    let s = {
        let mut s2 = String::new();
        for word in s.split_ascii_whitespace() {
            if !s2.is_empty() {
                s2.push(' ');
            }
            s2.push_str(word);
        }
        s2
    };

    let (src1, src2, src3) = if s.len() > 20 {
        (&s[..15], " ... ", &s[s.len() - 5..])
    } else {
        (&s[..], "", "")
    };
    let line = tcx.sess.source_map().lookup_char_pos(span.lo()).line;
    format!("{}: {}{}{}", line, src1, src2, src3)
}

struct AnalysisCallbacks;

impl rustc_driver::Callbacks for AnalysisCallbacks {
    fn after_expansion<'tcx>(
        &mut self,
        compiler: &rustc_interface::interface::Compiler,
        queries: &'tcx rustc_interface::Queries<'tcx>,
    ) -> rustc_driver::Compilation {
        queries.global_ctxt().unwrap().peek_mut().enter(|tcx| {
            let mut gacx = GlobalAnalysisCtxt::new(tcx);
            let mut gasn = GlobalAssignment::new(0, PermissionSet::UNIQUE, FlagSet::empty());
            for ldid in tcx.hir().body_owners() {
                eprintln!("\n\n ===== analyze {:?} =====", ldid);
                let ldid_const = WithOptConstParam::unknown(ldid);
                let mir = tcx.mir_built(ldid_const);
                inspect_mir(&mut gacx, &mut gasn, ldid_const, &mir.borrow());
            }
        });
        rustc_driver::Compilation::Continue
    }
}

fn main() -> rustc_interface::interface::Result<()> {
    let args = env::args().collect::<Vec<_>>();
    rustc_driver::RunCompiler::new(&args, &mut AnalysisCallbacks).run()
}
