use super::DataflowConstraints;
use crate::context::{AnalysisCtxt, LTy, PermissionSet, PointerId};
use crate::util::{self, describe_rvalue, Callee, RvalueDesc};
use rustc_hir::def_id::DefId;
use rustc_middle::mir::{
    BinOp, Body, Mutability, Operand, Place, PlaceRef, ProjectionElem, Rvalue, Statement,
    StatementKind, Terminator, TerminatorKind,
};
use rustc_middle::ty::{SubstsRef, TyKind};

/// Visitor that walks over the MIR, computing types of rvalues/operands/places and generating
/// constraints as a side effect.
struct TypeChecker<'tcx, 'a> {
    acx: &'a AnalysisCtxt<'a, 'tcx>,
    mir: &'a Body<'tcx>,
    constraints: DataflowConstraints,
    equiv_constraints: Vec<(PointerId, PointerId)>,
}

impl<'tcx> TypeChecker<'tcx, '_> {
    fn add_edge(&mut self, src: PointerId, dest: PointerId) {
        // Copying `src` to `dest` can discard permissions, but can't add new ones.
        self.constraints.add_subset(dest, src);
    }

    fn add_equiv(&mut self, a: PointerId, b: PointerId) {
        self.equiv_constraints.push((a, b));
    }

    fn record_access(&mut self, ptr: PointerId, mutbl: Mutability) {
        eprintln!("record_access({:?}, {:?})", ptr, mutbl);
        if ptr == PointerId::NONE {
            return;
        }
        match mutbl {
            Mutability::Mut => {
                self.constraints
                    .add_all_perms(ptr, PermissionSet::READ | PermissionSet::WRITE);
            }
            Mutability::Not => {
                self.constraints.add_all_perms(ptr, PermissionSet::READ);
            }
        }
    }

    pub fn visit_place(&mut self, pl: Place<'tcx>, mutbl: Mutability) -> LTy<'tcx> {
        self.visit_place_ref(pl.as_ref(), mutbl)
    }

    pub fn visit_place_ref(&mut self, pl: PlaceRef<'tcx>, mutbl: Mutability) -> LTy<'tcx> {
        let mut lty = self.acx.local_tys[pl.local];
        let mut prev_deref_ptr = None;

        for proj in pl.projection {
            match proj {
                ProjectionElem::Deref => {
                    // All derefs except the last are loads, to retrieve the pointer for the next
                    // deref.  However, if the overall `Place` is used mutably (as indicated by
                    // `mutbl`), then the previous derefs must be `&mut` as well.  The last deref
                    // may not be a memory access at all; for example, `&(*p).x` does not actually
                    // access the memory at `*p`.
                    if let Some(ptr) = prev_deref_ptr.take() {
                        self.record_access(ptr, mutbl);
                    }
                    prev_deref_ptr = Some(lty.label);
                    assert_eq!(lty.args.len(), 1);
                    lty = lty.args[0];
                }

                ProjectionElem::Field(f, _field_ty) => match lty.ty.kind() {
                    TyKind::Tuple(..) => {
                        lty = lty.args[f.as_usize()];
                    }
                    _ => todo!("field of {:?}", lty),
                },

                ref proj => panic!("unsupported projection {:?} in {:?}", proj, pl),
            }
        }

        if let Some(ptr) = prev_deref_ptr.take() {
            self.record_access(ptr, mutbl);
        }

        lty
    }

    pub fn visit_rvalue(&mut self, rv: &Rvalue<'tcx>) -> PointerId {
        eprintln!("visit_rvalue({:?}), desc = {:?}", rv, describe_rvalue(rv));

        match describe_rvalue(rv) {
            Some(RvalueDesc::Project { base, proj: _ }) => {
                // TODO: mutability should probably depend on mutability of the output ref/ptr
                let base_ty = self.visit_place_ref(base, Mutability::Not);
                base_ty.label
            }
            Some(RvalueDesc::AddrOfLocal { local, proj: _ }) => self.acx.addr_of_local[local],
            None => match *rv {
                Rvalue::Use(ref op) => self.visit_operand(op).label,
                Rvalue::BinaryOp(BinOp::Offset, _) => todo!("visit_rvalue BinOp::Offset"),
                Rvalue::BinaryOp(..) => PointerId::NONE,
                Rvalue::CheckedBinaryOp(BinOp::Offset, _) => todo!("visit_rvalue BinOp::Offset"),
                Rvalue::CheckedBinaryOp(..) => PointerId::NONE,
                Rvalue::Cast(_, _, ty) => {
                    assert!(!matches!(ty.kind(), TyKind::RawPtr(..) | TyKind::Ref(..)));
                    PointerId::NONE
                }
                _ => panic!("TODO: handle assignment of {:?}", rv),
            },
        }
    }

    pub fn visit_operand(&mut self, op: &Operand<'tcx>) -> LTy<'tcx> {
        match *op {
            Operand::Copy(pl) => {
                self.visit_place(pl, Mutability::Not)
            }
            Operand::Move(pl) => {
                self.visit_place(pl, Mutability::Not)
            }
            Operand::Constant(ref c) => {
                let ty = c.ty();
                // TODO
                self.acx.lcx().label(ty, &mut |_| PointerId::NONE)
            }
        }
    }

    fn do_assign(&mut self, pl_ptr: PointerId, rv_ptr: PointerId) {
        if pl_ptr != PointerId::NONE || rv_ptr != PointerId::NONE {
            assert!(pl_ptr != PointerId::NONE);
            assert!(rv_ptr != PointerId::NONE);
            self.add_edge(rv_ptr, pl_ptr);
        }
    }

    fn do_unify_pointees(&mut self, pl_lty: LTy<'tcx>, rv_lty: LTy<'tcx>) {
        if pl_lty.label == PointerId::NONE && rv_lty.label == PointerId::NONE {
            return;
        }
        assert!(pl_lty.label != PointerId::NONE);
        assert!(rv_lty.label != PointerId::NONE);
        assert_eq!(pl_lty.args.len(), 1);
        assert_eq!(rv_lty.args.len(), 1);

        let pl_pointee = pl_lty.args[0];
        let rv_pointee = rv_lty.args[0];
        assert_eq!(pl_pointee.ty, rv_pointee.ty);
        for (pl_sub_lty, rv_sub_lty) in pl_pointee.iter().zip(rv_pointee.iter()) {
            eprintln!("equate {:?} = {:?}", pl_sub_lty, rv_sub_lty);
            if pl_sub_lty.label != PointerId::NONE || rv_sub_lty.label != PointerId::NONE {
                assert!(pl_sub_lty.label != PointerId::NONE);
                assert!(rv_sub_lty.label != PointerId::NONE);
                self.add_equiv(pl_sub_lty.label, rv_sub_lty.label);
            }
        }
    }

    pub fn visit_statement(&mut self, stmt: &Statement<'tcx>) {
        eprintln!("visit_statement({:?})", stmt);
        // TODO(spernsteiner): other `StatementKind`s will be handled in the future
        #[allow(clippy::single_match)]
        match stmt.kind {
            StatementKind::Assign(ref x) => {
                let (pl, ref rv) = **x;
                let pl_lty = self.visit_place(pl, Mutability::Mut);
                let pl_ptr = pl_lty.label;

                // TODO: combine these
                let rv_ptr = self.visit_rvalue(rv);
                let rv_lty = self.acx.type_of(rv);

                self.do_assign(pl_ptr, rv_ptr);
                self.do_unify_pointees(pl_lty, rv_lty);
            }
            // TODO(spernsteiner): handle other `StatementKind`s
            _ => (),
        }
    }

    pub fn visit_terminator(&mut self, term: &Terminator<'tcx>) {
        eprintln!("visit_terminator({:?})", term.kind);
        let tcx = self.acx.tcx();
        // TODO(spernsteiner): other `TerminatorKind`s will be handled in the future
        #[allow(clippy::single_match)]
        match term.kind {
            TerminatorKind::Call {
                ref func,
                ref args,
                destination,
                target: _,
                ..
            } => {
                let func_ty = func.ty(self.mir, tcx);
                eprintln!("callee = {:?}", util::ty_callee(tcx, func_ty));
                match util::ty_callee(tcx, func_ty) {
                    Some(Callee::PtrOffset { .. }) => {
                        // We handle this like a pointer assignment.
                        let pl_lty = self.visit_place(destination, Mutability::Mut);
                        assert!(args.len() == 2);
                        let rv_lty = self.visit_operand(&args[0]);
                        self.do_assign(pl_lty.label, rv_lty.label);
                        self.do_unify_pointees(pl_lty, rv_lty);
                        let perms = PermissionSet::OFFSET_ADD | PermissionSet::OFFSET_SUB;
                        self.constraints.add_all_perms(rv_lty.label, perms);
                    }
                    Some(Callee::Other { def_id, substs }) => {
                        self.visit_call_other(def_id, substs, args, destination);
                    }
                    None => {}
                }
            }
            // TODO(spernsteiner): handle other `TerminatorKind`s
            _ => (),
        }
    }

    fn visit_call_other(
        &mut self,
        def_id: DefId,
        substs: SubstsRef<'tcx>,
        args: &[Operand<'tcx>],
        dest: Place<'tcx>,
    ) {
        let sig = match self.acx.gacx.fn_sigs.get(&def_id) {
            Some(&x) => x,
            None => todo!("call to unknown function {:?}", def_id),
        };
        if substs.non_erasable_generics().next().is_some() {
            todo!("call to generic function {:?} {:?}", def_id, substs);
        }

        // Process pseudo-assignments from `args` to the types declared in `sig`.
        for (arg_op, &input_lty) in args.iter().zip(sig.inputs.iter()) {
            let arg_lty = self.visit_operand(arg_op);
            self.do_assign(input_lty.label, arg_lty.label);
            self.do_unify_pointees(input_lty, arg_lty);
        }

        // Process a pseudo-assignment from the return type declared in `sig` to `dest`.
        let dest_lty = self.visit_place(dest, Mutability::Mut);
        let output_lty = sig.output;
        self.do_assign(dest_lty.label, output_lty.label);
        self.do_unify_pointees(dest_lty, output_lty);
    }
}

pub fn visit<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    mir: &Body<'tcx>,
) -> (DataflowConstraints, Vec<(PointerId, PointerId)>) {
    let mut tc = TypeChecker {
        acx,
        mir,
        constraints: DataflowConstraints::default(),
        equiv_constraints: Vec::new(),
    };

    for bb_data in mir.basic_blocks().iter() {
        for stmt in bb_data.statements.iter() {
            tc.visit_statement(stmt);
        }
        tc.visit_terminator(bb_data.terminator());
    }

    (tc.constraints, tc.equiv_constraints)
}
