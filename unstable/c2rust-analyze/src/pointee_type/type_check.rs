use super::constraint_set::{CTy, ConstraintSet, VarTable};
use crate::context::{AnalysisCtxt, LTy, PointerId};
use crate::panic_detail;
use crate::util::{describe_rvalue, ty_callee, Callee, RvalueDesc, UnknownDefCallee};
use log::*;
use rustc_middle::mir::{
    BinOp, Body, Location, Operand, Place, PlaceRef, ProjectionElem, Rvalue, Statement,
    StatementKind, Terminator, TerminatorKind,
};
use rustc_middle::ty::{Ty, TyKind};

struct TypeChecker<'tcx, 'a> {
    acx: &'a AnalysisCtxt<'a, 'tcx>,
    mir: &'a Body<'tcx>,
    constraints: ConstraintSet<'tcx>,
    vars: &'a mut VarTable<'tcx>,
}

impl<'tcx> TypeChecker<'tcx, '_> {
    fn use_pointer_at_type(&mut self, ptr: PointerId, ty: impl Into<CTy<'tcx>>) {
        if ptr.is_none() {
            return;
        }
        let cty = ty.into();
        trace!("use_pointer_at_type({ptr:?}, {cty:?})");
        self.constraints.contains_type(ptr, cty);
    }

    fn define_pointer(&mut self, ptr: PointerId) {
        if ptr.is_none() {
            return;
        }
        trace!("define_pointer({ptr:?})");
        self.constraints.all_types_compatible(ptr);
    }

    fn define_pointer_with_type(&mut self, ptr: PointerId, ty: impl Into<CTy<'tcx>>) {
        if ptr.is_none() {
            return;
        }
        let cty = ty.into();
        trace!("define_pointer_with_type({ptr:?}, {cty:?})");
        self.constraints.all_types_compatible_with(ptr, cty);
    }

    fn assign(&mut self, lhs: PointerId, rhs: PointerId) {
        if lhs.is_none() || rhs.is_none() {
            return;
        }
        trace!("assign({lhs:?}, {rhs:?})");
        // If `lhs` flows to a use at type `T`, then `rhs` also flows to a use at type `T`.
        self.constraints.subset(lhs, rhs);
    }

    /// Visit a `Place`, adding constraints as needed.
    ///
    /// As a convenience, this returns the `LTy` of the place, identical to `acx.type_of(pl)`.
    pub fn visit_place(&mut self, pl: Place<'tcx>) -> LTy<'tcx> {
        self.visit_place_ref(pl.as_ref())
    }

    /// Visit a `PlaceRef`, adding constraints as needed.
    ///
    /// As a convenience, this returns the `LTy` of the place, identical to `acx.type_of(pl)`.
    pub fn visit_place_ref(&mut self, pl: PlaceRef<'tcx>) -> LTy<'tcx> {
        trace!("visit_place_ref({pl:?})");
        let mut lty = self.acx.type_of(pl.local);
        for proj in pl.projection {
            if let ProjectionElem::Deref = proj {
                debug_assert!(matches!(
                    lty.ty.kind(),
                    TyKind::RawPtr(..) | TyKind::Ref(..)
                ));
                debug_assert_eq!(lty.args.len(), 1);
                self.use_pointer_at_type(lty.label, lty.args[0]);
            }
            lty = self.acx.projection_lty(lty, proj);
        }
        debug_assert_eq!(lty, self.acx.type_of(pl));
        lty
    }

    /// Visit an `Rvalue`, adding constraints as needed.  
    pub fn visit_rvalue(&mut self, rv: &Rvalue<'tcx>, lty: LTy<'tcx>) {
        trace!("visit_rvalue({rv:?}, {lty:?})");

        if let Some(RvalueDesc::Project {
            base, proj: &[], ..
        }) = describe_rvalue(rv)
        {
            // Special case for no-op projections like `&*p`.  Since the pointer is passed through
            // unchanged, we don't require the pointee type to actually match the type used for the
            // paired deref and address-of operations.
            let base_lty = self.visit_place_ref(base);
            // Propagate the pointee types of `base` into `rv`, and from there into the LHS of the
            // enclosing assignment.
            self.assign(lty.label, base_lty.label);
            return;
        }

        // Aside from the above special case, there is no special handling for projections here
        // (e.g. `util::describe_rvalue`).  Instead, we treat projections like `&(*p).x` as a
        // separate use (`*p`) and def (`&_.x`), using the concrete type of each part.  `offset`
        // projections are handled separately in a more sophisticated way that avoids overly
        // constraining the pointee type.

        match *rv {
            Rvalue::Use(ref op) => self.visit_operand(op),
            Rvalue::Repeat(ref op, _) => self.visit_operand(op),
            Rvalue::Ref(_rg, _kind, pl) => {
                self.visit_place(pl);
                debug_assert!(matches!(lty.ty.kind(), TyKind::Ref(..)));
                debug_assert_eq!(lty.args.len(), 1);
                self.define_pointer_with_type(lty.label, lty.args[0]);
            }
            Rvalue::ThreadLocalRef(_) => {}
            Rvalue::AddressOf(_mutbl, pl) => {
                self.visit_place(pl);
                debug_assert!(matches!(lty.ty.kind(), TyKind::RawPtr(..)));
                debug_assert_eq!(lty.args.len(), 1);
                self.define_pointer_with_type(lty.label, lty.args[0]);
            }
            Rvalue::Len(pl) => {
                self.visit_place(pl);
            }
            Rvalue::Cast(_kind, ref op, _ty) => {
                self.visit_operand(op);

                let op_lty = self.acx.type_of(op);
                self.assign(lty.label, op_lty.label);
            }
            Rvalue::BinaryOp(bin_op, ref ops) | Rvalue::CheckedBinaryOp(bin_op, ref ops) => {
                assert_ne!(bin_op, BinOp::Offset, "BinOp::Offset special case NYI");
                let (ref op1, ref op2) = **ops;
                self.visit_operand(op1);
                self.visit_operand(op2);
            }
            Rvalue::NullaryOp(_, _) => {}
            Rvalue::UnaryOp(_, ref op) => self.visit_operand(op),
            Rvalue::Discriminant(pl) => {
                self.visit_place(pl);
            }
            Rvalue::Aggregate(_, ref ops) => {
                // FIXME: Needs dataflow edges between `ops` types and the rvalue's `lty`, similar
                // to the corresponding case in `dataflow::type_check`.
                for op in ops {
                    self.visit_operand(op);
                }
            }
            Rvalue::ShallowInitBox(ref op, _) => self.visit_operand(op),
            Rvalue::CopyForDeref(pl) => {
                self.visit_place(pl);
            }
        }
    }

    pub fn visit_operand(&mut self, op: &Operand<'tcx>) {
        trace!("visit_operand({op:?})");
        match *op {
            Operand::Copy(pl) | Operand::Move(pl) => {
                self.visit_place(pl);
            }
            Operand::Constant(ref _c) => {
                // TODO: addr of static may show up as `Operand::Constant`
            }
        }
    }

    pub fn visit_statement(&mut self, stmt: &Statement<'tcx>, loc: Location) {
        trace!(
            "visit_statement({:?} @ {:?})",
            stmt.kind,
            stmt.source_info.span
        );
        let _g = panic_detail::set_current_span(stmt.source_info.span);

        if let StatementKind::Assign(ref x) = stmt.kind {
            let (pl, ref rv) = **x;
            let pl_lty = self.visit_place(pl);

            let rv_lty = self.acx.type_of_rvalue(rv, loc);
            self.visit_rvalue(rv, rv_lty);

            self.assign(pl_lty.label, rv_lty.label);
        }
    }

    pub fn visit_terminator(&mut self, term: &Terminator<'tcx>, _loc: Location) {
        trace!(
            "visit_terminator({:?} @ {:?})",
            term.kind,
            term.source_info.span
        );
        let _g = panic_detail::set_current_span(term.source_info.span);
        let tcx = self.acx.tcx();

        if let TerminatorKind::Call {
            ref func,
            ref args,
            destination,
            target: _,
            ..
        } = term.kind
        {
            for op in args {
                self.visit_operand(op);
            }
            let dest_lty = self.visit_place(destination);

            let func = func.ty(self.mir, tcx);
            self.visit_call(func, args, dest_lty);
        }
    }

    pub fn visit_call(&mut self, func: Ty<'tcx>, args: &[Operand<'tcx>], dest_lty: LTy<'tcx>) {
        let tcx = self.acx.tcx();
        let callee = ty_callee(tcx, func);
        debug!("callee = {callee:?}");
        match callee {
            Callee::Trivial => {}
            Callee::LocalDef { def_id, substs } => {
                let sig = self
                    .acx
                    .gacx
                    .fn_sigs
                    .get(&def_id)
                    .unwrap_or_else(|| panic!("LFnSig not found for {def_id:?}"));
                if substs.non_erasable_generics().next().is_some() {
                    todo!("call to generic function {def_id:?} {substs:?}");
                }

                // Process pseudo-assignments from `args` to the types declared in `sig`.
                for (arg_op, &input_lty) in args.iter().zip(sig.inputs.iter()) {
                    let arg_lty = self.acx.type_of(arg_op);
                    self.assign(input_lty.label, arg_lty.label);
                }

                // Process a pseudo-assignment from the return type declared in `sig` to `dest`.
                let output_lty = sig.output;
                self.assign(dest_lty.label, output_lty.label);
            }
            Callee::UnknownDef(UnknownDefCallee::Direct {
                ty: _,
                def_id,
                substs: _,
                is_foreign: true,
            }) if self.acx.gacx.known_fn(def_id).is_some() => {
                // TODO: no good handling for this currently - might need to expand KnownFn to
                // include information about expected/required pointee types
            }
            Callee::UnknownDef(_) => {
                error!("TODO: visit Callee::{callee:?}");
            }

            Callee::PtrOffset { .. } => {
                // Normal uses of `offset` don't change the pointee type but only step
                // forward/backward through a uniform array.  We treat it as passing through any
                // pointee types unchanged, like an assignment.
                //
                // In the future, we might want to make this handling more precise.  When `offset`
                // is called on `*mut T`, we could compare `size_of::<T>()` and the offset amount
                // to the size of the concrete pointee type we inferred to see whether this is an
                // ordinary "step through the array" case or whether it's doing something unusual
                // like stepping from a struct to a specific field within the struct.
                assert_eq!(args.len(), 2);
                let arg_lty = self.acx.type_of(&args[0]);
                self.assign(dest_lty.label, arg_lty.label);
            }

            Callee::SliceAsPtr { .. } => {
                // The input is a `Ref`, so its underlying type is known precisely.
                assert_eq!(args.len(), 1);
                let arg_lty = self.acx.type_of(&args[0]);
                assert!(matches!(arg_lty.ty.kind(), TyKind::Ref(..)));
                assert_eq!(arg_lty.args.len(), 1);
                let slice_lty = arg_lty.args[0];
                assert!(matches!(slice_lty.ty.kind(), TyKind::Slice(..)));
                assert_eq!(slice_lty.args.len(), 1);
                let elem_lty = slice_lty.args[0];
                self.define_pointer_with_type(dest_lty.label, elem_lty);
            }

            Callee::Malloc | Callee::Calloc => {
                // Currently, we just treat this as a definition of unknown type and assert that a
                // single common pointee type can be found.  In the future, we might expand this to
                // assert that the inferred pointee type matches the size passed to `malloc`.
                self.define_pointer(dest_lty.label);
            }
            Callee::Realloc => {
                // Currently, we treat this as passing through the pointee type unchanged.
                //
                // In the future, we might check the new size as described for `malloc`.
                assert_eq!(args.len(), 2);
                let arg_lty = self.acx.type_of(&args[0]);
                self.assign(dest_lty.label, arg_lty.label);
            }
            Callee::Free => {
                // Here we create a fresh inference variable and associate it with the argument
                // pointer.  This doesn't constraint the type, since `free` doesn't reveal anything
                // about the concrete type of the data, but it does ensure that the pointee type of
                // the argument operand matches the pointee type of other pointers to the same
                // allocation, which lets us remove a `void*` cast during rewriting.
                let var = self.vars.fresh();
                assert_eq!(args.len(), 1);
                let arg_lty = self.acx.type_of(&args[0]);
                self.use_pointer_at_type(arg_lty.label, var);
            }

            Callee::Memcpy => {
                // We treat the `memcpy` as loading from `*src` and then storing to `*dest`.  The
                // type of the load and store is unknown at this point (it definitely isn't the
                // actual type of `*src`/`*dest`, which is `void`), so we introduce a new inference
                // variable and solve for it later.
                //
                // In the future, we might check the copy length as described for `malloc`.
                let var = self.vars.fresh();
                assert_eq!(args.len(), 3);
                let dest_arg_lty = self.acx.type_of(&args[0]);
                let src_arg_lty = self.acx.type_of(&args[1]);
                self.use_pointer_at_type(dest_arg_lty.label, var);
                self.use_pointer_at_type(src_arg_lty.label, var);
                self.assign(dest_lty.label, dest_arg_lty.label);
            }
            Callee::Memset => {
                // We treat this much like `memcpy`, but with only a store, not a load.
                //
                // In the future, we might check the length as described for `malloc`.
                let var = self.vars.fresh();
                assert_eq!(args.len(), 3);
                let dest_arg_lty = self.acx.type_of(&args[0]);
                self.use_pointer_at_type(dest_lty.label, var);
                self.assign(dest_lty.label, dest_arg_lty.label);
            }
            Callee::SizeOf { .. } => {}
            Callee::IsNull => {
                // No constraints.
            }
            Callee::Null { .. } => {
                // No constraints.
            }
        }
    }
}

pub fn visit<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    mir: &Body<'tcx>,
    vars: &mut VarTable<'tcx>,
) -> ConstraintSet<'tcx> {
    let mut tc = TypeChecker {
        acx,
        mir,
        constraints: ConstraintSet::default(),
        vars,
    };

    for (bb, bb_data) in mir.basic_blocks().iter_enumerated() {
        for (i, stmt) in bb_data.statements.iter().enumerate() {
            tc.visit_statement(
                stmt,
                Location {
                    block: bb,
                    statement_index: i,
                },
            );
        }
        tc.visit_terminator(
            bb_data.terminator(),
            Location {
                statement_index: bb_data.statements.len(),
                block: bb,
            },
        );
    }

    tc.constraints
}
