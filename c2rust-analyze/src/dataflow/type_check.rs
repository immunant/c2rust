use super::DataflowConstraints;
use crate::c_void_casts::CVoidCastDirection;
use crate::context::{AnalysisCtxt, LTy, PermissionSet, PointerId};
use crate::util::{
    describe_rvalue, is_null_const, is_transmutable_ptr_cast, ty_callee, Callee, RvalueDesc,
};
use assert_matches::assert_matches;
use rustc_hir::def_id::DefId;
use rustc_middle::mir::{
    AggregateKind, BinOp, Body, CastKind, Location, Mutability, Operand, Place, PlaceRef,
    ProjectionElem, Rvalue, Statement, StatementKind, Terminator, TerminatorKind,
};
use rustc_middle::ty::adjustment::PointerCast;
use rustc_middle::ty::{SubstsRef, Ty, TyKind};

/// Visitor that walks over the MIR, computing types of rvalues/operands/places and generating
/// constraints as a side effect.
///
/// In general, the constraints we generate for an assignment are as follows:
///
/// * The outermost pointer type of the destination must have a subset of the permissions of the
///   outermost pointer type of the source.  That is, the assignment may drop permissions as it
///   copies the pointer from source to destination, but it cannot add any permissions.  Dropping
///   permissions during the assignment corresponds to inserting a cast between pointer types.
/// * All pointer types except the outermost must have equal permissions and flags in the source
///   and destination.  This is necessary because we generally can't change the inner pointer type
///   when performing a cast (for example, it's possible to convert `&[&[T]]` to `&&[T]` - take the
///   address of the first element - but not to `&[&T]]`).
struct TypeChecker<'tcx, 'a> {
    acx: &'a AnalysisCtxt<'a, 'tcx>,
    mir: &'a Body<'tcx>,
    /// Subset constraints on pointer permissions.  For example, this contains constraints like
    /// "the `PermissionSet` assigned to `PointerId` `l1` must be a subset of the `PermissionSet`
    /// assigned to `l2`".  See `dataflow::Constraint` for a full description of supported
    /// constraints.
    constraints: DataflowConstraints,
    /// Equivalence constraints on pointer permissions and flags.  An entry `(l1, l2)` in this list
    /// means that `PointerId`s `l1` and `l2` should be assigned exactly the same permissions and
    /// flags.  This ensures that the two pointers will be rewritten to the same safe type.
    ///
    /// Higher-level code eventually feeds the constraints recorded here into the union-find data
    /// structure defined in `crate::equiv`, so adding a constraint here has the effect of unifying
    /// the equivalence classes of the two `PointerId`s.
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

    pub fn visit_place(&mut self, pl: Place<'tcx>, mutbl: Mutability) {
        self.visit_place_ref(pl.as_ref(), mutbl)
    }

    pub fn visit_place_ref(&mut self, pl: PlaceRef<'tcx>, mutbl: Mutability) {
        let mut lty = self.acx.type_of(pl.local);
        let mut prev_deref_ptr = None;

        for proj in pl.projection {
            if let ProjectionElem::Deref = proj {
                // All derefs except the last are loads, to retrieve the pointer for the next
                // deref.  However, if the overall `Place` is used mutably (as indicated by
                // `mutbl`), then the previous derefs must be `&mut` as well.  The last deref
                // may not be a memory access at all; for example, `&(*p).x` does not actually
                // access the memory at `*p`.
                if let Some(ptr) = prev_deref_ptr.take() {
                    self.record_access(ptr, mutbl);
                }
                prev_deref_ptr = Some(lty.label);
            }
            lty = self.acx.project(lty, proj);
        }

        if let Some(ptr) = prev_deref_ptr.take() {
            self.record_access(ptr, mutbl);
        }
    }

    fn visit_cast(&mut self, cast_kind: CastKind, op: &Operand<'tcx>, to_lty: LTy<'tcx>) {
        let to_ty = to_lty.ty;
        let from_lty = self.acx.type_of(op);
        let from_ty = from_lty.ty;

        match cast_kind {
            CastKind::PointerFromExposedAddress => {
                // We support only one case here, which is the case of null pointers
                // constructed via casts such as `0 as *const T`
                if !op.constant().copied().map(is_null_const).unwrap_or(false) {
                    panic!("Creating non-null pointers from exposed addresses not supported");
                }
            }
            CastKind::PointerExposeAddress => {
                // Allow, as [`CastKind::PointerFromExposedAddress`] is the dangerous one,
                // and we'll catch (not allow) that above.
                // This becomes no longer a pointer, so we don't need to add any dataflow constraints
                // (until we try to handle [`CastKind::PointerFromExposedAddress`], if we do).
            }
            CastKind::Pointer(ptr_cast) => {
                // All of these [`PointerCast`]s are type checked by rustc already.
                // They don't involve arbitrary raw ptr to raw ptr casts
                // ([PointerCast::MutToConstPointer`] doesn't allow changing types),
                // which we need to check for safe transmutability,
                // and which are (currently) covered in [`CastKind::Misc`].
                // That's why there's a `match` here that does nothing;
                // it ensures if [`PointerCast`] is changed in a future `rustc` version,
                // this won't compile until we've checked that this reasoning is still accurate.
                match ptr_cast {
                    PointerCast::ReifyFnPointer => {}
                    PointerCast::UnsafeFnPointer => {}
                    PointerCast::ClosureFnPointer(_) => {}
                    PointerCast::MutToConstPointer => {}
                    PointerCast::ArrayToPointer => {}
                    PointerCast::Unsize => {}
                }
                self.do_assign_pointer_ids(to_lty.label, from_lty.label)
                // TODO add other dataflow constraints
            }
            CastKind::Misc => {
                match is_transmutable_ptr_cast(from_ty, to_ty) {
                    Some(true) => {
                        // TODO add other dataflow constraints
                    },
                    Some(false) => ::log::error!("TODO: unsupported ptr-to-ptr cast between pointee types not yet supported as safely transmutable: `{from_ty:?} as {to_ty:?}`"),
                    None => {}, // not a ptr cast (no dataflow constraints needed); let rustc typeck this
                };
            }
        }

        self.visit_operand(op)
    }

    pub fn visit_rvalue(&mut self, rv: &Rvalue<'tcx>, rvalue_lty: LTy<'tcx>) {
        let rv_desc = describe_rvalue(rv);
        eprintln!("visit_rvalue({rv:?}), desc = {rv_desc:?}");

        if let Some(desc) = rv_desc {
            match desc {
                RvalueDesc::Project { base, proj: _ } => {
                    // TODO: mutability should probably depend on mutability of the output ref/ptr
                    self.visit_place_ref(base, Mutability::Not);
                }
                RvalueDesc::AddrOfLocal { .. } => {}
            }
            return;
        }

        match *rv {
            Rvalue::Use(ref op) => self.visit_operand(op),
            Rvalue::Repeat(ref op, _) => {
                assert!(rvalue_lty.ty.is_array());
                assert_matches!(rvalue_lty.args, [elem_lty] => {
                    // Pseudo-assign from the operand to the element type of the array.
                    let op_lty = self.acx.type_of(op);
                    /*
                        TODO: This is the right thing to do here, though currently
                        it's a no-op because type_of_rvalue (whose result is passed
                        into this function as lty) constructs its result using
                        type_of(op), so elem_lty and op_lty will always be identical.
                        Eventually we'll want to change this so the Rvalue::Repeat gets
                        its own LTy with its own PointerIds, similar to the handling of
                        array aggregates. This would let us detect the need for a
                        cast on the operand of the repeat (we can't cast [&[u32]; 3]
                        to [&u32; 3], but we could cast the operand from &[u32] to
                        &u32 before doing the repeat, e.g. let x = [&my_slice[0]; 3];).
                    */
                    self.do_assign(elem_lty, op_lty);
                });
            }
            Rvalue::Ref(..) => {
                unreachable!("Rvalue::Ref should be handled by describe_rvalue instead")
            }
            Rvalue::ThreadLocalRef(..) => todo!("visit_rvalue ThreadLocalRef"),
            Rvalue::AddressOf(..) => {
                unreachable!("Rvalue::AddressOf should be handled by describe_rvalue instead")
            }
            Rvalue::Len(pl) => {
                self.visit_place(pl, Mutability::Not);
            }
            Rvalue::Cast(cast_kind, ref op, ty) => {
                assert_eq!(ty, rvalue_lty.ty);
                self.visit_cast(cast_kind, op, rvalue_lty);
            }
            Rvalue::BinaryOp(BinOp::Offset, _) => todo!("visit_rvalue BinOp::Offset"),
            Rvalue::BinaryOp(_, ref ops) => {
                self.visit_operand(&ops.0);
                self.visit_operand(&ops.1);
            }
            Rvalue::CheckedBinaryOp(BinOp::Offset, _) => todo!("visit_rvalue BinOp::Offset"),
            Rvalue::CheckedBinaryOp(_, ref ops) => {
                self.visit_operand(&ops.0);
                self.visit_operand(&ops.1);
            }
            Rvalue::NullaryOp(..) => {}
            Rvalue::UnaryOp(_, ref op) => {
                self.visit_operand(op);
            }

            Rvalue::Aggregate(ref kind, ref ops) => {
                for op in ops {
                    self.visit_operand(op);
                }
                match **kind {
                    AggregateKind::Array(..) => {
                        assert!(matches!(rvalue_lty.kind(), TyKind::Array(..)));
                        let elem_lty = assert_matches!(rvalue_lty.args, [elem_lty] => {
                            elem_lty
                        });
                        // Pseudo-assign from each operand to the element type of the array.
                        for op in ops {
                            let op_lty = self.acx.type_of(op);
                            self.do_assign(elem_lty, op_lty);
                        }
                    }
                    AggregateKind::Adt(adt_did, ..) => {
                        let base_adt_def = self.acx.tcx().adt_def(adt_did);
                        let fields = &base_adt_def.non_enum_variant().fields;
                        for (field, op) in fields.iter().zip(ops.iter()) {
                            let op_lty = self.acx.type_of(op);
                            let unresolved_field_lty = self.acx.gacx.field_tys[&field.did];
                            // resolve the generic type arguments in `field_lty` by referencing the `Ty` of `op`
                            let resolved_field_lty =
                                self.acx.lcx().subst(unresolved_field_lty, rvalue_lty.args);
                            // Pseudo-assign from each operand to the element type of the field.
                            self.do_assign(resolved_field_lty, op_lty);
                        }
                    }
                    AggregateKind::Tuple => {
                        assert!(matches!(rvalue_lty.kind(), TyKind::Tuple(..)));
                        // Pseudo-assign from each operand to the element type of the tuple.
                        for (op, elem_lty) in ops.iter().zip(rvalue_lty.args.iter()) {
                            let op_lty = self.acx.type_of(op);
                            self.do_assign(elem_lty, op_lty);
                        }
                    }
                    ref kind => todo!("Rvalue::Aggregate({:?})", kind),
                }
            }

            _ => panic!("TODO: handle assignment of {:?}", rv),
        }
    }

    pub fn visit_operand(&mut self, op: &Operand<'tcx>) {
        match *op {
            Operand::Copy(pl) | Operand::Move(pl) => {
                self.visit_place(pl, Mutability::Not);
            }
            Operand::Constant(ref _c) => {
                // TODO: addr of static may show up as `Operand::Constant`
            }
        }
    }

    fn do_equivalence_nested(&mut self, pl_lty: LTy<'tcx>, rv_lty: LTy<'tcx>) {
        // Add equivalence constraints for all nested pointers beyond the top level.
        assert_eq!(
            self.acx.tcx().erase_regions(pl_lty.ty),
            self.acx.tcx().erase_regions(rv_lty.ty)
        );
        for (&pl_sub_lty, &rv_sub_lty) in pl_lty.args.iter().zip(rv_lty.args.iter()) {
            self.do_unify(pl_sub_lty, rv_sub_lty);
        }
    }

    fn do_assign(&mut self, pl_lty: LTy<'tcx>, rv_lty: LTy<'tcx>) {
        // If the top-level types are pointers, add a dataflow edge indicating that `rv` flows into
        // `pl`.
        self.do_assign_pointer_ids(pl_lty.label, rv_lty.label);
        self.do_equivalence_nested(pl_lty, rv_lty);
    }

    /// Add a dataflow edge indicating that `rv_ptr` flows into `pl_ptr`.  If both `PointerId`s are
    /// `NONE`, this has no effect.
    fn do_assign_pointer_ids(&mut self, pl_ptr: PointerId, rv_ptr: PointerId) {
        if pl_ptr != PointerId::NONE || rv_ptr != PointerId::NONE {
            assert!(pl_ptr != PointerId::NONE);
            assert!(rv_ptr != PointerId::NONE);
            self.add_edge(rv_ptr, pl_ptr);
        }
    }

    /// Unify corresponding [`PointerId`]s in `pl_lty` and `rv_lty`.
    ///
    /// The two inputs must have identical underlying types.
    /// For any position where the underlying type has a pointer,
    /// this function unifies the [`PointerId`]s that `pl_lty` and `rv_lty` have at that position.
    /// For example, given
    ///
    /// ```
    /// # fn(
    /// pl_lty: *mut /*l1*/ *const /*l2*/ u8,
    /// rv_lty: *mut /*l3*/ *const /*l4*/ u8,
    /// # ) {}
    /// ```
    ///
    /// this function will unify `l1` with `l3` and `l2` with `l4`.
    fn do_unify(&mut self, pl_lty: LTy<'tcx>, rv_lty: LTy<'tcx>) {
        let rv_ty = self.acx.tcx().erase_regions(rv_lty.ty);
        let pl_ty = self.acx.tcx().erase_regions(pl_lty.ty);
        assert_eq!(rv_ty, pl_ty);
        for (sub_pl_lty, sub_rv_lty) in pl_lty.iter().zip(rv_lty.iter()) {
            eprintln!("equate {:?} = {:?}", sub_pl_lty, sub_rv_lty);
            if sub_pl_lty.label != PointerId::NONE || sub_rv_lty.label != PointerId::NONE {
                assert!(sub_pl_lty.label != PointerId::NONE);
                assert!(sub_rv_lty.label != PointerId::NONE);
                self.add_equiv(sub_pl_lty.label, sub_rv_lty.label);
            }
        }
    }

    pub fn visit_statement(&mut self, stmt: &Statement<'tcx>, loc: Location) {
        eprintln!("visit_statement({:?})", stmt);

        if self.acx.c_void_casts.should_skip_stmt(loc) {
            return;
        }

        // TODO(spernsteiner): other `StatementKind`s will be handled in the future
        #[allow(clippy::single_match)]
        match stmt.kind {
            StatementKind::Assign(ref x) => {
                let (pl, ref rv) = **x;
                self.visit_place(pl, Mutability::Mut);
                let pl_lty = self.acx.type_of(pl);

                let rv_lty = self.acx.type_of_rvalue(rv, loc);
                self.visit_rvalue(rv, rv_lty);
                self.do_assign(pl_lty, rv_lty);
            }
            // TODO(spernsteiner): handle other `StatementKind`s
            _ => (),
        }
    }

    pub fn visit_terminator(&mut self, term: &Terminator<'tcx>, loc: Location) {
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
                let func = func.ty(self.mir, tcx);
                self.visit_call(loc, func, args, destination);
            }
            // TODO(spernsteiner): handle other `TerminatorKind`s
            _ => (),
        }
    }

    pub fn visit_call(
        &mut self,
        loc: Location,
        func: Ty<'tcx>,
        args: &[Operand<'tcx>],
        destination: Place<'tcx>,
    ) {
        let tcx = self.acx.tcx();
        let callee = ty_callee(tcx, func);
        eprintln!("callee = {callee:?}");
        match callee {
            Callee::Trivial => {}
            Callee::UnknownDef { .. } => {
                log::error!("TODO: visit Callee::{callee:?}");
            }

            Callee::LocalDef { def_id, substs } => {
                self.visit_local_call(def_id, substs, args, destination);
            }

            Callee::PtrOffset { .. } => {
                // We handle this like a pointer assignment.
                self.visit_place(destination, Mutability::Mut);
                let pl_lty = self.acx.type_of(destination);
                assert!(args.len() == 2);
                self.visit_operand(&args[0]);
                let rv_lty = self.acx.type_of(&args[0]);
                self.do_assign(pl_lty, rv_lty);
                let perms = PermissionSet::OFFSET_ADD | PermissionSet::OFFSET_SUB;
                self.constraints.add_all_perms(rv_lty.label, perms);
            }

            Callee::SliceAsPtr { elem_ty, .. } => {
                // We handle this like an assignment, but with some adjustments due to the
                // difference in input and output types.
                self.visit_place(destination, Mutability::Mut);
                let pl_lty = self.acx.type_of(destination);
                assert!(args.len() == 1);
                self.visit_operand(&args[0]);
                let rv_lty = self.acx.type_of(&args[0]);

                // Map `rv_lty = &[i32]` to `rv_elem_lty = i32`
                let rv_pointee_lty = rv_lty.args[0];
                let rv_elem_lty = match *rv_pointee_lty.kind() {
                    TyKind::Array(..) | TyKind::Slice(..) => rv_pointee_lty.args[0],
                    TyKind::Str => self.acx.lcx().label(elem_ty, &mut |_| PointerId::NONE),
                    _ => unreachable!(),
                };

                // Map `pl_lty = *mut i32` to `pl_elem_lty = i32`
                let pl_elem_lty = pl_lty.args[0];

                self.do_unify(pl_elem_lty, rv_elem_lty);
                self.do_assign_pointer_ids(pl_lty.label, rv_lty.label);
            }

            Callee::Malloc | Callee::Calloc => {
                let out_ptr = self.acx.c_void_casts.get_adjusted_place_or_default_to(
                    loc,
                    CVoidCastDirection::From,
                    destination,
                );
                self.visit_place(out_ptr, Mutability::Mut);
            }
            Callee::Realloc => {
                let out_ptr = self.acx.c_void_casts.get_adjusted_place_or_default_to(
                    loc,
                    CVoidCastDirection::From,
                    destination,
                );
                let in_ptr = args[0]
                    .place()
                    .expect("Casts to/from null pointer are not yet supported");
                let in_ptr = self.acx.c_void_casts.get_adjusted_place_or_default_to(
                    loc,
                    CVoidCastDirection::To,
                    in_ptr,
                );
                self.visit_place(out_ptr, Mutability::Mut);
                let pl_lty = self.acx.type_of(out_ptr);
                assert!(args.len() == 2);
                self.visit_place(in_ptr, Mutability::Not);
                let rv_lty = self.acx.type_of(in_ptr);

                // input needs FREE permission
                let perms = PermissionSet::FREE;
                self.constraints.add_all_perms(rv_lty.label, perms);

                // unify inner-most pointer types
                self.do_equivalence_nested(pl_lty, rv_lty);
            }
            Callee::Free => {
                let in_ptr = args[0]
                    .place()
                    .expect("Casts to/from null pointer are not yet supported");
                let in_ptr = self.acx.c_void_casts.get_adjusted_place_or_default_to(
                    loc,
                    CVoidCastDirection::To,
                    in_ptr,
                );
                self.visit_place(destination, Mutability::Mut);
                assert!(args.len() == 1);

                let rv_lty = self.acx.type_of(in_ptr);
                let perms = PermissionSet::FREE;
                self.constraints.add_all_perms(rv_lty.label, perms);
            }

            Callee::IsNull => {
                assert!(args.len() == 1);
                self.visit_operand(&args[0]);
            }
        }
    }

    /// Visit a local call, where local means
    /// local to the current crate with a static, known definition.
    ///
    /// See [`Callee::LocalDef`].
    fn visit_local_call(
        &mut self,
        def_id: DefId,
        substs: SubstsRef<'tcx>,
        args: &[Operand<'tcx>],
        dest: Place<'tcx>,
    ) {
        let sig = self.acx.gacx.fn_sigs.get(&def_id)
            .unwrap_or_else(|| panic!("Callee::LocalDef LFnSig not found (unknown calls should've been Callee::UnknownDef): {def_id:?}"));
        if substs.non_erasable_generics().next().is_some() {
            todo!("call to generic function {def_id:?} {substs:?}");
        }

        // Process pseudo-assignments from `args` to the types declared in `sig`.
        for (arg_op, &input_lty) in args.iter().zip(sig.inputs.iter()) {
            self.visit_operand(arg_op);
            let arg_lty = self.acx.type_of(arg_op);
            self.do_assign(input_lty, arg_lty);
        }

        // Process a pseudo-assignment from the return type declared in `sig` to `dest`.
        self.visit_place(dest, Mutability::Mut);
        let dest_lty = self.acx.type_of(dest);
        let output_lty = sig.output;
        self.do_assign(dest_lty, output_lty);
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

    for (ptr, perms) in acx.string_literal_perms() {
        tc.constraints.add_all_perms(ptr, perms);
    }

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

    (tc.constraints, tc.equiv_constraints)
}
