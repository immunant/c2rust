use super::DataflowConstraints;
use crate::context::{AnalysisCtxt, LTy, PermissionSet, PointerId};
use crate::panic_detail;
use crate::pointee_type::PointeeTypes;
use crate::pointer_id::PointerTable;
use crate::recent_writes::RecentWrites;
use crate::util::{
    self, describe_rvalue, is_transmutable_ptr_cast, ty_callee, Callee, RvalueDesc,
    UnknownDefCallee,
};
use assert_matches::assert_matches;
use either::Either;
use log::{debug, error};
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
///
///
/// # Optional fields
///
/// Several fields of this visitor are wrapped in `Option`.  These are accessed using helper
/// methods that do nothing when the field is `None`.  We use this to run the visitor in two
/// different modes, one for computing equivalence constraints and the other for computing all
/// other dataflow constraints.  The two kinds of constraints are closely related, and it's easiest
/// to implement them both in a single visitor.  But we compute them in two separate passes because
/// equivalence constraints can be used to improve the quality of the `pointee_type` analysis, and
/// pointee results are needed to compute the dataflow constraints.
struct TypeChecker<'tcx, 'a> {
    acx: &'a AnalysisCtxt<'a, 'tcx>,
    mir: &'a Body<'tcx>,
    recent_writes: &'a RecentWrites,
    pointee_types: Option<PointerTable<'a, PointeeTypes<'tcx>>>,
    /// Subset constraints on pointer permissions.  For example, this contains constraints like
    /// "the `PermissionSet` assigned to `PointerId` `l1` must be a subset of the `PermissionSet`
    /// assigned to `l2`".  See `dataflow::Constraint` for a full description of supported
    /// constraints.
    constraints: Option<DataflowConstraints>,
    /// Equivalence constraints on pointer permissions and flags.  An entry `(l1, l2)` in this list
    /// means that `PointerId`s `l1` and `l2` should be assigned exactly the same permissions and
    /// flags.  This ensures that the two pointers will be rewritten to the same safe type.
    ///
    /// Higher-level code eventually feeds the constraints recorded here into the union-find data
    /// structure defined in `crate::equiv`, so adding a constraint here has the effect of unifying
    /// the equivalence classes of the two `PointerId`s.
    equiv_constraints: Option<Vec<(PointerId, PointerId)>>,
}

impl<'tcx> TypeChecker<'tcx, '_> {
    fn add_edge(&mut self, src: PointerId, dest: PointerId) {
        // Copying `src` to `dest` can discard permissions, but can't add new ones.
        if let Some(ref mut constraints) = self.constraints {
            constraints.add_subset(dest, src);
        }
    }

    fn add_edge_except(&mut self, src: PointerId, dest: PointerId, except: PermissionSet) {
        // Copying `src` to `dest` can discard permissions, but can't add new ones,
        // except for the specified exceptions.
        if let Some(ref mut constraints) = self.constraints {
            constraints.add_subset_except(dest, src, except);
        }
    }

    /// Add `Constraint::AllPerms`, which requires `ptr` to have all of the permissions listed in
    /// `perms`.
    fn add_all_perms(&mut self, ptr: PointerId, perms: PermissionSet) {
        if let Some(ref mut constraints) = self.constraints {
            constraints.add_all_perms(ptr, perms);
        }
    }

    /// Add `Constraint::NoPerms`, which requires `ptr` to have none of the permissions listed in
    /// `perms`.
    fn add_no_perms(&mut self, ptr: PointerId, perms: PermissionSet) {
        if let Some(ref mut constraints) = self.constraints {
            constraints.add_no_perms(ptr, perms);
        }
    }

    fn add_equiv(&mut self, a: PointerId, b: PointerId) {
        if let Some(ref mut equiv_constraints) = self.equiv_constraints {
            equiv_constraints.push((a, b));
        }
    }

    fn pointee_type(&self, ptr: PointerId) -> Option<LTy<'tcx>> {
        assert!(!ptr.is_none());
        self.pointee_types
            .as_ref()
            .and_then(|pointee_types| pointee_types[ptr].get_sole_lty())
    }

    fn record_access(&mut self, ptr: PointerId, mutbl: Mutability) {
        debug!("record_access({:?}, {:?})", ptr, mutbl);
        if ptr == PointerId::NONE {
            return;
        }
        match mutbl {
            Mutability::Mut => {
                self.add_all_perms(ptr, PermissionSet::READ | PermissionSet::WRITE);
            }
            Mutability::Not => {
                self.add_all_perms(ptr, PermissionSet::READ);
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
            lty = self.acx.projection_lty(lty, proj);
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
                if !util::is_null_const_operand(op) {
                    panic!("Creating non-null pointers from exposed addresses not supported");
                }
                // The target type of the cast must not have `NON_NULL` permission.
                self.add_no_perms(to_lty.label, PermissionSet::NON_NULL);
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
                        self.do_assign_pointer_ids(to_lty.label, from_lty.label);
                        // TODO add other dataflow constraints
                    }
                    Some(false) => {
                        self.do_assign_pointer_ids(to_lty.label, from_lty.label);
                        ::log::warn!("TODO: unsupported ptr-to-ptr cast between pointee types not yet supported as safely transmutable: `{from_ty:?} as {to_ty:?}`");
                    }

                    None => {} // not a ptr cast (no dataflow constraints needed); let rustc typeck this
                };
            }
        }

        self.visit_operand(op)
    }

    pub fn visit_rvalue(&mut self, rv: &Rvalue<'tcx>, rvalue_lty: LTy<'tcx>) {
        let rv_desc = describe_rvalue(rv);
        debug!("visit_rvalue({rv:?}), desc = {rv_desc:?}");

        if let Some(desc) = rv_desc {
            match desc {
                RvalueDesc::Project {
                    base,
                    proj: _,
                    mutbl,
                } => {
                    self.visit_place_ref(base, mutbl);
                    let base_ptr = self.acx.type_of(base).label;
                    // Note that even non-ptr copy operations are treated as no-op `Project`s.
                    if !base_ptr.is_none() {
                        self.record_access(base_ptr, mutbl);
                    }
                }
                RvalueDesc::AddrOfLocal { .. } => {}
            }
            let derived_lty = self.acx.derived_type_of_rvalue(rv);
            self.do_assign(rvalue_lty, derived_lty);
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
                            let unresolved_field_lty = self.acx.gacx.field_ltys[&field.did];
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

    fn do_assign_except(&mut self, pl_lty: LTy<'tcx>, rv_lty: LTy<'tcx>, except: PermissionSet) {
        self.do_assign_pointer_ids_except(pl_lty.label, rv_lty.label, except);
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

    fn do_assign_pointer_ids_except(
        &mut self,
        pl_ptr: PointerId,
        rv_ptr: PointerId,
        except: PermissionSet,
    ) {
        if pl_ptr != PointerId::NONE || rv_ptr != PointerId::NONE {
            assert!(pl_ptr != PointerId::NONE);
            assert!(rv_ptr != PointerId::NONE);
            self.add_edge_except(rv_ptr, pl_ptr, except);
        }
    }

    /// Unify corresponding [`PointerId`]s in `lty1` and `lty2`.
    ///
    /// The two inputs must have identical underlying types.
    /// For any position where the underlying type has a pointer,
    /// this function unifies the [`PointerId`]s that `lty1` and `lty2` have at that position.
    /// For example, given
    ///
    /// ```
    /// # fn(
    /// lty1: *mut /*l1*/ *const /*l2*/ u8,
    /// lty2: *mut /*l3*/ *const /*l4*/ u8,
    /// # ) {}
    /// ```
    ///
    /// this function will unify `l1` with `l3` and `l2` with `l4`.
    fn do_unify(&mut self, lty1: LTy<'tcx>, lty2: LTy<'tcx>) {
        assert_eq!(
            self.acx.tcx().erase_regions(lty1.ty),
            self.acx.tcx().erase_regions(lty2.ty)
        );
        for (sub_lty1, sub_lty2) in lty1.iter().zip(lty2.iter()) {
            debug!("equate {:?} = {:?}", sub_lty1, sub_lty2);
            if sub_lty1.label != PointerId::NONE || sub_lty2.label != PointerId::NONE {
                assert!(sub_lty1.label != PointerId::NONE);
                assert!(sub_lty2.label != PointerId::NONE);
                self.add_equiv(sub_lty1.label, sub_lty2.label);
            }
        }
    }

    pub fn visit_statement(&mut self, stmt: &Statement<'tcx>, loc: Location) {
        debug!("visit_statement({:?})", stmt);

        let _g = panic_detail::set_current_span(stmt.source_info.span);

        // TODO(spernsteiner): other `StatementKind`s will be handled in the future
        #[allow(clippy::single_match)]
        match stmt.kind {
            StatementKind::Assign(ref x) => {
                let (pl, ref rv) = **x;
                self.visit_place(pl, Mutability::Mut);
                let pl_lty = self.acx.type_of(pl);

                let rv_lty = self.acx.type_of_rvalue(rv, loc);
                self.visit_rvalue(rv, rv_lty);

                if self.acx.has_field_projection(rv) {
                    // Fields don't get offset permissions propagated to their base pointer
                    self.do_assign_except(
                        pl_lty,
                        rv_lty,
                        PermissionSet::OFFSET_ADD | PermissionSet::OFFSET_SUB,
                    )
                } else {
                    self.do_assign(pl_lty, rv_lty);
                }
            }
            // TODO(spernsteiner): handle other `StatementKind`s
            _ => (),
        }
    }

    pub fn visit_terminator(&mut self, term: &Terminator<'tcx>, loc: Location) {
        debug!("visit_terminator({:?})", term.kind);
        let tcx = self.acx.tcx();
        let _g = panic_detail::set_current_span(term.source_info.span);
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
        debug!("callee = {callee:?}");
        match callee {
            Callee::Trivial => {}
            Callee::LocalDef { def_id, substs } => {
                self.visit_local_call(def_id, substs, args, destination);
            }
            Callee::UnknownDef(UnknownDefCallee::Direct {
                ty: _,
                def_id,
                substs,
                is_foreign: true,
            }) if self.acx.gacx.known_fn(def_id).is_some() => {
                // As this is actually a known `fn`, we can treat it as a normal local call.
                self.visit_local_call(def_id, substs, args, destination);
            }
            Callee::UnknownDef(_) => {
                error!("TODO: visit Callee::{callee:?}");
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
                self.add_all_perms(rv_lty.label, perms);
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
                self.visit_place(destination, Mutability::Mut);

                // The output of `malloc` is known not to be a stack pointer.
                let pl_lty = self.acx.type_of(destination);
                self.add_no_perms(pl_lty.label, PermissionSet::STACK);
            }
            Callee::Realloc => {
                let out_ptr = destination;
                let in_ptr = args[0]
                    .place()
                    .expect("Casts to/from null pointer are not yet supported");
                self.visit_place(out_ptr, Mutability::Mut);
                let pl_lty = self.acx.type_of(out_ptr);
                assert!(args.len() == 2);
                self.visit_place(in_ptr, Mutability::Not);
                let rv_lty = self.acx.type_of(in_ptr);

                // input needs FREE permission
                let perms = PermissionSet::FREE;
                self.add_all_perms(rv_lty.label, perms);

                // Output loses the STACK permission.
                self.add_no_perms(pl_lty.label, PermissionSet::STACK);

                // unify inner-most pointer types
                self.do_equivalence_nested(pl_lty, rv_lty);
            }
            Callee::Free => {
                let in_ptr = args[0]
                    .place()
                    .expect("Casts to/from null pointer are not yet supported");
                self.visit_place(destination, Mutability::Mut);
                assert!(args.len() == 1);

                let rv_lty = self.acx.type_of(in_ptr);
                let perms = PermissionSet::FREE;
                self.add_all_perms(rv_lty.label, perms);
            }
            Callee::Memcpy => {
                let out_ptr = destination;

                let dest_ptr = args[0]
                    .place()
                    .expect("Casts to/from null pointer are not yet supported");
                self.visit_place(out_ptr, Mutability::Mut);
                assert!(args.len() == 3);
                self.visit_place(dest_ptr, Mutability::Mut);
                let rv_lty = self.acx.type_of(dest_ptr);

                // Figure out whether we're copying one element or (possibly) several.
                let mut maybe_offset_perm = PermissionSet::OFFSET_ADD;
                let rv_ptr = rv_lty.label;
                if let Some(pointee_lty) = self.pointee_type(rv_ptr) {
                    if self.operand_is_size_of_t(loc, &args[2], pointee_lty.ty) {
                        // The size is exactly the (original) size of the pointee type, so this
                        // `memset` is operating on a single element only.
                        maybe_offset_perm = PermissionSet::empty();
                    }
                }
                debug!("memcpy at {:?} needs offset? {:?}", loc, maybe_offset_perm);

                // input needs WRITE permission
                let perms = PermissionSet::WRITE | maybe_offset_perm;
                self.add_all_perms(rv_lty.label, perms);

                let src_ptr = args[1]
                    .place()
                    .expect("Casts to/from null pointer are not yet supported");

                self.visit_place(out_ptr, Mutability::Mut);
                let dest_ptr_lty = self.acx.type_of(out_ptr);
                assert!(args.len() == 3);
                self.visit_place(src_ptr, Mutability::Not);
                let src_ptr_casted_lty = self.acx.type_of(src_ptr);

                // input needs READ permission
                let perms = PermissionSet::READ | maybe_offset_perm;
                self.add_all_perms(src_ptr_casted_lty.label, perms);

                // Perform a pseudo-assignment for *dest = *src.
                let src_ptr_lty = self.acx.type_of(src_ptr);
                self.do_equivalence_nested(dest_ptr_lty.args[0], src_ptr_lty.args[0]);
            }
            Callee::Memset => {
                let dest_ptr = args[0]
                    .place()
                    .expect("Casts to/from null pointer are not yet supported");
                self.visit_place(destination, Mutability::Mut);
                assert!(args.len() == 3);

                let rv_lty = self.acx.type_of(dest_ptr);

                // Figure out whether we're writing to one element or (possibly) several.
                let mut maybe_offset_perm = PermissionSet::OFFSET_ADD;
                let rv_ptr = rv_lty.label;
                if let Some(pointee_lty) = self.pointee_type(rv_ptr) {
                    if self.operand_is_size_of_t(loc, &args[2], pointee_lty.ty) {
                        // The size is exactly the (original) size of the pointee type, so this
                        // `memset` is operating on a single element only.
                        maybe_offset_perm = PermissionSet::empty();
                    }
                }
                debug!("memset at {:?} needs offset? {:?}", loc, maybe_offset_perm);

                let perms = PermissionSet::WRITE | maybe_offset_perm;
                self.add_all_perms(rv_lty.label, perms);

                // TODO: the return values of `memcpy` are rarely used
                // and may not always be casted to a non-void-pointer,
                // so avoid unifying for now
                // let pl_lty = self.acx.type_of(out_ptr);
                // self.do_equivalence_nested(pl_lty, rv_lty);
            }
            Callee::SizeOf { .. } => {}
            Callee::IsNull => {
                assert!(args.len() == 1);
                self.visit_operand(&args[0]);
            }
            Callee::Null { .. } => {
                assert!(args.is_empty());
                self.visit_place(destination, Mutability::Mut);
                let pl_lty = self.acx.type_of(destination);
                // We are assigning a null pointer to `destination`, so it must not have the
                // `NON_NULL` flag.
                self.add_no_perms(pl_lty.label, PermissionSet::NON_NULL);
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

    /// Check whether the value of `op` at `loc` is equal to `mem::size_of::<ty>`.  Returns true if
    /// the value is definitely equal, or false if unsure.
    fn operand_is_size_of_t(&self, loc: Location, op: &Operand<'tcx>, ty: Ty<'tcx>) -> bool {
        let tcx = self.acx.tcx();
        let mut loc = loc;
        let mut op = op;
        loop {
            let pl = match *op {
                Operand::Copy(pl) | Operand::Move(pl) => pl,

                // TODO: handle the case where `size_of` has already been const-evaluated
                Operand::Constant(_) => return false,
            };

            if pl.projection.len() > 0 {
                return false;
            }
            let l = pl.local;
            let write_loc = match self.recent_writes.get_write_before(loc, l) {
                Some(x) => x,
                None => return false,
            };

            match self.mir.stmt_at(write_loc) {
                Either::Left(stmt) => {
                    if let StatementKind::Assign(ref x) = stmt.kind {
                        match x.1 {
                            Rvalue::Use(ref rhs_op) => {
                                loc = write_loc;
                                op = rhs_op;
                                continue;
                            }
                            Rvalue::Cast(CastKind::Misc, ref rhs_op, _) => {
                                // Allow casting from `usize` to `size_t`, for example.
                                //
                                // Note: we currently don't check that the cast preserves the
                                // actual value, so we might wrongly return `true` in some
                                // pathological cases like `size_of::<BigStruct>() as u8`.
                                loc = write_loc;
                                op = rhs_op;
                                continue;
                            }
                            _ => {}
                        }
                    }
                }
                Either::Right(term) => {
                    if let TerminatorKind::Call { ref func, .. } = term.kind {
                        let func_ty = func.ty(self.mir, tcx);
                        let callee = ty_callee(tcx, func_ty);
                        if let Callee::SizeOf { ty: call_ty } = callee {
                            if call_ty == ty {
                                return true;
                            }
                        }
                    }
                }
            }
            return false;
        }
    }
}

fn visit_common<'tcx>(tc: &mut TypeChecker<'tcx, '_>, mir: &Body<'tcx>) {
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
}

/// Process a MIR body to compute dataflow constraints.
pub fn visit<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    mir: &Body<'tcx>,
    recent_writes: &RecentWrites,
    pointee_types: PointerTable<PointeeTypes<'tcx>>,
) -> DataflowConstraints {
    let mut tc = TypeChecker {
        acx,
        mir,
        recent_writes,
        pointee_types: Some(pointee_types),
        constraints: Some(DataflowConstraints::default()),
        equiv_constraints: None,
    };

    for (ptr, perms, neg_perms) in acx.string_literal_perms() {
        tc.add_all_perms(ptr, perms);
        tc.add_no_perms(ptr, neg_perms);
    }

    visit_common(&mut tc, mir);
    tc.constraints.unwrap()
}

/// Process a MIR body to compute equivalence constraints.
pub fn visit_equiv<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    mir: &Body<'tcx>,
    recent_writes: &RecentWrites,
) -> Vec<(PointerId, PointerId)> {
    let mut tc = TypeChecker {
        acx,
        mir,
        recent_writes,
        pointee_types: None,
        constraints: None,
        equiv_constraints: Some(Vec::new()),
    };

    visit_common(&mut tc, mir);
    tc.equiv_constraints.unwrap()
}
