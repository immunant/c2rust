use crate::labeled_ty::{LabeledTy, LabeledTyCtxt};
use crate::pointer_id::{
    GlobalPointerTable, LocalPointerTable, NextGlobalPointerId, NextLocalPointerId, PointerTable,
    PointerTableMut,
};
use crate::util::{self, describe_rvalue, RvalueDesc};
use crate::AssignPointerIds;
use bitflags::bitflags;
use indexmap::IndexSet;
use rustc_hir::def_id::DefId;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::{
    Body, CastKind, Field, HasLocalDecls, Local, LocalDecls, Location, Operand, Place, PlaceElem,
    PlaceRef, Rvalue,
};
use rustc_middle::ty::adjustment::PointerCast;
use rustc_middle::ty::{AdtDef, FieldDef, Ty, TyCtxt, TyKind};
use std::collections::HashMap;
use std::ops::Index;

bitflags! {
    #[derive(Default)]
    pub struct PermissionSet: u16 {
        /// The value(s) accessible through this pointer can be read.
        const READ = 0x0001;
        /// The value(s) accessible through this pointer can be written.
        const WRITE = 0x0002;
        /// This pointer is unique: using an alias not derived from this
        /// pointer invalidates this pointer, after which it is not valid to use.
        const UNIQUE = 0x0004;
        /// This pointer is linear-typed.  Copying a `LINEAR` pointer to another `LINEAR` location
        /// moves the pointer and invalidates the source of the copy.  (However, a
        /// copy-and-downcast to a non-`LINEAR` location is a borrow, which does not invalidate the
        /// source pointer.)
        const LINEAR = 0x0008;
        /// This pointer can be offset in the positive direction.
        ///
        /// Offsetting the pointer in an unknown direction requires both `OFFSET_ADD` and
        /// `OFFSET_SUB`.  Offsetting by zero requires neither `OFFSET_ADD` nor `OFFSET_SUB`.
        const OFFSET_ADD = 0x0010;
        /// This pointer can be offset in the negative direction.
        const OFFSET_SUB = 0x0020;
        /// This pointer can be freed.
        const FREE = 0x0040;
    }
}

bitflags! {
    /// Additional flags describing a given pointer type.  These are mainly derived from
    /// `PermissionSet`, but don't follow the normal subtyping rules and propagation algorithm.
    #[derive(Default)]
    pub struct FlagSet: u16 {
        /// The pointee type is wrapped in `Cell`.  This is tracked separately from the
        /// `PermissionSet` since it depends on the past/future uses of the pointer in an unusual
        /// way, and it can't be freely discarded (or its inverse freely added) as is the case for
        /// everything in `PermissionSet`.
        const CELL = 0x0001;
    }
}

pub use crate::pointer_id::PointerId;

pub type LTy<'tcx> = LabeledTy<'tcx, PointerId>;
pub type LTyCtxt<'tcx> = LabeledTyCtxt<'tcx, PointerId>;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct LFnSig<'tcx> {
    pub inputs: &'tcx [LTy<'tcx>],
    pub output: LTy<'tcx>,
}

pub struct GlobalAnalysisCtxt<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub lcx: LTyCtxt<'tcx>,

    pub fn_sigs: HashMap<DefId, LFnSig<'tcx>>,

    pub field_tys: HashMap<DefId, LTy<'tcx>>,

    next_ptr_id: NextGlobalPointerId,
}

pub struct AnalysisCtxt<'a, 'tcx> {
    pub gacx: &'a mut GlobalAnalysisCtxt<'tcx>,

    pub local_decls: &'a LocalDecls<'tcx>,
    pub local_tys: IndexVec<Local, LTy<'tcx>>,
    pub c_void_ptrs: IndexSet<Place<'tcx>>,
    pub addr_of_local: IndexVec<Local, PointerId>,
    /// Types for certain [`Rvalue`]s.  Some `Rvalue`s introduce fresh [`PointerId`]s; to keep
    /// those `PointerId`s consistent, the `Rvalue`'s type must be stored rather than recomputed on
    /// the fly.
    pub rvalue_tys: HashMap<Location, LTy<'tcx>>,
    /// A mapping for substituting [`Place`]s adhering to the
    /// following pattern
    /// ```
    /// _1 = malloc(...);
    /// _2 = _1 as *mut T;
    /// ```
    ///
    /// In this case, `_1` would be mapped to `_2`, which is indicative
    /// of the amended statement:
    /// ```
    /// _2 = malloc(...);
    /// ```
    pub special_casts: HashMap<Place<'tcx>, Place<'tcx>>,

    next_ptr_id: NextLocalPointerId,
}

pub struct AnalysisCtxtData<'tcx> {
    local_tys: IndexVec<Local, LTy<'tcx>>,
    addr_of_local: IndexVec<Local, PointerId>,
    rvalue_tys: HashMap<Location, LTy<'tcx>>,
    next_ptr_id: NextLocalPointerId,
}

impl<'tcx> GlobalAnalysisCtxt<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>) -> GlobalAnalysisCtxt<'tcx> {
        GlobalAnalysisCtxt {
            tcx,
            lcx: LabeledTyCtxt::new(tcx),
            fn_sigs: HashMap::new(),
            field_tys: HashMap::new(),
            next_ptr_id: NextGlobalPointerId::new(),
        }
    }

    pub fn function_context<'a>(&'a mut self, mir: &'a Body<'tcx>) -> AnalysisCtxt<'a, 'tcx> {
        AnalysisCtxt::new(self, mir)
    }

    pub fn function_context_with_data<'a>(
        &'a mut self,
        mir: &'a Body<'tcx>,
        data: AnalysisCtxtData<'tcx>,
    ) -> AnalysisCtxt<'a, 'tcx> {
        AnalysisCtxt::from_data(self, mir, data)
    }

    pub fn new_pointer(&mut self) -> PointerId {
        self.next_ptr_id.next()
    }

    pub fn num_pointers(&self) -> usize {
        self.next_ptr_id.num_pointers()
    }

    /// Update all [`PointerId`]s in `self`, replacing each `p` with `map[p]`.  Also sets the "next
    /// [`PointerId`]" counter to `counter`.  `map` and `counter` are usually computed together via
    /// [`GlobalEquivSet::renumber`][crate::equiv::GlobalEquivSet::renumber].
    pub fn remap_pointers(
        &mut self,
        map: &GlobalPointerTable<PointerId>,
        counter: NextGlobalPointerId,
    ) {
        let GlobalAnalysisCtxt {
            tcx: _,
            lcx,
            ref mut fn_sigs,
            ref mut field_tys,
            ref mut next_ptr_id,
        } = *self;

        for sig in fn_sigs.values_mut() {
            sig.inputs = lcx.mk_slice(
                &sig.inputs
                    .iter()
                    .map(|&lty| remap_lty_pointers(lcx, map, lty))
                    .collect::<Vec<_>>(),
            );
            sig.output = remap_lty_pointers(lcx, map, sig.output);
        }

        for labeled_field in field_tys.values_mut() {
            *labeled_field = remap_lty_pointers(lcx, map, labeled_field);
        }

        *next_ptr_id = counter;
    }

    pub fn assign_pointer_to_field(&mut self, field: &FieldDef) {
        let lty = self.assign_pointer_ids(self.tcx.type_of(field.did));
        self.field_tys.insert(field.did, lty);
    }

    pub fn assign_pointer_to_fields(&mut self, did: DefId) {
        for field in self.tcx.adt_def(did).all_fields() {
            self.assign_pointer_to_field(field);
        }
    }
}

impl<'a, 'tcx> AnalysisCtxt<'a, 'tcx> {
    pub fn new(
        gacx: &'a mut GlobalAnalysisCtxt<'tcx>,
        mir: &'a Body<'tcx>,
    ) -> AnalysisCtxt<'a, 'tcx> {
        AnalysisCtxt {
            gacx,
            local_decls: &mir.local_decls,
            local_tys: IndexVec::new(),
            c_void_ptrs: IndexSet::new(),
            special_casts: HashMap::new(),
            addr_of_local: IndexVec::new(),
            rvalue_tys: HashMap::new(),
            next_ptr_id: NextLocalPointerId::new(),
        }
    }

    pub fn from_data(
        gacx: &'a mut GlobalAnalysisCtxt<'tcx>,
        mir: &'a Body<'tcx>,
        data: AnalysisCtxtData<'tcx>,
    ) -> AnalysisCtxt<'a, 'tcx> {
        let AnalysisCtxtData {
            local_tys,
            addr_of_local,
            rvalue_tys,
            next_ptr_id,
        } = data;
        AnalysisCtxt {
            gacx,
            local_decls: &mir.local_decls,
            local_tys,
            c_void_ptrs: IndexSet::new(),
            special_casts: HashMap::new(),
            addr_of_local,
            rvalue_tys,
            next_ptr_id,
        }
    }

    pub fn into_data(self) -> AnalysisCtxtData<'tcx> {
        AnalysisCtxtData {
            local_tys: self.local_tys,
            addr_of_local: self.addr_of_local,
            rvalue_tys: self.rvalue_tys,
            next_ptr_id: self.next_ptr_id,
        }
    }

    pub fn tcx(&self) -> TyCtxt<'tcx> {
        self.gacx.tcx
    }

    pub fn lcx(&self) -> LTyCtxt<'tcx> {
        self.gacx.lcx
    }

    pub fn new_pointer(&mut self) -> PointerId {
        self.next_ptr_id.next()
    }

    pub fn num_pointers(&self) -> usize {
        self.next_ptr_id.num_pointers()
    }

    pub fn type_of<T: TypeOf<'tcx>>(&self, x: T) -> LTy<'tcx> {
        x.type_of(self)
    }

    pub fn ptr_of<T: TypeOf<'tcx>>(&self, x: T) -> Option<PointerId> {
        let ptr = self.type_of(x).label;
        if ptr == PointerId::NONE {
            None
        } else {
            Some(ptr)
        }
    }

    pub fn type_of_rvalue(&self, rv: &Rvalue<'tcx>, loc: Location) -> LTy<'tcx> {
        if let Some(&lty) = self.rvalue_tys.get(&loc) {
            return lty;
        }

        if let Some(desc) = describe_rvalue(rv) {
            let ty = rv.ty(self, self.tcx());
            if matches!(ty.kind(), TyKind::Ref(..) | TyKind::RawPtr(..)) {
                let (pointee_lty, proj, ptr) = match desc {
                    RvalueDesc::Project { base, proj } => {
                        let base_lty = self.type_of(base);
                        eprintln!(
                            "rvalue = {:?}, desc = {:?}, base_lty = {:?}",
                            rv, desc, base_lty
                        );
                        (
                            self.project(base_lty, &PlaceElem::Deref),
                            proj,
                            base_lty.label,
                        )
                    }
                    RvalueDesc::AddrOfLocal { local, proj } => {
                        (self.type_of(local), proj, self.addr_of_local[local])
                    }
                };

                let mut pointee_lty = pointee_lty;
                for p in proj {
                    pointee_lty = self.project(pointee_lty, p);
                }

                let ty = rv.ty(self, self.tcx());
                let pointee_ty = match *ty.kind() {
                    TyKind::Ref(_, ty, _) => ty,
                    TyKind::RawPtr(tm) => tm.ty,
                    _ => unreachable!(
                        "got RvalueDesc for non-pointer Rvalue {:?} (of type {:?})",
                        rv, ty,
                    ),
                };
                assert_eq!(
                    self.tcx().erase_regions(pointee_ty),
                    self.tcx().erase_regions(pointee_lty.ty)
                );

                let args = self.lcx().mk_slice(&[pointee_lty]);
                return self.lcx().mk(ty, args, ptr);
            }
        }

        match *rv {
            Rvalue::Use(ref op) => self.type_of(op),
            Rvalue::CopyForDeref(pl) => self.type_of(pl),
            Rvalue::Repeat(ref op, _) => {
                let op_lty = self.type_of(op);
                let ty = rv.ty(self, self.tcx());
                assert!(matches!(ty.kind(), TyKind::Array(..)));
                let args = self.lcx().mk_slice(&[op_lty]);
                self.lcx().mk(ty, args, PointerId::NONE)
            }
            Rvalue::Ref(..) | Rvalue::AddressOf(..) => {
                unreachable!("should be handled by describe_rvalue case above")
            }
            Rvalue::ThreadLocalRef(..) => todo!("type_of ThreadLocalRef"),
            Rvalue::Cast(CastKind::Pointer(PointerCast::Unsize), ref op, ty) => {
                let pointee_ty = match *ty.kind() {
                    TyKind::Ref(_, ty, _) => ty,
                    TyKind::RawPtr(tm) => tm.ty,
                    _ => unreachable!("unsize cast has non-pointer output {:?}?", ty),
                };

                let op_lty = self.type_of(op);
                assert!(matches!(
                    op_lty.kind(),
                    TyKind::Ref(..) | TyKind::RawPtr(..)
                ));
                assert_eq!(op_lty.args.len(), 1);
                let op_pointee_lty = op_lty.args[0];

                match *pointee_ty.kind() {
                    TyKind::Slice(elem_ty) => {
                        assert!(matches!(op_pointee_lty.kind(), TyKind::Array(..)));
                        assert_eq!(op_pointee_lty.args.len(), 1);
                        let elem_lty = op_pointee_lty.args[0];
                        assert_eq!(elem_lty.ty, elem_ty);
                        assert_eq!(op_pointee_lty.label, PointerId::NONE);

                        let pointee_lty =
                            self.lcx()
                                .mk(pointee_ty, op_pointee_lty.args, op_pointee_lty.label);
                        let args = self.lcx().mk_slice(&[pointee_lty]);
                        self.lcx().mk(ty, args, op_lty.label)
                    }
                    _ => label_no_pointers(self, ty),
                }
            }
            Rvalue::Cast(_, ref op, ty) => {
                let op_lty = self.type_of(op);

                // We support this category of pointer casts as a special case.
                let op_is_ptr = matches!(op_lty.ty.kind(), TyKind::Ref(..) | TyKind::RawPtr(..));
                let op_pointee = op_is_ptr.then(|| op_lty.args[0]);
                let ty_pointee = match *ty.kind() {
                    TyKind::Ref(_, ty, _) => Some(ty),
                    TyKind::RawPtr(tm) => Some(tm.ty),
                    _ => None,
                };
                if op_pointee.is_some() && op_pointee.map(|lty| lty.ty) == ty_pointee {
                    // The source and target types are both pointers, and they have identical
                    // pointee types.  We label the target type with the same `PointerId`s as the
                    // source type in all positions.  This works because the two types have the
                    // same structure.
                    return self.lcx().mk(ty, op_lty.args, op_lty.label);
                }

                label_no_pointers(self, ty)
            }
            Rvalue::Len(..)
            | Rvalue::BinaryOp(..)
            | Rvalue::CheckedBinaryOp(..)
            | Rvalue::NullaryOp(..)
            | Rvalue::UnaryOp(..)
            | Rvalue::Discriminant(..) => {
                let ty = rv.ty(self, self.tcx());
                label_no_pointers(self, ty)
            }
            Rvalue::Aggregate(ref _kind, ref _vals) => todo!("type_of Aggregate: rv = {rv:?}"),
            Rvalue::ShallowInitBox(ref _op, _ty) => todo!("type_of ShallowInitBox: rv = {rv:?}"),
        }
    }

    pub fn project(&self, lty: LTy<'tcx>, proj: &PlaceElem<'tcx>) -> LTy<'tcx> {
        let adt_func = |_lty: LTy, adt_def: AdtDef, field: Field| {
            let field_def = &adt_def.non_enum_variant().fields[field.index()];
            let field_def_name = field_def.name;
            eprintln!("projecting into {adt_def:?}.{field_def_name:}");
            let res = *self.gacx.field_tys.get(&field_def.did).unwrap_or_else(|| {
                panic!("Could not find {adt_def:?}.{field_def_name:?} in field type map")
            });
            res
        };
        util::lty_project(lty, proj, adt_func)
    }
}

impl<'tcx> AnalysisCtxtData<'tcx> {
    /// Update all [`PointerId`]s in `self`, replacing each `p` with `map[p]`.  Also sets the "next
    /// [`PointerId`]" counter to `counter`.  `map` and `counter` are usually computed together via
    /// [`LocalEquivSet::renumber`][crate::equiv::LocalEquivSet::renumber].
    pub fn remap_pointers(
        &mut self,
        lcx: LTyCtxt<'tcx>,
        map: PointerTable<PointerId>,
        counter: NextLocalPointerId,
    ) {
        let AnalysisCtxtData {
            ref mut local_tys,
            ref mut addr_of_local,
            ref mut rvalue_tys,
            ref mut next_ptr_id,
        } = *self;

        for lty in local_tys {
            *lty = remap_lty_pointers(lcx, &map, lty);
        }

        for ptr in addr_of_local {
            if !ptr.is_none() {
                *ptr = map[*ptr];
            }
        }

        for lty in rvalue_tys.values_mut() {
            *lty = remap_lty_pointers(lcx, &map, lty);
        }

        *next_ptr_id = counter;
    }

    pub fn num_pointers(&self) -> usize {
        self.next_ptr_id.num_pointers()
    }
}

/// For every [`PointerId`] `p` that appears in `lty`, replace `p` with `map[p]` (except that
/// [`PointerId::NONE`] is left unchanged) and return the updated `LTy`.
fn remap_lty_pointers<'tcx, T>(lcx: LTyCtxt<'tcx>, map: &T, lty: LTy<'tcx>) -> LTy<'tcx>
where
    T: Index<PointerId, Output = PointerId>,
{
    lcx.relabel(lty, &mut |inner_lty| {
        if inner_lty.label.is_none() {
            PointerId::NONE
        } else {
            map[inner_lty.label]
        }
    })
}

impl<'tcx> HasLocalDecls<'tcx> for AnalysisCtxt<'_, 'tcx> {
    fn local_decls(&self) -> &LocalDecls<'tcx> {
        self.local_decls
    }
}

pub trait TypeOf<'tcx> {
    fn type_of(&self, acx: &AnalysisCtxt<'_, 'tcx>) -> LTy<'tcx>;
}

impl<'tcx, T: TypeOf<'tcx>> TypeOf<'tcx> for &T {
    fn type_of(&self, acx: &AnalysisCtxt<'_, 'tcx>) -> LTy<'tcx> {
        (**self).type_of(acx)
    }
}

impl<'tcx> TypeOf<'tcx> for Local {
    fn type_of(&self, acx: &AnalysisCtxt<'_, 'tcx>) -> LTy<'tcx> {
        acx.local_tys[*self]
    }
}

impl<'tcx> TypeOf<'tcx> for Place<'tcx> {
    fn type_of(&self, acx: &AnalysisCtxt<'_, 'tcx>) -> LTy<'tcx> {
        acx.type_of(self.as_ref())
    }
}

impl<'tcx> TypeOf<'tcx> for PlaceRef<'tcx> {
    fn type_of(&self, acx: &AnalysisCtxt<'_, 'tcx>) -> LTy<'tcx> {
        let mut ty = acx.type_of(self.local);
        for proj in self.projection {
            ty = acx.project(ty, proj);
        }
        ty
    }
}

impl<'tcx> TypeOf<'tcx> for Operand<'tcx> {
    fn type_of(&self, acx: &AnalysisCtxt<'_, 'tcx>) -> LTy<'tcx> {
        match *self {
            Operand::Move(pl) | Operand::Copy(pl) => acx.type_of(pl),
            Operand::Constant(ref c) => label_no_pointers(acx, c.ty()),
        }
    }
}

/// Label a type that contains no pointer types by applying `PointerId::NONE` everywhere.  Panics
/// if the type does contain pointers.
fn label_no_pointers<'tcx>(acx: &AnalysisCtxt<'_, 'tcx>, ty: Ty<'tcx>) -> LTy<'tcx> {
    acx.lcx().label(ty, &mut |inner_ty| {
        assert!(
            !matches!(inner_ty.kind(), TyKind::Ref(..) | TyKind::RawPtr(..)),
            "unexpected pointer type in {:?}",
            ty,
        );
        PointerId::NONE
    })
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct GlobalAssignment {
    pub perms: GlobalPointerTable<PermissionSet>,
    pub flags: GlobalPointerTable<FlagSet>,
}

impl GlobalAssignment {
    pub fn new(
        len: usize,
        default_perms: PermissionSet,
        default_flags: FlagSet,
    ) -> GlobalAssignment {
        GlobalAssignment {
            perms: GlobalPointerTable::from_raw(vec![default_perms; len]),
            flags: GlobalPointerTable::from_raw(vec![default_flags; len]),
        }
    }

    pub fn and<'a>(&'a mut self, local: &'a mut LocalAssignment) -> Assignment<'a> {
        Assignment {
            global: self,
            local,
        }
    }
}

pub struct LocalAssignment {
    pub perms: LocalPointerTable<PermissionSet>,
    pub flags: LocalPointerTable<FlagSet>,
}

impl LocalAssignment {
    pub fn new(
        len: usize,
        default_perms: PermissionSet,
        default_flags: FlagSet,
    ) -> LocalAssignment {
        LocalAssignment {
            perms: LocalPointerTable::from_raw(vec![default_perms; len]),
            flags: LocalPointerTable::from_raw(vec![default_flags; len]),
        }
    }
}

pub struct Assignment<'a> {
    global: &'a mut GlobalAssignment,
    local: &'a mut LocalAssignment,
}

impl Assignment<'_> {
    pub fn perms(&self) -> PointerTable<PermissionSet> {
        self.global.perms.and(&self.local.perms)
    }

    pub fn perms_mut(&mut self) -> PointerTableMut<PermissionSet> {
        self.global.perms.and_mut(&mut self.local.perms)
    }

    pub fn flags(&self) -> PointerTable<FlagSet> {
        self.global.flags.and(&self.local.flags)
    }

    #[allow(dead_code)]
    pub fn _flags_mut(&mut self) -> PointerTableMut<FlagSet> {
        self.global.flags.and_mut(&mut self.local.flags)
    }

    pub fn all_mut(&mut self) -> (PointerTableMut<PermissionSet>, PointerTableMut<FlagSet>) {
        (
            self.global.perms.and_mut(&mut self.local.perms),
            self.global.flags.and_mut(&mut self.local.flags),
        )
    }
}
