use crate::c_void_casts::CVoidCasts;
use crate::labeled_ty::{LabeledTy, LabeledTyCtxt};
use crate::pointer_id::{
    GlobalPointerTable, LocalPointerTable, NextGlobalPointerId, NextLocalPointerId, PointerTable,
    PointerTableMut,
};
use crate::util::{self, describe_rvalue, PhantomLifetime, RvalueDesc};
use crate::AssignPointerIds;
use bitflags::bitflags;
use log::*;
use rustc_hir::def_id::DefId;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::interpret::{self, AllocId, ConstValue, GlobalAlloc};
use rustc_middle::mir::{
    Body, Constant, ConstantKind, Field, HasLocalDecls, Local, LocalDecls, Location, Operand,
    Place, PlaceElem, PlaceRef, Rvalue,
};
use rustc_middle::ty::{AdtDef, FieldDef, Ty, TyCtxt, TyKind};
use std::collections::HashMap;
use std::ops::Index;

bitflags! {
    /// Permissions are created such that we allow dropping permissions in any assignment.
    /// This means removing a permission from a pointer's [`PermissionSet`]
    /// must allow the pointer to take on more values, not restrict it to fewer values.
    ///
    /// That's why, for example [`UNIQUE`] is named as such,
    /// as opposed to something like `ALIASED` (a pointer capability),
    /// as removing [`UNIQUE`] (`&mut`) allows more values to be taken on (`&`).
    ///
    /// Currently, we assume that all pointers are valid or null
    /// (see [the `std::ptr` safety docs](https://doc.rust-lang.org/std/ptr/index.html#safety)).
    /// We do not yet (here) consider unaligned or cast-from-integer pointers.
    ///
    /// [`UNIQUE`]: Self::UNIQUE
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

        /// This pointer is non-null.
        ///
        /// [`NON_NULL`] is set (or not) when the pointer is created,
        /// and it flows forward along dataflow edges.
        ///
        /// The following should be set to [`NON_NULL`]:
        /// * the results of [`Rvalue::Ref`] and [`Rvalue::AddressOf`]
        /// * the result of a known function like [`_.offset`] that never returns null pointers
        ///
        /// The following should not be set to [`NON_NULL`]:
        /// * [`core::ptr::null`]
        /// * [`core::ptr::null_mut`]
        /// * [`core::ptr::from_exposed_addr`] with the constant `0`
        /// * `0 as * {const,mut} _`, a cast from the constant `0` to a pointer type
        /// * anything else
        ///
        /// Non-zero but invalid pointers, such as those produced by:
        /// * [`core::ptr::invalid`]
        /// * [`core::ptr::invalid_mut`]
        /// * [`NonNull::dangling`]
        /// * [`core::ptr::from_exposed_addr`]
        /// * int-to-ptr casts though `as` or [`core::mem::transmute`]
        ///
        /// will be [`NON_NULL`], but for now,
        /// we do not consider and do not allow such non-null invalid pointers at all.
        ///
        /// [`NON_NULL`] pointers will become references, e.x. `&T`.\
        /// Non-[`NON_NULL`] pointers will become [`Option<&T>`].
        ///
        /// Casts/transitions from [`NON_NULL`] to non-[`NON_NULL`] will become [`Some(_)`].\
        /// Casts/transitions from non-[`NON_NULL`] to [`NON_NULL`] will become [`_.unwrap()`].
        ///
        /// [`_.is_null()`] on a [`NON_NULL`] pointer will become [`false`].\
        /// [`_.is_null()`] on a non-[`NON_NULL`] pointer will become [`_.is_some()`].
        ///
        /// Constant null pointers, like those produced by:
        /// * [`core::ptr::null`]
        /// * [`core::ptr::null_mut`]
        /// * [`core::ptr::from_exposed_addr`] with the constant `0`
        /// * `0 as * {const,mut} _`, a cast from the constant `0` to a pointer type
        ///
        /// will become [`None`].
        ///
        /// [`NON_NULL`]: Self::NON_NULL
        /// [`READ`]: Self::READ
        /// [`WRITE`]: Self::WRITE
        /// [`_.offset`]: core::ptr::offset
        /// [`NonNull::dangling`]: core::ptr::NonNull::dangling
        /// [`Some(_)`]: Some
        /// [`_.unwrap()`]: Option::unwrap
        /// [`_.is_null()`]: core::ptr::is_null
        /// [`_.is_some()`]: Option::is_some
        const NON_NULL = 0x0080;
    }
}

impl PermissionSet {
    /// The permissions for a (byte-)string literal.
    //
    // `.union` is used here since it's a `const fn`, unlike `BitOr::bitor`.
    pub const STRING_LITERAL: Self = Self::READ.union(Self::OFFSET_ADD);
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

    pub static_tys: HashMap<DefId, LTy<'tcx>>,
    pub addr_of_static: HashMap<DefId, PointerId>,

    next_ptr_id: NextGlobalPointerId,
}

pub struct AnalysisCtxt<'a, 'tcx> {
    pub gacx: &'a mut GlobalAnalysisCtxt<'tcx>,

    pub local_decls: &'a LocalDecls<'tcx>,
    pub local_tys: IndexVec<Local, LTy<'tcx>>,
    pub addr_of_local: IndexVec<Local, PointerId>,
    pub c_void_casts: CVoidCasts<'tcx>,
    /// Types for certain [`Rvalue`]s.  Some `Rvalue`s introduce fresh [`PointerId`]s; to keep
    /// those `PointerId`s consistent, the `Rvalue`'s type must be stored rather than recomputed on
    /// the fly.
    pub rvalue_tys: HashMap<Location, LTy<'tcx>>,

    /// [`Location`]s of (byte-)string literal [`rvalue_tys`](Self::rvalue_tys).
    pub string_literal_locs: Vec<Location>,

    next_ptr_id: NextLocalPointerId,
}

impl<'a, 'tcx> AnalysisCtxt<'_, 'tcx> {
    pub fn string_literal_tys(&'a self) -> impl Iterator<Item = LTy<'tcx>> + 'a {
        self.string_literal_locs
            .iter()
            .map(|loc| self.rvalue_tys[loc])
    }

    pub fn string_literal_perms(
        &'a self,
    ) -> impl Iterator<Item = (PointerId, PermissionSet)> + PhantomLifetime<'tcx> + 'a {
        self.string_literal_tys()
            .map(|lty| (lty.label, PermissionSet::STRING_LITERAL))
    }

    pub fn check_string_literal_perms(&self, asn: &Assignment) {
        for lty in self.string_literal_tys() {
            let ptr = lty.label;
            let expected_perms = PermissionSet::STRING_LITERAL;
            let mut actual_perms = asn.perms()[ptr];
            // Ignore `UNIQUE` as it gets automatically added to all permissions
            // and then removed later if it can't apply.
            // We don't care about `UNIQUE` for const refs, so just unset it here.
            actual_perms.set(PermissionSet::UNIQUE, false);
            assert_eq!(expected_perms, actual_perms);
        }
    }
}

pub struct AnalysisCtxtData<'tcx> {
    local_tys: IndexVec<Local, LTy<'tcx>>,
    addr_of_local: IndexVec<Local, PointerId>,
    rvalue_tys: HashMap<Location, LTy<'tcx>>,
    string_literal_locs: Vec<Location>,
    next_ptr_id: NextLocalPointerId,
}

impl<'tcx> GlobalAnalysisCtxt<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>) -> GlobalAnalysisCtxt<'tcx> {
        GlobalAnalysisCtxt {
            tcx,
            lcx: LabeledTyCtxt::new(tcx),
            fn_sigs: HashMap::new(),
            field_tys: HashMap::new(),
            static_tys: HashMap::new(),
            addr_of_static: HashMap::new(),
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
            ref mut static_tys,
            ref mut addr_of_static,
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

        for labeled_static in static_tys.values_mut() {
            *labeled_static = remap_lty_pointers(lcx, map, labeled_static);
        }

        for ptr in addr_of_static.values_mut() {
            if !ptr.is_none() {
                *ptr = map[*ptr];
            }
        }

        *next_ptr_id = counter;
    }

    pub fn assign_pointer_to_static(&mut self, did: DefId) {
        trace!("assign_pointer_to_static({:?})", did);
        let lty = self.assign_pointer_ids(self.tcx.type_of(did));
        let ptr = self.new_pointer();
        self.static_tys.insert(did, lty);
        self.addr_of_static.insert(did, ptr);
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
        let tcx = gacx.tcx;
        AnalysisCtxt {
            gacx,
            local_decls: &mir.local_decls,
            local_tys: IndexVec::new(),
            c_void_casts: CVoidCasts::new(mir, tcx),
            addr_of_local: IndexVec::new(),
            rvalue_tys: HashMap::new(),
            string_literal_locs: Default::default(),
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
            string_literal_locs,
            next_ptr_id,
        } = data;
        AnalysisCtxt {
            gacx,
            local_decls: &mir.local_decls,
            local_tys,
            c_void_casts: CVoidCasts::default(),
            addr_of_local,
            rvalue_tys,
            string_literal_locs,
            next_ptr_id,
        }
    }

    pub fn into_data(self) -> AnalysisCtxtData<'tcx> {
        AnalysisCtxtData {
            local_tys: self.local_tys,
            addr_of_local: self.addr_of_local,
            rvalue_tys: self.rvalue_tys,
            string_literal_locs: self.string_literal_locs,
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
            Rvalue::Cast(..) => panic!("Cast should be present in rvalue_tys"),
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
        let projection_lty = |_lty: LTy, adt_def: AdtDef, field: Field| {
            let field_def = &adt_def.non_enum_variant().fields[field.index()];
            let field_def_name = field_def.name;
            eprintln!("projecting into {adt_def:?}.{field_def_name:}");
            let res = *self.gacx.field_tys.get(&field_def.did).unwrap_or_else(|| {
                panic!("Could not find {adt_def:?}.{field_def_name:?} in field type map")
            });
            res
        };
        util::lty_project(lty, proj, projection_lty)
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
        let Self {
            local_tys,
            addr_of_local,
            rvalue_tys,
            string_literal_locs: _,
            next_ptr_id,
        } = self;

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

fn const_alloc_id(c: &Constant) -> Option<AllocId> {
    if let ConstantKind::Val(ConstValue::Scalar(interpret::Scalar::Ptr(ptr, _)), _ty) = c.literal {
        return Some(ptr.provenance);
    }
    None
}

fn find_static_for_alloc(tcx: &TyCtxt, id: AllocId) -> Option<DefId> {
    match tcx.try_get_global_alloc(id) {
        None => {} //hmm, ok
        Some(GlobalAlloc::Static(did)) => {
            if !tcx.is_foreign_item(did) {
                // local static referenced
                return Some(did);
            }
        }
        Some(_) => {}
    }
    None
}

impl<'tcx> TypeOf<'tcx> for Operand<'tcx> {
    fn type_of(&self, acx: &AnalysisCtxt<'_, 'tcx>) -> LTy<'tcx> {
        match *self {
            Operand::Move(pl) | Operand::Copy(pl) => acx.type_of(pl),
            Operand::Constant(ref c) => {
                // Constants of pointer type should only be pointers into static allocations.
                // Find the defid of the static and look up the pointer ID in gacx.static_tys
                if c.ty().is_any_ptr() {
                    if let Some(alloc_id) = const_alloc_id(c) {
                        if let Some(did) = find_static_for_alloc(&acx.gacx.tcx, alloc_id) {
                            let lty = acx
                                .gacx
                                .static_tys
                                .get(&did)
                                .cloned()
                                .unwrap_or_else(|| panic!("did {:?} not found", did));
                            let ptr = acx
                                .gacx
                                .addr_of_static
                                .get(&did)
                                .cloned()
                                .unwrap_or_else(|| panic!("did {:?} not found", did));
                            let args = acx.lcx().mk_slice(&[lty]);
                            assert!(matches!(
                                *c.ty().kind(),
                                TyKind::Ref(..) | TyKind::RawPtr(..)
                            ));
                            acx.lcx().mk(c.ty(), args, ptr)
                        } else {
                            panic!("no static found for alloc id {:?}", alloc_id)
                        }
                    } else {
                        panic!("constant was of ptr type but not a Scalar pointing into a static allocation: {:?}", c)
                    }
                } else {
                    label_no_pointers(acx, c.ty())
                }
            }
        }
    }
}

/// Label a type that contains no pointer types by applying `PointerId::NONE` everywhere.  Panics
/// if the type does contain pointers.
pub fn label_no_pointers<'tcx>(acx: &AnalysisCtxt<'_, 'tcx>, ty: Ty<'tcx>) -> LTy<'tcx> {
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
