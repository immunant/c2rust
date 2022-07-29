use crate::labeled_ty::{LabeledTy, LabeledTyCtxt};
use crate::pointer_id::{
    GlobalPointerTable, LocalPointerTable, NextGlobalPointerId, NextLocalPointerId, PointerTable,
    PointerTableMut,
};
use crate::util::{describe_rvalue, RvalueDesc};
use bitflags::bitflags;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::{
    Body, HasLocalDecls, Local, LocalDecls, Operand, Place, PlaceElem, PlaceRef, ProjectionElem,
    Rvalue,
};
use rustc_middle::ty::{Ty, TyCtxt, TyKind};
use std::cell::Cell;
use std::iter;

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

pub struct GlobalAnalysisCtxt<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub lcx: LTyCtxt<'tcx>,

    next_ptr_id: NextGlobalPointerId,
}

pub struct AnalysisCtxt<'a, 'tcx> {
    pub gacx: &'a mut GlobalAnalysisCtxt<'tcx>,

    pub local_decls: &'a LocalDecls<'tcx>,
    pub local_tys: IndexVec<Local, LTy<'tcx>>,
    pub addr_of_local: IndexVec<Local, PointerId>,

    next_ptr_id: NextLocalPointerId,
}

pub struct AnalysisCtxtData<'tcx> {
    local_tys: IndexVec<Local, LTy<'tcx>>,
    addr_of_local: IndexVec<Local, PointerId>,
    next_ptr_id: NextLocalPointerId,
}

impl<'tcx> GlobalAnalysisCtxt<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>) -> GlobalAnalysisCtxt<'tcx> {
        GlobalAnalysisCtxt {
            tcx,
            lcx: LabeledTyCtxt::new(tcx),
            next_ptr_id: NextGlobalPointerId::new(),
        }
    }

    pub fn enter_function<'a>(&'a mut self, mir: &'a Body<'tcx>) -> AnalysisCtxt<'a, 'tcx> {
        AnalysisCtxt::new(self, mir)
    }

    pub fn enter_function_with_data<'a>(
        &'a mut self,
        mir: &'a Body<'tcx>,
        data: AnalysisCtxtData<'tcx>,
    ) -> AnalysisCtxt<'a, 'tcx> {
        AnalysisCtxt::from_data(self, mir, data)
    }

    pub fn new_pointer(&self) -> PointerId {
        self.next_ptr_id.next()
    }

    pub fn num_pointers(&self) -> usize {
        self.next_ptr_id.num_pointers()
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
            addr_of_local: IndexVec::new(),
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
            next_ptr_id,
        } = data;
        AnalysisCtxt {
            gacx,
            local_decls: &mir.local_decls,
            local_tys,
            addr_of_local,
            next_ptr_id,
        }
    }

    pub fn into_data(self) -> AnalysisCtxtData<'tcx> {
        AnalysisCtxtData {
            local_tys: self.local_tys,
            addr_of_local: self.addr_of_local,
            next_ptr_id: self.next_ptr_id,
        }
    }

    pub fn tcx(&self) -> TyCtxt<'tcx> {
        self.gacx.tcx
    }

    pub fn lcx(&self) -> LTyCtxt<'tcx> {
        self.gacx.lcx
    }

    pub fn new_pointer(&self) -> PointerId {
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

    fn project(&self, lty: LTy<'tcx>, proj: &PlaceElem<'tcx>) -> LTy<'tcx> {
        match *proj {
            ProjectionElem::Deref => {
                assert!(matches!(lty.kind(), TyKind::Ref(..) | TyKind::RawPtr(..)));
                assert_eq!(lty.args.len(), 1);
                lty.args[0]
            }
            ProjectionElem::Field(f, _) => match lty.kind() {
                TyKind::Tuple(_) => lty.args[f.index()],
                TyKind::Adt(..) => todo!("type_of Field(Adt)"),
                _ => panic!("Field projection is unsupported on type {:?}", lty),
            },
            ProjectionElem::Index(..) | ProjectionElem::ConstantIndex { .. } => {
                todo!("type_of Index")
            }
            ProjectionElem::Subslice { .. } => todo!("type_of Subslice"),
            ProjectionElem::Downcast(..) => todo!("type_of Downcast"),
        }
    }
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

impl<'tcx> TypeOf<'tcx> for Rvalue<'tcx> {
    fn type_of(&self, acx: &AnalysisCtxt<'_, 'tcx>) -> LTy<'tcx> {
        if let Some(desc) = describe_rvalue(self) {
            let ty = self.ty(acx, acx.tcx());
            if matches!(ty.kind(), TyKind::Ref(..) | TyKind::RawPtr(..)) {
                let (pointee_lty, proj, ptr) = match desc {
                    RvalueDesc::Project { base, proj } => {
                        let base_lty = acx.type_of(base);
                        eprintln!(
                            "rvalue = {:?}, desc = {:?}, base_lty = {:?}",
                            self, desc, base_lty
                        );
                        (
                            acx.project(base_lty, &PlaceElem::Deref),
                            proj,
                            base_lty.label,
                        )
                    }
                    RvalueDesc::AddrOfLocal { local, proj } => {
                        (acx.type_of(local), proj, acx.addr_of_local[local])
                    }
                };

                let mut pointee_lty = pointee_lty;
                for p in proj {
                    pointee_lty = acx.project(pointee_lty, p);
                }

                let ty = self.ty(acx, acx.tcx());
                let pointee_ty = match *ty.kind() {
                    TyKind::Ref(_, ty, _) => ty,
                    TyKind::RawPtr(tm) => tm.ty,
                    _ => unreachable!(
                        "got RvalueDesc for non-pointer Rvalue {:?} (of type {:?})",
                        self, ty,
                    ),
                };
                assert_eq!(pointee_ty, pointee_lty.ty);

                let args = acx.lcx().mk_slice(&[pointee_lty]);
                return acx.lcx().mk(pointee_ty, args, ptr);
            }
        }

        match *self {
            Rvalue::Use(ref op) => acx.type_of(op),
            Rvalue::Repeat(ref op, _) => {
                let op_lty = acx.type_of(op);
                let ty = self.ty(acx, acx.tcx());
                assert!(matches!(ty.kind(), TyKind::Array(..)));
                let args = acx.lcx().mk_slice(&[op_lty]);
                acx.lcx().mk(ty, args, PointerId::NONE)
            }
            Rvalue::Ref(..) | Rvalue::AddressOf(..) => {
                unreachable!("should be handled by describe_rvalue case above")
            }
            Rvalue::ThreadLocalRef(..) => todo!("type_of ThreadLocalRef"),
            Rvalue::Cast(_, ref op, ty) => {
                let op_lty = acx.type_of(op);

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
                    return acx.lcx().mk(ty, op_lty.args, op_lty.label);
                }

                label_no_pointers(acx, ty)
            }
            Rvalue::Len(..)
            | Rvalue::BinaryOp(..)
            | Rvalue::CheckedBinaryOp(..)
            | Rvalue::NullaryOp(..)
            | Rvalue::UnaryOp(..)
            | Rvalue::Discriminant(..) => {
                let ty = self.ty(acx, acx.tcx());
                label_no_pointers(acx, ty)
            }
            Rvalue::Aggregate(ref kind, ref vals) => todo!("type_of Aggregate"),
            Rvalue::ShallowInitBox(ref op, ty) => todo!("type_of ShallowInitBox"),
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

    pub fn flags_mut(&mut self) -> PointerTableMut<FlagSet> {
        self.global.flags.and_mut(&mut self.local.flags)
    }

    pub fn all_mut(&mut self) -> (PointerTableMut<PermissionSet>, PointerTableMut<FlagSet>) {
        (
            self.global.perms.and_mut(&mut self.local.perms),
            self.global.flags.and_mut(&mut self.local.flags),
        )
    }
}
