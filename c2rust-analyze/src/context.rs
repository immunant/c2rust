use std::cell::Cell;
use bitflags::bitflags;
use rustc_index::vec::IndexVec;
use rustc_middle::mir::Local;
use rustc_middle::ty::TyCtxt;
use crate::labeled_ty::{LabeledTy, LabeledTyCtxt};


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
    }
}


#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct PointerId(u32);

impl PointerId {
    pub const NONE: PointerId = PointerId(u32::MAX);

    pub fn index(self) -> usize {
        self.0 as usize
    }

    pub fn is_none(self) -> bool {
        self == Self::NONE
    }
}


pub type LTy<'tcx> = LabeledTy<'tcx, PointerId>;
pub type LTyCtxt<'tcx> = LabeledTyCtxt<'tcx, PointerId>;

pub struct AnalysisCtxt<'tcx> {
    pub tcx: TyCtxt<'tcx>,
    pub lcx: LTyCtxt<'tcx>,

    pub local_tys: IndexVec<Local, LTy<'tcx>>,

    next_ptr_id: Cell<u32>,
}

impl<'tcx> AnalysisCtxt<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>) -> AnalysisCtxt<'tcx> {
        AnalysisCtxt {
            tcx,
            lcx: LabeledTyCtxt::new(tcx),
            local_tys: IndexVec::new(),
            next_ptr_id: Cell::new(0),
        }
    }

    pub fn new_pointer(&self) -> PointerId {
        let next = self.next_ptr_id.get();
        self.next_ptr_id.set(next + 1);
        PointerId(next)
    }

    pub fn num_pointers(&self) -> usize {
        self.next_ptr_id.get() as usize
    }
}
