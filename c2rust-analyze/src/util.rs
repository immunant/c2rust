use crate::labeled_ty::LabeledTy;
use rustc_hir::def::DefKind;
use rustc_hir::def_id::DefId;
use rustc_middle::mir::{
    Field, Local, Mutability, Operand, Place, PlaceElem, PlaceRef, ProjectionElem, Rvalue,
};
use rustc_middle::ty::{AdtDef, DefIdTree, SubstsRef, Ty, TyCtxt, TyKind, UintTy};
use std::collections::HashMap;
use std::fmt::Debug;

/// A mapping for substituting [`Place`]s adhering to the
/// following pattern
/// ```mir
/// _1 = malloc(...);
/// _2 = _1 as *mut T;
/// ```
///
/// In this case, `_1` would be mapped to `_2`, which is indicative
/// of the amended statement:
/// ```
/// _2 = malloc(...);
/// ```
#[derive(Default)]
pub struct CVoidCasts<'tcx>(pub HashMap<Place<'tcx>, Place<'tcx>>);

impl<'tcx> CVoidCasts<'tcx> {
    /// Checks if the casted-to or casted-from value is a [`*c_void`](core::ffi::c_void).
    pub fn contains(&self, lhs: &Place<'tcx>, rv: &Rvalue<'tcx>) -> bool {
        matches!(rv, Rvalue::Cast(_, Operand::Copy(p) | Operand::Move(p), _) if self.0.contains_key(p))
            || self.0.contains_key(lhs)
    }

    pub fn get_or_default_to(&self, p: &Place<'tcx>) -> Place<'tcx> {
        *self.0.get(p).unwrap_or(p)
    }
}

#[derive(Debug)]
pub enum RvalueDesc<'tcx> {
    /// A pointer projection, such as `&(*x.y).z`.  The rvalue is split into a base pointer
    /// expression (in this case `x.y`) and a projection (`.z`).  The `&` and `*` are implicit.
    Project {
        /// The base pointer of the projection.  This is guaranteed to evaluate to a pointer or
        /// reference type.
        ///
        /// This may contain derefs, indicating that the pointer was loaded through another
        /// pointer.  Only the outermost deref is implicit.  For example, `&(**x).y` has a `base`
        /// of `*x` and a `proj` of `.y`.
        base: PlaceRef<'tcx>,
        /// The projection applied to the pointer.  This contains no `Deref` projections.
        proj: &'tcx [PlaceElem<'tcx>],
    },
    /// The address of a local or one of its fields, such as `&x.y`.  The rvalue is split into a
    /// base local (in this case `x`) and a projection (`.y`).  The `&` is implicit.
    AddrOfLocal {
        local: Local,
        /// The projection applied to the local.  This contains no `Deref` projections.
        proj: &'tcx [PlaceElem<'tcx>],
    },
}

pub fn describe_rvalue<'tcx>(rv: &Rvalue<'tcx>) -> Option<RvalueDesc<'tcx>> {
    Some(match *rv {
        Rvalue::Use(ref op) => match *op {
            Operand::Move(pl) | Operand::Copy(pl) => RvalueDesc::Project {
                base: pl.as_ref(),
                proj: &[],
            },
            Operand::Constant(_) => return None,
        },
        Rvalue::Ref(_, _, pl) | Rvalue::AddressOf(_, pl) => {
            let projection = &pl.projection[..];
            match projection
                .iter()
                .rposition(|p| matches!(p, PlaceElem::Deref))
            {
                Some(i) => {
                    // `i` is the index of the last `ProjectionElem::Deref` in `pl`.
                    RvalueDesc::Project {
                        base: PlaceRef {
                            local: pl.local,
                            projection: &projection[..i],
                        },
                        proj: &projection[i + 1..],
                    }
                }
                None => {
                    // `pl` refers to a field/element of a local.
                    RvalueDesc::AddrOfLocal {
                        local: pl.local,
                        proj: projection,
                    }
                }
            }
        }
        _ => return None,
    })
}

#[derive(Debug)]
pub enum Callee<'tcx> {
    /// `<*mut T>::offset` or `<*const T>::offset`.
    PtrOffset {
        pointee_ty: Ty<'tcx>,
        mutbl: Mutability,
    },

    /// `<[T]>::as_ptr` and `<[T]>::as_mut_ptr` methods.  Also covers the array and str versions.
    SliceAsPtr {
        /// The pointee type.  This is either `TyKind::Slice`, `TyKind::Array`, or `TyKind::Str`.
        pointee_ty: Ty<'tcx>,

        /// The slice element type.  For `str`, this is `u8`.
        elem_ty: Ty<'tcx>,

        /// Mutability of the output pointer.
        mutbl: Mutability,
    },

    /// A [`Trivial`] library function is one that has no effect on pointer permissions in its caller.
    ///
    /// Thus, a [`Trivial`] function call requires no special handling.
    ///
    /// A function is [`Trivial`] if it has no argument or return types that are or contain a pointer.
    /// Note that "contains a pointer" is calculated recursively.
    /// There must not be any raw pointer accessible from that type.
    ///
    /// We ignore the possibility that a function may perform
    /// int-to-ptr casts (a la [`std::ptr::from_exposed_addr`]) internally,
    /// as handling such casts is very difficult and out of scope for now.
    ///
    /// References are allowed, because the existence of that reference in the first place
    /// carries much stronger semantics, so in the case that the reference is casted to a raw pointer,
    /// we can simply use the pointer permissions guaranteed by that reference.
    ///
    /// [`Trivial`]: Self::Trivial
    Trivial,

    /// libc::malloc
    Malloc,

    /// libc::calloc
    Calloc,

    /// libc::free
    Free,

    /// libc::realloc
    Realloc,

    /// core::ptr::is_null
    IsNull,

    /// Some other statically-known function, including functions defined in the current crate.
    Other {
        def_id: DefId,
        substs: SubstsRef<'tcx>,
    },
}

pub fn ty_callee<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> Option<Callee<'tcx>> {
    let (did, substs) = match *ty.kind() {
        TyKind::FnDef(did, substs) => (did, substs),
        _ => return None,
    };

    if let Some(callee) = builtin_callee(tcx, did, substs) {
        return Some(callee);
    }
    Some(Callee::Other {
        def_id: did,
        substs,
    })
}

fn builtin_callee<'tcx>(
    tcx: TyCtxt<'tcx>,
    did: DefId,
    _substs: SubstsRef<'tcx>,
) -> Option<Callee<'tcx>> {
    let name = tcx.item_name(did);

    match name.as_str() {
        "offset" => {
            // The `offset` inherent method of `*const T` and `*mut T`.
            let parent_did = tcx.parent(did);
            if tcx.def_kind(parent_did) != DefKind::Impl {
                return None;
            }
            if tcx.impl_trait_ref(parent_did).is_some() {
                return None;
            }
            let parent_impl_ty = tcx.type_of(parent_did);
            let (pointee_ty, mutbl) = match parent_impl_ty.kind() {
                TyKind::RawPtr(tm) => (tm.ty, tm.mutbl),
                _ => return None,
            };
            Some(Callee::PtrOffset { pointee_ty, mutbl })
        }

        name @ "as_ptr" | name @ "as_mut_ptr" => {
            // The `as_ptr` and `as_mut_ptr` inherent methods of `[T]`, `[T; n]`, and `str`.
            let parent_did = tcx.parent(did);
            if tcx.def_kind(parent_did) != DefKind::Impl {
                return None;
            }
            if tcx.impl_trait_ref(parent_did).is_some() {
                return None;
            }
            let parent_impl_ty = tcx.type_of(parent_did);
            let elem_ty = match *parent_impl_ty.kind() {
                TyKind::Array(ty, _) => ty,
                TyKind::Slice(ty) => ty,
                TyKind::Str => tcx.mk_mach_uint(UintTy::U8),
                _ => return None,
            };
            let mutbl = match name {
                "as_ptr" => Mutability::Not,
                "as_mut_ptr" => Mutability::Mut,
                _ => unreachable!(),
            };
            Some(Callee::SliceAsPtr {
                pointee_ty: parent_impl_ty,
                elem_ty,
                mutbl,
            })
        }

        "abort" | "exit" => {
            // `std::process::abort` and `std::process::exit`
            let path = tcx.def_path(did);
            if tcx.crate_name(path.krate).as_str() != "std" {
                return None;
            }
            if path.data.len() != 2 {
                return None;
            }
            if path.data[0].to_string() != "process" {
                return None;
            }
            Some(Callee::Trivial)
        }

        "size_of" => {
            // `core::mem::size_of`
            let path = tcx.def_path(did);
            if tcx.crate_name(path.krate).as_str() != "core" {
                return None;
            }
            if path.data.len() != 2 {
                return None;
            }
            if path.data[0].to_string() != "mem" {
                return None;
            }
            Some(Callee::Trivial)
        }

        "malloc" | "c2rust_test_typed_malloc" => {
            if matches!(tcx.def_kind(tcx.parent(did)), DefKind::ForeignMod) {
                return Some(Callee::Malloc);
            }
            None
        }

        "calloc" | "c2rust_test_typed_calloc" => {
            if matches!(tcx.def_kind(tcx.parent(did)), DefKind::ForeignMod) {
                return Some(Callee::Calloc);
            }
            None
        }

        "realloc" | "c2rust_test_typed_realloc" => {
            if matches!(tcx.def_kind(tcx.parent(did)), DefKind::ForeignMod) {
                return Some(Callee::Realloc);
            }
            None
        }

        "free" | "c2rust_test_typed_free" => {
            if matches!(tcx.def_kind(tcx.parent(did)), DefKind::ForeignMod) {
                return Some(Callee::Free);
            }
            None
        }

        "is_null" => {
            // The `offset` inherent method of `*const T` and `*mut T`.
            let parent_did = tcx.parent(did);
            if tcx.def_kind(parent_did) != DefKind::Impl {
                return None;
            }
            if tcx.impl_trait_ref(parent_did).is_some() {
                return None;
            }
            let parent_impl_ty = tcx.type_of(parent_did);
            let (_pointee_ty, _mutbl) = match parent_impl_ty.kind() {
                TyKind::RawPtr(tm) => (tm.ty, tm.mutbl),
                _ => return None,
            };
            Some(Callee::IsNull)
        }

        _ => {
            eprintln!("name: {name:?}");
            None
        }
    }
}

pub fn lty_project<'tcx, L: Debug>(
    lty: LabeledTy<'tcx, L>,
    proj: &PlaceElem<'tcx>,
    mut adt_func: impl FnMut(LabeledTy<'tcx, L>, AdtDef<'tcx>, Field) -> LabeledTy<'tcx, L>,
) -> LabeledTy<'tcx, L> {
    match *proj {
        ProjectionElem::Deref => {
            assert!(matches!(lty.kind(), TyKind::Ref(..) | TyKind::RawPtr(..)));
            assert_eq!(lty.args.len(), 1);
            lty.args[0]
        }
        ProjectionElem::Field(f, _) => match lty.kind() {
            TyKind::Tuple(_) => lty.args[f.index()],
            TyKind::Adt(def, _) => adt_func(lty, *def, f),
            _ => panic!("Field projection is unsupported on type {:?}", lty),
        },
        ProjectionElem::Index(..) | ProjectionElem::ConstantIndex { .. } => {
            assert!(matches!(lty.kind(), TyKind::Array(..) | TyKind::Slice(..)));
            assert_eq!(lty.args.len(), 1);
            lty.args[0]
        }
        ProjectionElem::Subslice { .. } => todo!("type_of Subslice"),
        ProjectionElem::Downcast(..) => todo!("type_of Downcast"),
    }
}
