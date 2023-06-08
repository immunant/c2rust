use crate::labeled_ty::LabeledTy;
use crate::trivial::IsTrivial;
use rustc_const_eval::interpret::Scalar;
use rustc_hir::def::DefKind;
use rustc_hir::def_id::DefId;
use rustc_middle::mir::{
    BasicBlock, BasicBlockData, Constant, Field, Local, Location, Mutability, Operand, Place,
    PlaceElem, PlaceRef, ProjectionElem, Rvalue, Statement, StatementKind,
};
use rustc_middle::ty::{
    self, AdtDef, DefIdTree, EarlyBinder, Subst, SubstsRef, Ty, TyCtxt, TyKind, UintTy,
};
use rustc_type_ir::IntTy;
use std::fmt::Debug;

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

    /// A function whose definition is not known.
    ///
    /// This could be for multiple reasons.
    ///
    /// A function could be `extern`, so there is no source for it.
    /// Sometimes the actual definition could be linked with a `use` (the ideal solution),
    /// but sometimes it's completely external and thus completely unknown,
    /// as it may be dynamically linked.
    ///
    /// It could also be a function pointer,
    /// for which there could be multiple definitions.
    /// While possible definitions could be statically determined as an optimization,
    /// this provides a safe fallback.
    ///
    /// Or it could a function in another non-local crate, such as `std`,
    /// as definitions of functions from other crates are not available,
    /// and we definitely can't rewrite them at all.
    UnknownDef { ty: Ty<'tcx> },

    /// A function that:
    /// * is in the current, local crate
    /// * is statically-known
    /// * has an accessible definition
    /// * is non-trivial
    /// * is non-builtin
    LocalDef {
        def_id: DefId,
        substs: SubstsRef<'tcx>,
    },

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
}

pub fn ty_callee<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> Callee<'tcx> {
    let is_trivial = || {
        let is_trivial = ty.fn_sig(tcx).is_trivial(tcx);
        eprintln!("{ty:?} is trivial: {is_trivial}");
        is_trivial
    };

    match *ty.kind() {
        ty::FnDef(did, substs) => {
            if is_trivial() {
                Callee::Trivial
            } else if let Some(callee) = builtin_callee(tcx, did, substs) {
                callee
            } else if !did.is_local() || tcx.def_kind(tcx.parent(did)) == DefKind::ForeignMod {
                Callee::UnknownDef { ty }
            } else {
                Callee::LocalDef {
                    def_id: did,
                    substs,
                }
            }
        }
        ty::FnPtr(..) => {
            if is_trivial() {
                Callee::Trivial
            } else {
                Callee::UnknownDef { ty }
            }
        }
        _ => Callee::UnknownDef { ty },
    }
}

fn builtin_callee<'tcx>(tcx: TyCtxt<'tcx>, did: DefId, substs: SubstsRef<'tcx>) -> Option<Callee> {
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
            let parent_impl_ty = EarlyBinder(tcx.type_of(parent_did)).subst(tcx, substs);
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
            let parent_impl_ty = EarlyBinder(tcx.type_of(parent_did)).subst(tcx, substs);
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

        "malloc" => {
            if matches!(tcx.def_kind(tcx.parent(did)), DefKind::ForeignMod) {
                return Some(Callee::Malloc);
            }
            None
        }

        "calloc" => {
            if matches!(tcx.def_kind(tcx.parent(did)), DefKind::ForeignMod) {
                return Some(Callee::Calloc);
            }
            None
        }

        "realloc" => {
            if matches!(tcx.def_kind(tcx.parent(did)), DefKind::ForeignMod) {
                return Some(Callee::Realloc);
            }
            None
        }

        "free" => {
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
            let parent_impl_ty = EarlyBinder(tcx.type_of(parent_did)).subst(tcx, substs);
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
    mut field_lty: impl FnMut(LabeledTy<'tcx, L>, AdtDef<'tcx>, Field) -> LabeledTy<'tcx, L>,
) -> LabeledTy<'tcx, L> {
    match *proj {
        ProjectionElem::Deref => {
            assert!(matches!(lty.kind(), TyKind::Ref(..) | TyKind::RawPtr(..)));
            assert_eq!(lty.args.len(), 1);
            lty.args[0]
        }
        ProjectionElem::Field(f, _) => match lty.kind() {
            TyKind::Tuple(_) => lty.args[f.index()],
            TyKind::Adt(def, _) => field_lty(lty, *def, f),
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

pub fn get_cast_place<'tcx>(rv: &Rvalue<'tcx>) -> Option<Place<'tcx>> {
    match rv {
        Rvalue::Cast(_, op, _) => op.place(),
        _ => None,
    }
}

pub fn terminator_location(block: BasicBlock, block_data: &BasicBlockData) -> Location {
    Location {
        block,
        statement_index: block_data.statements.len(),
    }
}

pub fn get_assign_sides<'tcx, 'a>(
    stmt: &'a Statement<'tcx>,
) -> Option<(Place<'tcx>, &'a Rvalue<'tcx>)> {
    let (pl, ref rv) = match &stmt.kind {
        StatementKind::Assign(it) => Some(&**it),
        _ => None,
    }?;
    Some((*pl, rv))
}

/// Check if a [`Constant`] is an integer constant that can be casted to a null pointer.
pub fn is_null_const(constant: Constant) -> bool {
    match constant.literal.try_to_scalar() {
        Some(Scalar::Int(i)) => i.is_null(),
        _ => false,
    }
}

pub trait PhantomLifetime<'a> {}
impl<'a, T: ?Sized> PhantomLifetime<'a> for T {}

/// Determine if `from` can be safely transmuted to `to`,
/// which is defined as `*(from as *const To)` being a safe operation,
/// where `from: *const From` and assuming `*from` already was safe.
///
/// Note that this is one-way, and is slightly different from [`core::mem::transmute`],
/// and more similar to [`core::mem::transmute_copy`].
///
/// This forms a reflexive, transitive, and non-symmetric (one-way) relation, named `~` below.
/// Formally, `A ~ B` iff whenever `*a` is well-defined (i.e., not UB),
/// `*(a as *const B)` is also well-defined, where `a: *const A`.
///
/// However, safe transmutability is difficult to check completely,
/// so this function only checks a subset of it,
/// with these formal rules for all types `A`, `B`:
///
/// * `A = B => A ~ B`
/// * `A ~ B => *A ~ *B`
/// * `uN ~ iN`, `iN ~ uN`, where `N` is an integer width
/// * `A ~ B => [A] ~ B`
/// * `A ~ B, N > 0 => [A; N] ~ B`, where `const N: usize`
///
/// Thus, [`true`] means it is definitely transmutable,
/// while [`false`] means it may not be transmutable.
pub fn is_transmutable_to<'tcx>(from: Ty<'tcx>, to: Ty<'tcx>) -> bool {
    let transmutable_ints = || {
        use IntTy::*;
        use UintTy::*;
        match (from.kind(), to.kind()) {
            (ty::Uint(u), ty::Int(i)) | (ty::Int(i), ty::Uint(u)) => {
                matches!(
                    (u, i),
                    (Usize, Isize) | (U8, I8) | (U16, I16) | (U32, I32) | (U64, I64)
                )
            }
            _ => false,
        }
    };

    let one_way_transmutable = || match *from.kind() {
        ty::Array(from, n) => {
            is_transmutable_to(from, to) && {
                let is_zero = n
                    .kind()
                    .try_to_scalar_int()
                    .map(|i| i.is_null())
                    .unwrap_or(false);
                !is_zero
            }
        }
        ty::Slice(from) => is_transmutable_to(from, to),
        _ => false,
    };

    from == to
        || is_transmutable_ptr_cast(from, to).unwrap_or(false)
        || transmutable_ints()
        || one_way_transmutable()
}

/// Determine if `from as to` is a ptr-to-ptr cast.
/// and if it is, if the pointee types are [safely transmutable](is_transmutable_to).
///
/// This returns [`Some`]`(is_transmutable)` if they're both pointers,
/// and [`None`] if its some other types.
///
/// See [`is_transmutable_to`] for the definition of safe transmutability.
pub fn is_transmutable_ptr_cast<'tcx>(from: Ty<'tcx>, to: Ty<'tcx>) -> Option<bool> {
    let from = from.builtin_deref(true)?.ty;
    let to = to.builtin_deref(true)?.ty;
    Some(is_transmutable_to(from, to))
}
