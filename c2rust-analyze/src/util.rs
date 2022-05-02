use rustc_hir::def::DefKind;
use rustc_middle::mir::{PlaceRef, PlaceElem, Rvalue, Operand, Local, Mutability};
use rustc_middle::ty::{TyCtxt, Ty, TyKind, DefIdTree};

#[derive(Debug)]
pub enum RvalueDesc<'tcx> {
    Project {
        base: PlaceRef<'tcx>,
        proj: &'tcx [PlaceElem<'tcx>],
    },
    AddrOfLocal {
        local: Local,
        proj: &'tcx [PlaceElem<'tcx>],
    },
}

pub fn describe_rvalue<'tcx>(rv: &Rvalue<'tcx>) -> Option<RvalueDesc<'tcx>> {
    Some(match *rv {
        Rvalue::Use(ref op) => match *op {
            Operand::Move(pl) |
            Operand::Copy(pl) => RvalueDesc::Project {
                base: pl.as_ref(),
                proj: &[],
            },
            Operand::Constant(_) => return None,
        },
        Rvalue::Ref(_, _, pl) |
        Rvalue::AddressOf(_, pl) => {
            let projection = &pl.projection[..];
            match projection.iter().rposition(|p| matches!(p, PlaceElem::Deref)) {
                Some(i) => {
                    // `i` is the index of the last `ProjectionElem::Deref` in `pl`.
                    RvalueDesc::Project {
                        base: PlaceRef {
                            local: pl.local,
                            projection: &projection[..i],
                        },
                        proj: &projection[i + 1 ..],
                    }
                },
                None => {
                    // `pl` refers to a field/element of a local.
                    RvalueDesc::AddrOfLocal {
                        local: pl.local,
                        proj: projection,
                    }
                },
            }
        },
        _ => return None,
    })
}


#[derive(Debug)]
pub enum Callee<'tcx> {
    PtrOffset { pointee_ty: Ty<'tcx>, mutbl: Mutability },
}

pub fn ty_callee<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> Option<Callee<'tcx>> {
    let (did, substs) = match *ty.kind() {
        TyKind::FnDef(did, substs) => (did, substs),
        _ => return None,
    };
    let name = tcx.item_name(did);
    let poly_sig = tcx.fn_sig(did);
    let sig = poly_sig.skip_binder();

    match name.as_str() {
        "offset" => {
            // The `offset` inherent method of `*const T` and `*mut T`.
            let parent_did = tcx.parent(did)?;
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
        },
        _ => None,
    }
}
