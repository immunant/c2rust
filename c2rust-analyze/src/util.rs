use rustc_middle::mir::{PlaceRef, PlaceElem, Rvalue, Operand, Local};

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

