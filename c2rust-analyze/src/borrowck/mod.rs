use self::atoms::{AllFacts, AtomMaps, Loan, Origin, Output, Path, SubPoint};
use crate::context::{AnalysisCtxt, PermissionSet};
use crate::dataflow::DataflowConstraints;
use crate::labeled_ty::{LabeledTy, LabeledTyCtxt};
use crate::pointer_id::PointerTableMut;
use crate::util::{describe_rvalue, RvalueDesc};
use crate::AdtMetadataTable;
use indexmap::{IndexMap, IndexSet};
use rustc_hir::def_id::DefId;
use rustc_middle::mir::{Body, BorrowKind, Local, LocalKind, Place, StatementKind, START_BLOCK};
use rustc_middle::ty::{EarlyBoundRegion, List, Region, Ty, TyKind};
use rustc_type_ir::RegionKind::ReEarlyBound;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

mod atoms;
mod def_use;
mod dump;
mod type_check;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, Default)]
pub struct Label<'tcx> {
    /// The [`Origin`] of this type
    pub origin: Option<Origin>,
    /// The [Origins](`Origin`) associated with each lifetime
    /// parameter of this type, if applicable
    pub origin_params: Option<&'tcx [(OriginParam, Origin)]>,
    pub perm: PermissionSet,
}

pub type LTy<'tcx> = LabeledTy<'tcx, Label<'tcx>>;
pub type LTyCtxt<'tcx> = LabeledTyCtxt<'tcx, Label<'tcx>>;

/// Metadata describing lifetimes and lifetime parameters of a `struct` field.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldMetadata<'tcx> {
    /// The lifetime parameters of a field, e.g. if a struct
    /// `foo<'a, 'b>` is a field of `bar<'c, 'd>` as field: `foo<'c, 'd>`,
    /// the lifetime params would be a set {'c, 'd}. For a reference such
    /// as &'r foo<'c, 'd>, the lifetime params in the label would be
    /// {'r}, and {'c, 'd} would be the label of the sole argument
    /// of the labeled reference type
    pub lifetime_params: LabeledTy<'tcx, &'tcx [OriginArg<'tcx>]>,
}

/// Metadata describing the lifetime parameters and fields of a `struct`.
#[derive(Clone, PartialEq, Eq)]
pub struct AdtMetadata<'tcx> {
    /// The lifetime parameters of a structure, including
    /// hypothetical lifetimes derived from pointer fields.
    pub lifetime_params: IndexSet<OriginParam>,
    pub field_info: IndexMap<DefId, FieldMetadata<'tcx>>,
}

impl Default for AdtMetadata<'_> {
    fn default() -> Self {
        Self {
            lifetime_params: IndexSet::new(),
            field_info: IndexMap::new(),
        }
    }
}

/// An origin parameter of a type to resolve in a MIR body
/// that will get mapped to a concrete [`Origin`] to provide to polonius.
#[derive(Hash, PartialEq, Eq, Clone, Copy)]
pub enum OriginParam {
    /// An existing [`EarlyBoundRegion`], i.e. `'a` in `struct A<'a>`
    Actual(EarlyBoundRegion),
    /// A hypothesized region derived from a pointer type,
    /// e.g. `'h0` derived from the pointer in `*mut foo`
    Hypothetical(i64),
}

/// An origin arg of a field type resolve in a MIR body
/// that will get mapped to a concrete [`Origin`] to provide to polonius.
#[derive(Hash, PartialEq, Eq, Clone, Copy)]
pub enum OriginArg<'tcx> {
    /// An existing [`Region`], i.e. `'a` in `&'a foo`.
    /// Can be [RegionKind::ReEarlyBound](`rustc_type_ir::RegionKind::ReEarlyBound`)
    /// or [RegionKind::ReStatic](`rustc_type_ir::RegionKind::ReStatic`)
    Actual(Region<'tcx>),
    /// A hypothesized region derived from a pointer type
    /// e.g. `'h0` derived from the pointer in `*mut foo`
    Hypothetical(i64),
}

impl<'tcx> std::fmt::Debug for OriginArg<'tcx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            OriginArg::Actual(r) => write!(f, "{:}", r),
            OriginArg::Hypothetical(h) => write!(f, "'h{h:?}"),
        }
    }
}

impl TryFrom<&OriginArg<'_>> for OriginParam {
    type Error = ();
    fn try_from(value: &OriginArg<'_>) -> Result<Self, Self::Error> {
        Ok(match value {
            OriginArg::Hypothetical(h) => OriginParam::Hypothetical(*h),
            OriginArg::Actual(r) => {
                if let ReEarlyBound(eb) = r.kind() {
                    OriginParam::Actual(eb)
                } else {
                    return Err(());
                }
            }
        })
    }
}

impl std::fmt::Debug for OriginParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            OriginParam::Actual(r) => write!(f, "{:}", r.name),
            OriginParam::Hypothetical(h) => write!(f, "'h{h:?}"),
        }
    }
}

pub fn borrowck_mir<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    dataflow: &DataflowConstraints,
    hypothesis: &mut PointerTableMut<PermissionSet>,
    name: &str,
    mir: &Body<'tcx>,
    adt_metadata: &AdtMetadataTable<'tcx>,
    field_tys: HashMap<DefId, crate::LTy<'tcx>>,
) {
    let mut i = 0;
    loop {
        eprintln!("run polonius");
        let (facts, maps, output) =
            run_polonius(acx, hypothesis, name, mir, adt_metadata, &field_tys);
        eprintln!(
            "polonius: iteration {}: {} errors, {} move_errors",
            i,
            output.errors.len(),
            output.move_errors.len(),
        );
        i += 1;

        if output.errors.is_empty() {
            break;
        }
        if i >= 20 {
            panic!()
        }

        let mut changed = false;
        for loans in output.errors.values() {
            for &loan in loans {
                let issued_point = facts
                    .loan_issued_at
                    .iter()
                    .find(|&&(_, l, _)| l == loan)
                    .map(|&(_, _, point)| point)
                    .unwrap_or_else(|| panic!("loan {:?} was never issued?", loan));
                let issued_loc = maps.get_point_location(issued_point);
                let stmt = mir.stmt_at(issued_loc).left().unwrap_or_else(|| {
                    panic!(
                        "loan {:?} was issued by a terminator (at {:?})?",
                        loan, issued_loc
                    );
                });
                let ptr = match stmt.kind {
                    StatementKind::Assign(ref x) => match describe_rvalue(&x.1) {
                        Some(RvalueDesc::Project { base, proj: _ }) => acx
                            .ptr_of(base)
                            .unwrap_or_else(|| panic!("missing pointer ID for {:?}", base)),
                        Some(RvalueDesc::AddrOfLocal { local, proj: _ }) => {
                            acx.addr_of_local[local]
                        }
                        None => panic!("loan {:?} was issued by unknown rvalue {:?}?", loan, x.1),
                    },
                    _ => panic!("loan {:?} was issued by non-assign stmt {:?}?", loan, stmt),
                };
                eprintln!("want to drop UNIQUE from pointer {:?}", ptr);

                if hypothesis[ptr].contains(PermissionSet::UNIQUE) {
                    hypothesis[ptr].remove(PermissionSet::UNIQUE);
                    changed = true;
                }
            }
        }

        eprintln!("propagate");
        changed |= dataflow.propagate(hypothesis);
        eprintln!("done propagating");

        if !changed {
            eprintln!(
                "{} unresolved borrowck errors in function {:?} (after {} iterations)",
                output.errors.len(),
                name,
                i,
            );
            break;
        }
    }
}

fn run_polonius<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    hypothesis: &PointerTableMut<PermissionSet>,
    name: &str,
    mir: &Body<'tcx>,
    adt_metadata: &AdtMetadataTable<'tcx>,
    field_tys: &HashMap<DefId, crate::LTy<'tcx>>,
) -> (AllFacts, AtomMaps<'tcx>, Output) {
    let tcx = acx.tcx();
    let mut facts = AllFacts::default();
    let mut maps = AtomMaps::default();

    // Start the origin counter at 3.  This has no effect on the semantics, but makes for easier
    // diffs between our facts and the facts generated by rustc.
    for _ in 0..3 {
        let _ = maps.origin();
    }

    //pretty::write_mir_fn(tcx, mir, &mut |_, _| Ok(()), &mut std::io::stdout()).unwrap();

    // Populate `cfg_edge`
    for (bb, bb_data) in mir.basic_blocks().iter_enumerated() {
        eprintln!("{:?}:", bb);

        for idx in 0..bb_data.statements.len() {
            eprintln!("  {}: {:?}", idx, bb_data.statements[idx]);
            let start = maps.point(bb, idx, SubPoint::Start);
            let mid = maps.point(bb, idx, SubPoint::Mid);
            let next_start = maps.point(bb, idx + 1, SubPoint::Start);
            facts.cfg_edge.push((start, mid));
            facts.cfg_edge.push((mid, next_start));
        }

        let term_idx = bb_data.statements.len();
        eprintln!("  {}: {:?}", term_idx, bb_data.terminator());
        let term_start = maps.point(bb, term_idx, SubPoint::Start);
        let term_mid = maps.point(bb, term_idx, SubPoint::Mid);
        facts.cfg_edge.push((term_start, term_mid));
        for succ in bb_data.terminator().successors() {
            let succ_start = maps.point(succ, 0, SubPoint::Start);
            facts.cfg_edge.push((term_mid, succ_start));
        }
    }

    // From rustc_borrowck::nll::populate_polonius_move_facts: "Non-arguments start out
    // deinitialised; we simulate this with an initial move".  On the other hand, arguments are
    // considered assigned at the entry point.
    let entry_point = maps.point(START_BLOCK, 0, SubPoint::Start);
    for local in mir.local_decls.indices() {
        if mir.local_kind(local) == LocalKind::Arg {
            let path = maps.path(
                &mut facts,
                Place {
                    local,
                    projection: List::empty(),
                },
            );
            facts.path_assigned_at_base.push((path, entry_point));
        } else {
            let path = maps.path(
                &mut facts,
                Place {
                    local,
                    projection: List::empty(),
                },
            );
            facts.path_moved_at_base.push((path, entry_point));
        }
    }

    // Populate `use_of_var_derefs_origin`, and generate `LTy`s for all locals.
    let ltcx = LabeledTyCtxt::new(tcx);
    let mut local_ltys = Vec::with_capacity(mir.local_decls.len());
    for local in mir.local_decls.indices() {
        let lty = assign_origins(
            ltcx,
            hypothesis,
            &mut facts,
            &mut maps,
            adt_metadata,
            acx.local_tys[local],
        );
        let var = maps.variable(local);
        lty.for_each_label(&mut |label| {
            if let Some(origin) = label.origin {
                facts.use_of_var_derefs_origin.push((var, origin));
            }
        });
        local_ltys.push(lty);
    }

    // Gather field permissions
    let mut field_permissions: HashMap<DefId, PermissionSet> = HashMap::new();
    for (did, lty) in field_tys {
        let perm = if lty.label.is_none() {
            PermissionSet::empty()
        } else {
            hypothesis[lty.label]
        };
        field_permissions.insert(*did, perm);
    }

    let mut loans = HashMap::<Local, Vec<(Path, Loan, BorrowKind)>>::new();
    // Populate `loan_issued_at` and `loans`.
    type_check::visit(
        tcx,
        ltcx,
        &mut facts,
        &mut maps,
        &mut loans,
        &local_ltys,
        &field_permissions,
        mir,
        adt_metadata,
    );

    // Populate `loan_invalidated_at`
    def_use::visit_loan_invalidated_at(acx.tcx(), &mut facts, &mut maps, &loans, mir);

    // Populate `var_defined/used/dropped_at` and `path_assigned/accessed_at_base`.
    def_use::visit(&mut facts, &mut maps, mir);

    dump::dump_facts_to_dir(&facts, &maps, format!("inspect/{}", name)).unwrap();

    let output = polonius_engine::Output::compute(&facts, polonius_engine::Algorithm::Naive, true);
    dump::dump_output_to_dir(&output, &maps, format!("inspect/{}", name)).unwrap();

    (facts, maps, output)
}

fn assign_origins<'tcx>(
    ltcx: LTyCtxt<'tcx>,
    hypothesis: &PointerTableMut<PermissionSet>,
    _facts: &mut AllFacts,
    maps: &mut AtomMaps<'tcx>,
    adt_metadata: &AdtMetadataTable<'tcx>,
    lty: crate::LTy<'tcx>,
) -> LTy<'tcx> {
    ltcx.relabel(lty, &mut |lty| {
        let perm = if lty.label.is_none() {
            PermissionSet::empty()
        } else {
            hypothesis[lty.label]
        };

        let construct_adt_origins = |ty: &Ty, amaps: &mut AtomMaps| -> Option<&_> {
            let adt_def = ty.ty_adt_def()?;

            // create a concrete origin for each actual or hypothetical
            // lifetime parameter in this ADT
            let origins: Vec<_> = adt_metadata
                .table
                .get(&adt_def.did())?
                .lifetime_params
                .iter()
                .map(|o| {
                    let pairing = (*o, amaps.origin());
                    eprintln!("pairing lifetime parameter with origin: {pairing:?}");
                    pairing
                })
                .collect();

            if origins.is_empty() {
                return None;
            }

            Some(ltcx.arena().alloc_slice(&origins[..]))
        };
        match lty.ty.kind() {
            TyKind::Ref(_, _, _) | TyKind::RawPtr(_) => {
                let origin = Some(maps.origin());
                Label {
                    origin,
                    origin_params: None,
                    perm,
                }
            }
            TyKind::Adt(..) => {
                let origin_params = construct_adt_origins(&lty.ty, maps);
                Label {
                    origin: None,
                    origin_params,
                    perm,
                }
            }
            _ => {
                // let origin_params = construct_adt_origins(&lty.ty, maps);
                Label {
                    origin: None,
                    origin_params: None,
                    perm,
                }
            }
        }
    })
}
