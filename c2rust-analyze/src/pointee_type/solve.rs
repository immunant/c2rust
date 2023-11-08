use super::constraint_set::{CTy, Constraint, ConstraintSet, VarTable};
use crate::context::LTy;
use crate::pointer_id::{OwnedPointerTable, PointerId, PointerTable, PointerTableMut};
use log::warn;
use std::collections::{HashMap, HashSet};
use std::ptr;

/// Add initial types to `ty_sets` based on the constraints in `cset`.
pub fn init_type_sets<'tcx>(
    cset: &ConstraintSet<'tcx>,
    mut ty_sets: PointerTableMut<HashSet<CTy<'tcx>>>,
) {
    for constraint in &cset.constraints {
        if let Constraint::ContainsType(ptr, cty) = *constraint {
            ty_sets[ptr].insert(cty);
        }
    }
}

fn index_both<'a, T>(
    pt: &'a mut PointerTableMut<T>,
    ptr1: PointerId,
    ptr2: PointerId,
) -> (&'a mut T, &'a mut T) {
    unsafe {
        assert_ne!(ptr1, ptr2);
        let x1 = ptr::addr_of_mut!(pt[ptr1]);
        let x2 = ptr::addr_of_mut!(pt[ptr2]);
        (&mut *x1, &mut *x2)
    }
}

/// Propagate pointee types through `Subset` relationships.  For each unsatisfied `Subset`
/// constraints, we add all pointee types in the subset to the superset, which satisfies the
/// constraint by expanding the superset.
///
/// The global portion of `ty_sets` is only the local view of the global pointee type sets, so it
/// can contain local `CTy::Var`s and refer to local `PointerId`s.
pub fn propagate_types<'tcx>(
    cset: &ConstraintSet<'tcx>,
    mut ty_sets: PointerTableMut<HashSet<CTy<'tcx>>>,
) {
    // Map from each `PointerId` to the `PointerId`s whose `ty_sets` should be supersets.
    let mut subset_graph = HashMap::<_, HashSet<_>>::new();
    // Set of `PointerId`s whose `ty_sets` were recently modified.  The changes to these `ty_sets`
    // need to be propagated to its supersets in `subset_graph`.
    let mut work_set = HashSet::new();
    for constraint in &cset.constraints {
        if let Constraint::Subset(ptr1, ptr2) = *constraint {
            let new = subset_graph
                .entry(ptr1)
                .or_insert_with(HashSet::new)
                .insert(ptr2);

            // Initial update: propagate pointee types from `ptr1` to `ptr2`.
            if new && !ty_sets[ptr1].is_subset(&ty_sets[ptr2]) {
                let (tys1, tys2) = index_both(&mut ty_sets, ptr1, ptr2);
                for cty in tys1.iter() {
                    tys2.insert(cty.clone());
                }
                // Since `ty_sets[ptr2]` was not a subset of `ty_sets[ptr1]`, we must have added at
                // least one element to `ty_sets[ptr2]`.
                work_set.insert(ptr2);
            }
        }
    }

    while let Some(&ptr1) = work_set.iter().next() {
        work_set.remove(&ptr1);
        let ptr2s = match subset_graph.get(&ptr1) {
            Some(x) => x,
            None => continue,
        };
        for &ptr2 in ptr2s {
            if !ty_sets[ptr1].is_subset(&ty_sets[ptr2]) {
                let (tys1, tys2) = index_both(&mut ty_sets, ptr1, ptr2);
                for cty in tys1.iter() {
                    tys2.insert(cty.clone());
                }
                // Since `ty_sets[ptr2]` was not a subset of `ty_sets[ptr1]`, we must have added at
                // least one element to `ty_sets[ptr2]`.
                work_set.insert(ptr2);
            }
        }
    }

    fn unify_types<'tcx>(
        var_table: &VarTable<'tcx>,
        ctys: &HashSet<CTy<'tcx>>,
        extra_cty: Option<CTy<'tcx>>,
    ) {
        let mut prev = extra_cty;
        for &cty in ctys {
            if let Some(prev) = prev {
                match var_table.unify(prev, cty) {
                    Ok(()) => {}
                    Err((ty1, ty2)) => {
                        warn!("unification failed: {ty1:?} != {ty2:?}");
                    }
                }
            }
            prev = Some(cty);
        }
    }

    // Currently, we just require all the types to unify.  In the future perhaps we can extend this
    // to do something smarter in cases where the set contains both `u8` and `[u8; 10]`, for
    // example.
    for constraint in &cset.constraints {
        if let Constraint::AllTypesCompatibleWith(ptr, cty) = *constraint {
            unify_types(&cset.var_table, &ty_sets[ptr], Some(cty));
        }
    }

    for (_, ctys) in ty_sets.iter() {
        unify_types(&cset.var_table, ctys, None);
    }

    #[cfg(debug_assertions)]
    {
        for constraint in &cset.constraints {
            if let Constraint::Subset(ptr1, ptr2) = *constraint {
                assert!(ty_sets[ptr1].is_subset(&ty_sets[ptr2]));
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Default)]
pub struct PointeeTypes<'tcx> {
    /// The possible pointee types for this pointer.
    pub ltys: HashSet<LTy<'tcx>>,
    /// If set, `ltys` is incomplete - the analysis identified pointee types that couldn't be
    /// exported into global scope.
    pub incomplete: bool,
}

impl<'tcx> PointeeTypes<'tcx> {
    /// Get the sole `LTy` in this set, if there is exactly one.
    pub fn get_sole_lty(&self) -> Option<LTy<'tcx>> {
        if self.incomplete || self.ltys.len() != 1 {
            return None;
        }
        let lty = *self.ltys.iter().next().unwrap();
        Some(lty)
    }

    pub fn merge(&mut self, other: PointeeTypes<'tcx>) {
        self.ltys.extend(other.ltys);
        self.incomplete |= other.incomplete;
    }
}

/// Copy `LTy`s from `pointee_tys` into `ty_sets` for processing by the analysis.
fn import<'tcx>(
    pointee_tys: PointerTable<PointeeTypes<'tcx>>,
    mut ty_sets: PointerTableMut<HashSet<CTy<'tcx>>>,
) {
    for (ptr, tys) in pointee_tys.iter() {
        let ty_set = &mut ty_sets[ptr];
        for &lty in &tys.ltys {
            ty_set.insert(CTy::Ty(lty));
        }
    }
}

/// Compute concrete `LTy`s for all the `CTy`s in `ty_sets`, and add them into `pointee_tys`.
fn export<'tcx>(
    var_table: &VarTable<'tcx>,
    ty_sets: PointerTable<HashSet<CTy<'tcx>>>,
    mut pointee_tys: PointerTableMut<PointeeTypes<'tcx>>,
) {
    for (ptr, ctys) in ty_sets.iter() {
        let out = &mut pointee_tys[ptr];
        for &cty in ctys {
            if let CTy::Ty(lty) = var_table.cty_rep(cty) {
                let mut ok = true;
                lty.for_each_label(&mut |p| {
                    if p.is_local() {
                        ok = false;
                    }
                });
                if ok {
                    out.ltys.insert(lty);
                    continue;
                }
            }
            // If we failed to export this `CTy`, mark the `PointeeTypes` incomplete.
            out.incomplete = true;
        }
    }
}

pub fn solve_constraints<'tcx>(
    cset: &ConstraintSet<'tcx>,
    mut pointee_tys: PointerTableMut<PointeeTypes<'tcx>>,
) {
    // Clear the `incomplete` flags for all local pointers.  If there are still non-exportable
    // types for those pointers, the flag will be set again in `export()`.
    for (_, tys) in pointee_tys.local_mut().iter_mut() {
        tys.incomplete = false;
    }

    let mut ty_sets = OwnedPointerTable::with_len_of(&pointee_tys.borrow());
    import(pointee_tys.borrow(), ty_sets.borrow_mut());
    init_type_sets(cset, ty_sets.borrow_mut());
    propagate_types(cset, ty_sets.borrow_mut());
    export(&cset.var_table, ty_sets.borrow(), pointee_tys.borrow_mut());
}
