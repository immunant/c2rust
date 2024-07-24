use crate::context::AnalysisCtxt;
use crate::pointer_id::{
    GlobalPointerTable, LocalPointerTable, NextGlobalPointerId, NextLocalPointerId, PointerId,
    PointerTable,
};
use rustc_middle::mir::Body;
use std::mem;

mod constraint_set;
mod solve;
mod type_check;

pub use self::constraint_set::{CTy, Constraint, ConstraintSet};
pub use self::solve::{solve_constraints, PointeeTypes};

pub fn generate_constraints<'tcx>(
    acx: &AnalysisCtxt<'_, 'tcx>,
    mir: &Body<'tcx>,
) -> ConstraintSet<'tcx> {
    type_check::visit(acx, mir)
}

pub fn remap_pointers_global<'tcx>(
    pointee_types: &mut GlobalPointerTable<PointeeTypes<'tcx>>,
    map: &GlobalPointerTable<PointerId>,
    count: usize,
) {
    let mut old = mem::replace(pointee_types, GlobalPointerTable::new(count));
    let new = pointee_types;
    for (old_ptr, old_val) in old.iter_mut() {
        // If there are multiple old pointers that map to the same new pointer, merge their sets.
        new[map[old_ptr]].merge(mem::take(old_val));
    }
}

pub fn remap_pointers_local<'tcx>(
    global_pointee_types: &mut GlobalPointerTable<PointeeTypes<'tcx>>,
    local_pointee_types: &mut LocalPointerTable<PointeeTypes<'tcx>>,
    map: PointerTable<PointerId>,
    local_base: u32,
    local_count: usize,
) {
    let mut old = mem::replace(
        local_pointee_types,
        LocalPointerTable::new(local_base, local_count),
    );
    let mut new = global_pointee_types.and_mut(local_pointee_types);
    for (old_ptr, old_val) in old.iter_mut() {
        // If there are multiple old pointers that map to the same new pointer, merge their sets.
        new[map[old_ptr]].merge(mem::take(old_val));
    }
}
