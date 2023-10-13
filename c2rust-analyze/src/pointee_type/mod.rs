use crate::context::AnalysisCtxt;
use rustc_middle::mir::Body;

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
