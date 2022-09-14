use rustc_index::vec::IndexVec;
use rustc_middle::{
    mir::{
        CastKind, Local, LocalDecl, Mutability, Operand, ProjectionElem, Rvalue, SourceInfo,
        Statement, StatementKind,
    },
    ty::{self, TyCtxt, TypeAndMut},
};
use rustc_span::DUMMY_SP;

use crate::{
    arg::{ArgKind, InstrumentationArg},
    mir_utils::remove_outer_deref,
};

/// Cast an argument from pointer to `usize`, if needed.
///
/// Casts `arg` to `usize` if needed, returning the cast statement and new,
/// `usize`-typed operand if the cast was needed. This cast statement must be
/// inserted into the function's body before the new operand is used.
/// `arg` will be used as a copy in the new statement,
/// so this statement must be inserted in a position where `arg` is alive.
pub fn cast_ptr_to_usize<'tcx>(
    tcx: TyCtxt<'tcx>,
    locals: &mut IndexVec<Local, LocalDecl<'tcx>>,
    arg: &InstrumentationArg<'tcx>,
) -> Option<(Vec<Statement<'tcx>>, Operand<'tcx>)> {
    let mut new_stmts = vec![];

    let arg_ty = arg.inner().ty(locals, tcx);

    let ptr = match arg {
        // If we were given an address as a `usize`, no conversion is necessary.
        InstrumentationArg::Op(ArgKind::AddressUsize(_arg)) => {
            assert!(
                arg_ty.is_integral() || arg_ty.is_unit(),
                "{:?}: {:?} is not of integral or unit type",
                arg,
                arg_ty
            );
            return None;
        }
        // From a reference `r`, cast through a raw ptr to a `usize`: `r as *mut _ as usize`.
        InstrumentationArg::Op(ArgKind::Reference(arg)) => {
            assert!(arg_ty.is_region_ptr());
            let inner_ty = arg_ty.builtin_deref(false).unwrap();
            let raw_ptr_ty = tcx.mk_ptr(inner_ty);
            let raw_ptr_local = locals.push(LocalDecl::new(raw_ptr_ty, DUMMY_SP));

            let mut deref = arg.place().expect("Can't get the address of a constant");
            let mut projs = Vec::with_capacity(deref.projection.len() + 1);
            projs.extend(deref.projection);
            projs.push(ProjectionElem::Deref);
            deref.projection = tcx.intern_place_elems(&projs);
            let cast_stmt = Statement {
                source_info: SourceInfo::outermost(DUMMY_SP),
                kind: StatementKind::Assign(Box::new((
                    raw_ptr_local.into(),
                    Rvalue::AddressOf(inner_ty.mutbl, deref),
                ))),
            };
            new_stmts.push(cast_stmt);
            Operand::Move(raw_ptr_local.into())
        }
        // From a raw pointer `r`, cast: `r as usize`.
        InstrumentationArg::Op(ArgKind::RawPtr(arg)) => {
            assert!(
                arg_ty.is_unsafe_ptr(),
                "{:?}: {:?} is not an unsafe ptr",
                arg,
                arg_ty
            );
            arg.to_copy()
        }
        // From a place to which a reference is also constructed,
        // create a raw ptr with `addr_of!`.
        InstrumentationArg::AddrOf(arg) => {
            let arg_place = arg.place().expect("Can't get the address of a constant");

            let arg_ty = arg_place.ty(locals, tcx).ty;
            let inner_ty = ty::TypeAndMut {
                ty: arg_ty,
                mutbl: Mutability::Not,
            };

            let raw_ptr_ty = tcx.mk_ptr(inner_ty);
            let raw_ptr_local = locals.push(LocalDecl::new(raw_ptr_ty, DUMMY_SP));

            let addr_of_stmt = Statement {
                source_info: SourceInfo::outermost(DUMMY_SP),
                kind: StatementKind::Assign(Box::new((
                    raw_ptr_local.into(),
                    Rvalue::AddressOf(inner_ty.mutbl, arg_place),
                ))),
            };
            new_stmts.push(addr_of_stmt);
            Operand::Move(raw_ptr_local.into())
        }
    };

    let ptr = {
        // Use `*const [(); 0]` as the opaque pointer type.
        let thin_raw_ptr_ty = tcx.mk_ptr(TypeAndMut {
            ty: tcx.mk_array(tcx.mk_unit(), 0),
            mutbl: Mutability::Not,
        });
        let casted_local = locals.push(LocalDecl::new(thin_raw_ptr_ty, DUMMY_SP));
        let casted_arg = Operand::Move(casted_local.into());
        let cast_stmt = Statement {
            source_info: SourceInfo::outermost(DUMMY_SP),
            kind: StatementKind::Assign(Box::new((
                casted_local.into(),
                Rvalue::Cast(CastKind::Misc, ptr, thin_raw_ptr_ty),
            ))),
        };
        new_stmts.push(cast_stmt);
        casted_arg
    };

    // Cast the raw ptr to a `usize` before passing to the instrumentation function.
    let usize_ty = tcx.mk_mach_uint(ty::UintTy::Usize);
    let casted_local = locals.push(LocalDecl::new(usize_ty, DUMMY_SP));
    let casted_arg = Operand::Move(casted_local.into());
    let cast_stmt = Statement {
        source_info: SourceInfo::outermost(DUMMY_SP),
        kind: StatementKind::Assign(Box::new((
            casted_local.into(),
            Rvalue::Cast(CastKind::PointerExposeAddress, ptr, usize_ty),
        ))),
    };
    new_stmts.push(cast_stmt);
    Some((new_stmts, casted_arg))
}
