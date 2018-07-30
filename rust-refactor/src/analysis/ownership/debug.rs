//! Debug printing.

use std::fmt;

use rustc::hir;
use rustc::ty::TypeVariants;
use rustc_data_structures::indexed_vec::Idx;

use analysis::labeled_ty::LabeledTy;

use super::{ConcretePerm, Perm};


pub struct Pretty<'tcx, L: 'tcx>(pub LabeledTy<'tcx, L>);

pub fn pretty_slice<'a, 'tcx, L>(tys: &'a [LabeledTy<'tcx, L>]) -> &'a [Pretty<'tcx, L>] {
    unsafe { ::std::mem::transmute(tys) }
}


pub struct PrettyLabel<L>(pub L);

impl fmt::Debug for PrettyLabel<ConcretePerm> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            ConcretePerm::Read => write!(fmt, "READ"),
            ConcretePerm::Write => write!(fmt, "WRITE"),
            ConcretePerm::Move => write!(fmt, "MOVE"),
        }
    }
}

impl<L> fmt::Debug for PrettyLabel<Option<L>> where L: Copy, PrettyLabel<L>: fmt::Debug {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            Some(x) => write!(fmt, "{:?}", PrettyLabel(x)),
            None => Ok(()),
        }
    }
}

impl<'tcx> fmt::Debug for PrettyLabel<bool> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if self.0 {
            write!(fmt, "T")
        } else {
            write!(fmt, "F")
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct PrintVar<'tcx>(pub Perm<'tcx>);

impl<'tcx> fmt::Debug for PrettyLabel<(ConcretePerm, PrintVar<'tcx>)> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{:?}{:?}", PrettyLabel((self.0).0), PrettyLabel((self.0).1))
    }
}

impl<'tcx> fmt::Debug for PrettyLabel<PrintVar<'tcx>> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match (self.0).0 {
            Perm::Concrete(_) => Ok(()),
            Perm::StaticVar(v) => write!(fmt, "#s{}", v.index()),
            Perm::SigVar(v) => write!(fmt, "#f{}", v.index()),
            Perm::InstVar(v) => write!(fmt, "#i{}", v.index()),
            Perm::LocalVar(v) => write!(fmt, "#l{}", v.index()),

            Perm::Min(ps) => {
                write!(fmt, "#min(")?;
                let mut first = true;
                for &p in ps {
                    match p {
                        Perm::Concrete(_) => continue,
                        _ => {},
                    }
                    if !first {
                        write!(fmt, ", ")?;
                    }
                    first = false;
                    write!(fmt, "{:?}", PrettyLabel(PrintVar(p)))?;
                }
                write!(fmt, ")")
            }
        }
    }
}



impl<'tcx, L> fmt::Debug for Pretty<'tcx, L> where L: Copy + fmt::Debug, PrettyLabel<L>: fmt::Debug {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.0.ty.sty {
            TypeVariants::TyRef(_, ty, m) =>
                write!(fmt, "&{}{:?} {:?}",
                       if m == hir::MutImmutable { "" } else { "mut " },
                       PrettyLabel(self.0.label),
                       Pretty(self.0.args[0])),
            TypeVariants::TyRawPtr(mty) =>
                write!(fmt, "*{} {:?} {:?}",
                       if mty.mutbl == hir::MutImmutable { "const" } else { "mut" },
                       PrettyLabel(self.0.label),
                       Pretty(self.0.args[0])),
            _ => write!(fmt, "{:?}", self.0.ty),
        }
    }
}
