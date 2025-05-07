use crate::context::LTy;
use crate::pointer_id::PointerId;
use std::cell::Cell;
use std::collections::HashSet;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum Constraint<'tcx> {
    /// The set of types for pointer `.0` must contain type `.1`.  This is used for "uses" of a
    /// pointer, where the pointer is dereferenced to load or store data of a certain type.
    ContainsType(PointerId, CTy<'tcx>),

    /// All possible types for pointer `.0` must be compatible with type `.1`.  This is used for
    /// "definitions" of a pointer, where the pointer is constructed by taking the address of data
    /// of a certain type.
    AllTypesCompatibleWith(PointerId, CTy<'tcx>),

    /// All possible types for pointer `.0` must be mutually compatible.  This is used for
    /// "definitions" of a pointer where the type of the data is unknown.
    AllTypesCompatible(PointerId),

    /// The set of types for pointer `.0` must be a subset of the set of types for pointer `.1`.
    /// Among other things, this is used for pointer assignments like `p = q`, where `p`'s types
    /// must be a subset of `q`'s (`Subset(p_ptr, q_ptr)`).
    Subset(PointerId, PointerId),
}

#[derive(Clone, Debug, Default)]
pub struct ConstraintSet<'tcx> {
    pub constraints: Vec<Constraint<'tcx>>,
    constraint_dedup: HashSet<Constraint<'tcx>>,
}

impl<'tcx> ConstraintSet<'tcx> {
    fn add(&mut self, c: Constraint<'tcx>) {
        if self.constraint_dedup.insert(c) {
            self.constraints.push(c);
        }
    }

    pub fn contains_type(&mut self, p: PointerId, cty: CTy<'tcx>) {
        self.add(Constraint::ContainsType(p, cty));
    }

    pub fn all_types_compatible_with(&mut self, p: PointerId, cty: CTy<'tcx>) {
        self.add(Constraint::AllTypesCompatibleWith(p, cty));
    }

    pub fn all_types_compatible(&mut self, p: PointerId) {
        self.add(Constraint::AllTypesCompatible(p));
    }

    pub fn subset(&mut self, p: PointerId, q: PointerId) {
        self.add(Constraint::Subset(p, q));
    }
}

/// A "constraint type", which is either an `LTy` or an inference variable.
///
/// Our current implementation of type inference / unification is very primitive.  In particular,
/// currently we allow inference variables only at top level, so constraints can refer to `T` but
/// not `*mut T` or `[T; 10]`.  Eventually we may need to replace this implementation with
/// something more flexible.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum CTy<'tcx> {
    Ty(LTy<'tcx>),
    /// An inference variable.  Note that inference variables are scoped to the local function;
    /// there are no global inference variables.
    Var(usize),
}

impl<'tcx> From<LTy<'tcx>> for CTy<'tcx> {
    fn from(x: LTy<'tcx>) -> CTy<'tcx> {
        CTy::Ty(x)
    }
}

#[derive(Clone, Debug, Default)]
pub struct VarTable<'tcx> {
    /// Equivalence class representative for each variable.  This can be either a known type
    /// (`CTy::Ty`) or an inference variable (`CTy::Var`).
    vars: Vec<Cell<CTy<'tcx>>>,
}

impl<'tcx> VarTable<'tcx> {
    pub fn fresh(&mut self) -> CTy<'tcx> {
        let cty = CTy::Var(self.vars.len());
        // Initially, the new variable is its own representative.
        self.vars.push(Cell::new(cty));
        cty
    }

    /// Obtain the representative for variable `var`.
    pub fn rep(&self, var: usize) -> CTy<'tcx> {
        let cty = self.vars[var].get();
        match cty {
            CTy::Var(var2) => {
                if var2 == var {
                    // `var` is the representative of its own equivalence class.
                    cty
                } else {
                    let rep = self.rep(var2);
                    // Do path compression.
                    self.vars[var].set(rep);
                    rep
                }
            }
            CTy::Ty(_) => {
                // `cty` is a concrete type, which is the representative of `var`'s class.
                cty
            }
        }
    }

    pub fn cty_rep(&self, cty: CTy<'tcx>) -> CTy<'tcx> {
        match cty {
            CTy::Ty(_) => cty,
            CTy::Var(v) => self.rep(v),
        }
    }

    /// Unify two types.  If both resolve to concrete types and those types are unequal, this
    /// returns `Err` with the two concrete types.
    pub fn unify(&self, cty1: CTy<'tcx>, cty2: CTy<'tcx>) -> Result<(), (LTy<'tcx>, LTy<'tcx>)> {
        match (self.cty_rep(cty1), self.cty_rep(cty2)) {
            (CTy::Var(v1), CTy::Var(v2)) => {
                // Make one the representative for the other.
                debug_assert_eq!(self.vars[v1].get(), CTy::Var(v1));
                debug_assert_eq!(self.vars[v2].get(), CTy::Var(v2));
                self.vars[v1].set(CTy::Var(v2));
                Ok(())
            }
            (CTy::Var(v), CTy::Ty(ty)) | (CTy::Ty(ty), CTy::Var(v)) => {
                debug_assert_eq!(self.vars[v].get(), CTy::Var(v));
                self.vars[v].set(CTy::Ty(ty));
                Ok(())
            }
            (CTy::Ty(ty1), CTy::Ty(ty2)) => {
                if ty1 == ty2 {
                    Ok(())
                } else {
                    Err((ty1, ty2))
                }
            }
        }
    }
}
