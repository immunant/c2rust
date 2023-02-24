use std::fmt::{self, Write};

use rustc_data_structures::intern::Interned;
use rustc_hir::{
    def_id::{CrateNum, DefId},
    definitions::DisambiguatedDefPathData,
};
use rustc_middle::{
    bug,
    ty::{
        self,
        print::{with_no_trimmed_paths, PrettyPrinter, Print, Printer},
        GenericArg, Ty, TyCtxt,
    },
};

struct CanonicalPathPrinter<'tcx> {
    tcx: TyCtxt<'tcx>,
    path: String,
    def_id: Option<DefId>,
}

impl<'tcx> Printer<'tcx> for CanonicalPathPrinter<'tcx> {
    type Error = std::fmt::Error;

    type Path = Self;
    type Region = Self;
    type Type = Self;
    type DynExistential = Self;
    type Const = Self;

    fn tcx(&self) -> TyCtxt<'tcx> {
        self.tcx
    }

    /// First save the [`DefId`] here in [`Self::def_id`],
    /// so we can access it later.
    fn print_def_path(
        mut self,
        def_id: DefId,
        substs: &'tcx [GenericArg<'tcx>],
    ) -> Result<Self::Path, Self::Error> {
        self.def_id = Some(def_id);
        self.default_print_def_path(def_id, substs)
    }

    fn print_region(self, _region: ty::Region<'_>) -> Result<Self::Region, Self::Error> {
        Ok(self)
    }

    fn print_type(mut self, ty: Ty<'tcx>) -> Result<Self::Type, Self::Error> {
        match *ty.kind() {
            // Types without identity.
            ty::Bool
            | ty::Char
            | ty::Int(_)
            | ty::Uint(_)
            | ty::Float(_)
            | ty::Str
            | ty::Array(_, _)
            | ty::Slice(_)
            | ty::RawPtr(_)
            | ty::Ref(_, _, _)
            | ty::FnPtr(_)
            | ty::Never
            | ty::Tuple(_)
            | ty::Dynamic(_, _) => self.pretty_print_type(ty),

            // Placeholders (all printed as `_` to uniformize them).
            ty::Param(_) | ty::Bound(..) | ty::Placeholder(_) | ty::Infer(_) | ty::Error(_) => {
                write!(self, "_")?;
                Ok(self)
            }

            // Types with identity (print the module path).
            ty::Adt(ty::AdtDef(Interned(&ty::AdtDefData { did: def_id, .. }, _)), substs)
            | ty::FnDef(def_id, substs)
            | ty::Opaque(def_id, substs)
            | ty::Projection(ty::ProjectionTy {
                item_def_id: def_id,
                substs,
            })
            | ty::Closure(def_id, substs)
            | ty::Generator(def_id, substs, _) => self.print_def_path(def_id, substs),
            ty::Foreign(def_id) => self.print_def_path(def_id, &[]),

            ty::GeneratorWitness(_) => bug!("type_name: unexpected `GeneratorWitness`"),
        }
    }

    fn print_const(self, ct: ty::Const<'tcx>) -> Result<Self::Const, Self::Error> {
        self.pretty_print_const(ct, false)
    }

    fn print_dyn_existential(
        mut self,
        predicates: &'tcx ty::List<ty::Binder<'tcx, ty::ExistentialPredicate<'tcx>>>,
    ) -> Result<Self::DynExistential, Self::Error> {
        let mut first = true;
        for p in predicates {
            if !first {
                write!(self, "+")?;
            }
            first = false;
            self = p.print(self)?;
        }
        Ok(self)
    }

    /// To have stable canonical paths for crate-local items,
    /// we use `crate` instead of the current crate name.
    /// To detect if we're in the current crate,
    /// we need the [`DefId`] that's being printed,
    /// so we save that in [`Self::def_id`] in [`Self::print_def_path`],
    /// which we access here.
    fn path_crate(mut self, cnum: CrateNum) -> Result<Self::Path, Self::Error> {
        if self.def_id.unwrap().is_local() {
            self.path.push_str("crate");
        } else {
            self.path.push_str(self.tcx.crate_name(cnum).as_str());
        }
        Ok(self)
    }

    fn path_qualified(
        self,
        self_ty: Ty<'tcx>,
        trait_ref: Option<ty::TraitRef<'tcx>>,
    ) -> Result<Self::Path, Self::Error> {
        self.pretty_path_qualified(self_ty, trait_ref)
    }

    fn path_append_impl(
        self,
        print_prefix: impl FnOnce(Self) -> Result<Self::Path, Self::Error>,
        _disambiguated_data: &DisambiguatedDefPathData,
        _self_ty: Ty<'tcx>,
        _trait_ref: Option<ty::TraitRef<'tcx>>,
    ) -> Result<Self::Path, Self::Error> {
        print_prefix(self)
    }

    fn path_append(
        mut self,
        print_prefix: impl FnOnce(Self) -> Result<Self::Path, Self::Error>,
        disambiguated_data: &DisambiguatedDefPathData,
    ) -> Result<Self::Path, Self::Error> {
        self = print_prefix(self)?;

        write!(self.path, "::{}", disambiguated_data.data).unwrap();

        Ok(self)
    }

    fn path_generic_args(
        self,
        print_prefix: impl FnOnce(Self) -> Result<Self::Path, Self::Error>,
        _args: &[GenericArg<'tcx>],
    ) -> Result<Self::Path, Self::Error> {
        print_prefix(self)
    }
}

impl<'tcx> PrettyPrinter<'tcx> for CanonicalPathPrinter<'tcx> {
    fn should_print_region(&self, _region: ty::Region<'_>) -> bool {
        false
    }
    fn comma_sep<T>(mut self, mut elems: impl Iterator<Item = T>) -> Result<Self, Self::Error>
    where
        T: Print<'tcx, Self, Output = Self, Error = Self::Error>,
    {
        if let Some(first) = elems.next() {
            self = first.print(self)?;
            for elem in elems {
                self.path.push_str(", ");
                self = elem.print(self)?;
            }
        }
        Ok(self)
    }

    fn generic_delimiters(
        mut self,
        f: impl FnOnce(Self) -> Result<Self, Self::Error>,
    ) -> Result<Self, Self::Error> {
        write!(self, "<")?;

        self = f(self)?;

        write!(self, ">")?;

        Ok(self)
    }
}

impl Write for CanonicalPathPrinter<'_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.path.push_str(s);
        Ok(())
    }
}

impl<'tcx> CanonicalPathPrinter<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>) -> Self {
        Self {
            tcx,
            path: String::with_capacity(16),
            def_id: None,
        }
    }
}

pub fn canonical_path<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> String {
    with_no_trimmed_paths! { CanonicalPathPrinter::new(tcx).print_type(ty).unwrap().path }
}
