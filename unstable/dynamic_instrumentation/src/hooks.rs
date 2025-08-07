use rustc_middle::ty::TyCtxt;
use rustc_span::{
    def_id::{DefId, CRATE_DEF_INDEX},
    Symbol,
};

#[derive(Clone, Copy)]
pub struct Hooks<'tcx> {
    tcx: TyCtxt<'tcx>,
    runtime_crate_did: DefId,
}

impl<'tcx> Hooks<'tcx> {
    pub fn new(tcx: TyCtxt<'tcx>) -> Self {
        let runtime_crate = tcx
            .crates(())
            .iter()
            .cloned()
            .find(|&krate| tcx.crate_name(krate).as_str() == "c2rust_analysis_rt")
            .unwrap();

        let runtime_crate_did = DefId {
            krate: runtime_crate,
            index: CRATE_DEF_INDEX,
        };

        Self {
            tcx,
            runtime_crate_did,
        }
    }

    fn try_find(&self, name: Symbol) -> Option<DefId> {
        Some(
            self.tcx
                .module_children(self.runtime_crate_did)
                .iter()
                .find(|child| child.ident.name == name)?
                .res
                .def_id(),
        )
    }

    fn find_from_symbol_using_name<'a>(&self, symbol: Symbol, name: impl Fn() -> &'a str) -> DefId {
        self.try_find(symbol).unwrap_or_else(|| {
            let name = name();
            panic!("could not find `{name}` hook");
        })
    }

    pub fn find_from_symbol(&self, name: Symbol) -> DefId {
        self.find_from_symbol_using_name(name, || name.as_str())
    }

    pub fn find(&self, name: &str) -> DefId {
        self.find_from_symbol_using_name(Symbol::intern(name), || name)
    }
}
