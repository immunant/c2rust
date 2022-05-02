#![feature(rustc_private)]
extern crate either;
extern crate rustc_arena;
extern crate rustc_ast;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;

use std::collections::HashMap;
use std::env;
use std::hash::Hash;
use std::mem;
use polonius_engine::{self, Atom, FactTypes};
use rustc_ast::ast::{Item, ItemKind, Visibility, VisibilityKind};
use rustc_ast::node_id::NodeId;
use rustc_ast::ptr::P;
use rustc_driver::Compilation;
use rustc_index::vec::IndexVec;
use rustc_interface::Queries;
use rustc_interface::interface::Compiler;
use rustc_middle::mir::{
    Body, BasicBlock, BasicBlockData, START_BLOCK, Terminator, TerminatorKind, SourceInfo, Local,
    LocalDecl, LocalKind, Mutability, Rvalue, AggregateKind, Place, Operand, Statement,
    StatementKind, BorrowKind, Constant, ConstantKind,
};
use rustc_middle::mir::interpret::{Allocation, ConstValue};
use rustc_middle::mir::pretty;
use rustc_middle::ty::{TyCtxt, Ty, TyKind, RegionKind, WithOptConstParam, List};
use rustc_middle::ty::query::{Providers, ExternProviders};
use rustc_session::Session;
use rustc_span::DUMMY_SP;
use rustc_span::def_id::{DefId, LocalDefId, CRATE_DEF_INDEX};
use rustc_span::symbol::Ident;
use rustc_target::abi::Align;
use crate::context::{AnalysisCtxt, PointerId, PermissionSet, LTy};


mod borrowck;
mod context;
mod dataflow;
mod labeled_ty;
mod util;


fn inspect_mir<'tcx>(
    tcx: TyCtxt<'tcx>,
    def: WithOptConstParam<LocalDefId>,
    mir: &Body<'tcx>,
) {
    let name = tcx.item_name(def.to_global().did);
    eprintln!("\nprocessing function {:?}", name);

    let mut acx = AnalysisCtxt::new(tcx);

    // Label all pointers in local variables.
    // TODO: also label pointers in Rvalue::Cast (and ShallowInitBox?)
    assert!(acx.local_tys.len() == 0);
    acx.local_tys = IndexVec::with_capacity(mir.local_decls.len());
    for (local, decl) in mir.local_decls.iter_enumerated() {
        let lty = assign_pointer_ids(&acx, decl.ty);
        let l = acx.local_tys.push(lty);
        assert_eq!(local, l);

        let ptr = acx.new_pointer();
        let l = acx.addr_of_local.push(ptr);
        assert_eq!(local, l);
    }

    let dataflow = self::dataflow::generate_constraints(&acx, mir);

    let mut hypothesis = Vec::with_capacity(acx.num_pointers());
    for _ in 0 .. acx.num_pointers() {
        hypothesis.push(PermissionSet::UNIQUE);
    }
    dataflow.propagate(&mut hypothesis);

    borrowck::borrowck_mir(&acx, &dataflow, &mut hypothesis, name.as_str(), mir);
}

fn assign_pointer_ids<'tcx>(
    acx: &AnalysisCtxt<'tcx>,
    ty: Ty<'tcx>,
) -> LTy<'tcx> {
    acx.lcx.label(ty, &mut |ty| match ty.kind() {
        TyKind::Ref(_, _, _) |
        TyKind::RawPtr(_) => acx.new_pointer(),
        _ => PointerId::NONE,
    })
}


struct AnalysisCallbacks;

impl rustc_driver::Callbacks for AnalysisCallbacks {
    fn config(&mut self, config: &mut rustc_interface::Config) {
        config.override_queries = Some(override_queries);
    }
}

fn override_queries(
    sess: &Session,
    providers: &mut Providers,
    extern_providers: &mut ExternProviders,
) {
    providers.mir_built = |tcx, def: WithOptConstParam<LocalDefId>| {
        let mut providers = Providers::default();
        rustc_mir_build::provide(&mut providers);
        let steal_mir = (providers.mir_built)(tcx, def);

        inspect_mir(tcx, def, &steal_mir.borrow());

        steal_mir
    };
}

fn main() -> rustc_interface::interface::Result<()> {
    let mut args = env::args().collect::<Vec<_>>();
    rustc_driver::RunCompiler::new(&args, &mut AnalysisCallbacks).run()
}
