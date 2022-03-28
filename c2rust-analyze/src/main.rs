#![feature(rustc_private)]
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


mod borrowck;
mod labeled_ty;


fn inspect_mir<'tcx>(
    tcx: TyCtxt<'tcx>,
    def: WithOptConstParam<LocalDefId>,
    mir: &Body<'tcx>,
) {
    borrowck::borrowck_mir(tcx, def, mir);
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
