#![feature(rustc_private)]
extern crate rustc_ast;
extern crate rustc_driver;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;

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
    LocalDecl, Mutability, Rvalue, AggregateKind, Place, Operand, Statement, StatementKind,
    BorrowKind, Constant, ConstantKind,
};
use rustc_middle::mir::interpret::{Allocation, ConstValue};
use rustc_middle::ty::{TyCtxt, RegionKind, WithOptConstParam};
use rustc_middle::ty::query::{Providers, ExternProviders};
use rustc_session::Session;
use rustc_span::DUMMY_SP;
use rustc_span::def_id::{DefId, LocalDefId, CRATE_DEF_INDEX};
use rustc_span::symbol::Ident;
use rustc_target::abi::Align;
use crate::atoms::{AllFacts, AtomMaps, SubPoint};

mod atoms;
mod dump;


fn inspect_mir<'tcx>(
    tcx: TyCtxt<'tcx>,
    def: WithOptConstParam<LocalDefId>,
    mir: &Body<'tcx>,
) {
    let mut facts = AllFacts::default();
    let mut maps = AtomMaps::default();

    // Populate `cfg_edge`
    for (bb, bb_data) in mir.basic_blocks().iter_enumerated() {
        for idx in 0 .. bb_data.statements.len() {
            let start = maps.point(bb, idx, SubPoint::Start);
            let mid = maps.point(bb, idx, SubPoint::Mid);
            let next_start = maps.point(bb, idx + 1, SubPoint::Start);
            facts.cfg_edge.push((start, mid));
            facts.cfg_edge.push((mid, next_start));
        }

        let term_idx = bb_data.statements.len();
        let term_start = maps.point(bb, term_idx, SubPoint::Start);
        let term_mid = maps.point(bb, term_idx, SubPoint::Mid);
        facts.cfg_edge.push((term_start, term_mid));
        for &succ in bb_data.terminator().successors() {
            let succ_start = maps.point(succ, 0, SubPoint::Start);
            facts.cfg_edge.push((term_mid, succ_start));
        }
    }

    let name = tcx.item_name(def.to_global().did);
    dump::dump_facts_to_dir(&facts, &maps, format!("inspect/{}", name)).unwrap();
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
