#![feature(min_specialization)]
#![feature(rustc_private)]
#![feature(map_try_insert)]

extern crate rustc_ast;
extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_mir_transform;
extern crate rustc_serialize;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;

mod assert;
mod builder;
mod graph;
mod query;
mod util;

use builder::{construct_pdg, read_event_log};
use clap::{Parser, ValueEnum};
use color_eyre::eyre;
use std::{
    collections::HashSet,
    fmt::{self, Display, Formatter},
    path::{Path, PathBuf},
};

use crate::builder::read_metadata;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, ValueEnum)]
enum ToPrint {
    Graphs,
    Counts,
    Events,
    LatestAssignments,
    WritePermissions,
}

impl Display for ToPrint {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.to_possible_value().unwrap().get_name())
    }
}

/// Construct and query a PDG from an instrumented program's event log.
#[derive(Debug, Parser)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Path to an event log from a run of an instrumented program.
    #[clap(long, value_parser)]
    event_log: PathBuf,

    /// Path to the instrumented program's metadata generated at compile/instrumentation time.
    #[clap(long, value_parser)]
    metadata: PathBuf,

    /// What to print.
    #[clap(long, value_parser, default_value = "graphs")]
    print: Vec<ToPrint>,
}

fn main() -> eyre::Result<()> {
    color_eyre::install()?;
    env_logger::init();
    let args = Args::parse();

    let print_args = args.print.iter().collect::<HashSet<_>>();
    let should_print = |to_print| print_args.contains(&to_print);

    let events = read_event_log(Path::new(&args.event_log))?;
    let metadata = read_metadata(Path::new(&args.metadata))?;
    let metadata = &metadata;

    if should_print(ToPrint::Events) {
        for event in &events {
            let mir_loc = metadata.get(event.mir_loc);
            let kind = &event.kind;
            println!("{mir_loc:?} -> {kind:?}");
        }
    }

    let pdg = construct_pdg(&events, metadata);
    pdg.assert_all_tests();

    if should_print(ToPrint::LatestAssignments) {
        for ((func_hash, local), p) in &pdg.latest_assignment {
            let func = &metadata.functions[func_hash];
            println!("({func}:{local:?}) => {p:?}");
        }
    }

    for (graph_id, graph) in pdg.graphs.iter_enumerated() {
        let needs_write = graph
            .needs_write_permission()
            .map(|node_id| node_id.as_usize())
            .collect::<Vec<_>>();
        if should_print(ToPrint::Graphs) {
            println!("{graph_id} {graph}");
        }
        if should_print(ToPrint::WritePermissions) {
            println!("nodes_that_need_write = {needs_write:?}");
        }
        if should_print(ToPrint::Graphs) || should_print(ToPrint::WritePermissions) {
            println!();
        }
    }

    if should_print(ToPrint::Counts) {
        let num_graphs = pdg.graphs.len();
        let num_nodes = pdg
            .graphs
            .iter()
            .map(|graph| graph.nodes.len())
            .sum::<usize>();
        println!("num_graphs = {num_graphs}");
        println!("num_nodes = {num_nodes}");
    }

    Ok(())
}
