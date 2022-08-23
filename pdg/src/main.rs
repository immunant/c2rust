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
use c2rust_analysis_rt::{events::Event, metadata::Metadata};
use clap::{Parser, ValueEnum};
use color_eyre::eyre;
use graph::Graphs;
use std::{
    fmt::{self, Display, Formatter},
    path::{Path, PathBuf},
    sync::Once,
};

use crate::builder::read_metadata;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, ValueEnum)]
pub enum ToPrint {
    Graphs,
    Counts,
    Events,
    LatestAssignments,
    WritePermissions,
    Metadata,
}

impl Display for ToPrint {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_str(self.to_possible_value().unwrap().get_name())
    }
}

pub struct Pdg {
    pub events: Vec<Event>,
    pub metadata: Metadata,
    pub graphs: Graphs,
}

impl Pdg {
    pub fn new(metadata_path: &Path, event_log_path: &Path) -> eyre::Result<Self> {
        let events = read_event_log(event_log_path)?;
        let metadata = read_metadata(metadata_path)?;
        let graphs = construct_pdg(&events, &metadata);
        Ok(Self {
            events,
            metadata,
            graphs,
        })
    }

    pub fn repr<'a>(&'a self, to_print: &'a [ToPrint]) -> PdgRepr<'a> {
        PdgRepr {
            pdg: self,
            to_print,
        }
    }
}

pub struct PdgRepr<'a> {
    pub pdg: &'a Pdg,
    pub to_print: &'a [ToPrint],
}

impl Display for PdgRepr<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let Self {
            pdg:
                Pdg {
                    events,
                    metadata,
                    graphs,
                },
            to_print,
        } = self;
        let should_print = |e| to_print.contains(&e);

        if should_print(ToPrint::Metadata) {
            writeln!(f, "{metadata:#?}")?;
        }

        if should_print(ToPrint::Events) {
            for event in events.iter() {
                let mir_loc = metadata.get(event.mir_loc);
                let kind = &event.kind;
                writeln!(f, "{mir_loc:?} -> {kind:?}")?;
            }
        }

        if should_print(ToPrint::LatestAssignments) {
            for ((func_hash, local), p) in &graphs.latest_assignment {
                let func = &metadata.functions[func_hash];
                writeln!(f, "({func}:{local:?}) => {p:?}")?;
            }
        }

        for graph in &graphs.graphs {
            let needs_write = graph
                .needs_write_permission()
                .map(|node_id| node_id.as_usize())
                .collect::<Vec<_>>();
            if should_print(ToPrint::Graphs) {
                writeln!(f, "{graph}")?;
            }
            if should_print(ToPrint::WritePermissions) {
                writeln!(f, "nodes_that_need_write = {needs_write:?}")?;
            }
            if should_print(ToPrint::Graphs) || should_print(ToPrint::WritePermissions) {
                writeln!(f)?;
            }
        }

        if should_print(ToPrint::Counts) {
            let num_graphs = graphs.graphs.len();
            let num_nodes = graphs
                .graphs
                .iter()
                .map(|graph| graph.nodes.len())
                .sum::<usize>();
            writeln!(f, "num_graphs = {num_graphs}")?;
            writeln!(f, "num_nodes = {num_nodes}")?;
        }

        Ok(())
    }
}

/// Construct and query a PDG from an instrumented program's event log.
#[derive(Debug, Parser)]
#[clap(author, version, about, long_about = None)]
pub struct Args {
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

static INIT: Once = Once::new();

/// Initialize things before running any code (in [`main`] or tests).
/// Call this as the first thing.
/// Will do nothing if [`init`] has already run.
pub fn init() {
    INIT.call_once(|| {
        // Throws an error if it's already been installed,
        // but if it's already installed, then we're good.
        // Shouldn't happen since we're inside of [`Once::call_once`],
        // but good to be safe, as there's no downside.
        let _: eyre::Result<()> = color_eyre::install();

        env_logger::init();
    });
}

fn main() -> eyre::Result<()> {
    init();
    let args = Args::parse();
    let pdg = Pdg::new(&args.metadata, &args.event_log)?;
    pdg.graphs.assert_all_tests();
    let repr = pdg.repr(&args.print);
    println!("{repr}");
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::{env, path::Path, process::Command};

    use color_eyre::eyre;

    use crate::init;

    #[test]
    fn analysis_test_pdg_snapshot() -> eyre::Result<()> {
        init();
        env::set_current_dir("..")?;
        let dir = Path::new("analysis/test");
        let status = Command::new("scripts/pdg.sh")
            .arg(dir)
            .env("PROFILE", "debug")
            .status()?;
        assert!(status.success());
        let pdg = fs_err::read_to_string(dir.join("pdg.log"))?;
        insta::assert_display_snapshot!(pdg);
        Ok(())
    }
}
