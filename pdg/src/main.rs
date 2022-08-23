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
    use std::{
        env,
        fmt::Display,
        path::{Path, PathBuf},
        process::Command,
    };

    use color_eyre::eyre::{self, ensure, eyre, Context};

    use crate::{Pdg, ToPrint};

    pub enum Profile {
        Debug,
        Release,
        // Will be used by future test code.
        #[allow(dead_code)]
        Other(String),
    }

    impl Profile {
        pub fn name(&self) -> &str {
            use Profile::*;
            match self {
                Debug => "dev",
                Release => "release",
                Other(other) => other.as_str(),
            }
        }

        pub fn dir_name(&self) -> &str {
            use Profile::*;
            match self {
                Debug => "debug",
                Release => "release",
                Other(other) => other.as_str(),
            }
        }
    }

    impl From<String> for Profile {
        fn from(profile: String) -> Self {
            use Profile::*;
            match profile.as_str() {
                "debug" => Debug,
                "release" => Release,
                _ => Other(profile),
            }
        }
    }

    impl Profile {
        pub fn current() -> eyre::Result<Self> {
            let profile =
                env::var("PROFILE").wrap_err(eyre!("should be set by `build.rs` from `cargo`"))?;
            Ok(profile.into())
        }
    }

    pub fn repo_dir() -> eyre::Result<PathBuf> {
        let crate_dir = env::var("CARGO_MANIFEST_DIR")?;
        let repo_dir = Path::new(&crate_dir)
            .parent()
            .ok_or_else(|| eyre!("`$CARGO_MANIFEST_DIR` should have a parent"))?;
        Ok(repo_dir.to_owned())
    }

    fn pdg_snapshot_inner(
        test_dir: &Path,
        profile: Profile,
        to_print: &[ToPrint],
    ) -> eyre::Result<impl Display> {
        let runtime_path = repo_dir()?.join("analysis/runtime");
        let manifest_path = test_dir.join("Cargo.toml");
        let target_dir = test_dir.join("instrument.target");
        let exe_dir = target_dir.join(profile.dir_name());
        let metadata_path = exe_dir.join("metadata.bc");
        let event_log_path = exe_dir.join("event.log.bc");

        let mut cmd = Command::new("cargo");
        cmd.current_dir(repo_dir()?)
            .args(&[
                "run",
                "--bin",
                "c2rust-instrument",
                "--profile",
                // Compile `c2rust-instrument` with the same profile us, `c2rust-pdg`, was compiled in.
                // Makes sense to match them, plus that one is probably already compiled.
                Profile::current()?.name(),
                "--",
                "--metadata",
            ])
            .arg(&metadata_path)
            .args(&["--set-runtime", "--runtime-path"])
            .arg(&runtime_path)
            .args(&["--", "run", "--manifest-path"])
            .arg(&manifest_path)
            .args(&["--profile", profile.name()])
            .env("METADATA_FILE", &metadata_path)
            .env("INSTRUMENT_BACKEND", "log")
            .env("INSTRUMENT_OUTPUT", &event_log_path)
            .env("INSTRUMENT_OUTPUT_APPEND", "false");
        let status = cmd.status()?;
        ensure!(status.success(), eyre!("{cmd:?} failed: {status}"));

        let pdg = Pdg::new(&metadata_path, &event_log_path)?;
        pdg.graphs.assert_all_tests();
        let repr = pdg.repr(to_print);
        Ok(repr.to_string())
    }

    pub fn pdg_snapshot(
        test_dir: impl AsRef<Path>,
        profile: Profile,
        to_print: &[ToPrint],
    ) -> eyre::Result<impl Display> {
        pdg_snapshot_inner(test_dir.as_ref(), profile, to_print)
    }

    fn analysis_test_pdg_snapshot(profile: Profile) -> eyre::Result<impl Display> {
        pdg_snapshot(repo_dir()?.join("analysis/test"), profile, {
            use ToPrint::*;
            &[Graphs, WritePermissions, Counts]
        })
    }

    use crate::init;

    #[test]
    fn analysis_test_pdg_snapshot_debug() -> eyre::Result<()> {
        init();
        let pdg = analysis_test_pdg_snapshot(Profile::Debug)?;
        insta::assert_display_snapshot!(pdg);
        Ok(())
    }
}
