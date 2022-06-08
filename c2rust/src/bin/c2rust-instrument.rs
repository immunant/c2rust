use std::fs;
use std::path::Path;

use clap::{App, Values, load_yaml};
use c2rust_dynamic_instrumentation::instrument;

fn main() -> anyhow::Result<()> {
    env_logger::init();
    let yaml = load_yaml!("../instrument.yaml");
    let matches = App::from_yaml(yaml).get_matches();

    let extra_args: Vec<&str> = match matches.values_of("extra-cargo-args") {
        Some(args) => args.collect(),
        None => Vec::new(),
    };
    let extra_args: Vec<String> = extra_args.into_iter().map(|x| x.to_owned()).collect();

    let metadata_file_path = Path::new(matches.value_of_os("METADATA_FILE").unwrap());
    let runtime_path = fs::canonicalize(matches.value_of_os("RUNTIME_PATH").unwrap()).unwrap();
    instrument(&metadata_file_path, &runtime_path, &extra_args)
}