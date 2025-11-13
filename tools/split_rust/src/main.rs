use std::collections::HashMap;
use std::env;
use std::fs;
use rust_util::collect::FileCollector;
use rust_util::item_span::item_spans;
use serde_json;

fn main() {
    let mut fc = FileCollector::default();
    fc.parse(env::args().nth(1).unwrap(), vec![], true).unwrap();
    let mut out = HashMap::new();
    for &(ref name, ref mod_path, ref ast) in &fc.files {
        eprintln!("visit {:?}", name);
        let src = fs::read_to_string(name).unwrap();
        for (item_path, lo, hi) in item_spans(mod_path.to_owned(), ast) {
            let snippet = &src[lo .. hi];
            out.insert(item_path.join("::"), snippet.to_owned());
        }
    }
    serde_json::to_writer(std::io::stdout(), &out).unwrap();
}
