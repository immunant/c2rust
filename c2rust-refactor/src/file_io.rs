use std::collections::{HashMap, HashSet};
use std::fs;
use std::io;
use std::mem;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use json::{self, JsonValue};
use log::info;
use rustc_ast::*;
use rustc_span::hygiene::SyntaxContext;
use rustc_span::source_map::{FileLoader, SourceFile, SourceMap};
use rustc_span::source_map::{Span, DUMMY_SP};
use rustc_span::symbol::Symbol;

use crate::rewrite::{self, TextRewrite};

#[allow(unused_variables)]
pub trait FileIO {
    /// Called to indicate the end of a rewriting operation.  Any `save_file` or `save_rewrites`
    /// operations since the previous `end_rewrite` (or since the construction of the `FileIO`
    /// object) are part of the logical rewrite.
    fn end_rewrite(&self, sm: &SourceMap) -> io::Result<()> {
        Ok(())
    }

    fn file_exists(&self, path: &Path) -> bool {
        fs::metadata(path).is_ok()
    }

    fn abs_path(&self, path: &Path) -> io::Result<PathBuf> {
        fs::canonicalize(path)
    }

    fn read_file(&self, path: &Path) -> io::Result<String>;
    fn write_file(&self, path: &Path, s: &str) -> io::Result<()>;
    fn save_rewrites(
        &self,
        sm: &SourceMap,
        sf: &SourceFile,
        rws: &[TextRewrite],
        nodes: &[(Span, NodeId)],
    ) -> io::Result<()> {
        Ok(())
    }
    fn save_marks(
        &self,
        krate: &Crate,
        sm: &SourceMap,
        node_id_map: &HashMap<NodeId, NodeId>,
        marks: &HashSet<(NodeId, Symbol)>,
    ) -> io::Result<()> {
        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum OutputMode {
    InPlace,
    Alongside,
    Print,
    PrintDiff,
    Json,
    Marks,
}

impl OutputMode {
    fn overwrites(self) -> bool {
        self == OutputMode::InPlace
    }

    fn write_dest(self, path: &Path) -> Option<PathBuf> {
        match self {
            OutputMode::InPlace => Some(path.to_owned()),
            OutputMode::Alongside => Some(path.with_extension("new")),
            _ => None,
        }
    }

    fn write_rewrites_json(self) -> bool {
        self == OutputMode::Json
    }

    fn write_marks_json(self) -> bool {
        self == OutputMode::Marks
    }
}

struct RealState {
    rewrite_counter: usize,
    rewrites_json: Vec<JsonValue>,
    file_state: HashMap<PathBuf, String>,
}

impl RealState {
    fn new() -> RealState {
        RealState {
            rewrite_counter: 0,
            rewrites_json: Vec::new(),
            file_state: HashMap::new(),
        }
    }
}

pub struct RealFileIO {
    output_modes: Vec<OutputMode>,
    state: Mutex<RealState>,
}

impl RealFileIO {
    pub fn new(modes: Vec<OutputMode>) -> RealFileIO {
        RealFileIO {
            output_modes: modes,
            state: Mutex::new(RealState::new()),
        }
    }
}

impl FileIO for RealFileIO {
    fn end_rewrite(&self, _sm: &SourceMap) -> io::Result<()> {
        let mut state = self.state.lock().unwrap();
        if self
            .output_modes
            .iter()
            .any(|&mode| mode.write_rewrites_json())
        {
            let js = mem::replace(&mut state.rewrites_json, Vec::new());
            let s = json::stringify_pretty(JsonValue::Array(js), 2);
            fs::write(
                Path::new(&format!("rewrites.{}.json", state.rewrite_counter)),
                s,
            )?;
        }
        state.rewrite_counter += 1;
        Ok(())
    }

    fn read_file(&self, path: &Path) -> io::Result<String> {
        let state = self.state.lock().unwrap();
        let path = fs::canonicalize(path)?;
        if let Some(s) = state.file_state.get(&path) {
            Ok(s.clone())
        } else {
            fs::read_to_string(&path)
        }
    }

    fn write_file(&self, path: &Path, s: &str) -> io::Result<()> {
        // Handling for specific cases
        for &mode in &self.output_modes {
            match mode {
                OutputMode::InPlace => {}   // Will write output below
                OutputMode::Alongside => {} // Will write output below
                OutputMode::Print => {
                    println!(" ==== {:?} ====\n{}\n =========", path, s);
                }
                OutputMode::PrintDiff => {
                    let old_s = self.read_file(path)?;
                    println!();
                    println!("--- old/{}", path.display());
                    println!("+++ new/{}", path.display());
                    rewrite::files::print_diff(&old_s, s);
                }
                OutputMode::Json => {}  // Handled in end_rewrite
                OutputMode::Marks => {} // Handled in save_marks
            }
        }

        {
            let mut state = self.state.lock().unwrap();

            // Common handling
            for &mode in &self.output_modes {
                if let Some(dest) = mode.write_dest(path) {
                    info!("writing to {:?}", dest);
                    fs::write(&dest, s)?;
                }
            }

            if !self.output_modes.iter().any(|&mode| mode.overwrites()) {
                // None of the modes actually updated the original file, so we
                // need to record the new content internally. If we're creating
                // a new module, we can't canonicalize the filename itself
                // (since it doesn't exist), so canonicalize its path and append
                // the filename.
                let abs_path = if path.is_relative() {
                    let parent_dir = Path::new(".").join(path.parent().unwrap());
                    let mut abs_path = fs::canonicalize(parent_dir)?;
                    abs_path.push(path.file_name().unwrap());
                    abs_path
                } else {
                    path.to_owned()
                };
                state.file_state.insert(abs_path, s.to_owned());
            }
        }

        Ok(())
    }

    fn save_rewrites(
        &self,
        sm: &SourceMap,
        sf: &SourceFile,
        rws: &[TextRewrite],
        nodes: &[(Span, NodeId)],
    ) -> io::Result<()> {
        if !self
            .output_modes
            .iter()
            .any(|&mode| mode.write_rewrites_json())
        {
            return Ok(());
        }

        let mut state = self.state.lock().unwrap();

        // We want to buffer the rewrites so we can emit a single `rewrites.json` at the end
        // instead of making one per modified file.  However, it's hard to safely buffer the
        // TextRewrites themselves, since they contain Spans, and Spans are (possibly) indexes into
        // a thread-local interner.  So we actually convert the rewrites to json here, and buffer
        // the json instead.
        let rw = rewrite::TextRewrite {
            old_span: DUMMY_SP,
            new_span: Span::new(sf.start_pos, sf.end_pos, SyntaxContext::root(), None),
            rewrites: rws.to_owned(),
            nodes: nodes.to_owned(),
            adjust: rewrite::TextAdjust::None,
        };
        state
            .rewrites_json
            .push(rewrite::json::encode_rewrite(sm, &rw));
        Ok(())
    }

    fn save_marks(
        &self,
        krate: &Crate,
        _sm: &SourceMap,
        node_id_map: &HashMap<NodeId, NodeId>,
        marks: &HashSet<(NodeId, Symbol)>,
    ) -> io::Result<()> {
        if !self
            .output_modes
            .iter()
            .any(|&mode| mode.write_marks_json())
        {
            return Ok(());
        }

        let s = rewrite::json::stringify_marks(krate, node_id_map, marks);
        let state = self.state.lock().unwrap();
        fs::write(
            Path::new(&format!("marks.{}.json", state.rewrite_counter)),
            s,
        )
    }
}

pub struct ArcFileIO(pub Arc<dyn FileIO + Sync + Send>);

impl FileLoader for ArcFileIO {
    fn file_exists(&self, path: &Path) -> bool {
        self.0.file_exists(path)
    }

    fn read_file(&self, path: &Path) -> io::Result<String> {
        self.0.read_file(path)
    }
}
