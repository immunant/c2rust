use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use syntax::source_map::{SourceMap, SourceFile, FileLoader};

use rewrite::{self, TextRewrite};


pub trait FileIO {
    /// Called to indicate the end of a rewriting operation.  Any `save_file` or `save_rewrites`
    /// operations since the previous `end_rewrite` (or since the construction of the `FileIO`
    /// object) are part of the logical rewrite.
    fn end_rewrite(&self, sm: &SourceMap) -> io::Result<()>;

    fn file_exists(&self, path: &Path) -> bool;
    fn abs_path(&self, path: &Path) -> io::Result<PathBuf>;
    fn read_file(&self, path: &Path) -> io::Result<String>;
    fn write_file(&self, path: &Path, s: &str) -> io::Result<()>;
    fn save_rewrites(&self,
                     sm: &SourceMap,
                     sf: &SourceFile,
                     rws: &[TextRewrite]) -> io::Result<()>;
}


#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum OutputMode {
    InPlace,
    Alongside,
    Print,
    PrintDiff,
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
}


struct RealState {
    rewrite_counter: usize,
    file_state: HashMap<PathBuf, String>,
}

impl RealState {
    fn new() -> RealState {
        RealState {
            rewrite_counter: 0,
            file_state: HashMap::new(),
        }
    }
}

pub struct RealFileIO {
    output_mode: OutputMode,
    state: Mutex<RealState>,
}

impl RealFileIO {
    pub fn new(mode: OutputMode) -> RealFileIO {
        RealFileIO {
            output_mode: mode,
            state: Mutex::new(RealState::new()),
        }
    }
}

impl FileIO for RealFileIO {
    fn end_rewrite(&self, _sm: &SourceMap) -> io::Result<()> {
        let mut state = self.state.lock().unwrap();
        state.rewrite_counter += 1;
        Ok(())
    }

    fn file_exists(&self, path: &Path) -> bool {
        fs::metadata(path).is_ok()
    }

    fn abs_path(&self, path: &Path) -> io::Result<PathBuf> {
        fs::canonicalize(path)
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
        match self.output_mode {
            OutputMode::InPlace => {},      // Will write output below
            OutputMode::Alongside => {},    // Will write output below
            OutputMode::Print => {
                println!(" ==== {:?} ====\n{}\n =========", path, s);
            },
            OutputMode::PrintDiff => {
                let old_s = self.read_file(path)?;
                println!();
                println!("--- old/{}", path.display());
                println!("+++ new/{}", path.display());
                rewrite::files::print_diff(&old_s, s);
            },
        }

        {
            let mut state = self.state.lock().unwrap();

            // Common handling
            if let Some(dest) = self.output_mode.write_dest(path) {
                info!("writing to {:?}", dest);
                fs::write(&dest, s)?;
            }

            if !self.output_mode.overwrites() {
                let abs_path = fs::canonicalize(path)?;
                state.file_state.insert(abs_path, s.to_owned());
            }
        }

        Ok(())
    }

    fn save_rewrites(&self,
                     _sm: &SourceMap,
                     _sf: &SourceFile,
                     _rws: &[TextRewrite]) -> io::Result<()> {
        Ok(())
    }
}


pub struct ArcFileIO(pub Arc<FileIO+Sync+Send>);

impl FileLoader for ArcFileIO {
    fn file_exists(&self, path: &Path) -> bool {
        self.0.file_exists(path)
    }

    fn abs_path(&self, path: &Path) -> Option<PathBuf> {
        self.0.abs_path(path).ok()
    }

    fn read_file(&self, path: &Path) -> io::Result<String> {
        self.0.read_file(path)
    }
}
