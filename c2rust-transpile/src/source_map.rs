use std::cmp::Ordering;

pub use c2rust_ast_exporter::clang_ast::{SrcFile, SrcLoc};

#[derive(Clone, Debug)]
pub struct SourceMap {
    // Vector of include paths, indexed by fileid. Each include path is the
    // sequence of #include statement locations and the file being included at
    // that location.
    file_map: Vec<Vec<SrcLoc>>
}

impl SourceMap {
    pub fn new(files: &[SrcFile]) -> Self {
        let mut file_map = vec![];
        for fileid in 0..files.len() {
            let mut include_path = vec![];
            let mut cur = &files[fileid];
            while let Some(include_loc) = &cur.include_loc {
                include_path.push(SrcLoc {
                    fileid: fileid as u64,
                    line: include_loc.line,
                    column: include_loc.column,
                });
                cur = &files[include_loc.fileid as usize];
            }
            include_path.reverse();
            file_map.push(include_path);
        }
        Self {
            file_map
        }
    }

    pub fn compare_src_locs(&self, a: &SrcLoc, b: &SrcLoc) -> Ordering {
        /// Compare `self` with `other`, without regard to file id
        fn cmp_pos(a: &SrcLoc, b: &SrcLoc) -> Ordering {
            if a.line == b.line && a.column == b.column {
                Ordering::Equal
            } else if a.line < b.line
                || b.line > a.line
                || a.column < b.column
            {
                Ordering::Less
            } else {
                Ordering::Greater
            }
        }
        let path_a = self.file_map[a.fileid as usize].clone();
        let path_b = self.file_map[b.fileid as usize].clone();
        for (include_a, include_b) in path_a.iter().zip(path_b.iter()) {
            if include_a.fileid != include_b.fileid {
                return cmp_pos(&include_a, &include_b);
            }
        }
        match path_a.len().cmp(&path_b.len()) {
            Ordering::Less => {
                // compare the place b was included in a'a file with a
                let b = path_b.get(path_a.len()).unwrap();
                cmp_pos(a, b)
            }
            Ordering::Equal => cmp_pos(a, b),
            Ordering::Greater => {
                // compare the place a was included in b's file with b
                let a = path_a.get(path_b.len()).unwrap();
                cmp_pos(a, b)
            }
        }
    }

    pub fn get_include_line(&self, file: FileId) -> Option<u64> {
        self.file_map[file].first().map(|loc| loc.line)
    }
}


/// Represents some AST node possibly with source location information bundled with it
#[derive(Debug, Clone)]
pub struct Located<T> {
    pub loc: Option<SrcLoc>,
    pub kind: T,
}

pub type FileId = usize;

impl<T> Located<T> {
    pub fn file_id(&self) -> Option<FileId> {
        self.loc.as_ref().map(|loc| loc.fileid as FileId)
    }
}
