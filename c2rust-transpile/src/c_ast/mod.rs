use crate::c_ast::c_decl::{CDecl, CDeclId, CDeclKind};
use crate::c_ast::c_expr::{CExpr, CExprId, CExprKind};
use crate::c_ast::c_stmt::{CLabelId, CStmt, CStmtId};
use crate::c_ast::c_type::{CType, CTypeId, CTypeKind};
use crate::c_ast::iterators::{DFNodes, SomeId};
use indexmap::IndexMap;
use itertools::Itertools;
use std::cell::RefCell;
use std::cmp::{Ordering, Reverse};
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Debug, Display};
use std::iter;
use std::mem;
use std::ops::Index;
use std::path::{Path, PathBuf};
use std::rc::Rc;

pub use self::conversion::*;
pub use self::print::Printer;
pub use c2rust_ast_exporter::clang_ast::{BuiltinVaListKind, SrcFile, SrcLoc, SrcSpan};

pub mod c_decl;
pub mod c_expr;
pub mod c_stmt;
pub mod c_type;
mod conversion;
pub mod iterators;
mod print;

/// AST context containing all of the nodes in the Clang AST
#[derive(Debug, Clone, Default)]
pub struct TypedAstContext {
    main_file: PathBuf,

    c_types: HashMap<CTypeId, CType>,
    c_exprs: HashMap<CExprId, CExpr>,
    c_stmts: HashMap<CStmtId, CStmt>,

    /// Decls require a stable iteration order as this map will be
    /// iterated over export all defined types during translation.
    c_decls: IndexMap<CDeclId, CDecl>,

    pub c_decls_top: Vec<CDeclId>,
    pub c_main: Option<CDeclId>,

    /// record fields and enum constants
    pub parents: HashMap<CDeclId, CDeclId>,

    /// Mapping from [`FileId`] to [`SrcFile`]. Deduplicated by file path.
    files: Vec<SrcFile>,

    /// Mapping from clang `fileid` to translator [`FileId`].
    file_map: Vec<FileId>,

    /// Vector of include paths, indexed by [`FileId`]. Each include path is the
    /// sequence of `#include` statement locations and the file being included at
    /// that location.
    include_map: Vec<Vec<SrcLoc>>,

    /// Names of the labels defined in the C source code.
    pub label_names: IndexMap<CLabelId, Rc<str>>,

    /// map expressions to the stack of macros they were expanded from
    pub macro_invocations: IndexMap<CExprId, Vec<CDeclId>>,

    /// map macro decls to the expressions they expand to
    pub macro_expansions: IndexMap<CDeclId, Vec<CExprId>>,

    /// map expressions to the text of the macro invocation they expanded from,
    /// if any
    pub macro_expansion_text: IndexMap<CExprId, String>,

    pub comments: Vec<Located<String>>,

    /// The key is the typedef decl being squashed away,
    /// and the value is the decl id to the corresponding structure
    pub prenamed_decls: IndexMap<CDeclId, CDeclId>,

    pub va_list_kind: BuiltinVaListKind,
    pub target: String,
}

/// Comments associated with a typed AST context
#[derive(Debug, Clone)]
pub struct CommentContext {
    comments_by_file: HashMap<FileId, RefCell<Vec<Located<String>>>>,
}

#[derive(Debug, Clone)]
pub struct DisplaySrcSpan {
    file: Option<PathBuf>,
    loc: SrcSpan,
}

impl Display for DisplaySrcSpan {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref file) = self.file {
            write!(
                f,
                "{}:{}:{}",
                file.display(),
                self.loc.begin_line,
                self.loc.begin_column
            )
        } else {
            Debug::fmt(self, f)
        }
    }
}

pub type FileId = usize;

/// Represents some AST node possibly with source location information bundled with it
#[derive(Debug, Clone)]
pub struct Located<T> {
    pub loc: Option<SrcSpan>,
    pub kind: T,
}

impl<T> Located<T> {
    pub fn begin_loc(&self) -> Option<SrcLoc> {
        self.loc.map(|loc| loc.begin())
    }
    pub fn end_loc(&self) -> Option<SrcLoc> {
        self.loc.map(|loc| loc.end())
    }
}

/// This holds a [`SrcLoc`] and its [`include_path`](TypedAstContext::include_path)
/// such that it is naturally ordered.
///
/// The include path starts from where the item is first included,
/// working its way back to the last include path,
/// which contains the definition of the item.
/// Thus, to compare them, we compare the include path first
/// and then the definition's location.
#[derive(PartialEq, Eq, Debug)]
struct SrcLocInclude<'a> {
    loc: SrcLoc,
    include_path: &'a [SrcLoc],
}

impl SrcLocInclude<'_> {
    fn cmp_iter(&self) -> impl Iterator<Item = SrcLoc> + '_ {
        // See docs on `Self` for why this is the right comparison.
        let Self { loc, include_path } = *self;
        include_path.iter().copied().chain([loc])
    }
}

impl Ord for SrcLocInclude<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.cmp_iter().cmp(other.cmp_iter())
    }
}

impl PartialOrd for SrcLocInclude<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl TypedAstContext {
    // TODO: build the TypedAstContext during initialization, rather than
    // building an empty one and filling it later.
    pub fn new(input_path: &Path, clang_files: &[SrcFile]) -> TypedAstContext {
        let main_file = input_path.to_owned();

        let mut include_map = vec![];
        for mut cur in clang_files {
            let mut include_path = vec![];
            while let Some(include_loc) = &cur.include_loc {
                include_path.push(*include_loc);
                cur = &clang_files[include_loc.fileid as usize];
            }
            include_path.reverse();
            // The first include should be from the input file.
            // If this is false, then we haven't found the full, correct include path.
            if let Some(root_include_path) = cur.path.as_deref() {
                assert_eq!(root_include_path, input_path);
            }
            include_map.push(include_path);
        }

        // Deduplicate paths, converting clang `fileid`s to our `FileId`s.
        let mut files: Vec<SrcFile> = vec![];
        let mut file_map: Vec<FileId> = vec![];
        for file in clang_files {
            if let Some(existing) = files.iter().position(|f| f.path == file.path) {
                file_map.push(existing);
            } else {
                file_map.push(files.len());
                files.push(file.clone());
            }
        }

        // Ensure that the main file is actually present. This is not the case if it is empty,
        // in which case the AST exporter's visitor would have never observed it.
        if !files.iter().any(|f| f.path.as_ref() == Some(&main_file)) {
            file_map.push(files.len());
            files.push(SrcFile {
                path: Some(main_file.clone()),
                include_loc: None,
            });
        }

        TypedAstContext {
            main_file,
            files,
            file_map,
            include_map,
            ..Default::default()
        }
    }

    pub fn display_loc(&self, loc: &Option<SrcSpan>) -> Option<DisplaySrcSpan> {
        loc.as_ref().map(|loc| DisplaySrcSpan {
            file: self.files[self.file_map[loc.fileid as usize]].path.clone(),
            loc: *loc,
        })
    }

    pub fn get_source_path<'a, T>(&'a self, node: &Located<T>) -> Option<&'a Path> {
        self.file_id(node)
            .and_then(|fileid| self.get_file_path(fileid))
    }

    pub fn get_file_path(&self, id: FileId) -> Option<&Path> {
        self.files[id].path.as_deref()
    }

    /// Lookup the include path for `loc`.
    /// The first [`SrcLoc`] returned should have been included from the input/main file.
    pub fn include_path(&self, loc: SrcLoc) -> &[SrcLoc] {
        let includes = &self.include_map[self.file_map[loc.fileid as usize]][..];
        if cfg!(debug_assertions) {
            if let Some(root_include) = includes.first() {
                let file_id = self.file_map[root_include.fileid as usize];
                // headers included via `-include` will not have an include path
                if let Some(path) = self.get_file_path(file_id) {
                    assert_eq!(path, self.main_file.as_path());
                }
            }
        }
        includes
    }

    /// Compare a [`SrcLoc`] based on its include path.
    pub fn cmp_loc_include(&self, loc: SrcLoc) -> impl Ord + Debug + '_ {
        SrcLocInclude {
            loc,
            include_path: self.include_path(loc),
        }
    }

    /// Compare a [`SrcSpan`] based on its include path.
    pub fn cmp_span_include<'a>(&'a self, span: &SrcSpan) -> impl Ord + Debug + 'a {
        self.cmp_loc_include(span.begin())
    }

    /// Compare a [`Located`] based on its include path.
    pub fn cmp_located_include<'a, T>(&'a self, located: &Located<T>) -> impl Ord + Debug + 'a {
        located.loc.map(|span| self.cmp_span_include(&span))
    }

    pub fn loc_to_string(&self, loc: SrcLoc) -> String {
        let SrcLoc {
            fileid,
            line,
            column,
        } = loc;
        let file_id = self.file_map[fileid as usize];
        let path = self
            .get_file_path(file_id)
            .unwrap_or_else(|| Path::new("?"));
        let path = path.display();
        format!("(fileid {fileid}) {path}:{line}:{column}")
    }

    pub fn loc_to_string_with_include_path(&self, loc: SrcLoc) -> String {
        iter::once(&loc)
            .chain(self.include_path(loc))
            .map(|&loc| self.loc_to_string(loc))
            .join("\n    included from ")
    }

    pub fn get_file_include_line_number(&self, file: FileId) -> Option<u64> {
        self.include_map[file].first().map(|loc| loc.line)
    }

    pub fn find_file_id(&self, path: &Path) -> Option<FileId> {
        self.files
            .iter()
            .position(|f| f.path.as_ref().map_or(false, |p| p == path))
    }

    pub fn file_id<T>(&self, located: &Located<T>) -> Option<FileId> {
        located
            .loc
            .as_ref()
            .and_then(|loc| self.file_map.get(loc.fileid as usize).copied())
    }

    pub fn get_src_loc(&self, id: SomeId) -> Option<SrcSpan> {
        use SomeId::*;
        match id {
            Stmt(id) => self.index(id).loc,
            Expr(id) => self.index(id).loc,
            Decl(id) => self.index(id).loc,
            Type(id) => self.index(id).loc,
        }
    }

    pub fn prune_unwanted_decls(&mut self, want_unused_functions: bool) {
        // Starting from a set of root declarations, walk each one to find declarations it
        // depends on. Then walk each of those, recursively.

        // Declarations we still need to walk.  Everything in here is also in `wanted`.
        let mut to_walk: Vec<CDeclId> = Vec::new();
        // Declarations accessible from a root.
        let mut wanted: HashSet<CDeclId> = HashSet::new();

        // Mark all the roots as wanted.  Roots are all top-level functions and variables that might
        // be visible from another compilation unit.
        //
        // In addition, mark any other (unused) function wanted if configured.
        for &decl_id in &self.c_decls_top {
            let decl = self.index(decl_id);
            use CDeclKind::*;
            let is_wanted = match decl.kind {
                Function {
                    body: Some(_),
                    is_global: true,
                    is_inline,
                    is_inline_externally_visible,
                    ..
                    // Depending on the C specification and dialect, an inlined function
                    // may be externally visible. We rely on clang to determine visibility.
                } if !is_inline || is_inline_externally_visible => true,
                Function {
                    body: Some(_),
                    ..
                } if want_unused_functions => true,
                Variable {
                    is_defn: true,
                    is_externally_visible: true,
                    ..
                } => true,
                Variable { ref attrs, .. } | Function { ref attrs, .. }
                    if attrs.contains(&Attribute::Used) => true,
                _ => false,
            };

            if is_wanted {
                to_walk.push(decl_id);
                wanted.insert(decl_id);
            }
        }

        // Add all referenced macros to the set of wanted decls
        // wanted.extend(self.macro_expansions.values().flatten());

        while let Some(enclosing_decl_id) = to_walk.pop() {
            for some_id in DFNodes::new(self, SomeId::Decl(enclosing_decl_id)) {
                use SomeId::*;
                match some_id {
                    Type(type_id) => {
                        if let CTypeKind::Elaborated(decl_type_id) = self.c_types[&type_id].kind {
                            // This is a reference to a previously declared type.  If we look
                            // through it we should(?) get something that looks like a declaration,
                            // which we can mark as wanted.
                            let decl_id = self.c_types[&decl_type_id]
                                .kind
                                .as_decl_or_typedef()
                                .expect("target of CTypeKind::Elaborated isn't a decl?");
                            if wanted.insert(decl_id) {
                                to_walk.push(decl_id);
                            }
                        } else {
                            // For everything else (including `Struct` etc.), DFNodes will walk the
                            // corresponding declaration.
                        }
                    }

                    Expr(expr_id) => {
                        let expr = &self[self.resolve_parens(expr_id)];
                        if let Some(macs) = self.macro_invocations.get(&expr_id) {
                            for mac_id in macs {
                                if wanted.insert(*mac_id) {
                                    to_walk.push(*mac_id);
                                }
                            }
                        }
                        if let CExprKind::DeclRef(_, decl_id, _) = &expr.kind {
                            if wanted.insert(*decl_id) {
                                to_walk.push(*decl_id);
                            }
                        }
                    }

                    Decl(decl_id) => {
                        if wanted.insert(decl_id) {
                            to_walk.push(decl_id);
                        }

                        if let CDeclKind::EnumConstant { .. } = self.c_decls[&decl_id].kind {
                            // Special case for enums.  The enum constant is used, so the whole
                            // enum is also used.
                            let parent_id = self.parents[&decl_id];
                            if wanted.insert(parent_id) {
                                to_walk.push(parent_id);
                            }
                        }
                    }

                    // Stmts can include decls, but we'll see the DeclId itself in a later
                    // iteration.
                    Stmt(_) => {}
                }
            }
        }

        // Unset c_main if we are not retaining its declaration
        if let Some(main_id) = self.c_main {
            if !wanted.contains(&main_id) {
                self.c_main = None;
            }
        }

        // Prune any declaration that isn't considered live
        self.c_decls
            .retain(|&decl_id, _decl| wanted.contains(&decl_id));

        // Prune top declarations that are not considered live
        self.c_decls_top.retain(|x| wanted.contains(x));
    }

    /// Sort the top-level declarations by file and source location
    /// so that we preserve the ordering of all declarations in each file.
    /// This preserves the order when we emit the converted declarations.
    pub fn sort_top_decls_for_emitting(&mut self) {
        let mut decls_top = mem::take(&mut self.c_decls_top);
        decls_top.sort_unstable_by_key(|&decl| self.cmp_located_include(self.index(decl)));
        self.c_decls_top = decls_top;
    }
}

impl CommentContext {
    pub fn empty() -> CommentContext {
        CommentContext {
            comments_by_file: HashMap::new(),
        }
    }

    /// Build a CommentContext from the comments in this `ast_context`
    pub fn new(ast_context: &mut TypedAstContext) -> CommentContext {
        let mut comments_by_file: HashMap<FileId, Vec<Located<String>>> = HashMap::new();

        // Group comments by their file
        for comment in &ast_context.comments {
            // Comments without a valid FileId are probably clang
            // compiler-internal definitions
            if let Some(file_id) = ast_context.file_id(comment) {
                comments_by_file
                    .entry(file_id)
                    .or_default()
                    .push(comment.clone());
            }
        }

        // Sort in REVERSE! Last element is the first in file source
        // ordering. This makes it easy to pop the next comment off.
        for comments in comments_by_file.values_mut() {
            for comment in comments.iter() {
                comment.loc.unwrap();
            }
            comments.sort_by_key(|comment| Reverse(ast_context.cmp_located_include(comment)));
        }

        let comments_by_file = comments_by_file
            .into_iter()
            .map(|(k, v)| (k, RefCell::new(v)))
            .collect();

        CommentContext { comments_by_file }
    }

    pub fn get_comments_before(&self, loc: SrcLoc, ctx: &TypedAstContext) -> Vec<String> {
        let file_id = ctx.file_map[loc.fileid as usize];
        let mut extracted_comments = vec![];
        let mut comments = match self.comments_by_file.get(&file_id) {
            None => return extracted_comments,
            Some(comments) => comments.borrow_mut(),
        };
        while !comments.is_empty() {
            let next_comment_loc = comments
                .last()
                .unwrap()
                .begin_loc()
                .expect("All comments must have a source location");

            let loc = ctx.cmp_loc_include(loc);
            let next_comment_loc = ctx.cmp_loc_include(next_comment_loc);
            if next_comment_loc >= loc {
                break;
            }

            extracted_comments.push(comments.pop().unwrap().kind);
        }
        extracted_comments
    }

    pub fn get_comments_before_located<T>(
        &self,
        located: &Located<T>,
        ctx: &TypedAstContext,
    ) -> Vec<String> {
        match located.begin_loc() {
            None => vec![],
            Some(loc) => self.get_comments_before(loc, ctx),
        }
    }

    pub fn peek_next_comment_on_line(
        &self,
        loc: SrcLoc,
        ctx: &TypedAstContext,
    ) -> Option<Located<String>> {
        let file_id = ctx.file_map[loc.fileid as usize];
        let comments = self.comments_by_file.get(&file_id)?.borrow();
        comments.last().and_then(|comment| {
            let next_comment_loc = comment
                .begin_loc()
                .expect("All comments must have a source location");
            if next_comment_loc.line != loc.line {
                None
            } else {
                Some(comment.clone())
            }
        })
    }

    /// Advance over the current comment in `file`
    pub fn advance_comment(&self, file: FileId) {
        if let Some(comments) = self.comments_by_file.get(&file) {
            let _ = comments.borrow_mut().pop();
        }
    }

    pub fn get_remaining_comments(&mut self, file_id: FileId) -> Vec<String> {
        match self.comments_by_file.remove(&file_id) {
            Some(comments) => comments.into_inner().into_iter().map(|c| c.kind).collect(),
            None => vec![],
        }
    }
}

/// Enumeration of supported attributes for Declarations
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Attribute {
    /// __attribute__((alias("foo"), __alias__("foo")))
    Alias(String),
    /// __attribute__((always_inline, __always_inline__))
    AlwaysInline,
    /// __attribute__((cold, __cold__))
    Cold,
    /// __attribute__((gnu_inline, __gnu_inline__))
    GnuInline,
    /// __attribute__((no_inline, __no_inline__))
    NoInline,
    NoReturn,
    NotNull,
    Nullable,
    /// __attribute__((section("foo"), __section__("foo")))
    Section(String),
    /// __attribute__((used, __used__))
    Used,
    /// __attribute((visibility("hidden")))
    Visibility(String),
    /// __attribute__((fallthrough, __fallthrough__))
    Fallthrough,
}

#[cfg(test)]
mod tests {
    use std::cmp::Ordering;

    use super::*;

    #[track_caller]
    fn check_transitivity<T, F, G>(elements: &[T], mut compare: F, fmt: G)
    where
        F: FnMut(&T, &T) -> Ordering,
        G: Fn(&T) -> String,
    {
        fn ord_to_str(ord: Ordering) -> &'static str {
            match ord {
                Ordering::Less => "<",
                Ordering::Equal => "==",
                Ordering::Greater => ">",
            }
        }

        let n = elements.len();
        for i in 0..n {
            let a = &elements[i];
            for j in 0..n {
                let b = &elements[j];
                for c in elements.iter().take(n) {
                    let ab = compare(a, b);
                    let bc = compare(b, c);
                    let ac = compare(a, c);
                    if ab == bc {
                        let [ab, bc, ac] = [ab, bc, ac].map(ord_to_str);
                        let [a, b, c] =
                            [a, b, c].map(|e| if ab == ac { String::new() } else { fmt(e) });
                        assert_eq!(ab, ac, "Total order (transitivity) has been violated: a {ab} b and b {bc} c, but a {ac} c
a = {a}
b = {b}
c = {c}
");
                    }
                }
            }
        }
    }

    #[test]
    fn test_compare_src_locs_ord() {
        let ctx = TypedAstContext {
            file_map: vec![0, 1, 2, 3, 4, 5, 4, 5],
            include_map: vec![
                vec![],
                vec![],
                vec![SrcLoc {
                    fileid: 2,
                    line: 6,
                    column: 10,
                }],
                vec![],
                vec![],
                vec![SrcLoc {
                    fileid: 5,
                    line: 6,
                    column: 10,
                }],
            ],
            files: vec![
                SrcFile {
                    path: Some(PathBuf::new()),
                    include_loc: None,
                };
                6
            ],
            ..Default::default()
        };
        let locs = &mut [
            SrcLoc {
                fileid: 5,
                line: 5,
                column: 1,
            },
            SrcLoc {
                fileid: 5,
                line: 5,
                column: 1,
            },
            SrcLoc {
                fileid: 1,
                line: 5,
                column: 1,
            },
            SrcLoc {
                fileid: 1,
                line: 5,
                column: 1,
            },
            SrcLoc {
                fileid: 1,
                line: 10,
                column: 1,
            },
            SrcLoc {
                fileid: 1,
                line: 5,
                column: 1,
            },
            SrcLoc {
                fileid: 2,
                line: 4,
                column: 1,
            },
            SrcLoc {
                fileid: 5,
                line: 3,
                column: 7,
            },
            SrcLoc {
                fileid: 5,
                line: 3,
                column: 1,
            },
            SrcLoc {
                fileid: 5,
                line: 3,
                column: 1,
            },
            SrcLoc {
                fileid: 5,
                line: 5,
                column: 7,
            },
            SrcLoc {
                fileid: 5,
                line: 5,
                column: 1,
            },
            SrcLoc {
                fileid: 5,
                line: 5,
                column: 1,
            },
            SrcLoc {
                fileid: 5,
                line: 5,
                column: 7,
            },
            SrcLoc {
                fileid: 5,
                line: 7,
                column: 7,
            },
            SrcLoc {
                fileid: 5,
                line: 7,
                column: 1,
            },
            SrcLoc {
                fileid: 5,
                line: 7,
                column: 1,
            },
            SrcLoc {
                fileid: 5,
                line: 9,
                column: 7,
            },
            SrcLoc {
                fileid: 5,
                line: 9,
                column: 1,
            },
            SrcLoc {
                fileid: 5,
                line: 9,
                column: 1,
            },
            SrcLoc {
                fileid: 5,
                line: 5,
                column: 7,
            },
            SrcLoc {
                fileid: 5,
                line: 5,
                column: 1,
            },
            SrcLoc {
                fileid: 5,
                line: 5,
                column: 1,
            },
            SrcLoc {
                fileid: 5,
                line: 8,
                column: 3,
            },
            SrcLoc {
                fileid: 5,
                line: 8,
                column: 1,
            },
            SrcLoc {
                fileid: 5,
                line: 7,
                column: 1,
            },
            SrcLoc {
                fileid: 5,
                line: 0,
                column: 4,
            },
            SrcLoc {
                fileid: 5,
                line: 0,
                column: 1,
            },
            SrcLoc {
                fileid: 5,
                line: 2,
                column: 1,
            },
            SrcLoc {
                fileid: 5,
                line: 98,
                column: 3,
            },
            SrcLoc {
                fileid: 5,
                line: 98,
                column: 1,
            },
            SrcLoc {
                fileid: 5,
                line: 202,
                column: 1,
            },
            SrcLoc {
                fileid: 5,
                line: 202,
                column: 1,
            },
            SrcLoc {
                fileid: 7,
                line: 1,
                column: 1,
            },
            SrcLoc {
                fileid: 7,
                line: 3,
                column: 1,
            },
        ];

        check_transitivity(
            locs,
            |&a, &b| -> Ordering {
                let a = ctx.cmp_loc_include(a);
                let b = ctx.cmp_loc_include(b);
                a.cmp(&b)
            },
            |loc| format!("{loc}"),
        );

        // This should not panic.
        locs.sort_unstable_by_key(|&loc| ctx.cmp_loc_include(loc));
    }
}
