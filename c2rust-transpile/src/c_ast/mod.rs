use crate::c_ast::c_expr::{BinOp, CExpr, CExprId, CExprKind, ConstIntExpr, UnTypeOp};
use crate::c_ast::c_type::{CFuncTypeId, CQualTypeId, CType, CTypeId, CTypeKind};
use crate::c_ast::iterators::{immediate_children_all_types, DFNodes, NodeVisitor, SomeId};
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use std::cell::RefCell;
use std::cmp::{Ordering, Reverse};
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Debug, Display};
use std::ops::Index;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::{iter, mem};

pub use self::conversion::*;
pub use self::print::Printer;
pub use c2rust_ast_exporter::clang_ast::{BuiltinVaListKind, SrcFile, SrcLoc, SrcSpan};

pub mod c_expr;
pub mod c_type;
mod conversion;
pub mod iterators;
mod print;

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Copy, Clone)]
pub struct CDeclId(pub u64);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Copy, Clone)]
pub struct CStmtId(pub u64);

// These are references into particular variants of AST nodes
pub type CLabelId = CStmtId; // Labels point into the 'StmtKind::Label' that declared the label
pub type CFieldId = CDeclId; // Records always contain 'DeclKind::Field's
pub type CParamId = CDeclId; // Parameters always contain 'DeclKind::Variable's
pub type CRecordId = CDeclId; // Record types need to point to 'DeclKind::Record'
pub type CTypedefId = CDeclId; // Typedef types need to point to 'DeclKind::Typedef'
pub type CEnumId = CDeclId; // Enum types need to point to 'DeclKind::Enum'
pub type CEnumConstantId = CDeclId; // Enum's need to point to child 'DeclKind::EnumConstant's

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

/// The range of source code locations for a C declaration.
#[derive(Copy, Clone)]
pub struct CDeclSrcRange {
    /// The earliest position where this declaration or its documentation might start.
    pub earliest_begin: SrcLoc,
    /// A position by which this declaration itself is known to have begun.
    /// Attributes or return type may possibly precede this position.
    pub strict_begin: SrcLoc,
    /// The end of the declaration, except for possible trailing semicolon.
    pub end: SrcLoc,
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

    /// Construct a map from top-level decls in the main file to their source ranges.
    pub fn top_decl_locs(&self) -> IndexMap<CDeclId, CDeclSrcRange> {
        let mut name_loc_map = IndexMap::new();
        let mut prev_end_loc = SrcLoc {
            fileid: 0,
            line: 0,
            column: 0,
        };
        // Sort decls by source location so we can reason about the possibly comment-containing gaps
        // between them.
        let mut decls_sorted = self.c_decls_top.clone();
        // Break ties in `begin_loc` (e.g. from `int a, b;`) using `end_loc`.
        decls_sorted
            .sort_by_key(|decl| (self.c_decls[decl].begin_loc(), self.c_decls[decl].end_loc()));
        for decl_id in &decls_sorted {
            let decl = &self.c_decls[decl_id];
            let begin_loc: SrcLoc = decl.begin_loc().expect("no begin loc for top-level decl");
            let end_loc: SrcLoc = decl.end_loc().expect("no end loc for top-level decl");

            // Skip fileid 0; this is not a real file, so these source locations aren't important.
            if begin_loc.fileid == 0 {
                continue;
            }
            if begin_loc == end_loc {
                log::warn!(
                    "zero-length source range for top-level decl; skipping. source ranges for \
                    top-level decls may be incorrect.\ndecl: {decl:?}"
                );
                continue;
            }

            // If encountering a new file, reset end of last top-level decl.
            if prev_end_loc.fileid != begin_loc.fileid {
                prev_end_loc = SrcLoc {
                    fileid: begin_loc.fileid,
                    line: 1,
                    column: 1,
                }
            }

            // This definition ends before the previous one does, i.e. it is nested.
            // This does not generally occur for regular definitions, e.g. variables within
            // functions, because the variables will not be top-level decls. But it can occur
            // for macros defined inside functions, since all macros are top-level decls!
            let is_nested = end_loc < prev_end_loc;

            // Clang emits declarations of builtins (and opaque types such as when encountering
            // `struct undeclared *`) at their first usage site. This means that they are usually
            // nested within another function, and (at least with how the C AST is currently
            // exported) they have an end location with line and column zero. Fix this up before
            // continuing to maintain the invariant that begin is not ordered after end.
            if is_nested
                && end_loc.line == 0
                && end_loc.column == 0
                && !(begin_loc.line == 0 && begin_loc.column == 0)
            {
                log::debug!("skipping nested decl with zero end line/col: {decl:?}");
                continue;
            }

            // If the beginning is not ordered after the end, skip this decl and warn.
            if begin_loc > end_loc {
                log::warn!(
                    "backward source range for top-level decl; skipping. source ranges for \
                    top-level decls may be incorrect.\ndecl: {decl:?}"
                );
                continue;
            }

            // End of the previous decl is the start of comments pertaining to the current one.
            let earliest_begin_loc = if is_nested { begin_loc } else { prev_end_loc };

            // Include only decls from the main file.
            if self.c_decls_top.contains(decl_id)
                && self.get_source_path(decl) == Some(&self.main_file)
            {
                // For multiple decls, e.g. `int a, b;`, `begin_loc` is shared, in which case it is
                // earlier than `earliest_begin_loc` for decls after the first; to maintain their
                // relative order we must either move `earliest_begin_loc` earlier or move
                // `begin_loc` later.
                // For now, we move `begin_loc` later, so that the range used by each variable from
                // a multiple decl does not overlap the others. If other tooling would benefit more
                // from maximal but overlapping ranges, we could go the other way.
                let begin_loc = begin_loc.max(earliest_begin_loc);
                let entry = CDeclSrcRange {
                    earliest_begin: earliest_begin_loc,
                    strict_begin: begin_loc,
                    end: end_loc,
                };
                name_loc_map.insert(*decl_id, entry);
            }
            if !is_nested {
                prev_end_loc = end_loc;
            }
        }
        name_loc_map
    }

    pub fn iter_decls(&self) -> indexmap::map::Iter<'_, CDeclId, CDecl> {
        self.c_decls.iter()
    }

    pub fn iter_mut_decls(&mut self) -> indexmap::map::IterMut<'_, CDeclId, CDecl> {
        self.c_decls.iter_mut()
    }

    pub fn get_decl(&self, key: &CDeclId) -> Option<&CDecl> {
        self.c_decls.get(key)
    }

    /// Return the list of types for a list of declared function parameters.
    ///
    /// Returns `None` if one of the parameters is not a `CDeclKind::Variable`, e.g. if it was not a
    /// function parameter but actually some other kind of declaration.
    pub fn tys_of_params(&self, parameters: &[CDeclId]) -> Option<Vec<CQualTypeId>> {
        parameters
            .iter()
            .map(|p| match self.index(*p).kind {
                CDeclKind::Variable { typ, .. } => Some(CQualTypeId::new(typ.ctype)),
                _ => None,
            })
            .collect()
    }

    /// Return the most precise possible CTypeKind for the given function declaration.
    /// Specifically, ensures that arguments' types are not resolved to underlying types if they were
    /// declared as typedefs, but returned as those typedefs.
    ///
    /// The passed CDeclId must refer to a function declaration.
    pub fn fn_decl_ty_with_declared_args(&self, func_decl: &CDeclKind) -> CTypeKind {
        if let CDeclKind::Function {
            typ, parameters, ..
        } = func_decl
        {
            let typ = self.resolve_type_id(*typ);
            let decl_arg_tys = self.tys_of_params(parameters).unwrap();
            let typ_kind = &self[typ].kind;
            if let &CTypeKind::Function(ret, ref _arg_tys, a, b, c) = typ_kind {
                return CTypeKind::Function(ret, decl_arg_tys, a, b, c);
            }
            panic!("expected {typ:?} to be CTypeKind::Function, but it was {typ_kind:?}")
        }
        panic!("expected a CDeclKind::Function, but passed {func_decl:?}")
    }

    pub fn is_const_stmt(&self, stmt: CStmtId) -> bool {
        let is_const = |stmt| self.is_const_stmt(stmt);
        let is_const_expr = |expr| self.is_const_expr(expr);

        use CStmtKind::*;
        match self[stmt].kind {
            Case(expr, stmt, _const_expr) => is_const_expr(expr) && is_const(stmt),
            Default(stmt) => is_const(stmt),
            Compound(ref stmts) => stmts.iter().copied().all(is_const),
            Expr(expr) => is_const_expr(expr),
            Empty => true,
            If {
                scrutinee,
                true_variant,
                false_variant,
            } => {
                is_const_expr(scrutinee)
                    && is_const(true_variant)
                    && false_variant.map_or(true, is_const)
            }
            Switch { scrutinee, body } => is_const_expr(scrutinee) && is_const(body),
            While { condition, body } => is_const_expr(condition) && is_const(body),
            DoWhile { body, condition } => is_const(body) && is_const_expr(condition),
            ForLoop {
                init,
                condition,
                increment,
                body,
            } => {
                init.map_or(true, is_const)
                    && condition.map_or(true, is_const_expr)
                    && increment.map_or(true, is_const_expr)
                    && is_const(body)
            }
            Break => true,
            Continue => true,
            Return(expr) => expr.map_or(true, is_const_expr),
            Decls(ref _decls) => true,
            Asm { .. } => false,
            Attributed {
                attributes: _,
                substatement,
            } => is_const(substatement),
            // `goto`s are tricky, because they can be non-local
            // and jump out of the context of the macro.
            // A `goto` and its labels are `const` if the whole state machine
            // we compile to has all `const` statements,
            // but determining what that is exactly is trickier,
            // and might depend on the context in which the macro is used.
            // This is probably fairly uncommon, so we just assume it's not `const` for now.
            // Note that in C, labels are for `goto`s.
            // There are no labeled `break`s and `continue`s.
            Label(_stmt) => false,
            Goto(_label) => false,
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
                        let expr = self.index(expr_id);
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

    /// Bubble types of unary and binary operators up from their args into the expression type.
    ///
    /// In Clang 15 and below, the Clang AST resolves typedefs in the expression type of unary and
    /// binary expressions. For example, a BinaryExpr node adding two `size_t` expressions will be
    /// given an `unsigned long` type rather than the `size_t` typedef type. This behavior changed
    /// in Clang 16. This method adjusts AST node types to match those produced by Clang 16 and
    /// newer; on these later Clang versions, it should have no effect.
    ///
    /// This pass is necessary because we reify some typedef types (such as `size_t`) into their own
    /// distinct Rust types. As such, we need to make sure we know the exact type to generate when
    /// we translate an expr, not just its resolved type (looking through typedefs).
    pub fn bubble_expr_types(&mut self) {
        struct BubbleExprTypes<'a> {
            ast_context: &'a mut TypedAstContext,
        }

        impl<'a> NodeVisitor for BubbleExprTypes<'a> {
            fn children(&mut self, id: SomeId) -> Vec<SomeId> {
                immediate_children_all_types(self.ast_context, id)
            }

            fn post(&mut self, id: SomeId) {
                let e = match id {
                    SomeId::Expr(e) => e,
                    _ => return,
                };

                let new_ty = match self.ast_context.c_exprs[&e].kind {
                    CExprKind::Conditional(_ty, _cond, lhs, rhs) => {
                        let lhs_type_id =
                            self.ast_context.c_exprs[&lhs].kind.get_qual_type().unwrap();
                        let rhs_type_id =
                            self.ast_context.c_exprs[&rhs].kind.get_qual_type().unwrap();

                        let lhs_resolved_ty = self.ast_context.resolve_type(lhs_type_id.ctype);
                        let rhs_resolved_ty = self.ast_context.resolve_type(rhs_type_id.ctype);

                        if CTypeKind::PULLBACK_KINDS.contains(&lhs_resolved_ty.kind) {
                            Some(lhs_type_id)
                        } else if CTypeKind::PULLBACK_KINDS.contains(&rhs_resolved_ty.kind) {
                            Some(rhs_type_id)
                        } else {
                            None
                        }
                    }
                    CExprKind::Binary(_ty, op, lhs, rhs, _, _) => {
                        let rhs_type_id =
                            self.ast_context.c_exprs[&rhs].kind.get_qual_type().unwrap();
                        let lhs_kind = &self.ast_context.c_exprs[&lhs].kind;
                        let lhs_type_id = lhs_kind.get_qual_type().unwrap();

                        let lhs_resolved_ty = self.ast_context.resolve_type(lhs_type_id.ctype);
                        let rhs_resolved_ty = self.ast_context.resolve_type(rhs_type_id.ctype);

                        let neither_ptr = !lhs_resolved_ty.kind.is_pointer()
                            && !rhs_resolved_ty.kind.is_pointer();

                        if op.all_types_same() && neither_ptr {
                            if CTypeKind::PULLBACK_KINDS.contains(&lhs_resolved_ty.kind) {
                                Some(lhs_type_id)
                            } else {
                                Some(rhs_type_id)
                            }
                        } else if op == BinOp::ShiftLeft || op == BinOp::ShiftRight {
                            Some(lhs_type_id)
                        } else {
                            return;
                        }
                    }
                    CExprKind::Unary(_ty, op, e, _idk) => op.expected_result_type(
                        self.ast_context,
                        self.ast_context.c_exprs[&e].kind.get_qual_type().unwrap(),
                    ),
                    CExprKind::Paren(_ty, e) => self.ast_context.c_exprs[&e].kind.get_qual_type(),
                    CExprKind::UnaryType(_, op, _, _) => {
                        // All of these `UnTypeOp`s should return `size_t`.
                        let kind = match op {
                            UnTypeOp::SizeOf => CTypeKind::Size,
                            UnTypeOp::AlignOf => CTypeKind::Size,
                            UnTypeOp::PreferredAlignOf => CTypeKind::Size,
                        };
                        let ty = self
                            .ast_context
                            .type_for_kind(&kind)
                            .expect("CTypeKind::Size should be size_t");
                        Some(CQualTypeId::new(ty))
                    }
                    _ => return,
                };
                let ty = self
                    .ast_context
                    .c_exprs
                    .get_mut(&e)
                    .and_then(|e| e.kind.get_qual_type_mut());
                if let (Some(ty), Some(new_ty)) = (ty, new_ty) {
                    *ty = new_ty;
                };
            }
        }

        for decl in self.c_decls_top.clone() {
            BubbleExprTypes { ast_context: self }.visit_tree(SomeId::Decl(decl));
        }
    }

    /// Sort the top-level declarations by file and source location
    /// so that we preserve the ordering of all declarations in each file.
    /// This preserves the order when we emit the converted declarations.
    pub fn sort_top_decls_for_emitting(&mut self) {
        let mut decls_top = mem::take(&mut self.c_decls_top);
        decls_top.sort_unstable_by_key(|&decl| self.cmp_located_include(self.index(decl)));
        self.c_decls_top = decls_top;
    }

    pub fn has_inner_struct_decl(&self, decl_id: CDeclId) -> bool {
        matches!(
            self.index(decl_id).kind,
            CDeclKind::Struct {
                manual_alignment: Some(_),
                ..
            }
        )
    }

    pub fn is_packed_struct_decl(&self, decl_id: CDeclId) -> bool {
        use CDeclKind::*;
        matches!(
            self.index(decl_id).kind,
            Struct {
                is_packed: true,
                ..
            } | Struct {
                max_field_alignment: Some(_),
                ..
            }
        )
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

impl Index<CDeclId> for TypedAstContext {
    type Output = CDecl;

    fn index(&self, index: CDeclId) -> &CDecl {
        match self.c_decls.get(&index) {
            None => panic!("Could not find {:?} in TypedAstContext", index),
            Some(ty) => ty,
        }
    }
}

impl Index<CStmtId> for TypedAstContext {
    type Output = CStmt;

    fn index(&self, index: CStmtId) -> &CStmt {
        match self.c_stmts.get(&index) {
            None => panic!("Could not find {:?} in TypedAstContext", index),
            Some(ty) => ty,
        }
    }
}

/// All of our AST types should have location information bundled with them
pub type CDecl = Located<CDeclKind>;
pub type CStmt = Located<CStmtKind>;

#[derive(Debug, Clone)]
pub enum CDeclKind {
    // http://clang.llvm.org/doxygen/classclang_1_1FunctionDecl.html
    Function {
        is_global: bool,
        is_inline: bool,
        is_implicit: bool,
        is_extern: bool,
        is_inline_externally_visible: bool,
        typ: CFuncTypeId,
        name: String,
        parameters: Vec<CParamId>,
        body: Option<CStmtId>,
        attrs: IndexSet<Attribute>,
    },

    // http://clang.llvm.org/doxygen/classclang_1_1VarDecl.html
    Variable {
        has_static_duration: bool,
        has_thread_duration: bool,
        is_externally_visible: bool,
        is_defn: bool,
        ident: String,
        initializer: Option<CExprId>,
        typ: CQualTypeId,
        attrs: IndexSet<Attribute>,
    },

    // Enum (http://clang.llvm.org/doxygen/classclang_1_1EnumDecl.html)
    Enum {
        name: Option<String>,
        variants: Vec<CEnumConstantId>,
        integral_type: Option<CQualTypeId>,
    },

    EnumConstant {
        name: String,
        value: ConstIntExpr,
    },

    // Typedef
    Typedef {
        name: String,
        typ: CQualTypeId,
        is_implicit: bool,
        target_dependent_macro: Option<String>,
    },

    // Struct
    Struct {
        name: Option<String>,
        fields: Option<Vec<CFieldId>>,
        is_packed: bool,
        manual_alignment: Option<u64>,
        max_field_alignment: Option<u64>,
        platform_byte_size: u64,
        platform_alignment: u64,
    },

    // Union
    Union {
        name: Option<String>,
        fields: Option<Vec<CFieldId>>,
        is_packed: bool,
    },

    // Field
    Field {
        name: String,
        typ: CQualTypeId,
        bitfield_width: Option<u64>,
        platform_bit_offset: u64,
        platform_type_bitwidth: u64,
    },

    MacroObject {
        name: String,
        // replacements: Vec<CExprId>,
    },

    MacroFunction {
        name: String,
        // replacements: Vec<CExprId>,
    },

    NonCanonicalDecl {
        canonical_decl: CDeclId,
    },

    StaticAssert {
        assert_expr: CExprId,
        message: Option<String>,
    },
}

impl CDeclKind {
    pub fn get_name(&self) -> Option<&String> {
        use CDeclKind::*;
        Some(match self {
            Function { name: i, .. } => i,
            Variable { ident: i, .. } => i,
            Typedef { name: i, .. } => i,
            EnumConstant { name: i, .. } => i,
            Enum { name: Some(i), .. } => i,
            Struct { name: Some(i), .. } => i,
            Union { name: Some(i), .. } => i,
            Field { name: i, .. } => i,
            MacroObject { name, .. } => name,
            _ => return None,
        })
    }
}

/// Represents a statement in C (6.8 Statements)
///
/// Reflects the types in <http://clang.llvm.org/doxygen/classclang_1_1Stmt.html>
#[derive(Debug, Clone)]
pub enum CStmtKind {
    // Labeled statements (6.8.1)
    //
    // All of these have a `CStmtId` to represent the substatement that comes after them
    Label(CStmtId),
    Case(CExprId, CStmtId, ConstIntExpr),
    Default(CStmtId),

    // Compound statements (6.8.2)
    Compound(Vec<CStmtId>),

    // Expression and null statements (6.8.3)
    Expr(CExprId),
    Empty,

    // Selection statements (6.8.4)
    If {
        scrutinee: CExprId,
        true_variant: CStmtId,
        false_variant: Option<CStmtId>,
    },
    Switch {
        scrutinee: CExprId,
        body: CStmtId,
    },

    // Iteration statements (6.8.5)
    While {
        condition: CExprId,
        body: CStmtId,
    },
    DoWhile {
        body: CStmtId,
        condition: CExprId,
    },
    ForLoop {
        init: Option<CStmtId>,
        condition: Option<CExprId>,
        increment: Option<CExprId>,
        body: CStmtId,
    },

    // Jump statements (6.8.6)
    Goto(CLabelId),
    Break,
    Continue,
    Return(Option<CExprId>),

    // Declarations (variables, etc.)
    Decls(Vec<CDeclId>),

    // GCC inline assembly
    Asm {
        asm: String,
        inputs: Vec<AsmOperand>,
        outputs: Vec<AsmOperand>,
        clobbers: Vec<String>,
        is_volatile: bool,
    },

    // Statements annotated with attributes. The substatement can be a NULL
    // statement in case of __attribute__((__fallthrough__)) at the end of a
    // case statement
    Attributed {
        attributes: Vec<Attribute>,
        substatement: CStmtId,
    },
}

#[derive(Clone, Debug)]
pub struct AsmOperand {
    pub constraints: String,
    pub expression: CExprId,
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
