use crate::c_ast::iterators::{immediate_children_all_types, NodeVisitor};
use crate::iterators::{DFNodes, SomeId};
use c2rust_ast_exporter::clang_ast::LRValue;
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Debug, Display};
use std::ops::Index;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::{iter, mem};

pub use self::conversion::*;
pub use self::print::Printer;
pub use c2rust_ast_exporter::clang_ast::{BuiltinVaListKind, SrcFile, SrcLoc, SrcSpan};

mod conversion;
pub mod iterators;
mod print;

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Copy, Clone)]
pub struct CTypeId(pub u64);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Copy, Clone)]
pub struct CExprId(pub u64);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Copy, Clone)]
pub struct CDeclId(pub u64);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Copy, Clone)]
pub struct CStmtId(pub u64);

// These are references into particular variants of AST nodes
pub type CLabelId = CStmtId; // Labels point into the 'StmtKind::Label' that declared the label
pub type CFieldId = CDeclId; // Records always contain 'DeclKind::Field's
pub type CParamId = CDeclId; // Parameters always contain 'DeclKind::Variable's
pub type CFuncTypeId = CTypeId; // Function declarations always have types which are 'TypeKind::Function'
pub type CRecordId = CDeclId; // Record types need to point to 'DeclKind::Record'
pub type CTypedefId = CDeclId; // Typedef types need to point to 'DeclKind::Typedef'
pub type CEnumId = CDeclId; // Enum types need to point to 'DeclKind::Enum'
pub type CEnumConstantId = CDeclId; // Enum's need to point to child 'DeclKind::EnumConstant's

/// AST context containing all of the nodes in the Clang AST
#[derive(Debug, Clone, Default)]
pub struct TypedAstContext {
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

impl TypedAstContext {
    // TODO: build the TypedAstContext during initialization, rather than
    // building an empty one and filling it later.
    pub fn new(input_path: &Path, clang_files: &[SrcFile]) -> TypedAstContext {
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

        TypedAstContext {
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
        &self.include_map[self.file_map[loc.fileid as usize]]
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

    /// Compare two [`SrcLoc`]s based on their include path.
    pub fn compare_src_locs(&self, a: &SrcLoc, b: &SrcLoc) -> Ordering {
        /// Compare without regard to `fileid`.
        fn cmp_pos(a: &SrcLoc, b: &SrcLoc) -> Ordering {
            (a.line, a.column).cmp(&(b.line, b.column))
        }

        use Ordering::*;
        let path_a = self.include_path(*a);
        let path_b = self.include_path(*b);

        // Find the first include that does not match between the two
        let common_len = path_a.len().min(path_b.len());
        let order = path_a[..common_len].cmp(&path_b[..common_len]);
        if order != Equal {
            return order;
        }

        // Either all parent includes are the same, or the include paths are of different lengths
        // because .zip() stops when one of the iterators is empty.
        match path_a.len().cmp(&path_b.len()) {
            Less => {
                // a has the shorter path, which means b was included in a's file
                // so extract that include and compare the position to a
                let b = &path_b[path_a.len()];
                cmp_pos(a, b)
            }
            Equal => a.cmp(b), // a and b have the same include path and are thus in the same file
            Greater => {
                // b has the shorter path, which means a was included in b's file
                // so extract that include and compare the position to b
                let a = &path_a[path_b.len()];
                cmp_pos(a, b)
            }
        }
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

    pub fn iter_decls(&self) -> indexmap::map::Iter<CDeclId, CDecl> {
        self.c_decls.iter()
    }

    pub fn iter_mut_decls(&mut self) -> indexmap::map::IterMut<CDeclId, CDecl> {
        self.c_decls.iter_mut()
    }

    pub fn get_decl(&self, key: &CDeclId) -> Option<&CDecl> {
        self.c_decls.get(key)
    }

    pub fn is_null_expr(&self, expr_id: CExprId) -> bool {
        use CExprKind::*;
        match self[expr_id].kind {
            ExplicitCast(_, _, CastKind::NullToPointer, _, _)
            | ImplicitCast(_, _, CastKind::NullToPointer, _, _) => true,

            ExplicitCast(ty, e, CastKind::BitCast, _, _)
            | ImplicitCast(ty, e, CastKind::BitCast, _, _) => {
                self.resolve_type(ty.ctype).kind.is_pointer() && self.is_null_expr(e)
            }

            _ => false,
        }
    }

    /// Predicate for struct, union, and enum declarations without
    /// bodies. These forward declarations are suitable for use as
    /// the targets of pointers
    pub fn is_forward_declared_type(&self, typ: CTypeId) -> bool {
        use CDeclKind::*;
        || -> Option<()> {
            let decl_id = self.resolve_type(typ).kind.as_underlying_decl()?;
            matches!(
                self[decl_id].kind,
                Struct { fields: None, .. }
                    | Union { fields: None, .. }
                    | Enum {
                        integral_type: None,
                        ..
                    }
            )
            .then(|| ())
        }()
        .is_some()
    }

    /// Follow a chain of typedefs and return true iff the last typedef is named
    /// `__builtin_va_list` thus naming the type clang uses to represent `va_list`s.
    pub fn is_builtin_va_list(&self, typ: CTypeId) -> bool {
        match self.index(typ).kind {
            CTypeKind::Typedef(decl) => match &self.index(decl).kind {
                CDeclKind::Typedef {
                    name: name_,
                    typ: ty,
                    ..
                } => {
                    if name_ == "__builtin_va_list" {
                        true
                    } else {
                        self.is_builtin_va_list(ty.ctype)
                    }
                }
                _ => panic!("Typedef decl did not point to a typedef"),
            },
            _ => false,
        }
    }

    /// Predicate for types that are used to implement C's `va_list`.
    /// FIXME: can we get rid of this method and use `is_builtin_va_list` instead?
    pub fn is_va_list_struct(&self, typ: CTypeId) -> bool {
        // detect `va_list`s based on typedef (should work across implementations)
        //        if self.is_builtin_va_list(typ) {
        //            return true;
        //        }

        // detect `va_list`s based on type (assumes struct-based implementation)
        let resolved_ctype = self.resolve_type(typ);
        use CTypeKind::*;
        match resolved_ctype.kind {
            Struct(record_id) => {
                if let CDeclKind::Struct {
                    name: Some(ref name_),
                    ..
                } = &self[record_id].kind
                {
                    name_ == "__va_list_tag" || name_ == "__va_list"
                } else {
                    false
                }
            }
            // va_list is a 1 element array; return true iff element type is struct __va_list_tag
            ConstantArray(typ, 1) => self.is_va_list(typ),
            _ => false,
        }
    }

    /// Predicate for pointers to types that are used to implement C's `va_list`.
    pub fn is_va_list(&self, typ: CTypeId) -> bool {
        use BuiltinVaListKind::*;
        match self.va_list_kind {
            CharPtrBuiltinVaList | VoidPtrBuiltinVaList | X86_64ABIBuiltinVaList => {
                match self.resolve_type(typ).kind {
                    CTypeKind::Pointer(CQualTypeId { ctype, .. })
                    | CTypeKind::ConstantArray(ctype, _) => self.is_va_list_struct(ctype),
                    _ => false,
                }
            }

            AArch64ABIBuiltinVaList => self.is_va_list_struct(typ),

            AAPCSABIBuiltinVaList => {
                // The mechanism applies: va_list is a `struct __va_list { ... }` as per
                // https://documentation-service.arm.com/static/5f201281bb903e39c84d7eae
                // ("Procedure Call Standard for the Arm Architecture Release 2020Q2, Document
                // number IHI 0042J") Section 8.1.4 "Additional Types"
                self.is_va_list_struct(typ)
            }

            kind => unimplemented!("va_list type {:?} not yet implemented", kind),
        }
    }

    /// Predicate for function pointers
    pub fn is_function_pointer(&self, typ: CTypeId) -> bool {
        let resolved_ctype = self.resolve_type(typ);
        use CTypeKind::*;
        if let Pointer(p) = resolved_ctype.kind {
            matches!(self.resolve_type(p.ctype).kind, Function { .. })
        } else {
            false
        }
    }

    /// Can the given field decl be a flexible array member?
    pub fn maybe_flexible_array(&self, typ: CTypeId) -> bool {
        let field_ty = self.resolve_type(typ);
        use CTypeKind::*;
        matches!(field_ty.kind, IncompleteArray(_) | ConstantArray(_, 0 | 1))
    }

    pub fn get_pointee_qual_type(&self, typ: CTypeId) -> Option<CQualTypeId> {
        let resolved_ctype = self.resolve_type(typ);
        if let CTypeKind::Pointer(p) = resolved_ctype.kind {
            Some(p)
        } else {
            None
        }
    }

    /// Resolve expression value, ignoring any casts
    pub fn resolve_expr(&self, expr_id: CExprId) -> (CExprId, &CExprKind) {
        let expr = &self.index(expr_id).kind;
        use CExprKind::*;
        match expr {
            ImplicitCast(_, subexpr, _, _, _)
            | ExplicitCast(_, subexpr, _, _, _)
            | Paren(_, subexpr) => self.resolve_expr(*subexpr),
            _ => (expr_id, expr),
        }
    }

    /// Find underlying expression beneath any implicit casts.
    pub fn beneath_implicit_casts(&self, expr_id: CExprId) -> CExprId {
        let expr = &self.index(expr_id).kind;
        use CExprKind::*;
        match expr {
            ImplicitCast(_, subexpr, _, _, _) => {
                return self.beneath_implicit_casts(*subexpr);
            }
            _ => {}
        }
        expr_id
    }

    /// Resolve true expression type, iterating through any casts and variable
    /// references.
    pub fn resolve_expr_type_id(&self, expr_id: CExprId) -> Option<(CExprId, CTypeId)> {
        let expr = &self.index(expr_id).kind;
        let mut ty = expr.get_type();
        use CExprKind::*;
        match expr {
            ImplicitCast(_, subexpr, _, _, _)
            | ExplicitCast(_, subexpr, _, _, _)
            | Paren(_, subexpr) => {
                return self.resolve_expr_type_id(*subexpr);
            }
            DeclRef(_, decl_id, _) => {
                let decl = self.index(*decl_id);
                use CDeclKind::*;
                match decl.kind {
                    Function { typ, .. } => {
                        ty = Some(self.resolve_type_id(typ));
                    }
                    Variable { typ, .. } | Typedef { typ, .. } => {
                        ty = Some(self.resolve_type_id(typ.ctype));
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        ty.map(|ty| (expr_id, ty))
    }

    pub fn type_for_kind(&self, kind: &CTypeKind) -> Option<CTypeId> {
        self.c_types
            .iter()
            .find_map(|(id, k)| if kind == &k.kind { Some(*id) } else { None })
    }

    pub fn resolve_type_id(&self, typ: CTypeId) -> CTypeId {
        use CTypeKind::*;
        let ty = match self.index(typ).kind {
            Attributed(ty, _) => ty.ctype,
            Elaborated(ty) => ty,
            Decayed(ty) => ty,
            TypeOf(ty) => ty,
            Paren(ty) => ty,
            Typedef(decl) => match self.index(decl).kind {
                CDeclKind::Typedef { typ: ty, .. } => ty.ctype,
                _ => panic!("Typedef decl did not point to a typedef"),
            },
            _ => return typ,
        };
        self.resolve_type_id(ty)
    }

    pub fn resolve_type(&self, typ: CTypeId) -> &CType {
        let resolved_typ_id = self.resolve_type_id(typ);
        self.index(resolved_typ_id)
    }

    /// Extract decl of referenced function.
    /// Looks for ImplicitCast(FunctionToPointerDecay, DeclRef(function_decl))
    pub fn fn_declref_decl(&self, func_expr: CExprId) -> Option<&CDeclKind> {
        use CastKind::FunctionToPointerDecay;
        if let CExprKind::ImplicitCast(_, fexp, FunctionToPointerDecay, _, _) = self[func_expr].kind
        {
            if let CExprKind::DeclRef(_ty, decl_id, _rv) = &self[fexp].kind {
                let decl = &self.index(*decl_id).kind;
                assert!(matches!(decl, CDeclKind::Function { .. }));
                return Some(decl);
            }
        }
        None
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

    /// Return the id of the most precise possible type for the function referenced by the given
    /// expression, if any.
    pub fn fn_declref_ty_with_declared_args(&self, func_expr: CExprId) -> Option<CQualTypeId> {
        if let Some(func_decl @ CDeclKind::Function { .. }) = self.fn_declref_decl(func_expr) {
            let kind_with_declared_args = self.fn_decl_ty_with_declared_args(func_decl);
            let specific_typ = self
                .type_for_kind(&kind_with_declared_args)
                .unwrap_or_else(|| panic!("no type for kind {kind_with_declared_args:?}"));
            return Some(CQualTypeId::new(specific_typ));
        }
        None
    }

    /// Pessimistically try to check if an expression has side effects. If it does, or we can't tell
    /// that it doesn't, return `false`.
    pub fn is_expr_pure(&self, expr: CExprId) -> bool {
        use CExprKind::*;
        let pure = |expr| self.is_expr_pure(expr);
        match self.index(expr).kind {
            BadExpr |
            ShuffleVector(..) |
            ConvertVector(..) |
            Call(..) |
            Unary(_, UnOp::PreIncrement, _, _) |
            Unary(_, UnOp::PostIncrement, _, _) |
            Unary(_, UnOp::PreDecrement, _, _) |
            Unary(_, UnOp::PostDecrement, _, _) |
            Binary(_, BinOp::Assign, _, _, _, _) |
            InitList { .. } |
            ImplicitValueInit { .. } |
            Predefined(..) |
            Statements(..) | // TODO: more precision
            VAArg(..) |
            Atomic{..} => false,

            Literal(_, _) |
            DeclRef(_, _, _) |
            UnaryType(_, _, _, _) |
            OffsetOf(..) |
            ConstantExpr(..) => true,

            DesignatedInitExpr(_,_,e) |
            ImplicitCast(_, e, _, _, _) |
            ExplicitCast(_, e, _, _, _) |
            Member(_, e, _, _, _) |
            Paren(_, e) |
            CompoundLiteral(_, e) |
            Unary(_, _, e, _) => pure(e),

            Binary(_, op, _, _, _, _) if op.underlying_assignment().is_some() => false,
            Binary(_, _, lhs, rhs, _, _) => pure(lhs) && pure(rhs),

            ArraySubscript(_, lhs, rhs, _) => pure(lhs) && pure(rhs),
            Conditional(_, c, lhs, rhs) => pure(c) && pure(lhs) && pure(rhs),
            BinaryConditional(_, c, rhs) => pure(c) && pure(rhs),
            Choose(_, c, lhs, rhs, _) => pure(c) && pure(lhs) && pure(rhs),
        }
    }

    /// Pessimistically try to check if an expression doesn't return.
    /// If it does, or we can't tell that it doesn't, return `false`.
    pub fn expr_diverges(&self, expr_id: CExprId) -> bool {
        let func_id = match self.index(expr_id).kind {
            CExprKind::Call(_, func_id, _) => func_id,
            _ => return false,
        };

        let type_id = match self[func_id].kind.get_type() {
            None => return false,
            Some(t) => t,
        };
        let pointed_id = match self.index(type_id).kind {
            CTypeKind::Pointer(pointer_qualtype) => pointer_qualtype.ctype,
            _ => return false,
        };

        match self.index(pointed_id).kind {
            CTypeKind::Function(_, _, _, no_return, _) => no_return,
            _ => false,
        }
    }

    /// Pessimistically try to check if an expression is `const`.
    /// If it's not, or we can't tell if it is, return `false`.
    ///
    /// This should be a top-down, pessimistic/conservative analysis.
    pub fn is_const_expr(&self, expr: CExprId) -> bool {
        let is_const = |expr| self.is_const_expr(expr);

        use CExprKind::*;
        match self[expr].kind {
            // A literal is always `const`.
            Literal(_, _) => true,
            // Unary ops should be `const`.
            // TODO handle `f128` or use the primitive type.
            Unary(_, _, expr, _) => is_const(expr),
            // Not sure what a `None` `CExprId` means here
            // or how to detect a `sizeof` of a VLA, which is non-`const`,
            // although it seems we don't handle `sizeof(VLAs)`
            // correctly in macros elsewhere already.
            UnaryType(_, _, expr, _) => expr.map_or(true, is_const),
            // Not sure what a `OffsetOfKind::Variable` means.
            OffsetOf(_, _) => true,
            // `ptr::offset` (ptr `BinOp::Add`) was `const` stabilized in `1.61.0`.
            // `ptr::offset_from` (ptr `BinOp::Subtract`) was `const` stabilized in `1.65.0`.
            // TODO `f128` is not yet handled, as we should eventually
            // switch to the (currently unstable) `f128` primitive type (#1262).
            Binary(_, _, lhs, rhs, _, _) => is_const(lhs) && is_const(rhs),
            // `as` casts are always `const`.
            ImplicitCast(_, expr, _, _, _) => is_const(expr),
            // `as` casts are always `const`.
            // TODO This is `const`, although there's a bug #853.
            ExplicitCast(_, expr, _, _, _) => is_const(expr),
            // This is used in `const` locations like `match` patterns and array lengths, so it must be `const`.
            ConstantExpr(_, _, _) => true,
            // A reference in an already otherwise `const` context should be `const` itself.
            DeclRef(_, _, _) => true,
            Call(_, fn_expr, ref args) => {
                let is_const_fn = false; // TODO detect which `fn`s are `const`.
                is_const(fn_expr) && args.iter().copied().all(is_const) && is_const_fn
            }
            Member(_, expr, _, _, _) => is_const(expr),
            ArraySubscript(_, array, index, _) => is_const(array) && is_const(index),
            Conditional(_, cond, if_true, if_false) => {
                is_const(cond) && is_const(if_true) && is_const(if_false)
            }
            BinaryConditional(_, cond, if_false) => is_const(cond) && is_const(if_false),
            InitList(_, ref inits, _, _) => inits.iter().copied().all(is_const),
            ImplicitValueInit(_) => true,
            Paren(_, expr) => is_const(expr),
            CompoundLiteral(_, expr) => is_const(expr),
            Predefined(_, expr) => is_const(expr),
            Statements(_, stmt) => self.is_const_stmt(stmt),
            VAArg(_, expr) => is_const(expr),
            // SIMD is not yet `const` in Rust.
            ShuffleVector(_, _) | ConvertVector(_, _) => false,
            DesignatedInitExpr(_, _, expr) => is_const(expr),
            Choose(_, cond, if_true, if_false, _) => {
                is_const(cond) && is_const(if_true) && is_const(if_false)
            }
            // Atomics are not yet `const` in Rust.
            Atomic { .. } => false,
            BadExpr => false,
        }
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

    pub fn sort_top_decls(&mut self) {
        // Group and sort declarations by file and by position
        let mut decls_top = mem::take(&mut self.c_decls_top);
        decls_top.sort_unstable_by(|a, b| {
            let a = self.index(*a);
            let b = self.index(*b);
            use Ordering::*;
            match (&a.loc, &b.loc) {
                (None, None) => Equal,
                (None, _) => Less,
                (_, None) => Greater,
                (Some(a), Some(b)) => self.compare_src_locs(&a.begin(), &b.begin()),
            }
        });
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

    pub fn is_aligned_struct_type(&self, typ: CTypeId) -> bool {
        if let Some(decl_id) = self.resolve_type(typ).kind.as_underlying_decl() {
            if let CDeclKind::Struct {
                manual_alignment: Some(_),
                ..
            } = self.index(decl_id).kind
            {
                return true;
            }
        }
        false
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
            comments.sort_by(|a, b| {
                ast_context.compare_src_locs(&b.loc.unwrap().begin(), &a.loc.unwrap().begin())
            });
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
            if ctx.compare_src_locs(&next_comment_loc, &loc) != Ordering::Less {
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

impl Index<CTypeId> for TypedAstContext {
    type Output = CType;

    fn index(&self, index: CTypeId) -> &CType {
        match self.c_types.get(&index) {
            None => panic!("Could not find {:?} in TypedAstContext", index),
            Some(ty) => ty,
        }
    }
}

impl Index<CExprId> for TypedAstContext {
    type Output = CExpr;
    fn index(&self, index: CExprId) -> &CExpr {
        static BADEXPR: CExpr = Located {
            loc: None,
            kind: CExprKind::BadExpr,
        };
        match self.c_exprs.get(&index) {
            None => &BADEXPR, // panic!("Could not find {:?} in TypedAstContext", index),
            Some(e) => {
                // Transparently index through Paren expressions
                if let CExprKind::Paren(_, subexpr) = e.kind {
                    self.index(subexpr)
                } else {
                    e
                }
            }
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
pub type CExpr = Located<CExprKind>;
pub type CType = Located<CTypeKind>;

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

/// An OffsetOf Expr may or may not be a constant
#[derive(Debug, Clone)]
pub enum OffsetOfKind {
    /// An Integer Constant Expr
    Constant(u64),
    /// Contains more information to generate
    /// an offset_of! macro invocation
    /// Struct Type, Field Decl Id, Index Expr
    Variable(CQualTypeId, CDeclId, CExprId),
}

/// Represents an expression in C (6.5 Expressions)
///
/// This is modeled on Clang's APIs, so where documentation
/// is lacking here, look at Clang.
///
/// We've kept a qualified type on every node since Clang has this information available, and since
/// the semantics of translations of certain constructs often depend on the type of the things they
/// are given.
///
/// As per the C standard, qualifiers on types make sense only on lvalues.
#[derive(Debug, Clone)]
pub enum CExprKind {
    /// Literal.
    Literal(CQualTypeId, CLiteral),

    /// Unary operator.
    Unary(CQualTypeId, UnOp, CExprId, LRValue),

    /// Unary type operator.
    UnaryType(CQualTypeId, UnTypeOp, Option<CExprId>, CQualTypeId),

    /// `offsetof` expression.
    OffsetOf(CQualTypeId, OffsetOfKind),

    /// Binary operator.
    Binary(
        CQualTypeId,
        BinOp,
        CExprId,
        CExprId,
        Option<CQualTypeId>,
        Option<CQualTypeId>,
    ),

    /// Implicit cast.
    ImplicitCast(CQualTypeId, CExprId, CastKind, Option<CFieldId>, LRValue),

    /// Explicit cast.
    ExplicitCast(CQualTypeId, CExprId, CastKind, Option<CFieldId>, LRValue),

    /// Constant context expression.
    ConstantExpr(CQualTypeId, CExprId, Option<ConstIntExpr>),

    /// Reference to a decl (a variable, for instance).
    // TODO: consider enforcing what types of declarations are allowed here
    DeclRef(CQualTypeId, CDeclId, LRValue),

    /// Function call.
    Call(CQualTypeId, CExprId, Vec<CExprId>),

    /// Member access.
    Member(CQualTypeId, CExprId, CDeclId, MemberKind, LRValue),

    /// Array subscript access.
    ArraySubscript(CQualTypeId, CExprId, CExprId, LRValue),

    /// Ternary conditional operator.
    Conditional(CQualTypeId, CExprId, CExprId, CExprId),

    /// Binary conditional operator `?:` (GNU extension).
    BinaryConditional(CQualTypeId, CExprId, CExprId),

    /// Initializer list.
    ///
    /// * type
    /// * initializers
    /// * union field
    /// * syntactic form
    InitList(CQualTypeId, Vec<CExprId>, Option<CFieldId>, Option<CExprId>),

    /// Designated initializer.
    ImplicitValueInit(CQualTypeId),

    /// Parenthesized expression.
    ///
    /// Ignored, but needed so we have a corresponding node.
    Paren(CQualTypeId, CExprId),

    /// Compound literal.
    CompoundLiteral(CQualTypeId, CExprId),

    /// Predefined expression.
    Predefined(CQualTypeId, CExprId),

    /// Statement expression.
    Statements(CQualTypeId, CStmtId),

    /// Variable argument list.
    VAArg(CQualTypeId, CExprId),

    /// Unsupported shuffle vector operation.
    ShuffleVector(CQualTypeId, Vec<CExprId>),

    /// Unsupported convert vector operation.
    ConvertVector(CQualTypeId, Vec<CExprId>),

    /// From syntactic form of initializer list expressions.
    DesignatedInitExpr(CQualTypeId, Vec<Designator>, CExprId),

    /// GNU choose expression.
    ///
    /// * condition
    /// * true expr
    /// * false expr
    /// * was condition true?
    Choose(CQualTypeId, CExprId, CExprId, CExprId, bool),

    /// GNU/C11 atomic expression.
    Atomic {
        typ: CQualTypeId,
        name: String,
        ptr: CExprId,
        order: CExprId,
        val1: Option<CExprId>,
        order_fail: Option<CExprId>,
        val2: Option<CExprId>,
        weak: Option<CExprId>,
    },

    BadExpr,
}

#[derive(Copy, Debug, Clone)]
pub enum MemberKind {
    Arrow,
    Dot,
}

impl CExprKind {
    pub fn lrvalue(&self) -> LRValue {
        match *self {
            CExprKind::Unary(_, _, _, lrvalue)
            | CExprKind::DeclRef(_, _, lrvalue)
            | CExprKind::ImplicitCast(_, _, _, _, lrvalue)
            | CExprKind::ExplicitCast(_, _, _, _, lrvalue)
            | CExprKind::Member(_, _, _, _, lrvalue)
            | CExprKind::ArraySubscript(_, _, _, lrvalue) => lrvalue,
            _ => LRValue::RValue,
        }
    }

    pub fn get_qual_type(&self) -> Option<CQualTypeId> {
        self.clone().get_qual_type_mut().copied()
    }

    pub fn get_qual_type_mut(&mut self) -> Option<&mut CQualTypeId> {
        match self {
            CExprKind::BadExpr => None,
            CExprKind::Literal(ty, _)
            | CExprKind::OffsetOf(ty, _)
            | CExprKind::Unary(ty, _, _, _)
            | CExprKind::UnaryType(ty, _, _, _)
            | CExprKind::Binary(ty, _, _, _, _, _)
            | CExprKind::ImplicitCast(ty, _, _, _, _)
            | CExprKind::ExplicitCast(ty, _, _, _, _)
            | CExprKind::DeclRef(ty, _, _)
            | CExprKind::Call(ty, _, _)
            | CExprKind::Member(ty, _, _, _, _)
            | CExprKind::ArraySubscript(ty, _, _, _)
            | CExprKind::Conditional(ty, _, _, _)
            | CExprKind::BinaryConditional(ty, _, _)
            | CExprKind::InitList(ty, _, _, _)
            | CExprKind::ImplicitValueInit(ty)
            | CExprKind::Paren(ty, _)
            | CExprKind::CompoundLiteral(ty, _)
            | CExprKind::Predefined(ty, _)
            | CExprKind::Statements(ty, _)
            | CExprKind::VAArg(ty, _)
            | CExprKind::ShuffleVector(ty, _)
            | CExprKind::ConvertVector(ty, _)
            | CExprKind::DesignatedInitExpr(ty, _, _)
            | CExprKind::ConstantExpr(ty, _, _) => Some(ty),
            CExprKind::Choose(ty, _, _, _, _) | CExprKind::Atomic { typ: ty, .. } => Some(ty),
        }
    }

    pub fn get_type(&self) -> Option<CTypeId> {
        self.get_qual_type().map(|x| x.ctype)
    }

    /// Try to determine the truthiness or falsiness of the expression. Return `None` if we can't
    /// say anything.
    pub fn get_bool(&self) -> Option<bool> {
        match *self {
            CExprKind::Literal(_, ref lit) => Some(lit.get_bool()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CastKind {
    BitCast,
    LValueToRValue,
    NoOp,
    ToUnion,
    ArrayToPointerDecay,
    FunctionToPointerDecay,
    NullToPointer,
    IntegralToPointer,
    PointerToIntegral,
    ToVoid,
    IntegralCast,
    IntegralToBoolean,
    IntegralToFloating,
    FloatingToIntegral,
    FloatingToBoolean,
    BooleanToSignedIntegral,
    PointerToBoolean,
    FloatingCast,
    FloatingRealToComplex,
    FloatingComplexToReal,
    FloatingComplexCast,
    FloatingComplexToIntegralComplex,
    IntegralRealToComplex,
    IntegralComplexToReal,
    IntegralComplexToBoolean,
    IntegralComplexCast,
    IntegralComplexToFloatingComplex,
    BuiltinFnToFnPtr,
    ConstCast,
    VectorSplat,
    AtomicToNonAtomic,
    NonAtomicToAtomic,
}

/// Represents a unary operator in C (6.5.3 Unary operators) and GNU C extensions
#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    AddressOf,     // &x
    Deref,         // *x
    Plus,          // +x
    PostIncrement, // x++
    PreIncrement,  // ++x
    Negate,        // -x
    PostDecrement, // x--
    PreDecrement,  // --x
    Complement,    // ~x
    Not,           // !x
    Real,          // [GNU C] __real x
    Imag,          // [GNU C] __imag x
    Extension,     // [GNU C] __extension__ x
    Coawait,       // [C++ Coroutines] co_await x
}

impl UnOp {
    pub fn as_str(&self) -> &'static str {
        use UnOp::*;
        match self {
            AddressOf => "&",
            Deref => "*",
            Plus => "+",
            PreIncrement => "++",
            PostIncrement => "++",
            Negate => "-",
            PreDecrement => "--",
            PostDecrement => "--",
            Complement => "~",
            Not => "!",
            Real => "__real",
            Imag => "__imag",
            Extension => "__extension__",
            Coawait => "co_await",
        }
    }

    /// Obtain the expected type of a unary expression based on the operator and its argument type
    pub fn expected_result_type(
        &self,
        ast_context: &TypedAstContext,
        arg_type: CQualTypeId,
    ) -> Option<CQualTypeId> {
        use UnOp::*;
        let resolved_ty = ast_context.resolve_type(arg_type.ctype);
        Some(match self {
            // We could construct CTypeKind::Pointer here, but it is not guaranteed to have a
            // corresponding `CTypeId` in the `TypedAstContext`, so bail out instead
            AddressOf => return None,
            Deref => {
                if let CTypeKind::Pointer(inner) = resolved_ty.kind {
                    inner
                } else {
                    panic!("dereferencing non-pointer type!")
                }
            }
            Not => {
                return ast_context
                    .type_for_kind(&CTypeKind::Int)
                    .map(CQualTypeId::new)
            }
            Real | Imag => {
                if let CTypeKind::Complex(inner) = resolved_ty.kind {
                    CQualTypeId::new(inner)
                } else {
                    panic!("__real or __imag applied to non-complex type!")
                }
            }
            Coawait => panic!("trying to propagate co_await type"),
            _ => CQualTypeId::new(arg_type.ctype),
        })
    }
}

impl Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Represents a unary type operator in C
#[derive(Debug, Clone, Copy)]
pub enum UnTypeOp {
    SizeOf,
    AlignOf,
    PreferredAlignOf,
}

impl UnTypeOp {
    pub fn as_str(&self) -> &'static str {
        use UnTypeOp::*;
        match self {
            SizeOf => "sizeof",
            AlignOf => "alignof",
            PreferredAlignOf => "__alignof",
        }
    }
}

impl Display for UnTypeOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl UnOp {
    /// Check is the operator is rendered before or after is operand.
    pub fn is_prefix(&self) -> bool {
        !matches!(*self, UnOp::PostIncrement | UnOp::PostDecrement)
    }
}

/// Represents a binary operator in C (6.5.5 Multiplicative operators - 6.5.14 Logical OR operator)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Multiply,     // *
    Divide,       // /
    Modulus,      // %
    Add,          // +
    Subtract,     // -
    ShiftLeft,    // <<
    ShiftRight,   // >>
    Less,         // <
    Greater,      // >
    LessEqual,    // <=
    GreaterEqual, // >=
    EqualEqual,   // ==
    NotEqual,     // !=
    BitAnd,       // &
    BitXor,       // ^
    BitOr,        // |
    And,          // &&
    Or,           // ||

    AssignAdd,        // +=
    AssignSubtract,   // -=
    AssignMultiply,   // *=
    AssignDivide,     // /=
    AssignModulus,    // %=
    AssignBitXor,     // ^=
    AssignShiftLeft,  // <<=
    AssignShiftRight, // >>=
    AssignBitOr,      // |=
    AssignBitAnd,     // &=

    Assign, // =
    Comma,  // ,
}

impl BinOp {
    pub fn as_str(&self) -> &'static str {
        use BinOp::*;
        match self {
            Multiply => "*",
            Divide => "/",
            Modulus => "%",
            Add => "+",
            Subtract => "-",
            ShiftLeft => "<<",
            ShiftRight => ">>",
            Less => "<",
            Greater => ">",
            LessEqual => "<=",
            GreaterEqual => ">=",
            EqualEqual => "==",
            NotEqual => "!=",
            BitAnd => "&",
            BitXor => "^",
            BitOr => "|",
            And => "&&",
            Or => "||",

            AssignAdd => "+=",
            AssignSubtract => "-=",
            AssignMultiply => "*=",
            AssignDivide => "/=",
            AssignModulus => "%=",
            AssignBitXor => "^=",
            AssignShiftLeft => "<<=",
            AssignShiftRight => ">>=",
            AssignBitOr => "|=",
            AssignBitAnd => "&=",

            Assign => "=",
            Comma => ", ",
        }
    }

    /// Does the rust equivalent of this operator have type (T, T) -> U?
    #[rustfmt::skip]
    pub fn input_types_same(&self) -> bool {
        use BinOp::*;
        self.all_types_same() || matches!(self,
            Less | Greater | LessEqual | GreaterEqual | EqualEqual | NotEqual
            | And | Or
            | AssignAdd | AssignSubtract | AssignMultiply | AssignDivide | AssignModulus
            | AssignBitXor | AssignShiftLeft | AssignShiftRight | AssignBitOr | AssignBitAnd
            | Assign
        )
    }

    /// Does the rust equivalent of this operator have type (T, T) -> T?
    /// This ignores cases where one argument is a pointer and we translate to `.offset()`.
    pub fn all_types_same(&self) -> bool {
        use BinOp::*;
        matches!(
            self,
            Multiply | Divide | Modulus | Add | Subtract | BitAnd | BitXor | BitOr
        )
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl BinOp {
    /// Maps compound assignment operators to operator underlying them, and returns `None` for all
    /// other operators.
    ///
    /// For example, `AssignAdd` maps to `Some(Add)` but `Add` maps to `None`.
    pub fn underlying_assignment(&self) -> Option<BinOp> {
        use BinOp::*;
        Some(match *self {
            AssignAdd => Add,
            AssignSubtract => Subtract,
            AssignMultiply => Multiply,
            AssignDivide => Divide,
            AssignModulus => Modulus,
            AssignBitXor => BitXor,
            AssignShiftLeft => ShiftLeft,
            AssignShiftRight => ShiftRight,
            AssignBitOr => BitOr,
            AssignBitAnd => BitAnd,
            _ => return None,
        })
    }

    /// Determines whether or not this is an assignment op
    pub fn is_assignment(&self) -> bool {
        matches!(self, Self::Assign) || self.underlying_assignment().is_some()
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum IntBase {
    Dec,
    Hex,
    Oct,
}

#[derive(Debug, Clone)]
pub enum CLiteral {
    Integer(u64, IntBase), // value and base
    Character(u64),
    Floating(f64, String),
    String(Vec<u8>, u8), // Literal bytes and unit byte width
}

impl CLiteral {
    /// Determine the truthiness or falsiness of the literal.
    pub fn get_bool(&self) -> bool {
        use CLiteral::*;
        match *self {
            Integer(x, _) => x != 0u64,
            Character(x) => x != 0u64,
            Floating(x, _) => x != 0f64,
            _ => true,
        }
    }
}

/// Represents a constant integer expression as used in a case expression
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ConstIntExpr {
    U(u64),
    I(i64),
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

/// Type qualifiers (6.7.3)
#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
pub struct Qualifiers {
    /// The `const` qualifier, which marks lvalues as non-assignable.
    ///
    /// We make use of `const` in only two places:
    ///   * Variable and function bindings (which matches up to Rust's `mut` or not bindings)
    ///   * The pointed type in pointers (which matches up to Rust's `*const`/`*mut`)
    pub is_const: bool,

    pub is_restrict: bool,

    /// The `volatile` qualifier, which prevents the compiler from reordering accesses through such
    /// qualified lvalues past other observable side effects (other accesses, or sequence points).
    ///
    /// The part here about not reordering (or changing in any way) access to something volatile
    /// can be replicated in Rust via `std::ptr::read_volatile`  and `std::ptr::write_volatile`.
    /// Since Rust's execution model is still unclear, I am unsure that we get all of the guarantees
    /// `volatile` needs, especially regarding reordering of other side-effects.
    ///
    /// To see where we use `volatile`, check the call-sites of `Translation::volatile_write` and
    /// `Translation::volatile_read`.
    pub is_volatile: bool,
}

impl Qualifiers {
    /// Aggregate qualifier information from two sources.
    pub fn and(self, other: Qualifiers) -> Qualifiers {
        Qualifiers {
            is_const: self.is_const || other.is_const,
            is_restrict: self.is_restrict || other.is_restrict,
            is_volatile: self.is_volatile || other.is_volatile,
        }
    }
}

/// Qualified type
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct CQualTypeId {
    pub qualifiers: Qualifiers,
    pub ctype: CTypeId,
}

impl CQualTypeId {
    pub fn new(ctype: CTypeId) -> Self {
        Self {
            qualifiers: Qualifiers::default(),
            ctype,
        }
    }
}

// TODO: these may be interesting, but I'm not sure if they fit here:
//
//  * UnaryTransformType <http://clang.llvm.org/doxygen/classclang_1_1UnaryTransformType.html>
//  * AdjustedType <http://clang.llvm.org/doxygen/classclang_1_1AdjustedType.html>

/// Represents a type in C (6.2.5 Types)
///
/// Reflects the types in <http://clang.llvm.org/doxygen/classclang_1_1Type.html>
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CTypeKind {
    Void,

    // Boolean type (6.2.5.2)
    Bool,

    // Character type (6.2.5.3)
    Char,

    // Signed types (6.2.5.4)
    SChar,
    Short,
    Int,
    Long,
    LongLong,

    // Unsigned types (6.2.5.6) (actually this also includes `_Bool`)
    UChar,
    UShort,
    UInt,
    ULong,
    ULongLong,

    // Real floating types (6.2.5.10). Ex: `double`
    Float,
    Double,
    LongDouble,

    // Clang specific types
    Int128,
    UInt128,

    Complex(CTypeId),

    // Pointer types (6.7.5.1)
    Pointer(CQualTypeId),

    // C++ Reference
    Reference(CQualTypeId),

    // Array types (6.7.5.2)
    //
    // A qualifier on an array type means the same thing as a qualifier on its element type. Since
    // Clang tracks the qualifiers in both places, we choose to discard qualifiers on the element
    // type.
    //
    // The size expression on a variable-length array is optional, it might be replaced with `*`
    ConstantArray(CTypeId, usize),
    IncompleteArray(CTypeId),
    VariableArray(CTypeId, Option<CExprId>),

    // Type of type or expression (GCC extension)
    TypeOf(CTypeId),
    TypeOfExpr(CExprId),

    // Function type (6.7.5.3)
    //
    // Note a function taking no arguments should have one `void` argument. Functions without any
    // arguments are in K&R format.
    // Flags: is_variable_argument, is_noreturn, has prototype
    Function(CQualTypeId, Vec<CQualTypeId>, bool, bool, bool),

    // Type definition type (6.7.7)
    Typedef(CTypedefId),

    // Represents a pointer type decayed from an array or function type.
    Decayed(CTypeId),
    Elaborated(CTypeId),

    // Type wrapped in parentheses
    Paren(CTypeId),

    // Struct type
    Struct(CRecordId),

    // Union type
    Union(CRecordId),

    // Enum definition type
    Enum(CEnumId),

    BuiltinFn,

    Attributed(CQualTypeId, Option<Attribute>),

    BlockPointer(CQualTypeId),

    Vector(CQualTypeId, usize),

    Half,
    BFloat16,

    // ARM Scalable Vector Extension types
    // TODO: represent all the individual types in AArch64SVEACLETypes.def
    UnhandledSveType,

    Float128,
    // Atomic types (6.7.2.4)
    Atomic(CQualTypeId),

    // Rust sized types, pullback'd into C so that we can treat uint16_t, etc. as real types.
    Int8,
    Int16,
    Int32,
    Int64,
    IntPtr,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    UIntPtr,
    IntMax,
    UIntMax,
    Size,
    SSize,
    PtrDiff,
    WChar,
}

impl CTypeKind {
    pub const PULLBACK_KINDS: [CTypeKind; 16] = {
        use CTypeKind::*;
        [
            Int8, Int16, Int32, Int64, IntPtr, UInt8, UInt16, UInt32, UInt64, UIntPtr, IntMax,
            UIntMax, Size, SSize, PtrDiff, WChar,
        ]
    };

    pub fn as_str(&self) -> &'static str {
        use CTypeKind::*;
        match self {
            Void => "void",
            Bool => "_Bool",
            Char => "char",
            SChar => "signed char",
            Short => "signed short",
            Int => "int",
            Long => "long",
            LongLong => "long long",
            UChar => "unsigned char",
            UShort => "unsigned short",
            UInt => "unsigned int",
            ULong => "unsigned long",
            ULongLong => "unsigned long long",
            Float => "float",
            Double => "double",
            LongDouble => "long double",
            Int128 => "__int128",
            UInt128 => "unsigned __int128",
            Half => "half",
            BFloat16 => "bfloat16",
            Float128 => "__float128",

            Int8 => "int8_t",
            Int16 => "int16_t",
            Int32 => "int32_t",
            Int64 => "int64_t",
            IntPtr => "intptr_t",
            UInt8 => "uint8_t",
            UInt16 => "uint16_t",
            UInt32 => "uint32_t",
            UInt64 => "uint64_t",
            UIntPtr => "uintptr_t",
            IntMax => "intmax_t",
            UIntMax => "uintmax_t",
            Size => "size_t",
            SSize => "ssize_t",
            PtrDiff => "ptrdiff_t",
            WChar => "wchar_t",

            _ => unimplemented!("Printer::print_type({:?})", self),
        }
    }
}

impl Display for CTypeKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Designator {
    Index(u64),
    Range(u64, u64),
    Field(CFieldId),
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

impl CTypeKind {
    pub fn is_pointer(&self) -> bool {
        matches!(*self, Self::Pointer { .. })
    }

    pub fn is_bool(&self) -> bool {
        matches!(*self, Self::Bool)
    }

    pub fn is_enum(&self) -> bool {
        matches!(*self, Self::Enum { .. })
    }

    pub fn is_integral_type(&self) -> bool {
        self.is_unsigned_integral_type() || self.is_signed_integral_type()
    }

    pub fn is_unsigned_integral_type(&self) -> bool {
        use CTypeKind::*;
        matches!(
            self,
            Bool | UChar
                | UInt
                | UShort
                | ULong
                | ULongLong
                | UInt128
                | UInt8
                | UInt16
                | UInt32
                | UInt64
                | UIntPtr
                | UIntMax
                | Size
                | WChar
        )
    }

    pub fn is_signed_integral_type(&self) -> bool {
        use CTypeKind::*;
        // `Char` is true on the platforms we handle
        matches!(
            self,
            Char | SChar
                | Int
                | Short
                | Long
                | LongLong
                | Int128
                | Int8
                | Int16
                | Int32
                | Int64
                | IntPtr
                | IntMax
                | SSize
                | PtrDiff
        )
    }

    pub fn is_floating_type(&self) -> bool {
        use CTypeKind::*;
        matches!(self, Float | Double | LongDouble)
    }

    pub fn as_underlying_decl(&self) -> Option<CDeclId> {
        use CTypeKind::*;
        match *self {
            Struct(decl_id) | Union(decl_id) | Enum(decl_id) => Some(decl_id),
            _ => None,
        }
    }

    pub fn as_decl_or_typedef(&self) -> Option<CDeclId> {
        use CTypeKind::*;
        match *self {
            Typedef(decl_id) | Struct(decl_id) | Union(decl_id) | Enum(decl_id) => Some(decl_id),
            _ => None,
        }
    }

    pub fn is_vector(&self) -> bool {
        matches!(self, Self::Vector { .. })
    }

    /// Choose the smaller, simpler of the two types if they are cast-compatible.
    pub fn smaller_compatible_type(ty1: CTypeKind, ty2: CTypeKind) -> Option<CTypeKind> {
        let int = Self::is_integral_type;
        let float = Self::is_floating_type;

        use CTypeKind::*;
        let ty = match (&ty1, &ty2) {
            (ty, ty2) if ty == ty2 => ty1,
            (Void, _) => ty2,
            (Bool, ty) | (ty, Bool) if int(ty) => Bool,

            (Char, ty) | (ty, Char) if int(ty) => Char,
            (SChar, ty) | (ty, SChar) if int(ty) => SChar,
            (UChar, ty) | (ty, UChar) if int(ty) => UChar,

            (Short, ty) | (ty, Short) if int(ty) => Short,
            (UShort, ty) | (ty, UShort) if int(ty) => UShort,

            (Int, ty) | (ty, Int) if int(ty) => Int,
            (UInt, ty) | (ty, UInt) if int(ty) => UInt,

            (Float, ty) | (ty, Float) if float(ty) || int(ty) => Float,

            (Long, ty) | (ty, Long) if int(ty) => Long,
            (ULong, ty) | (ty, ULong) if int(ty) => ULong,

            (Double, ty) | (ty, Double) if float(ty) || int(ty) => Double,

            (LongLong, ty) | (ty, LongLong) if int(ty) => LongLong,
            (ULongLong, ty) | (ty, ULongLong) if int(ty) => ULongLong,

            (LongDouble, ty) | (ty, LongDouble) if float(ty) || int(ty) => LongDouble,

            (Int128, ty) | (ty, Int128) if int(ty) => Int128,
            (UInt128, ty) | (ty, UInt128) if int(ty) => UInt128,

            // Integer to pointer conversion. We want to keep the integer and
            // cast to a pointer at use.
            (Pointer(_), ty) if int(ty) => ty2,
            (ty, Pointer(_)) if int(ty) => ty1,

            // Array to pointer decay. We want to use the array and push the
            // decay to the use of the value.
            (Pointer(ptr_ty), ConstantArray(arr_ty, _))
            | (Pointer(ptr_ty), IncompleteArray(arr_ty))
            | (Pointer(ptr_ty), VariableArray(arr_ty, _))
                if ptr_ty.ctype == *arr_ty =>
            {
                ty2
            }
            (ConstantArray(arr_ty, _), Pointer(ptr_ty))
            | (IncompleteArray(arr_ty), Pointer(ptr_ty))
            | (VariableArray(arr_ty, _), Pointer(ptr_ty))
                if ptr_ty.ctype == *arr_ty =>
            {
                ty1
            }

            _ => return None,
        };
        Some(ty)
    }

    /// Return the element type of a pointer or array
    pub fn element_ty(&self) -> Option<CTypeId> {
        Some(match *self {
            Self::Pointer(ty) => ty.ctype,
            Self::ConstantArray(ty, _) => ty,
            Self::IncompleteArray(ty) => ty,
            Self::VariableArray(ty, _) => ty,
            _ => return None,
        })
    }
}

#[cfg(test)]
mod tests {
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
            |a, b| ctx.compare_src_locs(a, b),
            |loc| format!("{loc}"),
        );

        // This should not panic.
        locs.sort_unstable_by(|a, b| ctx.compare_src_locs(a, b));
    }
}
