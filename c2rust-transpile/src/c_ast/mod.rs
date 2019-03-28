use c2rust_ast_exporter::clang_ast::LRValue;
use std::collections::{HashMap,HashSet,BTreeMap};
use indexmap::{IndexMap, IndexSet};
use std::ops::Index;
use std::path::PathBuf;

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Copy, Clone)]
pub struct CTypeId(pub u64);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Copy, Clone)]
pub struct CExprId(pub u64);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Copy, Clone)]
pub struct CDeclId(pub u64);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Copy, Clone)]
pub struct CStmtId(pub u64);

// These are references into particular variants of AST nodes
pub type CLabelId = CStmtId;  // Labels point into the 'StmtKind::Label' that declared the label
pub type CFieldId = CDeclId;  // Records always contain 'DeclKind::Field's
pub type CParamId = CDeclId;  // Parameters always contain 'DeclKind::Variable's
pub type CFuncTypeId = CTypeId;  // Function declarations always have types which are 'TypeKind::Function'
pub type CRecordId = CDeclId;  // Record types need to point to 'DeclKind::Record'
pub type CTypedefId = CDeclId;  // Typedef types need to point to 'DeclKind::Typedef'
pub type CEnumId = CDeclId;  // Enum types need to point to 'DeclKind::Enum'
pub type CEnumConstantId = CDeclId;  // Enum's need to point to child 'DeclKind::EnumConstant's

pub use self::conversion::*;
pub use self::print::Printer;
use super::diagnostics::Diagnostic;

mod conversion;
mod print;
pub mod iterators;

/// AST context containing all of the nodes in the Clang AST
#[derive(Debug, Clone)]
pub struct TypedAstContext {
    pub c_types: HashMap<CTypeId, CType>,
    pub c_exprs: HashMap<CExprId, CExpr>,
    pub c_stmts: HashMap<CStmtId, CStmt>,

    // Decls require a stable iteration order as this map will be
    // iterated over export all defined types during translation.
    pub c_decls: IndexMap<CDeclId, CDecl>,

    pub c_decls_top: Vec<CDeclId>,
    pub c_main: Option<CDeclId>,
    pub c_files: HashMap<u64, String>,
    pub parents: HashMap<CDeclId, CDeclId>, // record fields and enum constants

    pub comments: Vec<Located<String>>,

    // The key is the typedef decl being squashed away,
    // and the value is the decl id to the corresponding structure
    pub prenamed_decls: IndexMap<CDeclId, CDeclId>,
}

/// Comments associated with a typed AST context
#[derive(Debug, Clone)]
pub struct CommentContext {
    decl_comments: HashMap<CDeclId, Vec<String>>,
    stmt_comments: HashMap<CStmtId, Vec<String>>,
}

impl TypedAstContext {
    pub fn new() -> TypedAstContext {
        TypedAstContext {
            c_types: HashMap::new(),
            c_exprs: HashMap::new(),
            c_decls: IndexMap::new(),
            c_stmts: HashMap::new(),

            c_decls_top: Vec::new(),
            c_main: None,
            c_files: HashMap::new(),
            parents: HashMap::new(),

            comments: vec![],
            prenamed_decls: IndexMap::new(),
        }
    }

    pub fn is_null_expr(&self, expr_id: CExprId) -> bool {
        match self[expr_id].kind {
            CExprKind::ExplicitCast(_, _, CastKind::NullToPointer, _, _) |
            CExprKind::ImplicitCast(_, _, CastKind::NullToPointer, _, _) => true,

            CExprKind::ExplicitCast(ty, e, CastKind::BitCast, _, _) |
            CExprKind::ImplicitCast(ty, e, CastKind::BitCast, _, _) =>
                self.resolve_type(ty.ctype).kind.is_pointer() && self.is_null_expr(e),

            _ => false,
        }
    }

    /// Predicate for struct, union, and enum declarations without
    /// bodies. These forward declarations are suitable for use as
    /// the targets of pointers
    pub fn is_forward_declared_type(&self, typ: CTypeId) -> bool {
        match self.resolve_type(typ).kind.as_underlying_decl() {
            Some(decl_id) => {
                match self[decl_id].kind {
                    CDeclKind::Struct { fields: None, .. } => true,
                    CDeclKind::Union { fields: None, .. } => true,
                    CDeclKind::Enum { integral_type: None, .. } => true,
                    _ => false,
                }
            }
            _ => false,
        }
    }

    /// Predicate for function pointers
    pub fn is_function_pointer(&self, typ: CTypeId) -> bool {
        let resolved_ctype = self.resolve_type(typ);
        if let CTypeKind::Pointer(p) = resolved_ctype.kind {
            if let CTypeKind::Function { .. } = self.resolve_type(p.ctype).kind {
                true
            } else { false }
        } else { false }
    }

    pub fn resolve_type_id(&self, typ: CTypeId) -> CTypeId {
        match self.index(typ).kind {
            CTypeKind::Attributed(ty, _) => self.resolve_type_id(ty.ctype),
            CTypeKind::Elaborated(ty) => self.resolve_type_id(ty),
            CTypeKind::Decayed(ty) => self.resolve_type_id(ty),
            CTypeKind::TypeOf(ty) => self.resolve_type_id(ty),
            CTypeKind::Paren(ty) => self.resolve_type_id(ty),
            CTypeKind::Typedef(decl) => {
                match self.index(decl).kind {
                    CDeclKind::Typedef { typ: ty, .. } => self.resolve_type_id(ty.ctype),
                    _ => panic!("Typedef decl did not point to a typedef"),
                }
            },
            _ => typ,
        }
    }

    pub fn resolve_type(&self, typ: CTypeId) -> &CType {
        let resolved_typ_id = self.resolve_type_id(typ);
        self.index(resolved_typ_id)
    }

    /// Pessimistically try to check if an expression has side effects. If it does, or we can't tell
    /// that it doesn't, return `false`.
    pub fn is_expr_pure(&self, expr: CExprId) -> bool {
        match self.index(expr).kind {
            CExprKind::BadExpr |
            CExprKind::ShuffleVector(..) |
            CExprKind::ConvertVector(..) |
            CExprKind::Call(..) |
            CExprKind::Unary(_, UnOp::PreIncrement, _, _) |
            CExprKind::Unary(_, UnOp::PostIncrement, _, _) |
            CExprKind::Unary(_, UnOp::PreDecrement, _, _) |
            CExprKind::Unary(_, UnOp::PostDecrement, _, _) |
            CExprKind::Binary(_, BinOp::Assign, _, _, _, _) |
            CExprKind::InitList { .. } |
            CExprKind::ImplicitValueInit { .. } |
            CExprKind::Predefined(..) |
            CExprKind::Statements(..) | // TODO: more precision
            CExprKind::VAArg(..) => false,

            CExprKind::Literal(_, _) |
            CExprKind::DeclRef(_, _, _) |
            CExprKind::UnaryType(_, _, _, _) |
            CExprKind::OffsetOf(..) => true,

            CExprKind::DesignatedInitExpr(_,_,e) |
            CExprKind::ImplicitCast(_, e, _, _, _) |
            CExprKind::ExplicitCast(_, e, _, _, _) |
            CExprKind::Member(_, e, _, _, _) |
            CExprKind::CompoundLiteral(_, e) |
            CExprKind::Unary(_, _, e, _) => self.is_expr_pure(e),

            CExprKind::Binary(_, op, _, _, _, _) if op.underlying_assignment().is_some() => false,
            CExprKind::Binary(_, _, lhs, rhs, _, _) => self.is_expr_pure(lhs) && self.is_expr_pure(rhs),

            CExprKind::ArraySubscript(_, lhs, rhs, _) => self.is_expr_pure(lhs) && self.is_expr_pure(rhs),
            CExprKind::Conditional(_, c, lhs, rhs) => self.is_expr_pure(c) && self.is_expr_pure(lhs) && self.is_expr_pure(rhs),
            CExprKind::BinaryConditional(_, lhs, rhs) => self.is_expr_pure(lhs) && self.is_expr_pure(rhs),
        }
    }

    // Pessimistically try to check if an expression doesn't return. If it does, or we can't tell
    /// that it doesn't, return `false`.
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


    pub fn prune_unused_decls(&mut self) {
        use self::iterators::{SomeId, DFNodes};
        // Starting from a set of root declarations, walk each one to find declarations it
        // dependens on.  Then walk each of those, recursively.

        // Declarations we still need to walk.  Everything in here is also in `used`.
        let mut to_walk: Vec<CDeclId> = Vec::new();
        // Declarations accessible from a root.
        let mut used: HashSet<CDeclId> = HashSet::new();

        // Mark all the roots as used.  Roots are all top-level functions and variables that might
        // be visible from another compilation unit.
        for &decl_id in &self.c_decls_top {
            let decl = self.index(decl_id);
            match decl.kind {
                CDeclKind::Function { body: Some(_), is_global: true, is_inline: false, .. } => {
                    to_walk.push(decl_id);
                    used.insert(decl_id);
                },
                CDeclKind::Variable { is_defn: true, is_externally_visible: true, .. } => {
                    to_walk.push(decl_id);
                    used.insert(decl_id);
                },
                CDeclKind::Variable { ref attrs, .. } |
                CDeclKind::Function { ref attrs, .. } if attrs.contains(&Attribute::Used) => {
                    to_walk.push(decl_id);
                    used.insert(decl_id);
                },
                _ => {},
            }
        }

        while let Some(enclosing_decl_id) = to_walk.pop() {
            for some_id in DFNodes::new(self, SomeId::Decl(enclosing_decl_id)) {
                match some_id {
                    SomeId::Type(type_id) => {
                        match self.c_types[&type_id].kind {
                            // This is a reference to a previously declared type.  If we look
                            // through it we should(?) get something that looks like a declaration,
                            // which we can mark as used.
                            CTypeKind::Elaborated(decl_type_id) => {
                                let decl_id = self.c_types[&decl_type_id].kind
                                    .as_decl_or_typedef()
                                    .expect("target of CTypeKind::Elaborated isn't a decl?");
                                if used.insert(decl_id) {
                                    to_walk.push(decl_id);
                                }
                            },

                            // For everything else (including `Struct` etc.), DFNodes will walk the
                            // corresponding declaration.
                            _ => {},
                        }
                    },

                    SomeId::Expr(expr_id) => {
                        match self.c_exprs[&expr_id].kind {
                            CExprKind::DeclRef(_, decl_id, _) => {
                                if used.insert(decl_id) {
                                    to_walk.push(decl_id);
                                }
                            },

                            _ => {},
                        }
                    },

                    SomeId::Decl(decl_id) => {
                        if used.insert(decl_id) {
                            to_walk.push(decl_id);
                        }

                        match self.c_decls[&decl_id].kind {
                            CDeclKind::EnumConstant { .. } => {
                                // Special case for enums.  The enum constant is used, so the whole
                                // enum is also used.
                                let parent_id = self.parents[&decl_id];
                                if used.insert(parent_id) {
                                    to_walk.push(parent_id);
                                }
                            },
                            _ => {},
                        }
                    },

                    // Stmts can include decls, but we'll see the DeclId itself in a later
                    // iteration.
                    SomeId::Stmt(_) => {},
                }
            }
        }

        // Unset c_main if we are not retaining its declaration
        if let Some(main_id) = self.c_main {
            if !used.contains(&main_id) {
                self.c_main = None;
            }
        }

        // Prune any declaration that isn't considered live
        self.c_decls.retain(|&decl_id, _decl|
            used.contains(&decl_id)
        );

        // Prune top declarations that are not considered live
        self.c_decls_top.retain(|x| used.contains(x));
    }
}

impl CommentContext {
    pub fn empty() -> CommentContext {
        CommentContext {
            decl_comments: HashMap::new(),
            stmt_comments: HashMap::new(),
        }
    }


    // Try to match up every comment with a declaration or a statement
    pub fn new(
        ast_context: &mut TypedAstContext
    ) -> CommentContext {

        // Group and sort declarations by file and by position
        let mut decls: HashMap<u64, Vec<(SrcLoc, CDeclId)>> = HashMap::new();
        for (decl_id, ref loc_decl) in &ast_context.c_decls {
            if let Some(ref loc) = loc_decl.loc {
                decls.entry(loc.fileid).or_insert(vec![]).push((loc.clone(), *decl_id));
            }
        }
        decls.iter_mut().for_each(|(_, v)| v.sort());

        // Group and sort statements by file and by position
        let mut stmts: HashMap<u64, Vec<(SrcLoc, CStmtId)>> = HashMap::new();
        for (stmt_id, ref loc_stmt) in &ast_context.c_stmts {
            if let Some(ref loc) = loc_stmt.loc {
                stmts.entry(loc.fileid).or_insert(vec![]).push((loc.clone(), *stmt_id));
            }
        }
        stmts.iter_mut().for_each(|(_, v)| v.sort());


        let mut decl_comments_map: HashMap<CDeclId, BTreeMap<SrcLoc, String>> = HashMap::new();
        let mut stmt_comments_map: HashMap<CStmtId, BTreeMap<SrcLoc, String>> = HashMap::new();

        let empty_vec1 = &vec![];
        let empty_vec2 = &vec![];


        // Match comments to declarations and statements
        while let Some(Located { loc, kind: str }) = ast_context.comments.pop() {
            if let Some(loc) = loc {
                let this_file_decls = decls.get(&loc.fileid).unwrap_or(empty_vec1);
                let this_file_stmts = stmts.get(&loc.fileid).unwrap_or(empty_vec2);

                // Find the closest declaration and statement
                let decl_ix = this_file_decls
                    .binary_search_by_key(&loc.line, |&(ref l,_)| l.line)
                    .unwrap_or_else(|x| x);
                let stmt_ix = this_file_stmts
                    .binary_search_by_key(&loc.line, |&(ref l,_)| l.line)
                    .unwrap_or_else(|x| x);

                // Prefer the one that is higher up (biasing towards declarations if there is a tie)
                match (this_file_decls.get(decl_ix), this_file_stmts.get(stmt_ix)) {
                    (Some(&(ref l1, d)), Some(&(ref l2, s))) => {
                        if l1 > l2 {
                            stmt_comments_map.entry(s).or_insert(BTreeMap::new()).insert(loc, str);
                        } else {
                            decl_comments_map.entry(d).or_insert(BTreeMap::new()).insert(loc, str);
                        }
                    }
                    (Some(&(_, d)), None) => {
                        decl_comments_map.entry(d).or_insert(BTreeMap::new()).insert(loc, str);
                    }
                    (None, Some(&(_, s))) => {
                        stmt_comments_map.entry(s).or_insert(BTreeMap::new()).insert(loc, str);
                    }
                    (None, None) => {
                        diag!(Diagnostic::Comments, "Didn't find a target node for the comment '{}'", str);
                    },
                };
            }
        }

        // Flatten out the nested comment maps
        let decl_comments = decl_comments_map
          .into_iter()
          .map(|(decl_id, map)| (decl_id, map.into_iter().map(|(_, v)| v).collect()))
          .collect();
        let stmt_comments = stmt_comments_map
          .into_iter()
          .map(|(decl_id, map)| (decl_id, map.into_iter().map(|(_, v)| v).collect()))
          .collect();

        CommentContext { decl_comments, stmt_comments }
    }

    // Extract the comment for a given declaration
    pub fn remove_decl_comment(&mut self, decl_id: CDeclId) -> Vec<String> {
        self.decl_comments.remove(&decl_id).unwrap_or(vec![])
    }

    // Extract the comment for a given statement
    pub fn remove_stmt_comment(&mut self, stmt_id: CStmtId) -> Vec<String> {
        self.stmt_comments.remove(&stmt_id).unwrap_or(vec![])
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
        static BADEXPR: CExpr = Located { loc: None, kind: CExprKind::BadExpr, };
        match self.c_exprs.get(&index) {
            None => &BADEXPR, // panic!("Could not find {:?} in TypedAstContext", index),
            Some(ty) => ty,
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

/// Represents a position inside a C source file
#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone)]
pub struct SrcLoc {
    pub fileid: u64,
    pub line: u64,
    pub column: u64,
    pub file_path: Option<PathBuf>,
}


/// Represents some AST node possibly with source location information bundled with it
#[derive(Debug, Clone)]
pub struct Located<T> {
    pub loc: Option<SrcLoc>,
    pub kind: T,
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
    },

    // Field
    Field {
        name: String,
        typ: CQualTypeId,
        bitfield_width: Option<u64>,
        platform_bit_offset: u64,
        platform_type_bitwidth: u64,
    },
}

impl CDeclKind {
    pub fn get_name(&self) -> Option<&String> {
        match self {
            &CDeclKind::Function { name: ref i, .. } => Some(i),
            &CDeclKind::Variable { ident: ref i, .. } => Some(i),
            &CDeclKind::Typedef { name: ref i, .. } => Some(i),
            &CDeclKind::EnumConstant { name: ref i, .. } => Some(i),
            &CDeclKind::Enum { name: Some(ref i), .. } => Some(i),
            &CDeclKind::Struct { name: Some(ref i), .. } => Some(i),
            &CDeclKind::Union { name: Some(ref i), .. } => Some(i),
            &CDeclKind::Field { name: ref i, .. } => Some(i),
            _ => None,
        }
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
/// We've kept a qualified type on every node since Clang has this information available, and since
/// the semantics of translations of certain constructs often depend on the type of the things they
/// are given.
///
/// As per the C standard, qualifiers on types make sense only on lvalues.
#[derive(Debug, Clone)]
pub enum CExprKind {
    // Literals
    Literal(CQualTypeId, CLiteral),

    // Unary operator.
    Unary(CQualTypeId, UnOp, CExprId, LRValue),

    // Unary type operator.
    UnaryType(CQualTypeId, UnTypeOp, Option<CExprId>, CQualTypeId),

    // Offsetof expression.
    OffsetOf(CQualTypeId, OffsetOfKind),

    // Binary operator
    Binary(CQualTypeId, BinOp, CExprId, CExprId, Option<CQualTypeId>, Option<CQualTypeId>),

    // Implicit cast
    ImplicitCast(CQualTypeId, CExprId, CastKind, Option<CFieldId>, LRValue),

    // Explicit cast
    ExplicitCast(CQualTypeId, CExprId, CastKind, Option<CFieldId>, LRValue),

    // Reference to a decl (a variable, for instance)
    // TODO: consider enforcing what types of declarations are allowed here
    DeclRef(CQualTypeId, CDeclId, LRValue),

    // Function call
    Call(CQualTypeId, CExprId, Vec<CExprId>),

    // Member access
    Member(CQualTypeId, CExprId, CDeclId, MemberKind, LRValue),

    // Array subscript access
    ArraySubscript(CQualTypeId, CExprId, CExprId, LRValue),

    // Ternary conditional operator
    Conditional(CQualTypeId, CExprId, CExprId, CExprId),

    // Binary conditional operator ?: GNU extension
    BinaryConditional(CQualTypeId, CExprId, CExprId),

    // Initializer list - type, initializers, union field, syntactic form
    InitList(CQualTypeId, Vec<CExprId>, Option<CFieldId>, Option<CExprId>),

    // Designated initializer
    ImplicitValueInit(CQualTypeId),

    // Compound literal
    CompoundLiteral(CQualTypeId, CExprId),

    // Predefined expr
    Predefined(CQualTypeId, CExprId),

    // Statement expression
    Statements(CQualTypeId, CStmtId),

    // Variable argument list
    VAArg(CQualTypeId, CExprId),

    // Unsupported vector operations,
    ShuffleVector(CQualTypeId, Vec<CExprId>),
    ConvertVector(CQualTypeId, Vec<CExprId>),

    // From syntactic form of initializer list expressions
    DesignatedInitExpr(CQualTypeId, Vec<Designator>, CExprId),

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
            CExprKind::Unary(_, _, _, lrvalue) |
            CExprKind::DeclRef(_, _, lrvalue) |
            CExprKind::ImplicitCast(_, _, _, _, lrvalue) |
            CExprKind::ExplicitCast(_, _, _, _, lrvalue) |
            CExprKind::Member(_, _, _, _, lrvalue) |
            CExprKind::ArraySubscript(_, _, _, lrvalue) => lrvalue,
            _ => LRValue::RValue,
        }
    }

    pub fn get_qual_type(&self) -> Option<CQualTypeId> {
        match *self {
            CExprKind::BadExpr => None,
            CExprKind::Literal(ty, _) |
            CExprKind::OffsetOf(ty, _) |
            CExprKind::Unary(ty, _, _, _) |
            CExprKind::UnaryType(ty, _, _, _) |
            CExprKind::Binary(ty, _, _, _, _, _) |
            CExprKind::ImplicitCast(ty, _, _, _, _) |
            CExprKind::ExplicitCast(ty, _, _, _, _) |
            CExprKind::DeclRef(ty, _, _) |
            CExprKind::Call(ty, _, _) |
            CExprKind::Member(ty, _, _, _, _) |
            CExprKind::ArraySubscript(ty, _, _, _) |
            CExprKind::Conditional(ty, _, _, _) |
            CExprKind::BinaryConditional(ty, _, _) |
            CExprKind::InitList(ty, _, _, _) |
            CExprKind::ImplicitValueInit(ty) |
            CExprKind::CompoundLiteral(ty, _) |
            CExprKind::Predefined(ty, _) |
            CExprKind::Statements(ty, _) |
            CExprKind::VAArg(ty, _) |
            CExprKind::ShuffleVector(ty, _) |
            CExprKind::ConvertVector(ty, _) |
            CExprKind::DesignatedInitExpr(ty,_,_) => Some(ty),
        }
    }

    pub fn get_type(&self) -> Option<CTypeId> {
        self.get_qual_type().map(|x|x.ctype)
    }

    /// Try to determine the truthiness or falsiness of the expression. Return `None` if we can't
    /// say anything.
    pub fn get_bool(&self) -> Option<bool> {
        match *self{
            CExprKind::Literal(_, ref lit) => Some(lit.get_bool()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
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
}

/// Represents a unary operator in C (6.5.3 Unary operators) and GNU C extensions
#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    AddressOf,      // &x
    Deref,          // *x
    Plus,           // +x
    PostIncrement,  // x++
    PreIncrement,   // ++x
    Negate,         // -x
    PostDecrement,  // x--
    PreDecrement,   // --x
    Complement,     // ~x
    Not,            // !x
    Real,           // [GNU C] __real x
    Imag,           // [GNU C] __imag x
    Extension,      // [GNU C] __extension__ x
    Coawait,        // [C++ Coroutines] co_await x
}

/// Represents a unary type operator in C
#[derive(Debug, Clone, Copy)]
pub enum UnTypeOp {
    SizeOf,
    AlignOf,
}

impl UnOp {

    /// Check is the operator is rendered before or after is operand.
    pub fn is_prefix(&self) -> bool {
        match *self {
            UnOp::PostIncrement => false,
            UnOp::PostDecrement => false,
            _ => true,
        }
    }
}

/// Represents a binary operator in C (6.5.5 Multiplicative operators - 6.5.14 Logical OR operator)
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Multiply,         // *
    Divide,           // /
    Modulus,          // %
    Add,              // +
    Subtract,         // -
    ShiftLeft,        // <<
    ShiftRight,       // >>
    Less,             // <
    Greater,          // >
    LessEqual,        // <=
    GreaterEqual,     // >=
    EqualEqual,       // ==
    NotEqual,         // !=
    BitAnd,           // &
    BitXor,           // ^
    BitOr,            // |
    And,              // &&
    Or,               // ||

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

    Assign,           // =
    Comma,            // ,
}

impl BinOp {

    /// Maps compound assignment operators to operator underlying them, and returns `None` for all
    /// other operators.
    ///
    /// For example, `AssignAdd` maps to `Some(Add)` but `Add` maps to `None`.
    pub fn underlying_assignment(&self) -> Option<BinOp> {
        match *self {
            BinOp::AssignAdd => Some(BinOp::Add),
            BinOp::AssignSubtract => Some(BinOp::Subtract),
            BinOp::AssignMultiply => Some(BinOp::Multiply),
            BinOp::AssignDivide => Some(BinOp::Divide),
            BinOp::AssignModulus => Some(BinOp::Modulus),
            BinOp::AssignBitXor => Some(BinOp::BitXor),
            BinOp::AssignShiftLeft => Some(BinOp::ShiftLeft),
            BinOp::AssignShiftRight => Some(BinOp::ShiftRight),
            BinOp::AssignBitOr => Some(BinOp::BitOr),
            BinOp::AssignBitAnd => Some(BinOp::BitAnd),
            _ => None,
        }
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum IntBase {
    Dec, Hex, Oct
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
        match *self{
            CLiteral::Integer(x, _) => x != 0u64,
            CLiteral::Character(x) => x != 0u64,
            CLiteral::Floating(x, _) => x != 0f64,
            _ => true

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
}

#[derive(Clone, Debug)]
pub struct AsmOperand {
    pub constraints: String,
    pub expression: CExprId,
}

/// Type qualifiers (6.7.3)
#[derive(Debug, Copy, Clone, Default, PartialEq)]
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
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct CQualTypeId {
    pub qualifiers: Qualifiers,
    pub ctype: CTypeId,
}


// TODO: these may be interesting, but I'm not sure if they fit here:
//
//  * UnaryTransformType <http://clang.llvm.org/doxygen/classclang_1_1UnaryTransformType.html>
//  * AdjustedType <http://clang.llvm.org/doxygen/classclang_1_1AdjustedType.html>

/// Represents a type in C (6.2.5 Types)
///
/// Reflects the types in <http://clang.llvm.org/doxygen/classclang_1_1Type.html>
#[derive(Debug, Clone, PartialEq)]
pub enum CTypeKind {
    /* Builtin types:
     * <https://github.com/llvm-mirror/clang/blob/master/include/clang/AST/BuiltinTypes.def> */

    // Void type (6.2.5.19)
    Void,

    // Boolean type (6.2.5.2)
    Bool,

    // Character type (6.2.5.3)
    Char,

    // Signed types (6.2.5.4)
    SChar, Short, Int, Long, LongLong,

    // Unsigned types (6.2.5.6) (actually this also includes `_Bool`)
    UChar, UShort, UInt, ULong, ULongLong,

    // Real floating types (6.2.5.10). Ex: `double`
    Float, Double, LongDouble,

    // Clang specific types
    Int128, UInt128,

    /* Compound types <https://github.com/llvm-mirror/clang/blob/master/include/clang/AST/TypeNodes.def> */

    // Complex types (6.2.5.11). Ex: `float _Complex`.
    Complex(CTypeId),

    // Pointer types (6.7.5.1)
    Pointer(CQualTypeId),

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
    // arguments and in K&R format.
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
}

#[derive(Copy, Clone, Debug)]
pub enum Designator {
    Index(u64),
    Range(u64,u64),
    Field(CFieldId),
}

/// Enumeration of supported attributes for Declarations
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Attribute {
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
}

impl CTypeKind {

    pub fn is_pointer(&self) -> bool {
        match *self {
            CTypeKind::Pointer{..} => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match *self {
            CTypeKind::Bool => true,
            _ => false,
        }
    }

    pub fn is_enum(&self) -> bool {
        match *self {
            CTypeKind::Enum{..} => true,
            _ => false,
        }
    }

    pub fn is_integral_type(&self) -> bool {
        self.is_unsigned_integral_type() || self.is_signed_integral_type()
    }

    pub fn is_unsigned_integral_type(&self) -> bool {
        match *self {
            CTypeKind::Bool => true,
            CTypeKind::UChar => true,
            CTypeKind::UInt => true,
            CTypeKind::UShort => true,
            CTypeKind::ULong => true,
            CTypeKind::ULongLong => true,
            CTypeKind::UInt128 => true,
            _ => false,
        }
    }

    pub fn is_signed_integral_type(&self) -> bool {
        match *self {
            CTypeKind::Char => true, // true on the platforms we handle
            CTypeKind::SChar => true,
            CTypeKind::Int => true,
            CTypeKind::Short => true,
            CTypeKind::Long => true,
            CTypeKind::LongLong => true,
            CTypeKind::Int128 => true,
            _ => false,
        }
    }

    pub fn is_floating_type(&self) -> bool {
        match *self {
            CTypeKind::Float => true,
            CTypeKind::Double => true,
            CTypeKind::LongDouble => true,
            _ => false,
        }
    }

    pub fn as_underlying_decl(&self) -> Option<CDeclId> {
        match *self {
            CTypeKind::Struct(decl_id) |
            CTypeKind::Union(decl_id) |
            CTypeKind::Enum(decl_id) => Some(decl_id),
            _ => None
        }
    }

    pub fn as_decl_or_typedef(&self) -> Option<CDeclId> {
        match *self {
            CTypeKind::Typedef(decl_id) |
            CTypeKind::Struct(decl_id) |
            CTypeKind::Union(decl_id) |
            CTypeKind::Enum(decl_id) => Some(decl_id),
            _ => None
        }
    }
}
