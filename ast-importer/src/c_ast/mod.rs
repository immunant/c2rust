use std::collections::{HashMap,HashSet};
use indexmap::IndexMap;
use std::ops::Index;

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
        }
    }

    pub fn is_null_expr(&self, expr_id: CExprId) -> bool {
        match self[expr_id].kind {
            CExprKind::ExplicitCast(_, _, CastKind::NullToPointer, _) |
            CExprKind::ImplicitCast(_, _, CastKind::NullToPointer, _) => true,

            CExprKind::ExplicitCast(ty, e, CastKind::BitCast, _) |
            CExprKind::ImplicitCast(ty, e, CastKind::BitCast, _) =>
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
            CExprKind::Call(_, _, _) |
            CExprKind::Unary(_, UnOp::PreIncrement, _) |
            CExprKind::Unary(_, UnOp::PostIncrement, _) |
            CExprKind::Unary(_, UnOp::PreDecrement, _) |
            CExprKind::Unary(_, UnOp::PostDecrement, _) |
            CExprKind::Binary(_, BinOp::Assign, _, _, _, _) |
            CExprKind::InitList { .. } |
            CExprKind::ImplicitValueInit { .. } |
            CExprKind::Predefined(_, _) |
            CExprKind::Statements(..) => false, // TODO: more precision

            CExprKind::Literal(_, _) |
            CExprKind::DeclRef(_, _) |
            CExprKind::UnaryType(_, _, _, _) |
            CExprKind::OffsetOf(..) => true,

            CExprKind::DesignatedInitExpr(_,_,e) |
            CExprKind::ImplicitCast(_, e, _, _) |
            CExprKind::ExplicitCast(_, e, _, _) |
            CExprKind::Member(_, e, _, _) |
            CExprKind::CompoundLiteral(_, e) |
            CExprKind::VAArg(_, e) |
            CExprKind::Unary(_, _, e) => self.is_expr_pure(e),

            CExprKind::Binary(_, op, _, _, _, _) if op.underlying_assignment().is_some() => false,
            CExprKind::Binary(_, _, lhs, rhs, _, _) => self.is_expr_pure(lhs) && self.is_expr_pure(rhs),

            CExprKind::ArraySubscript(_, lhs, rhs) => self.is_expr_pure(lhs) && self.is_expr_pure(rhs),
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
            CTypeKind::Function(_, _, _, no_return) => no_return,
            _ => false,
        }
    }


    pub fn simplify(&mut self) {
        // Set of declarations that should be preserved
        let mut live: HashSet<CDeclId> = HashSet::new();

        // Vector of types that need to be visited which can cause declarations to be live
        let mut type_queue: Vec<CTypeId> = vec![];

        // All variable and function definitions are considered live
        for (&decl_id, decl) in &self.c_decls {
            match decl.kind {
                CDeclKind::Function { typ, body: Some(_), .. } => {
                    live.insert(decl_id);
                    type_queue.push(typ); // references the return type
                }
                CDeclKind::Variable { is_defn: true, .. } => { live.insert(decl_id); }
                _ => {}
            }
        }

        for stmt in self.c_stmts.values() {
            if let CStmtKind::Decls(ref decl_ids) = stmt.kind {
                live.extend(decl_ids);
                for decl_id in decl_ids {
                    match self.c_decls[decl_id].kind {
                        CDeclKind::Typedef { typ, .. } => type_queue.push(typ.ctype),
                        CDeclKind::Enum { ref variants, .. } => live.extend(variants),
                        CDeclKind::Struct { fields: Some(ref arr), .. } => {
                            live.extend(arr);
                            for &field_id in arr {
                                if let CDeclKind::Field { typ, .. } = self[field_id].kind {
                                    type_queue.push(typ.ctype)
                                }
                            }
                        }
                        CDeclKind::Union { fields: Some(ref arr), .. } => {
                            for &field_id in arr {
                                live.insert(field_id);
                                if let CDeclKind::Field { typ, .. } = self[field_id].kind {
                                    type_queue.push(typ.ctype)
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        // All expressions are considered live (this is an overapproximation if an otherwise
        // unused function declaration uses a VLA and that VLA's size expression mentions
        // some definitions.
        for expr in self.c_exprs.values() {
            if let Some(t) = expr.kind.get_type() {
                type_queue.push(t);
            }

            match expr.kind {
                // Could mention external functions, variables, and enum constants
                CExprKind::DeclRef(_, decl_id) => {
                    live.insert(decl_id);
                    // This declref could refer to an enum constant, so we want to keep the enum
                    // declaration for that constant live
                    if let Some(&parent_id) = self.parents.get(&decl_id) {
                        if live.insert(parent_id) {
                            if let CDeclKind::Enum { ref variants, .. } = self[parent_id].kind {
                                live.extend(variants);
                            }
                        }
                    }
                }
                CExprKind::UnaryType(_, _, _, type_id) => { type_queue.push(type_id.ctype); }
                _ => {}
            }
        }

        // Track all the variables associated with pruned function prototypes for removal
        let mut bad_variables: HashSet<CDeclId> = HashSet::new();

        self.c_decls.retain(|&decl_id, decl| {
            if live.contains(&decl_id) { return true; }
            match &decl.kind {
                &CDeclKind::Function { body: None, ref parameters, .. } => {
                    bad_variables.extend(parameters);
                    false
                },
                _ => true,
            }
        });

        // Clean up all of the variables associated with the removed function prototypes
        for decl_id in bad_variables {
            self.c_decls.remove(&decl_id);
        }

        // Check variables after unused function declarations are removed so that their parameters
        // don't keep extra types alive
        for (decl_id, decl) in &self.c_decls {
            if let CDeclKind::Variable { typ, .. } = decl.kind {
                live.insert(*decl_id);
                type_queue.push(typ.ctype);
            }
        }

        // Recursively traverse the set of types that were reachable above marking all of the
        // transitively reachable declarations as live
        let mut types_visited: HashSet<CTypeId> = HashSet::new();
        while let Some(type_id) = type_queue.pop() {
            if !types_visited.insert(type_id) { continue }

            match self.c_types[&type_id].kind {
                // Leaf nodes
                CTypeKind::Void | CTypeKind::Bool | CTypeKind::Char | CTypeKind::SChar |
                CTypeKind::Short | CTypeKind::Int | CTypeKind::Long | CTypeKind::LongLong |
                CTypeKind::UChar | CTypeKind::UShort | CTypeKind::UInt | CTypeKind::ULong |
                CTypeKind::ULongLong | CTypeKind::Float | CTypeKind::Double |
                CTypeKind::LongDouble | CTypeKind::Int128 | CTypeKind::UInt128 |
                CTypeKind::TypeOfExpr(_) | CTypeKind::BuiltinFn | CTypeKind::Half => {}

                // Types with CTypeId fields
                CTypeKind::Complex(type_id) | CTypeKind::Paren(type_id) |
                CTypeKind::ConstantArray(type_id, _) | CTypeKind::Elaborated(type_id) |
                CTypeKind::TypeOf(type_id) | CTypeKind::Decayed(type_id) |
                CTypeKind::IncompleteArray(type_id) | CTypeKind::VariableArray(type_id, _) =>
                    type_queue.push(type_id),

                // Types with CQualtypeId fields
                CTypeKind::Pointer(qtype_id) | CTypeKind::Attributed(qtype_id, _) |
                CTypeKind::BlockPointer(qtype_id) | CTypeKind::Vector(qtype_id) =>
                    type_queue.push(qtype_id.ctype),

                CTypeKind::Function(qtype_id, ref qtype_ids, _, _) => {
                    type_queue.push(qtype_id.ctype);
                    type_queue.extend(qtype_ids.iter().map(|x| x.ctype));
                }

                CTypeKind::Typedef(decl_id) => {
                    if live.insert(decl_id) {
                        if let CDeclKind::Typedef { typ, .. } = self[decl_id].kind {
                            type_queue.push(typ.ctype);
                        }
                    }
                }

                CTypeKind::Struct(decl_id) => {
                    if live.insert(decl_id) {
                        if let CDeclKind::Struct { fields: Some(ref arr), .. } = self[decl_id].kind {
                            live.extend(arr);
                            for &field_id in arr {
                                if let CDeclKind::Field { typ, .. } = self[field_id].kind {
                                    type_queue.push(typ.ctype)
                                }
                            }
                        }
                    }
                }

                CTypeKind::Union(decl_id) => {
                    if live.insert(decl_id) {
                        if let CDeclKind::Union { fields: Some(ref arr), .. } = self[decl_id].kind {
                            live.extend(arr);
                            for &field_id in arr {
                                if let CDeclKind::Field { typ, .. } = self[field_id].kind {
                                    type_queue.push(typ.ctype)
                                }
                            }
                        }
                    }
                }

                CTypeKind::Enum(decl_id) => {
                    if live.insert(decl_id) {
                        if let CDeclKind::Enum { variants: ref arr, .. } = self[decl_id].kind {
                            live.extend(arr);
                        }
                    }
                }
            }
        }

        // Prune out any declaration that isn't considered live
        self.c_decls.retain(|&decl_id, _decl|
            live.contains(&decl_id)
        );

        // Prune out top declarations that are not considered live
        self.c_decls_top.retain(|x| live.contains(x));
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
            if let Some(loc) = loc_decl.loc {
                decls.entry(loc.fileid).or_insert(vec![]).push((loc, *decl_id));
            }
        }
        decls.iter_mut().for_each(|(_, v)| v.sort());

        // Group and sort statements by file and by position
        let mut stmts: HashMap<u64, Vec<(SrcLoc, CStmtId)>> = HashMap::new();
        for (stmt_id, ref loc_stmt) in &ast_context.c_stmts {
            if let Some(loc) = loc_stmt.loc {
                stmts.entry(loc.fileid).or_insert(vec![]).push((loc, *stmt_id));
            }
        }
        stmts.iter_mut().for_each(|(_, v)| v.sort());


        let mut decl_comments: HashMap<CDeclId, Vec<String>> = HashMap::new();
        let mut stmt_comments: HashMap<CStmtId, Vec<String>> = HashMap::new();

        let empty_vec1 = &vec![];
        let empty_vec2 = &vec![];


        // Match comments to declarations and statements
        while let Some(Located { loc, kind: comment_str }) = ast_context.comments.pop() {
            if let Some(loc) = loc {
                let this_file_decls = decls.get(&loc.fileid).unwrap_or(empty_vec1);
                let this_file_stmts = stmts.get(&loc.fileid).unwrap_or(empty_vec2);

                // Find the closest declaration and statement
                let decl_ix = this_file_decls
                    .binary_search_by_key(&loc.line, |&(l,_)| l.line)
                    .unwrap_or_else(|x| x);
                let stmt_ix = this_file_stmts
                    .binary_search_by_key(&loc.line, |&(l,_)| l.line)
                    .unwrap_or_else(|x| x);

                // Prefer the one that is higher up (biasing towards declarations if there is a tie)
                match (this_file_decls.get(decl_ix), this_file_stmts.get(stmt_ix)) {
                    (Some(&(l1, d)), Some(&(l2, s))) => {
                        if l1 > l2 {
                            stmt_comments.entry(s).or_insert(vec![]).push(comment_str);
                        } else {
                            decl_comments.entry(d).or_insert(vec![]).push(comment_str);
                        }
                    }
                    (Some(&(_, d)), None) => {
                        decl_comments.entry(d).or_insert(vec![]).push(comment_str);
                    }
                    (None, Some(&(_, s))) => {
                        stmt_comments.entry(s).or_insert(vec![]).push(comment_str);
                    }
                    (None, None) => {
                        eprintln!("Didn't find a target node for the comment '{}'", comment_str);
                    },
                };
            }
        }

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
#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Copy, Clone)]
pub struct SrcLoc {
    pub fileid: u64,
    pub line: u64,
    pub column: u64,
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
        is_extern: bool,
        is_inline: bool,
        is_implicit: bool,
        typ: CFuncTypeId,
        name: String,
        parameters: Vec<CParamId>,
        body: Option<CStmtId>,
    },

    // http://clang.llvm.org/doxygen/classclang_1_1VarDecl.html
    Variable {
        is_static: bool,
        is_extern: bool,
        is_defn: bool,
        ident: String,
        initializer: Option<CExprId>,
        typ: CQualTypeId,
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
    Unary(CQualTypeId, UnOp, CExprId),

    // Unary type operator.
    UnaryType(CQualTypeId, UnTypeOp, Option<CExprId>, CQualTypeId),

    // Offsetof expression.
    OffsetOf(CQualTypeId, u64),

    // Binary operator
    Binary(CQualTypeId, BinOp, CExprId, CExprId, Option<CQualTypeId>, Option<CQualTypeId>),

    // Implicit cast
    ImplicitCast(CQualTypeId, CExprId, CastKind, Option<CFieldId>),

    // Explicit cast
    ExplicitCast(CQualTypeId, CExprId, CastKind, Option<CFieldId>),

    // Reference to a decl (a variable, for instance)
    // TODO: consider enforcing what types of declarations are allowed here
    DeclRef(CQualTypeId, CDeclId),

    // Function call
    Call(CQualTypeId, CExprId, Vec<CExprId>),

    // Member access
    Member(CQualTypeId, CExprId, CDeclId, MemberKind),

    // Array subscript access
    ArraySubscript(CQualTypeId, CExprId, CExprId),

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
    ShuffleVector(CQualTypeId),
    ConvertVector(CQualTypeId),

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
    pub fn get_qual_type(&self) -> Option<CQualTypeId> {
        match *self {
            CExprKind::BadExpr => None,
            CExprKind::Literal(ty, _) |
            CExprKind::OffsetOf(ty, _) |
            CExprKind::Unary(ty, _, _) |
            CExprKind::UnaryType(ty, _, _, _) |
            CExprKind::Binary(ty, _, _, _, _, _) |
            CExprKind::ImplicitCast(ty, _, _, _) |
            CExprKind::ExplicitCast(ty, _, _, _) |
            CExprKind::DeclRef(ty, _) |
            CExprKind::Call(ty, _, _) |
            CExprKind::Member(ty, _, _, _) |
            CExprKind::ArraySubscript(ty, _, _) |
            CExprKind::Conditional(ty, _, _, _) |
            CExprKind::BinaryConditional(ty, _, _) |
            CExprKind::InitList(ty, _, _, _) |
            CExprKind::ImplicitValueInit(ty) |
            CExprKind::CompoundLiteral(ty, _) |
            CExprKind::Predefined(ty, _) |
            CExprKind::Statements(ty, _) |
            CExprKind::VAArg(ty, _) |
            CExprKind::ShuffleVector(ty) |
            CExprKind::ConvertVector(ty) |
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

#[derive(Debug, Clone, Copy)]
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
#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone)]
pub enum CLiteral {
    Integer(u64),
    Character(u64),
    Floating(f64),
    String(Vec<u8>, u8), // Literal bytes and unit byte width
}

impl CLiteral {
    /// Determine the truthiness or falsiness of the literal.
    pub fn get_bool(&self) -> bool {
        match *self{
            CLiteral::Integer(x) => x != 0u64,
            CLiteral::Character(x) => x != 0u64,
            CLiteral::Floating(x) => x != 0f64,
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
#[derive(Debug, Copy, Clone, Default)]
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

impl PartialEq for Qualifiers {
    fn eq(&self, other: &Qualifiers) -> bool {
        self.is_const == other.is_const &&
        self.is_restrict == other.is_restrict &&
        self.is_volatile == other.is_volatile
    }
}

/// Qualified type
#[derive(Debug, Copy, Clone)]
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
#[derive(Debug, Clone)]
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
    // Flags: is_variable_argument, is_noreturn
    Function(CQualTypeId, Vec<CQualTypeId>, bool, bool),

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

    Vector(CQualTypeId),

    Half,
}

#[derive(Copy, Clone, Debug)]
pub enum Designator {
    Index(u64),
    Range(u64,u64),
    Field(CFieldId),
}

#[derive(Copy, Clone, Debug)]
pub enum Attribute {
    NoReturn,
    NotNull,
    Nullable,
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
}
