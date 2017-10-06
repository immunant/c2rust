use std::collections::HashMap;

// In order to avoid lifetime hell and mirror as closely as possible what we get from Clang, we
// store references to AST nodes in HashMap's in TypedAstContext.
pub type CTypeId = u64;
pub type CExprId = u64;
pub type CDeclId = u64;
pub type CStmtId = u64;

// These are references into particular variants of AST nodes
pub type CLabelId = CStmtId;  // Labels point into the 'StmtKind::Label' that declared the label
pub type CStmtOrDeclId = u64; // Points to either a stmt or a decl

/// AST context containing all of the nodes in the Clang AST
#[derive(Debug)]
pub struct TypedAstContext {
  pub c_types: HashMap<CTypeId, CType>,
  pub c_exprs: HashMap<CExprId, CExpr>,
  pub c_decls: HashMap<CDeclId, CDecl>,
  pub c_stmts: HashMap<CStmtId, CStmt>,
  
  pub c_files: HashMap<u64, String>,
}

impl TypedAstContext {
  pub fn new() -> TypedAstContext {
    TypedAstContext {
      c_types: HashMap::new(),
      c_exprs: HashMap::new(),
      c_decls: HashMap::new(),
      c_stmts: HashMap::new(),
      
      c_files: HashMap::new(),
    }
  }
}

/// Represents a position inside a C source file
#[derive(Debug,Copy,Clone)]
pub struct SrcLoc {
  pub line: u64,
  pub column: u64,
  pub fileid: u64,
}

/// Represents some AST node possibly with source location information bundled with it
#[derive(Debug)]
pub struct Located<T> {
  pub loc: Option<SrcLoc>,
  pub kind: T,
}

/// All of our AST types should have location information bundled with them
type CDecl = Located<CDeclKind>;
type CStmt = Located<CStmtKind>;
type CExpr = Located<CExprKind>;
type CType = Located<CTypeKind>;


// TODO:
//
#[derive(Debug)]
pub enum CDeclKind {
  // http://clang.llvm.org/doxygen/classclang_1_1FunctionDecl.html
  Function {
    /* TODO: Parameters,*/
    typ: CTypeId,
    name: String,
    body: CStmtId,
  }
  
  // Enum       // http://clang.llvm.org/doxygen/classclang_1_1EnumDecl.html
  // Variable    // http://clang.llvm.org/doxygen/classclang_1_1VarDecl.html
  // Typedef     // http://clang.llvm.org/doxygen/classclang_1_1TypedefNameDecl.html
  // Record
}

// TODO:
//
/// Represents an expression in C (6.5 Expressions)
#[derive(Debug)]
pub enum CExprKind {
}


/// Represents a statement in C (6.8 Statements)
///
/// Reflects the types in <http://clang.llvm.org/doxygen/classclang_1_1Stmt.html>
#[derive(Debug)]
pub enum CStmtKind {
  // Labeled statements (6.8.1)
  Label(CStmtId),
  Case(CExpr, CStmtId), // The second argument is only the immediately following statement
  Default(CStmtId),

  // Compound statements (6.8.2)
  Compound(Vec<CStmtOrDeclId>),
  
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
    init: CStmtId,      // This can be an 'Expr'
    condition: CExprId,
    increment: CExprId,
    body: CStmtId,
  },

  // Jump statements (6.8.6)
  Goto(CLabelId),
  Break,
  Continue,
  Return(Option<CExprId>),
}

/// Type qualifiers (6.7.3)
#[derive(Debug)]
pub struct Qualifiers {
  pub is_const: bool,
  pub is_restrict: bool,
  pub is_volatile: bool,
}

/// Qualified type
#[derive(Debug)]
pub struct CQualTypeId {
  pub qualifiers: Qualifiers,
  pub ctype: CTypeId,
}


// TODO: these may be interesting, but I'm not sure if they fit here:
//  
//  * ElaboratedType <http://clang.llvm.org/doxygen/classclang_1_1ElaboratedType.html>
//  * UnaryTranformType <http://clang.llvm.org/doxygen/classclang_1_1UnaryTransformType.html>
//  * AdjustedType <http://clang.llvm.org/doxygen/classclang_1_1AdjustedType.html>

/// Represents a type in C (6.2.5 Types)
///
/// Reflects the types in <http://clang.llvm.org/doxygen/classclang_1_1Type.html>
#[derive(Debug)]
pub enum CTypeKind {
  /* Builtin types: <https://github.com/llvm-mirror/clang/include/clang/AST/BuiltinTypes.def> */

  // Void type (6.2.5.19)
  Void,
  
  // Boolean type (6.2.5.2)
  Bool,
  
  Size,
  
  // Character type (6.2.5.3)
  Char,
  
  // Signed types (6.2.5.4)
  SChar, Short, Int, Long, LongLong,
  
  // Unsigned types (6.2.5.6) (actually this also includes `_Bool`)
  UChar, UShort, UInt, ULong, ULongLong,

  // Real floating types (6.2.5.10). Ex: `double`
  Float, Double, LongDouble,

  
  /* Compound types <https://github.com/llvm-mirror/clang/include/clang/AST/TypeNodes.def> */ 
  
  // Complex types (6.2.5.11). Ex: `float _Complex`.
  Complex(CTypeId),

  // Pointer types (6.7.5.1)
  Pointer(CQualTypeId),

  // Array types (6.7.5.2)
  ConstantArray(CQualTypeId, usize),
  IncompleteArray(CQualTypeId),
  VariableArray(CQualTypeId, CExprId),

  // Type of type or expression (GCC extension)
  TypeOf(CQualTypeId),
  TypeOfExpr(CExprId),

  // K&R stype function type (6.7.5.3). Ex: `int foo()`.
  FunctionNoProto(CQualTypeId),

  // Function type always with arguments (6.7.5.3). Ex: `int bar(void)`
  FunctionProto(CQualTypeId, Vec<CQualTypeId>),

  // Type definition type (6.7.7)
  Typedef(CDeclId),     // TODO make a type synonym for this: not all decls could be the target of a typedef

  // Represents a pointer type decayed from an array or function type.
  Decayed(CQualTypeId, CQualTypeId),

  // Struct or union type
  RecordType(CDeclId),  // TODO same comment as Typedef
  EnumType(CDeclId),    // TODO same comment as Typedef
}


