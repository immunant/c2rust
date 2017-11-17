use std::collections::HashMap;
use std::ops::Index;

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Copy, Clone)]
pub struct CTypeId(u64);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Copy, Clone)]
pub struct CExprId(u64);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Copy, Clone)]
pub struct CDeclId(u64);

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Copy, Clone)]
pub struct CStmtId(u64);

// These are references into particular variants of AST nodes
pub type CLabelId = CStmtId;  // Labels point into the 'StmtKind::Label' that declared the label
pub type CFieldId = CDeclId;  // Records always contain 'DeclKind::Field's
pub type CParamId = CDeclId;  // Parameters always contain 'DeclKind::Variable's
pub type CFuncTypeId = CTypeId;  // Function declarations always have types which are 'TypeKind::Function'
pub type CRecordId = CDeclId;  // Record types need to point to 'DeclKind::Record'
pub type CTypedefId = CDeclId;  // Typedef types need to point to 'DeclKind::Typedef'

pub use self::conversion::*;
pub use self::print::Printer;

mod conversion;
mod print;

/// AST context containing all of the nodes in the Clang AST
#[derive(Debug, Clone)]
pub struct TypedAstContext {
    pub c_types: HashMap<CTypeId, CType>,
    pub c_exprs: HashMap<CExprId, CExpr>,
    pub c_decls: HashMap<CDeclId, CDecl>,
    pub c_stmts: HashMap<CStmtId, CStmt>,

    pub c_decls_top: Vec<CDeclId>,
    pub c_files: HashMap<u64, String>,
}

impl TypedAstContext {
    pub fn new() -> TypedAstContext {
        TypedAstContext {
            c_types: HashMap::new(),
            c_exprs: HashMap::new(),
            c_decls: HashMap::new(),
            c_stmts: HashMap::new(),

            c_decls_top: Vec::new(),
            c_files: HashMap::new(),
        }
    }

    pub fn resolve_type_id(&self, typ: CTypeId) -> CTypeId {
        match self.index(typ).kind {
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

            CExprKind::Call(_, _, _) => false,
            CExprKind::Literal(_, _) => true,
            CExprKind::DeclRef(_, _) => true,

            CExprKind::ImplicitCast(_, e, _) => self.is_expr_pure(e),
            CExprKind::ExplicitCast(_, e, _) => self.is_expr_pure(e),
            CExprKind::Member(_, e, _, _) => self.is_expr_pure(e),

            CExprKind::Unary(_, UnOp::PreIncrement, _) => false,
            CExprKind::Unary(_, UnOp::PostIncrement, _) => false,
            CExprKind::Unary(_, UnOp::PreDecrement, _) => false,
            CExprKind::Unary(_, UnOp::PostDecrement, _) => false,
            CExprKind::Unary(_, _, e) => self.is_expr_pure(e),
            CExprKind::UnaryType(_, _, _) => true,

            CExprKind::Binary(_, BinOp::Assign, _, _) => false,
            CExprKind::Binary(_, op, _, _) if op.underlying_assignment().is_some() => false,
            CExprKind::Binary(_, _, lhs, rhs) => self.is_expr_pure(lhs) && self.is_expr_pure(rhs),

            CExprKind::ArraySubscript(_, lhs, rhs) => self.is_expr_pure(lhs) && self.is_expr_pure(rhs),
            CExprKind::Conditional(_, c, lhs, rhs) => self.is_expr_pure(c) && self.is_expr_pure(lhs) && self.is_expr_pure(rhs),

            CExprKind::InitList{..} => false,
            CExprKind::ImplicitValueInit{..} => false,
            CExprKind::CompoundLiteral(_, val) => self.is_expr_pure(val),
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
        match self.c_exprs.get(&index) {
            None => panic!("Could not find {:?} in TypedAstContext", index),
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
#[derive(Debug, Copy, Clone)]
pub struct SrcLoc {
    pub line: u64,
    pub column: u64,
    pub fileid: u64,
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
        typ: CFuncTypeId,
        name: String,
        parameters: Vec<CParamId>,
        body: Option<CStmtId>,
    },

    // http://clang.llvm.org/doxygen/classclang_1_1VarDecl.html
    Variable {
        is_static: bool,
        is_extern: bool,
        ident: String,
        initializer: Option<CExprId>,
        typ: CQualTypeId,
    },

    // Enum       // http://clang.llvm.org/doxygen/classclang_1_1EnumDecl.html

    // Typedef
    Typedef {
        name: String,
        typ: CQualTypeId,
    },

    // Struct
    Struct {
        name: Option<String>,
        fields: Vec<CFieldId>,
    },

    // Union
    Union {
        name: Option<String>,
        fields: Vec<CFieldId>,
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
//            &CDeclKind::Record { ref name, fields } => ???,
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
    UnaryType(CQualTypeId, UnTypeOp, CQualTypeId),

    // Binary operator
    Binary(CQualTypeId, BinOp, CExprId, CExprId),

    // Implicit cast
    ImplicitCast(CQualTypeId, CExprId, CastKind),

    // Explicit cast
    ExplicitCast(CQualTypeId, CExprId, CastKind),

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

    // Initializer list
    InitList(CQualTypeId, Vec<CExprId>),

    // Designated initializer
    ImplicitValueInit(CQualTypeId),

    // Compound literal
    CompoundLiteral(CQualTypeId, CExprId),
}

#[derive(Copy, Debug, Clone)]
pub enum MemberKind {
    Arrow,
    Dot,
}

impl CExprKind {
    pub fn get_qual_type(&self) -> CQualTypeId {
        match *self {
            CExprKind::Literal(ty, _) => ty,
            CExprKind::Unary(ty, _, _) => ty,
            CExprKind::UnaryType(ty, _, _) => ty,
            CExprKind::Binary(ty, _, _, _) => ty,
            CExprKind::ImplicitCast(ty, _, _) => ty,
            CExprKind::ExplicitCast(ty, _, _) => ty,
            CExprKind::DeclRef(ty, _) => ty,
            CExprKind::Call(ty, _, _) => ty,
            CExprKind::Member(ty, _, _, _) => ty,
            CExprKind::ArraySubscript(ty, _, _) => ty,
            CExprKind::Conditional(ty, _, _, _) => ty,
            CExprKind::InitList(ty, _) => ty,
            CExprKind::ImplicitValueInit(ty) => ty,
            CExprKind::CompoundLiteral(ty, _) => ty,
        }
    }

    pub fn get_type(&self) -> CTypeId {
        self.get_qual_type().ctype
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
}

/// Represents a unary operator in C (6.5.3 Unary operators)
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

/// Represents a statement in C (6.8 Statements)
///
/// Reflects the types in <http://clang.llvm.org/doxygen/classclang_1_1Stmt.html>
#[derive(Debug, Clone)]
pub enum CStmtKind {
    // Labeled statements (6.8.1)
    Label(CStmtId),
    Case(CExpr, CStmtId), // The second argument is only the immediately following statement
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
    //
    // A qualifier on an array type means the same thing as a qualifier on its element type. Since
    // Clang tracks the qualifiers in both places, we choose to discard qualifiers on the element
    // type.
    ConstantArray(CTypeId, usize),
    IncompleteArray(CTypeId),
    VariableArray(CTypeId, CExprId),

    // Type of type or expression (GCC extension)
    TypeOf(CTypeId),
    TypeOfExpr(CExprId),

    // Function type (6.7.5.3)
    //
    // Note a function taking no arguments should have one `void` argument. Functions without any
    // arguments and in K&R format.
    Function(CQualTypeId, Vec<CQualTypeId>),

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

    Enum(CDeclId),    // TODO same comment as Typedef
}

impl CTypeKind {

    pub fn is_pointer(&self) -> bool {
        match *self {
            CTypeKind::Pointer{..} => true,
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
            _ => false,
        }
    }

    pub fn is_signed_integral_type(&self) -> bool {
        match *self {
            CTypeKind::SChar => true,
            CTypeKind::Int => true,
            CTypeKind::Short => true,
            CTypeKind::Long => true,
            CTypeKind::LongLong => true,
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
}
