use crate::c_ast::c_decl::{CDeclId, CDeclKind, CFieldId};
use crate::c_ast::c_stmt::CStmtId;
use crate::c_ast::c_type::{CQualTypeId, CTypeId, CTypeKind};
use crate::c_ast::{Located, TypedAstContext};
use c2rust_ast_exporter::clang_ast::LRValue;
use std::fmt::{self, Debug, Display};
use std::ops::Index;

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Copy, Clone)]
pub struct CExprId(pub u64);

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
    Unary(CQualTypeId, CUnOp, CExprId, LRValue),

    /// Unary type operator.
    UnaryType(CQualTypeId, CUnTypeOp, Option<CExprId>, CQualTypeId),

    /// `offsetof` expression.
    OffsetOf(CQualTypeId, OffsetOfKind),

    /// Binary operator.
    Binary(
        CQualTypeId,
        CBinOp,
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

pub type CExpr = Located<CExprKind>;

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

#[derive(Copy, Debug, Clone)]
pub enum MemberKind {
    Arrow,
    Dot,
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

impl CastKind {
    pub fn from_types(source_ty_kind: &CTypeKind, target_ty_kind: &CTypeKind) -> Option<Self> {
        Some(match (source_ty_kind, target_ty_kind) {
            (CTypeKind::VariableArray(..), CTypeKind::Pointer(..))
            | (CTypeKind::ConstantArray(..), CTypeKind::Pointer(..))
            | (CTypeKind::IncompleteArray(..), CTypeKind::Pointer(..)) => {
                CastKind::ArrayToPointerDecay
            }

            (CTypeKind::Function(..), CTypeKind::Pointer(..)) => CastKind::FunctionToPointerDecay,

            (_, CTypeKind::Pointer(..)) if source_ty_kind.is_integral_type() => {
                CastKind::IntegralToPointer
            }

            (CTypeKind::Pointer(..), CTypeKind::Bool) => CastKind::PointerToBoolean,

            (CTypeKind::Pointer(..), _) if target_ty_kind.is_integral_type() => {
                CastKind::PointerToIntegral
            }

            (_, CTypeKind::Bool) if source_ty_kind.is_integral_type() => {
                CastKind::IntegralToBoolean
            }

            (CTypeKind::Bool, _) if target_ty_kind.is_signed_integral_type() => {
                CastKind::BooleanToSignedIntegral
            }

            (_, _) if source_ty_kind.is_integral_type() && target_ty_kind.is_integral_type() => {
                CastKind::IntegralCast
            }

            (_, _) if source_ty_kind.is_integral_type() && target_ty_kind.is_floating_type() => {
                CastKind::IntegralToFloating
            }

            (_, CTypeKind::Bool) if source_ty_kind.is_floating_type() => {
                CastKind::FloatingToBoolean
            }

            (_, _) if source_ty_kind.is_floating_type() && target_ty_kind.is_integral_type() => {
                CastKind::FloatingToIntegral
            }

            (_, _) if source_ty_kind.is_floating_type() && target_ty_kind.is_floating_type() => {
                CastKind::FloatingCast
            }

            (CTypeKind::Pointer(..), CTypeKind::Pointer(..)) => CastKind::BitCast,

            // Ignoring Complex casts for now
            _ => return None,
        })
    }
}

/// Represents a unary operator in C (6.5.3 Unary operators) and GNU C extensions
#[derive(Debug, Clone, Copy)]
pub enum CUnOp {
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

impl CUnOp {
    pub fn as_str(&self) -> &'static str {
        use CUnOp::*;
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
        use CUnOp::*;
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

impl Display for CUnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Represents a unary type operator in C
#[derive(Debug, Clone, Copy)]
pub enum CUnTypeOp {
    SizeOf,
    AlignOf,
    PreferredAlignOf,
}

impl CUnTypeOp {
    pub fn as_str(&self) -> &'static str {
        use CUnTypeOp::*;
        match self {
            SizeOf => "sizeof",
            AlignOf => "alignof",
            PreferredAlignOf => "__alignof",
        }
    }
}

impl Display for CUnTypeOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl CUnOp {
    /// Check if the operator is rendered before or after its operand.
    pub fn is_prefix(&self) -> bool {
        !matches!(*self, CUnOp::PostIncrement | CUnOp::PostDecrement)
    }
}

/// Represents a binary operator in C (6.5.5 Multiplicative operators - 6.5.14 Logical OR operator)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CBinOp {
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

impl CBinOp {
    pub fn as_str(&self) -> &'static str {
        use CBinOp::*;
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
        use CBinOp::*;
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
        use CBinOp::*;
        matches!(
            self,
            Multiply | Divide | Modulus | Add | Subtract | BitAnd | BitXor | BitOr
        )
    }
}

impl Display for CBinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl CBinOp {
    /// Maps compound assignment operators to operator underlying them, and returns `None` for all
    /// other operators.
    ///
    /// For example, `AssignAdd` maps to `Some(Add)` but `Add` maps to `None`.
    pub fn underlying_assignment(&self) -> Option<CBinOp> {
        use CBinOp::*;
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

#[derive(Copy, Clone, Debug)]
pub enum Designator {
    Index(u64),
    Range(u64, u64),
    Field(CFieldId),
}

impl TypedAstContext {
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

    /// Returns the expression inside any number of nested parentheses.
    pub fn resolve_parens(&self, mut expr_id: CExprId) -> CExprId {
        while let CExprKind::Paren(_, subexpr) = self.index(expr_id).kind {
            expr_id = subexpr;
        }

        expr_id
    }

    /// Returns the expression inside an `__extension__` operator.
    pub fn resolve_extension(&self, expr_id: CExprId) -> CExprId {
        if let CExprKind::Unary(_, CUnOp::Extension, subexpr, _) = self.index(expr_id).kind {
            subexpr
        } else {
            expr_id
        }
    }

    /// Unwraps a predefined expression, if there is one.
    pub fn unwrap_predefined_ident(&self, mut expr_id: CExprId) -> CExprId {
        expr_id = self.resolve_extension(self.resolve_parens(expr_id));

        if let CExprKind::Predefined(_, subexpr) = self.index(expr_id).kind {
            subexpr
        } else {
            expr_id
        }
    }

    /// Unwraps the underlying expression beneath any casts.
    pub fn unwrap_cast_expr(&self, mut expr_id: CExprId) -> CExprId {
        while let CExprKind::Paren(_, subexpr)
        | CExprKind::ImplicitCast(_, subexpr, _, _, _)
        | CExprKind::ExplicitCast(_, subexpr, _, _, _) = self.index(expr_id).kind
        {
            expr_id = subexpr;
        }

        expr_id
    }

    /// Unwraps the underlying expression beneath any implicit casts.
    pub fn unwrap_implicit_cast_expr(&self, mut expr_id: CExprId) -> CExprId {
        while let CExprKind::ImplicitCast(_, subexpr, _, _, _) = self.index(expr_id).kind {
            expr_id = subexpr;
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
            Unary(_, CUnOp::PreIncrement, _, _) |
            Unary(_, CUnOp::PostIncrement, _, _) |
            Unary(_, CUnOp::PreDecrement, _, _) |
            Unary(_, CUnOp::PostDecrement, _, _) |
            Binary(_, CBinOp::Assign, _, _, _, _) |
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
            // `ptr::offset` (ptr `CBinOp::Add`) was `const` stabilized in `1.61.0`.
            // `ptr::offset_from` (ptr `CBinOp::Subtract`) was `const` stabilized in `1.65.0`.
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
