use crate::c_ast::c_expr::{CExprId, ConstIntExpr};
use crate::c_ast::{Attribute, CDeclId, Located, TypedAstContext};
use std::fmt::Debug;
use std::ops::Index;

#[derive(Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Copy, Clone)]
pub struct CStmtId(pub u64);

// These are references into particular variants of AST nodes
pub type CLabelId = CStmtId; // Labels point into the 'StmtKind::Label' that declared the label

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

pub type CStmt = Located<CStmtKind>;

#[derive(Clone, Debug)]
pub struct AsmOperand {
    pub constraints: String,
    pub expression: CExprId,
}

impl TypedAstContext {
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
