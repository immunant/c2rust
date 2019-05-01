use crate::c_ast::*;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum SomeId {
    Stmt(CStmtId),
    Expr(CExprId),
    Decl(CDeclId),
    Type(CTypeId),
}

macro_rules! from_some_id {
    ( $field_type:ty, $con_name:ident, $proj_name:ident ) => {
        impl From<$field_type> for SomeId {
            fn from(a: $field_type) -> Self {
                SomeId::$con_name(a)
            }
        }
        impl SomeId {
            pub fn $proj_name(self) -> Option<$field_type> {
                match self {
                    SomeId::$con_name(x) => Some(x),
                    _ => None,
                }
            }
        }
    };
}

from_some_id!(CExprId, Expr, expr);
from_some_id!(CStmtId, Stmt, stmt);
from_some_id!(CDeclId, Decl, decl);
from_some_id!(CTypeId, Type, type_);

/// Like the vec macro except that it calls the into method on all list elements
macro_rules! intos {
    ( $( $x:expr ),* ) => { vec![ $( $x.into(), )* ] };
}

fn immediate_expr_children(kind: &CExprKind) -> Vec<SomeId> {
    use crate::c_ast::CExprKind::*;
    match *kind {
        BadExpr => vec![],
        DesignatedInitExpr(..) => vec![], // the relevant information will be found in the semantic initializer
        ShuffleVector(..) | ConvertVector(..) => vec![],
        OffsetOf(..) | Literal(..) | ImplicitValueInit(..) => vec![],
        DeclRef(..) => vec![], // don't follow references back!
        Unary(_ty, _op, subexpr, _) => intos![subexpr],
        UnaryType(_ty, _op, opt_expr_id, _) => opt_expr_id.iter().map(|&x| x.into()).collect(),
        Binary(_ty, _op, lhs, rhs, _, _) => intos![lhs, rhs],
        Call(_, f, ref args) => {
            let mut res = intos![f];
            for &a in args {
                res.push(a.into())
            }
            res
        }
        ArraySubscript(_, l, r, _) => intos![l, r],
        Conditional(_, c, t, e)
        | Choose(_, c, t, e, _) => intos![c, t, e],
        BinaryConditional(_, c, t) => intos![c, t],
        InitList(_, ref xs, _, _) => xs.iter().map(|&x| x.into()).collect(),
        ImplicitCast(_, e, _, _, _)
        | ExplicitCast(_, e, _, _, _)
        | Member(_, e, _, _, _)
        | Paren(_, e)
        | CompoundLiteral(_, e)
        | Predefined(_, e)
        | VAArg(_, e) => intos![e],
        Statements(_, s) => vec![s.into()],
    }
}

fn immediate_expr_children_all_types(kind: &CExprKind) -> Vec<SomeId> {
    use crate::c_ast::CExprKind::*;
    match *kind {
        BadExpr => vec![],
        DesignatedInitExpr(..) => vec![], // the relevant information will be found in the semantic initializer
        ShuffleVector(_, ref kids) | ConvertVector(_, ref kids) => {
            kids.iter().map(|&x| x.into()).collect()
        }
        OffsetOf(..) | Literal(..) | ImplicitValueInit(..) => vec![],
        DeclRef(..) => vec![], // don't follow references back!
        Unary(_ty, _op, subexpr, _) => intos![subexpr],
        UnaryType(_ty, _op, opt_expr_id, qty) => {
            let mut res = intos![qty.ctype];
            if let Some(expr_id) = opt_expr_id {
                res.push(expr_id.into());
            }
            res
        }
        Binary(_ty, _op, lhs, rhs, _, _) => intos![lhs, rhs],
        Call(_, f, ref args) => {
            let mut res = intos![f];
            for &a in args {
                res.push(a.into())
            }
            res
        }
        ArraySubscript(_, l, r, _) => intos![l, r],
        Conditional(_, c, t, e)
        | Choose(_, c, t, e, _) => intos![c, t, e],
        BinaryConditional(_, c, t) => intos![c, t],
        InitList(_, ref xs, _, _) => xs.iter().map(|&x| x.into()).collect(),
        Member(_, e, _, _, _) | Predefined(_, e) => intos![e],
        // Normally we don't step into the result type annotation field, because it's not really
        // part of the expression.  But for `ExplicitCast`, the result type is actually the cast's
        // target type as written by the user.  The other expr kinds here work similarly.
        ExplicitCast(qty, e, _, _, _)
        | ImplicitCast(qty, e, _, _, _)
        | Paren(qty, e)
        | CompoundLiteral(qty, e)
        | VAArg(qty, e) => {
            intos![qty.ctype, e]
        }
        Statements(_, s) => vec![s.into()],
    }
}

fn immediate_decl_children(kind: &CDeclKind) -> Vec<SomeId> {
    use crate::c_ast::CDeclKind::*;
    match *kind {
        Function {
            typ,
            ref parameters,
            body,
            ..
        } => {
            let mut res = intos![typ];
            res.extend(parameters.iter().map(|&x| -> SomeId { x.into() }));
            res.extend(body.iter().map(|&x| -> SomeId { x.into() }));
            res
        }
        Variable {
            typ, initializer, ..
        } => {
            let mut res = intos![typ.ctype];
            for x in initializer {
                res.push(x.into())
            }
            res
        }
        Enum {
            ref variants,
            integral_type,
            ..
        } => {
            let mut res: Vec<SomeId> = variants.iter().map(|&x| x.into()).collect();
            if let Some(qty) = integral_type {
                res.push(qty.ctype.into());
            }
            res
        }
        EnumConstant { .. } => vec![],
        Typedef { typ, .. } => intos![typ.ctype],
        Struct { ref fields, .. } => fields.iter().flat_map(|x| x).map(|&x| x.into()).collect(),
        Union { ref fields, .. } => fields.iter().flat_map(|x| x).map(|&x| x.into()).collect(),
        Field { typ, .. } => intos![typ.ctype],
        MacroObject {
            ref replacements, ..
        } => replacements.iter().map(|&x| x.into()).collect(),
    }
}

fn immediate_stmt_children(kind: &CStmtKind) -> Vec<SomeId> {
    use crate::c_ast::CStmtKind::*;
    match *kind {
        Expr(e) => intos![e],
        Label(s) => intos![s],
        Case(e, s, _) => intos![e, s],
        Default(s) => intos![s],

        // Compound statements (6.8.2)
        Compound(ref xs) => xs.iter().map(|&x| x.into()).collect(),
        Empty => vec![],
        If {
            scrutinee: e,
            true_variant: s,
            false_variant: opt_s,
        } => {
            let mut res = intos![e, s];
            for &x in &opt_s {
                res.push(x.into())
            }
            res
        }
        Switch {
            scrutinee: e,
            body: s,
        } => intos![e, s],

        While {
            condition: e,
            body: s,
        } => intos![e, s],
        DoWhile {
            body: s,
            condition: e,
        } => intos![s, e],
        ForLoop {
            init: a,
            condition: b,
            increment: c,
            body: d,
        } => {
            let mut res = vec![];
            for &x in &a {
                res.push(x.into())
            }
            for &x in &b {
                res.push(x.into())
            }
            for &x in &c {
                res.push(x.into())
            }
            res.push(d.into());
            res
        }
        Goto(_) => vec![], // Don't follow the reference to the label
        Break => vec![],
        Continue => vec![],
        Return(ref opt_e) => opt_e.iter().map(|&x| x.into()).collect(),

        Decls(ref decl_ids) => decl_ids.iter().map(|&x| x.into()).collect(),

        Asm {
            ref inputs,
            ref outputs,
            ..
        } => {
            let mut res = vec![];
            for list in vec![inputs, outputs] {
                for elt in list {
                    res.push(elt.expression.into())
                }
            }
            res
        }
    }
}

fn immediate_type_children(kind: &CTypeKind) -> Vec<SomeId> {
    use crate::c_ast::CTypeKind::*;
    match *kind {
        Elaborated(_) => vec![], // These are references to previous definitions
        TypeOfExpr(e) => intos![e],
        Void | Bool | Short | Int | Long | LongLong | UShort | UInt | ULong | ULongLong | SChar
        | UChar | Char | Double | LongDouble | Float | Int128 | UInt128 | BuiltinFn | Half => {
            vec![]
        }

        Pointer(qtype) | Attributed(qtype, _) | BlockPointer(qtype) | Vector(qtype, _) => {
            intos![qtype.ctype]
        }

        Decayed(ctype)
        | Paren(ctype)
        | TypeOf(ctype)
        | Complex(ctype)
        | ConstantArray(ctype, _)
        | IncompleteArray(ctype) => intos![ctype],

        Struct(decl_id) | Union(decl_id) | Enum(decl_id) | Typedef(decl_id) => intos![decl_id],

        VariableArray(elt, cnt) => {
            let mut res = intos![elt];
            for x in cnt {
                res.push(x.into())
            }
            res
        }
        Function(ret, ref params, _, _, _) => {
            let mut res = intos![ret.ctype];
            for p in params {
                res.push(p.ctype.into())
            }
            res
        }
    }
}

fn immediate_children(context: &TypedAstContext, s_or_e: SomeId) -> Vec<SomeId> {
    match s_or_e {
        SomeId::Stmt(stmt_id) => immediate_stmt_children(&context[stmt_id].kind),
        SomeId::Expr(expr_id) => immediate_expr_children(&context[expr_id].kind),
        SomeId::Decl(decl_id) => immediate_decl_children(&context[decl_id].kind),
        SomeId::Type(type_id) => immediate_type_children(&context[type_id].kind),
    }
}

fn immediate_children_all_types(context: &TypedAstContext, s_or_e: SomeId) -> Vec<SomeId> {
    match s_or_e {
        SomeId::Stmt(stmt_id) => immediate_stmt_children(&context[stmt_id].kind),
        SomeId::Expr(expr_id) => immediate_expr_children_all_types(&context[expr_id].kind),
        SomeId::Decl(decl_id) => immediate_decl_children(&context[decl_id].kind),
        SomeId::Type(type_id) => immediate_type_children(&context[type_id].kind),
    }
}

pub struct DFExpr<'context> {
    context: &'context TypedAstContext,
    stack: Vec<SomeId>,
}

impl<'context> DFExpr<'context> {
    pub fn new(context: &'context TypedAstContext, start: SomeId) -> Self {
        DFExpr {
            context,
            stack: vec![start],
        }
    }
    pub fn prune(&mut self, n: usize) {
        let new_len = self.stack.len() - n;
        self.stack.truncate(new_len)
    }
}

impl<'context> Iterator for DFExpr<'context> {
    type Item = SomeId;
    fn next(&mut self) -> Option<Self::Item> {
        let result = self.stack.pop();

        if let Some(i) = result {
            // Compute list of immediate children
            let children = immediate_children(self.context, i);
            // Add children in reverse order since we visit the end of the stack first
            self.stack.extend(children.into_iter().rev())
        }

        result
    }
}

/// Depth-first traversal of all AST nodes.  After visiting each node, iteration proceeds to nodes
/// that are "contained in" that node.  For example, after visiting a `CExprKind::Binary`, it will
/// visit the LHS and RHS expression nodes, but it will not visit the LHS, RHS, and result type
/// nodes that are also referenced from the `Binary` expression.
pub struct DFNodes<'context> {
    context: &'context TypedAstContext,
    stack: Vec<SomeId>,
}

impl<'context> DFNodes<'context> {
    pub fn new(context: &'context TypedAstContext, start: SomeId) -> Self {
        DFNodes {
            context,
            stack: vec![start],
        }
    }
    pub fn prune(&mut self, n: usize) {
        let new_len = self.stack.len() - n;
        self.stack.truncate(new_len)
    }
}

impl<'context> Iterator for DFNodes<'context> {
    type Item = SomeId;
    fn next(&mut self) -> Option<Self::Item> {
        let result = self.stack.pop();

        if let Some(i) = result {
            // Compute list of immediate children
            let children = immediate_children_all_types(self.context, i);
            // Add children in reverse order since we visit the end of the stack first
            self.stack.extend(children.into_iter().rev())
        }

        result
    }
}
