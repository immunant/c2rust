use c_ast::*;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum SomeId {
    Stmt(CStmtId),
    Expr(CExprId),
    Decl(CDeclId),
    Type(CTypeId),
}

macro_rules! from_some_id {
    ( $x:ty, $y:ident ) => {
        impl From<$x> for SomeId {
            fn from(a: $x) -> Self { SomeId::$y(a) }
        }
    };
}

from_some_id!(CExprId, Expr);
from_some_id!(CStmtId, Stmt);
from_some_id!(CDeclId, Decl);
from_some_id!(CTypeId, Type);

/// Like the vec macro except that it calls the into method on all list elements
macro_rules! intos {
    ( $( $x:expr ),* ) => { vec![ $( $x.into(), )* ] };
}

pub struct DFExpr<'context> {
    context: &'context TypedAstContext,
    stack: Vec<SomeId>,
}

impl<'context> DFExpr<'context> {
    pub fn new(context: &'context TypedAstContext, start: SomeId) -> Self {
        DFExpr {
            context, stack: vec![start]
        }
    }
    pub fn prune(&mut self, n: usize) {
        let new_len = self.stack.len() - n;
        self.stack.truncate(new_len)
    }
}

fn immediate_expr_children(kind: &CExprKind) -> Vec<SomeId> {
    use c_ast::CExprKind::*;
    match *kind {
        Literal(..) => vec![],
        Unary(_ty, _op, subexpr) => intos![subexpr],
        UnaryType(_ty, _op, opt_expr_id, _) => opt_expr_id.iter().map(|&x| x.into()).collect(),
        OffsetOf(..) => vec![],
        Binary(_ty, _op, lhs, rhs, _, _) => intos![lhs, rhs],
        ImplicitCast(_, e, _, _) => intos![e],
        ExplicitCast(_, e, _, _) => intos![e],
        DeclRef(_, _) => vec![], // don't follow references back!
        Call(_, f, ref args) => {
            let mut res = intos![f];
            for &a in args { res.push(a.into()) }
            res
        }
        Member(_, e, _, _) => intos![e],
        ArraySubscript(_, l, r) => intos![l,r],
        Conditional(_, c, t, e) => intos![c,t,e],
        BinaryConditional(_, c, t) => intos![c,t],
        InitList(_, ref xs, _) => xs.iter().map(|&x| x.into()).collect(),
        ImplicitValueInit(_) => vec![],
        CompoundLiteral(_, e) => intos![e],
        Predefined(_, e) => intos![e],
        Statements(_, s) => vec![s.into()],
    }
}

fn immediate_decl_children(kind: &CDeclKind) -> Vec<SomeId> {
    use c_ast::CDeclKind::*;
    match *kind {
        Function { ref parameters, body, .. } => {
            let i1 = parameters.iter().map(|&x|x.into());
            let i2 = body.iter().map(|&x|x.into());
            i1.chain(i2).collect()
        }
        Variable { typ, initializer, .. } => {
            let mut res = intos![typ.ctype];
            for x in initializer { res.push(x.into()) }
            res
        }
        Enum { ref variants, .. } =>
            variants.iter().map(|&x| x.into()).collect(),
        EnumConstant { .. } => vec![],
        Typedef { typ, .. } => intos![typ.ctype],
        Struct { ref fields, .. } =>
            fields.iter().flat_map(|x| x).map(|&x| x.into()).collect(),
        Union { ref fields, .. } =>
            fields.iter().flat_map(|x| x).map(|&x| x.into()).collect(),
        Field { typ, .. } => intos![typ.ctype],
    }
}

fn immediate_stmt_children(kind: &CStmtKind) -> Vec<SomeId> {
    use c_ast::CStmtKind::*;
    match *kind {
        Expr(e) => intos![e],
        Label(s) => intos![s],
        Case(e, s, _) => intos![e,s],
        Default(s) => intos![s],

        // Compound statements (6.8.2)
        Compound(ref xs) => xs.iter().map(|&x|x.into()).collect(),
        Empty => vec![],
        If {
            scrutinee: e,
            true_variant: s,
            false_variant: opt_s,
        } => {
            let mut res = intos![e,s];
            for &x in &opt_s { res.push(x.into()) }
            res
        },
        Switch {
            scrutinee: e,
            body: s,
        } => intos![e,s],

        While {
            condition: e,
            body: s,
        } => intos![e,s],
        DoWhile {
            body: s,
            condition: e,
        } => intos![s,e],
        ForLoop {
            init: a,
            condition: b,
            increment: c,
            body: d,
        } => {
            let mut res = vec![];
            for &x in &a { res.push(x.into()) }
            for &x in &b { res.push(x.into()) }
            for &x in &c { res.push(x.into()) }
            res.push(d.into());
            res
        },
        Goto(_) => vec![], // Don't follow the reference to the label
        Break => vec![],
        Continue => vec![],
        Return(ref opt_e) => opt_e.iter().map(|&x|x.into()).collect(),

        Decls(ref decl_ids) => decl_ids.iter().map(|&x|x.into()).collect(),

        Asm {
            ref inputs,
            ref outputs, ..
        } => {
            let mut res = vec![];
            for list in vec![inputs,outputs] {
                for elt in list {
                    res.push(elt.expression.into())
                }
            }
            res
        },
    }
}

fn immediate_type_children(kind: &CTypeKind) -> Vec<SomeId> {
    use c_ast::CTypeKind::*;
    match *kind {
        Elaborated(_) => vec![], // These are references to previous definitions
        TypeOfExpr(e) => intos![e],
        Void | Bool | Short | Int | Long | LongLong | UShort | UInt | ULong | ULongLong | SChar |
        UChar | Char | Double | LongDouble | Float | Int128 | UInt128 | BuiltinFn => vec![],

        Pointer(qtype) | Attributed(qtype, _) | BlockPointer(qtype) => intos![qtype.ctype],

        Decayed(ctype) | Paren(ctype) | TypeOf(ctype) | Complex(ctype) |
        ConstantArray(ctype, _) | IncompleteArray(ctype) => intos![ctype],

        Struct(decl_id) | Union(decl_id) | Enum(decl_id) | Typedef(decl_id) => intos![decl_id],

        VariableArray(elt, cnt) => {
            let mut res = intos![elt];
            for x in cnt { res.push(x.into()) }
            res
        },
        Function(ret, ref params, _, _) => {
            let mut res = intos![ret.ctype];
            for p in params { res.push(p.ctype.into()) }
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