use c2rust_ast_builder::mk;
use std::iter::FromIterator;
use syntax::ast::{Block, Expr, Stmt};
use syntax::ptr::P;

#[derive(Debug)]
pub struct WithStmts<T> {
    stmts: Vec<Stmt>,
    val: T,
    is_unsafe: bool,
}

impl<T> WithStmts<T> {
    pub fn new(stmts: Vec<Stmt>, val: T) -> Self {
        WithStmts {
            stmts,
            val,
            is_unsafe: false,
        }
    }
    pub fn new_val(val: T) -> Self {
        WithStmts {
            stmts: vec![],
            val,
            is_unsafe: false,
        }
    }
    pub fn new_unsafe_val(val: T) -> Self {
        WithStmts {
            stmts: vec![],
            val,
            is_unsafe: true,
        }
    }
    pub fn and_then<U, E, F>(self, f: F) -> Result<WithStmts<U>, E>
    where
        F: FnOnce(T) -> Result<WithStmts<U>, E>,
    {
        let mut next = f(self.val)?;
        let mut stmts = self.stmts;
        stmts.append(&mut next.stmts);
        Ok(WithStmts {
            val: next.val,
            stmts,
            is_unsafe: self.is_unsafe || next.is_unsafe,
        })
    }
    pub fn map<U, F>(self, f: F) -> WithStmts<U>
    where
        F: FnOnce(T) -> U,
    {
        WithStmts {
            val: f(self.val),
            stmts: self.stmts,
            is_unsafe: self.is_unsafe,
        }
    }
    pub fn result_map<U, E, F>(self, f: F) -> Result<WithStmts<U>, E>
    where
        F: FnOnce(T) -> Result<U, E>,
    {
        Ok(WithStmts {
            val: f(self.val)?,
            stmts: self.stmts,
            is_unsafe: self.is_unsafe,
        })
    }

    pub fn set_unsafe(&mut self) {
        self.is_unsafe = true;
    }

    pub fn merge_unsafe(&mut self, is_unsafe: bool) {
        self.is_unsafe = self.is_unsafe || is_unsafe;
    }

    pub fn into_stmts(self) -> Vec<Stmt> {
        self.stmts
    }
    pub fn into_value(self) -> T {
        self.val
    }
    pub fn discard_unsafe(self) -> (Vec<Stmt>, T) {
        (self.stmts, self.val)
    }

    pub fn stmts(&self) -> &[Stmt] {
        &self.stmts
    }
    pub fn stmts_mut(&mut self) -> &mut Vec<Stmt> {
        &mut self.stmts
    }

    pub fn is_unsafe(&self) -> bool {
        self.is_unsafe
    }

    pub fn add_stmt(&mut self, stmt: Stmt) {
        self.stmts.push(stmt);
    }

    pub fn prepend_stmts(&mut self, mut stmts: Vec<Stmt>) {
        stmts.append(&mut self.stmts);
        self.stmts = stmts;
    }
}

impl WithStmts<P<Expr>> {
    /// Package a series of statements and an expression into one block expression
    pub fn to_expr(self) -> P<Expr> {
        if self.stmts.is_empty() {
            self.val
        } else {
            mk().block_expr(self.to_block())
        }
    }

    /// Package a series of statements and an expression into one block
    pub fn to_block(mut self) -> P<Block> {
        self.stmts.push(mk().expr_stmt(self.val));
        mk().block(self.stmts)
    }

    pub fn to_unsafe_pure_expr(self) -> Option<P<Expr>> {
        let is_unsafe = self.is_unsafe;
        self.to_pure_expr()
            .map(|expr| {
                if is_unsafe {
                    mk().block_expr(
                        mk().unsafe_().block(
                            vec![mk().expr_stmt(expr)]
                        )
                    )
                } else {
                    expr
                }
            })
    }

    pub fn to_pure_expr(self) -> Option<P<Expr>> {
        if self.stmts.is_empty() {
            Some(self.val)
        } else {
            None
        }
    }

    pub fn with_stmts_opt<T>(opt: Option<WithStmts<T>>) -> WithStmts<Option<T>> {
        match opt {
            None => WithStmts::new_val(None),
            Some(x) => WithStmts {
                val: Some(x.val),
                stmts: x.stmts,
                is_unsafe: x.is_unsafe,
            },
        }
    }
}

impl<T> FromIterator<WithStmts<T>> for WithStmts<Vec<T>>
{
    fn from_iter<I: IntoIterator<Item = WithStmts<T>>>(value: I) -> Self {
        let mut stmts = vec![];
        let mut res = vec![];
        for mut val in value.into_iter() {
            stmts.append(val.stmts_mut());
            res.push(val.into_value());
        }
        WithStmts::new(stmts, res)
    }
}
