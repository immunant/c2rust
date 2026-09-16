use c2rust_ast_builder::mk;
use std::iter::FromIterator;
use std::mem;
use syn::{Block, Expr, Item, Stmt};

#[derive(Debug, Clone)]
pub(crate) struct Tagged<T> {
    val: T,
    is_unsafe: bool,
}

impl<T> Tagged<T> {
    pub(crate) fn new(val: T) -> Self {
        Tagged {
            val,
            is_unsafe: false,
        }
    }

    pub(crate) fn is_unsafe(&self) -> bool {
        self.is_unsafe
    }

    pub(crate) fn set_unsafe(mut self) -> Self {
        self.is_unsafe = true;
        self
    }

    pub(crate) fn discard_unsafe(self) -> T {
        self.val
    }

    pub(crate) fn map<U>(self, f: impl FnOnce(T) -> U) -> Tagged<U> {
        Tagged {
            val: f(self.val),
            is_unsafe: self.is_unsafe,
        }
    }

    pub(crate) fn flat_map<U>(self, f: impl FnOnce(T) -> U) -> <Tagged<U> as Flattenable>::Output
    where
        Tagged<U>: Flattenable,
    {
        self.map(f).flatten()
    }

    pub(crate) fn zip<U>(self, next: Tagged<U>) -> Tagged<(T, U)> {
        Tagged {
            val: (self.val, next.val),
            is_unsafe: self.is_unsafe || next.is_unsafe,
        }
    }
}

pub(crate) type TaggedExpr = Tagged<Box<Expr>>;

impl TaggedExpr {
    /// If `is_unsafe` is true, wraps `val` in an `unsafe` block and unsets `is_unsafe`.
    pub(crate) fn wrap_unsafe(mut self) -> Self {
        if mem::take(&mut self.is_unsafe) {
            self.val = mk().unsafe_block_expr(vec![mk().expr_stmt(self.val)]);
        }

        self
    }

    /// If `is_unsafe` is true, wraps `val` in an `unsafe` block. Then returns `val`.
    pub(crate) fn into_wrapped_expr(self) -> Box<Expr> {
        self.wrap_unsafe().val
    }
}

impl<T> From<T> for Tagged<T> {
    fn from(value: T) -> Self {
        Tagged::new(value)
    }
}

impl<T> Flattenable for Tagged<Tagged<T>> {
    type Output = Tagged<T>;

    fn flatten(self) -> Self::Output {
        let Tagged { val, is_unsafe } = self;
        Tagged {
            val: val.val,
            is_unsafe: is_unsafe || val.is_unsafe,
        }
    }
}

#[derive(Clone, Debug)]
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

    pub fn and_then<U, F>(self, f: F) -> WithStmts<U>
    where
        F: FnOnce(T) -> WithStmts<U>,
    {
        self.flat_map(f)
    }

    pub fn and_then_try<U, E, F>(self, f: F) -> Result<WithStmts<U>, E>
    where
        F: FnOnce(T) -> Result<WithStmts<U>, E>,
    {
        self.try_flat_map(f)
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

    pub fn try_map<U, E, F>(self, f: F) -> Result<WithStmts<U>, E>
    where
        F: FnOnce(T) -> Result<U, E>,
    {
        Ok(WithStmts {
            val: f(self.val)?,
            stmts: self.stmts,
            is_unsafe: self.is_unsafe,
        })
    }

    pub(crate) fn flat_map<U>(self, f: impl FnOnce(T) -> U) -> <WithStmts<U> as Flattenable>::Output
    where
        WithStmts<U>: Flattenable,
    {
        self.map(f).flatten()
    }

    pub(crate) fn try_flat_map<U, E>(
        self,
        f: impl FnOnce(T) -> Result<U, E>,
    ) -> Result<<WithStmts<U> as Flattenable>::Output, E>
    where
        WithStmts<U>: Flattenable,
    {
        Ok(self.try_map(f)?.flatten())
    }

    pub fn zip<U>(self, mut next: WithStmts<U>) -> WithStmts<(T, U)> {
        let mut stmts = self.stmts;
        stmts.append(&mut next.stmts);
        WithStmts {
            val: (self.val, next.val),
            stmts,
            is_unsafe: self.is_unsafe || next.is_unsafe,
        }
    }

    pub fn set_unsafe(mut self) -> Self {
        self.is_unsafe = true;
        self
    }

    pub fn merge_unsafe(mut self, is_unsafe: bool) -> Self {
        self.is_unsafe = self.is_unsafe || is_unsafe;
        self
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

    /// If all statements in self.stmts are [`Item`] statements, returns the contained [`Item`]s.
    /// Otherwise, returns [`None`].
    pub fn stmts_to_items(&mut self) -> Option<Vec<Box<Item>>> {
        let all_are_items = self.stmts.iter().all(|stmt| matches!(stmt, Stmt::Item(_)));
        all_are_items.then(|| {
            std::mem::take(&mut self.stmts)
                .into_iter()
                .map(|stmt| match stmt {
                    Stmt::Item(item) => Box::new(item),
                    _ => unreachable!(),
                })
                .collect()
        })
    }

    pub fn is_unsafe(&self) -> bool {
        self.is_unsafe
    }

    pub fn add_stmt(mut self, stmt: Stmt) -> Self {
        self.stmts.push(stmt);
        self
    }

    pub fn prepend_stmts(mut self, mut stmts: Vec<Stmt>) -> Self {
        stmts.append(&mut self.stmts);
        self.stmts = stmts;
        self
    }

    pub fn is_pure(&self) -> bool {
        self.stmts.is_empty()
    }
}

impl WithStmts<Box<Expr>> {
    /// Package a series of statements and an expression into one block expression
    pub fn to_expr(self) -> Box<Expr> {
        if self.stmts.is_empty() {
            self.val
        } else {
            mk().block_expr(self.to_block())
        }
    }

    /// Package a series of statements and an expression into one block
    pub fn to_block(mut self) -> Block {
        self.stmts.push(mk().expr_stmt(self.val));
        mk().block(self.stmts)
    }

    /// If `is_unsafe` is true, wraps `val` in an `unsafe` block and unsets `is_unsafe`.
    pub fn wrap_unsafe(mut self) -> Self {
        if mem::take(&mut self.is_unsafe) {
            self.val = mk().unsafe_block_expr(vec![mk().expr_stmt(self.val)]);
        }

        self
    }

    pub fn to_pure_expr(self) -> Option<Box<Expr>> {
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

impl<T> From<Tagged<T>> for WithStmts<T> {
    fn from(value: Tagged<T>) -> Self {
        let is_unsafe = value.is_unsafe();
        WithStmts {
            stmts: vec![],
            val: value.discard_unsafe(),
            is_unsafe,
        }
    }
}

impl<T> Flattenable for WithStmts<WithStmts<T>> {
    type Output = WithStmts<T>;

    fn flatten(self) -> Self::Output {
        let WithStmts {
            mut stmts,
            val,
            is_unsafe,
        } = self;
        stmts.extend(val.stmts);
        WithStmts {
            val: val.val,
            stmts,
            is_unsafe: is_unsafe || val.is_unsafe,
        }
    }
}

impl<T> Flattenable for WithStmts<Tagged<T>> {
    type Output = WithStmts<T>;

    fn flatten(self) -> Self::Output {
        let WithStmts {
            stmts,
            val,
            is_unsafe,
        } = self;
        WithStmts {
            val: val.val,
            stmts,
            is_unsafe: is_unsafe || val.is_unsafe,
        }
    }
}

impl<T> Flattenable for Tagged<WithStmts<T>> {
    type Output = WithStmts<T>;

    fn flatten(self) -> Self::Output {
        let Tagged { val, is_unsafe } = self;
        WithStmts {
            val: val.val,
            stmts: val.stmts,
            is_unsafe: is_unsafe || val.is_unsafe,
        }
    }
}

impl<T> FromIterator<WithStmts<T>> for WithStmts<Vec<T>> {
    fn from_iter<I: IntoIterator<Item = WithStmts<T>>>(value: I) -> Self {
        let mut stmts = vec![];
        let mut res = vec![];
        let mut is_unsafe = false;
        for mut val in value.into_iter() {
            is_unsafe |= val.is_unsafe();
            stmts.append(val.stmts_mut());
            res.push(val.into_value());
        }
        WithStmts::new(stmts, res).merge_unsafe(is_unsafe)
    }
}

pub(crate) trait Flattenable {
    type Output;
    fn flatten(self) -> Self::Output;
}
