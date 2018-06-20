use syntax::ast::{Stmt, Block, Expr};
use idiomize::ast_manip::make_ast::mk;
use syntax::ptr::P;

#[derive(Debug)]
pub struct WithStmts<T> {
    pub stmts: Vec<Stmt>,
    pub val: T,
}

impl<T> WithStmts<T> {
    pub fn new(val: T) -> Self {
        WithStmts { stmts: vec![], val }
    }
    pub fn and_then<U, F>(self, f: F) -> WithStmts<U>
        where F: FnOnce(T) -> WithStmts<U> {

        let mut next = f(self.val);
        let mut stmts = self.stmts;
        stmts.append(&mut next.stmts);
        WithStmts {
            val: next.val,
            stmts,
        }
    }
    pub fn map<U, F>(self, f: F) -> WithStmts<U>
        where F: FnOnce(T) -> U {

        WithStmts {
            val: f(self.val),
            stmts: self.stmts,
        }
    }
    pub fn result_map<U, E, F>(self, f: F) -> Result<WithStmts<U>, E>
        where F: FnOnce(T) -> Result<U, E> {

        Ok(WithStmts {
            val: f(self.val)?,
            stmts: self.stmts,
        })
    }
}

impl WithStmts<P<Expr>> {
    /// Package a series of statements and an expression into one block expression
    pub fn to_expr(mut self) -> P<Expr> {
        if self.stmts.is_empty() {
            self.val
        } else {
            self.stmts.push(mk().expr_stmt(self.val));
            mk().block_expr(mk().block(self.stmts))
        }
    }

    /// Package a series of statements and an expression into one block
    pub fn to_block(mut self) -> P<Block> {
        self.stmts.push(mk().expr_stmt(self.val));
        mk().block(self.stmts)
    }
}
