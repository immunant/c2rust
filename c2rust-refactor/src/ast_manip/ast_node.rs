use derive_more::{From, TryInto};
use std::convert::TryFrom;

use rustc_ast::ast;
use rustc_ast::ptr::P;

#[derive(Copy, Clone, Debug, From, TryInto)]
pub enum AstNodeRef<'a> {
    Crate(&'a ast::Crate),
    Expr(&'a ast::Expr),
    Pat(&'a ast::Pat),
    Ty(&'a ast::Ty),
    Stmt(&'a ast::Stmt),
    Item(&'a ast::Item),
    ForeignItem(&'a ast::ForeignItem),
    Block(&'a ast::Block),
}

#[derive(Clone, Debug, From, TryInto)]
pub enum AstNode {
    Crate(ast::Crate),
    Expr(P<ast::Expr>),
    Pat(P<ast::Pat>),
    Ty(P<ast::Ty>),
    Stmts(Vec<ast::Stmt>),
    Stmt(ast::Stmt),
    Item(P<ast::Item>),
}

// These impls should get auto-derived, see
// https://github.com/JelteF/derive_more/issues/69
impl<'a> TryFrom<&'a AstNode> for &'a ast::Crate {
    type Error = &'static str;
    fn try_from(value: &'a AstNode) -> Result<Self, Self::Error> {
        match value {
            AstNode::Crate(ref x) => Ok(x),
            _ => Err("Only &AstNode::Crate can be converted to &Crate"),
        }
    }
}

impl<'a> TryFrom<&'a AstNode> for &'a P<ast::Expr> {
    type Error = &'static str;
    fn try_from(value: &'a AstNode) -> Result<Self, Self::Error> {
        match value {
            AstNode::Expr(ref x) => Ok(x),
            _ => Err("Only &AstNode::Expr can be converted to &P<Expr>"),
        }
    }
}

impl<'a> TryFrom<&'a AstNode> for &'a P<ast::Pat> {
    type Error = &'static str;
    fn try_from(value: &'a AstNode) -> Result<Self, Self::Error> {
        match value {
            AstNode::Pat(ref x) => Ok(x),
            _ => Err("Only &AstNode::Pat can be converted to &P<Pat>"),
        }
    }
}

impl<'a> TryFrom<&'a AstNode> for &'a P<ast::Ty> {
    type Error = &'static str;
    fn try_from(value: &'a AstNode) -> Result<Self, Self::Error> {
        match value {
            AstNode::Ty(ref x) => Ok(x),
            _ => Err("Only &AstNode::Ty can be converted to &P<Ty>"),
        }
    }
}

impl<'a> TryFrom<&'a AstNode> for &'a Vec<ast::Stmt> {
    type Error = &'static str;
    fn try_from(value: &'a AstNode) -> Result<Self, Self::Error> {
        match value {
            AstNode::Stmts(ref x) => Ok(x),
            _ => Err("Only &AstNode::Stmts can be converted to &Vec<Stmt>"),
        }
    }
}

impl<'a> TryFrom<&'a AstNode> for &'a ast::Stmt {
    type Error = &'static str;
    fn try_from(value: &'a AstNode) -> Result<Self, Self::Error> {
        match value {
            AstNode::Stmt(ref x) => Ok(x),
            _ => Err("Only &AstNode::Stmt can be converted to &Stmt"),
        }
    }
}

impl<'a> TryFrom<&'a AstNode> for &'a P<ast::Item> {
    type Error = &'static str;
    fn try_from(value: &'a AstNode) -> Result<Self, Self::Error> {
        match value {
            AstNode::Item(ref x) => Ok(x),
            _ => Err("Only &AstNode::Item can be converted to &Item"),
        }
    }
}
