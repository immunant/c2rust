//! Functions for rewriting sequences of stmts or items, using `Cursor<T>`.
use crate::ast_manip::mut_visit::{self, MutVisitor};
use rustc_ast::ptr::P;
use rustc_ast::{Block, Item, ItemKind, ModKind, Stmt};
use std::mem;

use crate::ast_manip::MutVisit;
use crate::util::cursor::Cursor;

struct BlockFolder<F: FnMut(&mut Cursor<Stmt>)> {
    f: F,
}

impl<F: FnMut(&mut Cursor<Stmt>)> MutVisitor for BlockFolder<F> {
    fn visit_block(&mut self, b: &mut P<Block>) {
        let stmts = mem::take(&mut b.stmts);
        let mut stmt_cursor = Cursor::from_vec(stmts.into_iter().collect());
        (self.f)(&mut stmt_cursor);
        b.stmts = stmt_cursor.into_vec().into();
        mut_visit::walk_block(self, b)
    }
}

/// Rewrite every block by manipulating a `Cursor` for the `Stmt`s inside.
pub fn fold_blocks<T, F>(target: &mut T, callback: F)
where
    T: MutVisit,
    F: FnMut(&mut Cursor<Stmt>),
{
    target.visit(&mut BlockFolder { f: callback })
}

struct ModuleFolder<F: FnMut(&mut Cursor<P<Item>>)> {
    f: F,
}

impl<F: FnMut(&mut Cursor<P<Item>>)> MutVisitor for ModuleFolder<F> {
    fn visit_crate(&mut self, krate: &mut rustc_ast::Crate) {
        let items = mem::take(&mut krate.items);
        let mut curs = Cursor::from_vec(items.into_iter().collect());
        (self.f)(&mut curs);
        krate.items = curs.into_vec().into();
        mut_visit::walk_crate(self, krate)
    }

    fn visit_item(&mut self, i: &mut P<Item>) {
        if let ItemKind::Mod(_, ModKind::Loaded(ref mut items_ref, ..)) = i.kind {
            let items = mem::take(items_ref);
            let mut curs = Cursor::from_vec(items.into_iter().collect());
            (self.f)(&mut curs);
            *items_ref = curs.into_vec().into();
        }
        mut_visit::walk_item(self, i)
    }
}

/// Rewrite every module by manipulating a `Cursor` for the `Item`s inside.
pub fn fold_modules<T, F>(target: &mut T, callback: F)
where
    T: MutVisit,
    F: FnMut(&mut Cursor<P<Item>>),
{
    target.visit(&mut ModuleFolder { f: callback })
}
