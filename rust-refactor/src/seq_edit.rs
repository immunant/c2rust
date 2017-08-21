use syntax::ast::{Block, Stmt, Item, Mod};
use syntax::fold::{self, Folder};
use syntax::ptr::P;

use cursor::Cursor;
use fold::Fold;


struct BlockFolder<F: FnMut(&mut Cursor<Stmt>)> {
    f: F,
}

impl<F: FnMut(&mut Cursor<Stmt>)> Folder for BlockFolder<F> {
    fn fold_block(&mut self, b: P<Block>) -> P<Block> {
        let b = b.map(|mut b| {
            let mut stmt_cursor = Cursor::from_vec(b.stmts);
            (self.f)(&mut stmt_cursor);
            b.stmts = stmt_cursor.into_vec();
            b
        });
        fold::noop_fold_block(b, self)
    }
}

pub fn fold_blocks<T, F>(target: T, callback: F) -> <T as Fold>::Result
        where T: Fold,
              F: FnMut(&mut Cursor<Stmt>) {
    target.fold(&mut BlockFolder { f: callback })
}


struct ModuleFolder<F: FnMut(&mut Cursor<P<Item>>)> {
    f: F,
}

impl<F: FnMut(&mut Cursor<P<Item>>)> Folder for ModuleFolder<F> {
    fn fold_mod(&mut self, mut m: Mod) -> Mod {
        let mut curs = Cursor::from_vec(m.items);
        (self.f)(&mut curs);
        m.items = curs.into_vec();
        fold::noop_fold_mod(m, self)
    }
}

pub fn fold_modules<T, F>(target: T, callback: F) -> <T as Fold>::Result
        where T: Fold,
              F: FnMut(&mut Cursor<P<Item>>) {
    target.fold(&mut ModuleFolder { f: callback })
}
