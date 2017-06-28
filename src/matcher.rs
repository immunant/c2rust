use std::collections::hash_map::{HashMap, Entry};
use std::result;
use syntax::ast::{Ident, Expr, Pat, Stmt, Item, Crate, Mac};
use syntax::symbol::Symbol;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::visit::{self, Visitor};
use syntax::util::small_vector::SmallVector;

use bindings::Bindings;
use fold::Fold;


pub type Result = result::Result<(), Error>;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Error {
    NotImplemented,
    VariantMismatch,
    LengthMismatch,
    SymbolMismatch,

    // For nonlinear patterns, it's possible that the 2nd+ occurrence of the variable in the
    // pattern matches a different ident/expr/stmt than the 1st occurrence.
    NonlinearMismatch,
}

#[derive(Debug)]
pub struct MatchCtxt {
    pub bindings: Bindings,
}

impl MatchCtxt {
    pub fn new() -> MatchCtxt {
        MatchCtxt {
            bindings: Bindings::new(),
        }
    }

    pub fn from_match<T: TryMatch>(pat: &T, target: &T) -> result::Result<MatchCtxt, Error> {
        let mut m = MatchCtxt::new();
        m.try_match(pat, target)?;
        Ok(m)
    }

    pub fn try_match<T: TryMatch>(&mut self, pat: &T, target: &T) -> Result {
        let r = pat.try_match(target, self);
        r
    }

    pub fn try_capture_ident(&mut self, sym: Symbol, ident: &Ident) -> Result {
        let ok = self.bindings.try_add_ident(sym, ident.clone());
        if ok { Ok(()) } else { Err(Error::NonlinearMismatch) }
    }

    pub fn try_capture_expr(&mut self, sym: Symbol, expr: &P<Expr>) -> Result {
        let ok = self.bindings.try_add_expr(sym, expr.clone());
        if ok { Ok(()) } else { Err(Error::NonlinearMismatch) }
    }

    pub fn try_capture_pat(&mut self, sym: Symbol, pat: &P<Pat>) -> Result {
        let ok = self.bindings.try_add_pat(sym, pat.clone());
        if ok { Ok(()) } else { Err(Error::NonlinearMismatch) }
    }

    pub fn try_capture_stmt(&mut self, sym: Symbol, stmt: &P<Stmt>) -> Result {
        let ok = self.bindings.try_add_stmt(sym, stmt.clone());
        if ok { Ok(()) } else { Err(Error::NonlinearMismatch) }
    }
}

pub trait TryMatch {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> Result;
}



pub trait Pattern<'a, F>: TryMatch+Sized
        where F: FnMut(Self, Bindings) -> Self + 'a {
    type Folder: Folder;
    fn make_folder(self, callback: F) -> Self::Folder;
}


macro_rules! gen_pattern_impl {
    (
        pattern = $Pat:ty;
        folder = $PatternFolder:ident;

        $(
            // Capture the ident "self" from the outer context, so it can be used in the `finish`
            // expression.
            fn $fold_thing:ident ( &mut $slf:ident , $arg:ident : $ArgTy:ty ) -> $RetTy:ty {
                $finish:expr
            }
        )*
    ) => {
        pub struct $PatternFolder<F>
                where F: FnMut($Pat, Bindings) -> $Pat {
            pattern: $Pat,
            callback: F,
        }

        impl<'a, F> Folder for $PatternFolder<F>
                where F: FnMut($Pat, Bindings) -> $Pat + 'a {
            $(
                fn $fold_thing(&mut $slf, $arg: $ArgTy) -> $RetTy {
                    let $arg =
                        if let Ok(mcx) = MatchCtxt::from_match(&$slf.pattern, &$arg) {
                            ($slf.callback)($arg, mcx.bindings)
                        } else {
                            $arg
                        };

                    $finish
                }
            )*
        }

        impl<'a, F> Pattern<'a, F> for $Pat
                where F: FnMut($Pat, Bindings) -> $Pat + 'a {
            type Folder = $PatternFolder<F>;
            fn make_folder(self, callback: F) -> Self::Folder {
                $PatternFolder {
                    pattern: self,
                    callback: callback,
                }
            }
        }
    };
}

gen_pattern_impl! {
    pattern = P<Expr>;
    folder = ExprPatternFolder;

    fn fold_expr(&mut self, e: P<Expr>) -> P<Expr> {
        e.map(|e| fold::noop_fold_expr(e, self))
    }
}

gen_pattern_impl! {
    pattern = Stmt;
    folder = StmtPatternFolder;

    fn fold_stmt(&mut self, s: Stmt) -> SmallVector<Stmt> {
        fold::noop_fold_stmt(s, self)
    }
}


/// Find every match for `pattern` within `target`, and rewrite each one by invoking `callback`.
pub fn fold_match<'a, P, T, F>(pattern: P, target: T, callback: F) -> <T as Fold>::Result
        where P: Pattern<'a, F>,
              T: Fold,
              F: FnMut(P, Bindings) -> P + 'a {
    let mut f = pattern.make_folder(callback);
    target.fold(&mut f)
}
