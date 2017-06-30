use std::collections::hash_map::HashMap;
use std::result;
use syntax::ast::{Ident, Expr, Pat, Stmt, Block};
use syntax::symbol::Symbol;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::util::small_vector::SmallVector;

use bindings::{self, Bindings, IntoSymbol};
use fold::Fold;
use util::AsSymbol;


pub type Result<T> = result::Result<T, Error>;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Error {
    VariantMismatch,
    LengthMismatch,
    SymbolMismatch,

    // For nonlinear patterns, it's possible that the 2nd+ occurrence of the variable in the
    // pattern matches a different ident/expr/stmt than the 1st occurrence.
    NonlinearMismatch,
}

#[derive(Clone, Debug)]
pub struct MatchCtxt {
    pub bindings: Bindings,
    pub types: HashMap<Symbol, bindings::Type>,
}

impl MatchCtxt {
    pub fn new() -> MatchCtxt {
        MatchCtxt {
            bindings: Bindings::new(),
            types: HashMap::new(),
        }
    }

    pub fn try_match<T: TryMatch>(&mut self, pat: &T, target: &T) -> Result<()> {
        let r = pat.try_match(target, self);
        r
    }

    /// Clone this context and try to perform a match in the clone, returning `Ok` if it succeeds.
    pub fn clone_match<T: TryMatch>(&self, pat: &T, target: &T) -> Result<MatchCtxt> {
        let mut m = self.clone();
        m.try_match(pat, target)?;
        Ok(m)
    }


    pub fn set_type<S: IntoSymbol>(&mut self, name: S, ty: bindings::Type) {
        let name = name.into_symbol();

        if let Some(&old_ty) = self.types.get(&name) {
            assert!(ty == old_ty,
                    "tried to set type of {:?} to {:?}, but its type is already set to {:?}",
                    name, ty, old_ty);
        }

        if let Some(old_ty) = self.bindings.get_type(name) {
            assert!(ty == old_ty,
                    "tried to set type of {:?} to {:?}, but it already has a value of type {:?}",
                    name, ty, old_ty);
        }

        self.types.insert(name, ty);
    }


    pub fn maybe_capture_ident(&mut self, pattern: &Ident, target: &Ident) -> Result<bool> {
        let sym = match pattern.as_symbol() {
            Some(x) => x,
            None => return Ok(false),
        };

        // This is a valid ident pattern if it was explicitly given type "ident".  Or, if its name
        // starts with "__", then it's a valid pattern for any binding type.
        match self.types.get(&sym) {
            Some(&bindings::Type::Ident) => {},
            None if sym.as_str().starts_with("__") => {},
            _ => return Ok(false),
        }

        let ok = self.bindings.try_add_ident(sym, target.clone());
        if ok { Ok(true) } else { Err(Error::NonlinearMismatch) }
    }

    pub fn maybe_capture_expr(&mut self, pattern: &Expr, target: &Expr) -> Result<bool> {
        let sym = match pattern.as_symbol() {
            Some(x) => x,
            None => return Ok(false),
        };

        match self.types.get(&sym) {
            Some(&bindings::Type::Expr) => {},
            None if sym.as_str().starts_with("__") => {},
            _ => return Ok(false),
        }

        let ok = self.bindings.try_add_expr(sym, P(target.clone()));
        if ok { Ok(true) } else { Err(Error::NonlinearMismatch) }
    }

    pub fn maybe_capture_pat(&mut self, pattern: &Pat, target: &Pat) -> Result<bool> {
        let sym = match pattern.as_symbol() {
            Some(x) => x,
            None => return Ok(false),
        };

        match self.types.get(&sym) {
            Some(&bindings::Type::Pat) => {},
            None if sym.as_str().starts_with("__") => {},
            _ => return Ok(false),
        }

        let ok = self.bindings.try_add_pat(sym, P(target.clone()));
        if ok { Ok(true) } else { Err(Error::NonlinearMismatch) }
    }

    pub fn maybe_capture_stmt(&mut self, pattern: &Stmt, target: &Stmt) -> Result<bool> {
        let sym = match pattern.as_symbol() {
            Some(x) => x,
            None => return Ok(false),
        };

        match self.types.get(&sym) {
            Some(&bindings::Type::Stmt) => {},
            None if sym.as_str().starts_with("__") => {},
            _ => return Ok(false),
        }

        let ok = self.bindings.try_add_stmt(sym, target.clone());
        if ok { Ok(true) } else { Err(Error::NonlinearMismatch) }
    }
}

pub trait TryMatch {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> Result<()>;
}



pub trait Pattern<'a, F>: TryMatch+Sized
        where F: FnMut(Self, Bindings) -> Self + 'a {
    type Folder: Folder;
    fn make_folder(self, init_mcx: MatchCtxt, callback: F) -> Self::Folder;
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
            init_mcx: MatchCtxt,
            callback: F,
        }

        impl<'a, F> Folder for $PatternFolder<F>
                where F: FnMut($Pat, Bindings) -> $Pat + 'a {
            $(
                fn $fold_thing(&mut $slf, $arg: $ArgTy) -> $RetTy {
                    let $arg =
                        if let Ok(mcx) = $slf.init_mcx.clone_match(&$slf.pattern, &$arg) {
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
            fn make_folder(self, init_mcx: MatchCtxt, callback: F) -> Self::Folder {
                $PatternFolder {
                    pattern: self,
                    init_mcx: init_mcx,
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

pub struct MultiStmtPatternFolder<F>
        where F: FnMut(Vec<Stmt>, Bindings) -> Vec<Stmt> {
    pattern: Vec<Stmt>,
    init_mcx: MatchCtxt,
    callback: F,
}

impl<'a, F> Folder for MultiStmtPatternFolder<F>
        where F: FnMut(Vec<Stmt>, Bindings) -> Vec<Stmt> {
    fn fold_block(&mut self, b: P<Block>) -> P<Block> {
        assert!(self.pattern.len() > 0);

        let mut stmts = b.stmts.clone();
        let mut i = 0;
        let mut any_matched = false;
        'top: while i < stmts.len() {
            let mut mcx = match self.init_mcx.clone_match(&self.pattern[0], &stmts[i]) {
                Ok(x) => x,
                Err(_) => { i += 1; continue; },
            };

            // The first pattern matched at `i`.  Now try to match the rest, under the same `mcx`.
            if i + self.pattern.len() > stmts.len() {
                i += 1;
                continue;
            }

            for j in 1 .. self.pattern.len() {
                match mcx.try_match(&self.pattern[j], &stmts[i + j]) {
                    Ok(()) => {},
                    Err(_) => { i += 1; continue 'top; },
                }
            }

            // Successfully matched all the stmts, producing `mcx`.  Now replace a chunk of `stmts`
            // with the result of running the callback.
            any_matched = true;

            // Split `stmts` into `stmts`, `old`, and `rest`.
            let mut old = stmts.split_off(i);
            let mut rest = old.split_off(self.pattern.len());

            let mut new = (self.callback)(old, mcx.bindings);

            // Reassemble `stmts`, placing `new` at the end of the substituted region.
            stmts.append(&mut new);
            i = stmts.len();
            stmts.append(&mut rest);
        }

        if any_matched {
            b.map(|b| Block { stmts: stmts, ..b })
        } else {
            b
        }
    }
}

impl<'a, F> Pattern<'a, F> for Vec<Stmt>
        where F: FnMut(Vec<Stmt>, Bindings) -> Vec<Stmt> + 'a {
    type Folder = MultiStmtPatternFolder<F>;
    fn make_folder(self, init_mcx: MatchCtxt, callback: F) -> Self::Folder {
        MultiStmtPatternFolder {
            pattern: self,
            init_mcx: init_mcx,
            callback: callback,
        }
    }
}


/// Find every match for `pattern` within `target`, and rewrite each one by invoking `callback`.
pub fn fold_match<'a, P, T, F>(pattern: P, target: T, callback: F) -> <T as Fold>::Result
        where P: Pattern<'a, F>,
              T: Fold,
              F: FnMut(P, Bindings) -> P + 'a {
    fold_match_with(MatchCtxt::new(), pattern, target, callback)
}

/// Find every match for `pattern` within `target`, and rewrite each one by invoking `callback`.
pub fn fold_match_with<'a, P, T, F>(init_mcx: MatchCtxt,
                                    pattern: P,
                                    target: T,
                                    callback: F) -> <T as Fold>::Result
        where P: Pattern<'a, F>,
              T: Fold,
              F: FnMut(P, Bindings) -> P + 'a {
    let mut f = pattern.make_folder(init_mcx, callback);
    target.fold(&mut f)
}
