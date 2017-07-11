use std::collections::hash_map::HashMap;
use std::cmp;
use std::result;
use syntax::ast::{Ident, Path, Expr, Pat, Ty, Stmt, Block};
use syntax::symbol::Symbol;
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::util::small_vector::SmallVector;

use bindings::{self, Bindings};
use fold::Fold;
use util::AsSymbol;
use util::IntoSymbol;


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

    pub fn from_match<T: TryMatch>(pat: &T, target: &T) -> Result<MatchCtxt> {
        let mut m = MatchCtxt::new();
        m.try_match(pat, target)?;
        Ok(m)
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

    pub fn maybe_capture_label(&mut self, pattern: &Ident, target: &Ident) -> Result<bool> {
        let sym = match pattern.as_symbol() {
            Some(x) => x,
            None => return Ok(false),
        };

        // Labels use lifetime syntax, but are `Ident`s instead of `Lifetime`s.
        // TODO: should probably distinguish idents, labels, and lifetimes at some point
        match self.types.get(&sym) {
            Some(&bindings::Type::Ident) => {},
            None if sym.as_str().starts_with("'__") => {},
            _ => return Ok(false),
        }

        let ok = self.bindings.try_add_ident(sym, target.clone());
        if ok { Ok(true) } else { Err(Error::NonlinearMismatch) }
    }

    pub fn maybe_capture_path(&mut self, pattern: &Path, target: &Path) -> Result<bool> {
        let sym = match pattern.as_symbol() {
            Some(x) => x,
            None => return Ok(false),
        };

        match self.types.get(&sym) {
            Some(&bindings::Type::Path) => {},
            None if sym.as_str().starts_with("__") => {},
            _ => return Ok(false),
        }

        let ok = self.bindings.try_add_path(sym, target.clone());
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

    pub fn maybe_capture_ty(&mut self, pattern: &Ty, target: &Ty) -> Result<bool> {
        let sym = match pattern.as_symbol() {
            Some(x) => x,
            None => return Ok(false),
        };

        match self.types.get(&sym) {
            Some(&bindings::Type::Ty) => {},
            None if sym.as_str().starts_with("__") => {},
            _ => return Ok(false),
        }

        let ok = self.bindings.try_add_ty(sym, P(target.clone()));
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



pub trait Pattern: TryMatch+Sized {
    fn apply_folder<T, F>(self,
                          init_mcx: MatchCtxt,
                          callback: F,
                          target: T) -> <T as Fold>::Result
        where T: Fold,
              F: FnMut(Self, Bindings) -> Self;
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

        impl<F> Folder for $PatternFolder<F>
                where F: FnMut($Pat, Bindings) -> $Pat {
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

        impl Pattern for $Pat {
            fn apply_folder<T, F>(self,
                                  init_mcx: MatchCtxt,
                                  callback: F,
                                  target: T) -> <T as Fold>::Result
                where T: Fold,
                      F: FnMut(Self, Bindings) -> Self {
                let mut f = $PatternFolder {
                    pattern: self,
                    init_mcx: init_mcx,
                    callback: callback,
                };
                target.fold(&mut f)
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

impl<F> Folder for MultiStmtPatternFolder<F>
        where F: FnMut(Vec<Stmt>, Bindings) -> Vec<Stmt> {
    fn fold_block(&mut self, b: P<Block>) -> P<Block> {
        assert!(self.pattern.len() > 0);

        let mut new_stmts = Vec::with_capacity(b.stmts.len());
        let mut last = 0;

        let mut i = 0;
        while i < b.stmts.len() {
            let mut mcx = self.init_mcx.clone();
            let result = match_multi_stmt(&mut mcx, &self.pattern, &b.stmts[i..]);
            if let Some(consumed) = result {
                new_stmts.extend_from_slice(&b.stmts[last .. i]);

                let consumed_stmts = b.stmts[i .. i + consumed].to_owned();
                let mut replacement = (self.callback)(consumed_stmts, mcx.bindings);
                new_stmts.append(&mut replacement);

                i += cmp::max(consumed, 1);
                last = i;
            } else {
                // If the pattern starts with a glob, then trying to match it at `i + 1` will fail
                // just the same as at `i`.
                if self.pattern.len() > 0 && is_multi_stmt_glob(&self.init_mcx, &self.pattern[0]) {
                    break;
                } else {
                    i += 1;
                }
            }
        }

        let b =
            if last == 0 {
                b
            } else {
                new_stmts.extend_from_slice(&b.stmts[last ..]);
                b.map(|b| Block { stmts: new_stmts, ..b })
            };

        fold::noop_fold_block(b, self)
    }
}

pub fn match_multi_stmt(mcx: &mut MatchCtxt, pattern: &[Stmt], target: &[Stmt]) -> Option<usize> {
    if pattern.len() == 0 {
        return Some(0);
    }

    if is_multi_stmt_glob(mcx, &pattern[0]) {
        let name = pattern[0].as_symbol().unwrap();
        for i in (0 .. target.len() + 1).rev() {
            let orig_mcx = mcx.clone();
            if let Some(consumed) = match_multi_stmt(mcx, &pattern[1..], &target[i..]) {
                let ok = mcx.bindings.try_add_multi_stmt(name, target[..i].to_owned());
                if ok {
                    return Some(i + consumed);
                }
            }
            *mcx = orig_mcx;
        }
        None
    } else {
        let mut i = 0;
        while i < pattern.len() {
            if is_multi_stmt_glob(mcx, &pattern[i]) {
                // Stop current processing, and go match a glob instead.
                match match_multi_stmt(mcx, &pattern[i..], &target[i..]) {
                    Some(consumed) => return Some(i + consumed),
                    None => return None,
                }
            }

            if i >= target.len() {
                return None;
            }

            let r = mcx.try_match(&pattern[i], &target[i]);
            match r {
                Ok(_) => {},
                Err(_) => return None,
            }
            i += 1;
        }
        assert!(i == pattern.len());
        Some(pattern.len())
    }
}

fn is_multi_stmt_glob(mcx: &MatchCtxt, pattern: &Stmt) -> bool {
    let sym = match pattern.as_symbol() {
        Some(x) => x,
        None => return false,
    };

    match mcx.types.get(&sym) {
        Some(&bindings::Type::MultiStmt) => {},
        None if sym.as_str().starts_with("__m_") => {},
        _ => return false,
    }

    true
}

impl Pattern for Vec<Stmt> {
    fn apply_folder<T, F>(self,
                          init_mcx: MatchCtxt,
                          callback: F,
                          target: T) -> <T as Fold>::Result
        where T: Fold,
              F: FnMut(Self, Bindings) -> Self {
        let mut f = MultiStmtPatternFolder {
            pattern: self,
            init_mcx: init_mcx,
            callback: callback,
        };
        target.fold(&mut f)
    }
}


/// Find every match for `pattern` within `target`, and rewrite each one by invoking `callback`.
pub fn fold_match<P, T, F>(pattern: P, target: T, callback: F) -> <T as Fold>::Result
        where P: Pattern,
              T: Fold,
              F: FnMut(P, Bindings) -> P {
    fold_match_with(MatchCtxt::new(), pattern, target, callback)
}

/// Find every match for `pattern` within `target`, and rewrite each one by invoking `callback`.
pub fn fold_match_with<P, T, F>(init_mcx: MatchCtxt,
                                pattern: P,
                                target: T,
                                callback: F) -> <T as Fold>::Result
        where P: Pattern,
              T: Fold,
              F: FnMut(P, Bindings) -> P {
    pattern.apply_folder(init_mcx, callback, target)
}
