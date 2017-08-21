use std::collections::hash_map::HashMap;
use std::cmp;
use std::result;
use rustc::hir::def_id::DefId;
use rustc::hir::map::Node;
use syntax::ast::{Ident, Path, Expr, ExprKind, Pat, Ty, TyKind, Stmt, Block};
use syntax::symbol::Symbol;
use syntax::fold::{self, Folder};
use syntax::parse::PResult;
use syntax::parse::parser::Parser;
use syntax::parse::token::Token;
use syntax::ptr::P;
use syntax::tokenstream::ThinTokenStream;
use syntax::util::move_map::MoveMap;
use syntax::util::small_vector::SmallVector;

use api::DriverCtxtExt;
use bindings::{self, Bindings};
use command::CommandState;
use driver;
use fold::Fold;
use get_node_id::GetNodeId;
use reflect;
use util::PatternSymbol;
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

    /// A `marked!` pattern tried to match a non-marked node.
    NotMarked,

    /// A `def!` pattern failed to match because the target is not a reference to the expected
    /// item.
    DefMismatch,

    /// A `typed!` macro failed to match because the target's type did not match the type pattern.
    WrongType,

    BadSpecialPattern(Symbol),
}

#[derive(Clone)]
pub struct MatchCtxt<'a, 'hir: 'a, 'gcx: 'tcx + 'a, 'tcx: 'a> {
    pub bindings: Bindings,
    pub types: HashMap<Symbol, bindings::Type>,
    st: &'a CommandState,
    cx: &'a driver::Ctxt<'a, 'hir, 'gcx, 'tcx>,
}

impl<'a, 'hir, 'gcx, 'tcx> MatchCtxt<'a, 'hir, 'gcx, 'tcx> {
    pub fn new(st: &'a CommandState,
               cx: &'a driver::Ctxt<'a, 'hir, 'gcx, 'tcx>) -> MatchCtxt<'a, 'hir, 'gcx, 'tcx> {
        MatchCtxt {
            bindings: Bindings::new(),
            types: HashMap::new(),
            st: st,
            cx: cx,
        }
    }

    pub fn try_match<T: TryMatch>(&mut self, pat: &T, target: &T) -> Result<()> {
        let r = pat.try_match(target, self);
        r
    }

    pub fn from_match<T: TryMatch>(st: &'a CommandState,
                                   cx: &'a driver::Ctxt<'a, 'hir, 'gcx, 'tcx>,
                                   pat: &T,
                                   target: &T) -> Result<MatchCtxt<'a, 'hir, 'gcx, 'tcx>> {
        let mut m = MatchCtxt::new(st, cx);
        m.try_match(pat, target)?;
        Ok(m)
    }

    /// Clone this context and try to perform a match in the clone, returning `Ok` if it succeeds.
    pub fn clone_match<T: TryMatch>(&self, pat: &T, target: &T)
                                    -> Result<MatchCtxt<'a, 'hir, 'gcx, 'tcx>> {
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
        let sym = match pattern.pattern_symbol() {
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
        let sym = match pattern.pattern_symbol() {
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
        let sym = match pattern.pattern_symbol() {
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
        let sym = match pattern.pattern_symbol() {
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
        let sym = match pattern.pattern_symbol() {
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
        let sym = match pattern.pattern_symbol() {
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
        let sym = match pattern.pattern_symbol() {
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


    pub fn do_marked<T, F>(&mut self,
                           tts: &ThinTokenStream,
                           func: F,
                           target: &T) -> Result<()>
            where T: TryMatch + GetNodeId,
                  F: for<'b> FnOnce(&mut Parser<'b>) -> PResult<'b, T> {
        let mut p = Parser::new(&self.cx.session().parse_sess,
                                tts.clone().into(),
                                None, false, false);
        let pattern = func(&mut p).unwrap();

        let label =
            if p.eat(&Token::Comma) {
                p.parse_ident().unwrap().name
            } else {
                "target".into_symbol()
            };

        if !self.st.marked(target.get_node_id(), label) {
            return Err(Error::NotMarked);
        }

        self.try_match(&pattern, target)
    }

    fn do_def_impl(&mut self,
                   tts: &ThinTokenStream,
                   opt_def_id: Option<DefId>,
                   target_path: Option<&Path>) -> Result<()> {
        let mut p = Parser::new(&self.cx.session().parse_sess,
                                tts.clone().into(),
                                None, false, false);
        let name = p.parse_ident().unwrap().name;
        let label =
            if p.eat(&Token::Comma) {
                p.parse_ident().unwrap().name
            } else {
                "target".into_symbol()
            };

        let def_id = match_or!([opt_def_id] Some(x) => x;
                               return Err(Error::DefMismatch));
        let node_id = match_or!([self.cx.hir_map().as_local_node_id(def_id)] Some(x) => x;
                                return Err(Error::DefMismatch));
        if !self.st.marked(node_id, label) {
            return Err(Error::DefMismatch);
        }

        let node = match_or!([self.cx.hir_map().get_if_local(def_id)] Some(x) => x;
                             return Err(Error::DefMismatch));
        let node_name = match node {
            Node::NodeItem(i) => i.name,
            Node::NodeForeignItem(i) => i.name,
            Node::NodeTraitItem(i) => i.name,
            Node::NodeImplItem(i) => i.name,
            _ => panic!("expected item-like"),
        };
        if node_name != name {
            return Err(Error::DefMismatch);
        }

        if let Some(path) = target_path {
            self.bindings.add_def_path(name, label, path.clone());
        }

        Ok(())
    }

    pub fn do_def_expr(&mut self, tts: &ThinTokenStream, target: &Expr) -> Result<()> {
        let opt_def_id = self.cx.try_resolve_expr(target);
        let opt_path = match target.node {
            ExprKind::Path(None, ref p) => Some(p),
            _ => None,
        };
        self.do_def_impl(tts, opt_def_id, opt_path)
    }

    pub fn do_def_ty(&mut self, tts: &ThinTokenStream, target: &Ty) -> Result<()> {
        let opt_def_id = self.cx.try_resolve_ty(target);
        let opt_path = match target.node {
            TyKind::Path(None, ref p) => Some(p),
            _ => None,
        };
        self.do_def_impl(tts, opt_def_id, opt_path)
    }

    pub fn do_typed<T, F>(&mut self,
                          tts: &ThinTokenStream,
                          func: F,
                          target: &T) -> Result<()>
            where T: TryMatch + GetNodeId,
                  F: for<'b> FnOnce(&mut Parser<'b>) -> PResult<'b, T> {
        let mut p = Parser::new(&self.cx.session().parse_sess,
                                tts.clone().into(),
                                None, false, false);
        let pattern = func(&mut p).unwrap();
        p.expect(&Token::Comma).unwrap();
        let ty_pattern = p.parse_ty().unwrap();

        let tcx_ty = self.cx.node_type(target.get_node_id());
        let ast_ty = reflect::reflect_tcx_ty(self.cx.ty_ctxt(), tcx_ty);

        if self.try_match(&ty_pattern, &ast_ty).is_err() {
            return Err(Error::WrongType);
        }

        self.try_match(&pattern, target)
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

        // Capture the ident "self" from the outer context, so it can be used in the expressions.
        fn $fold_thing:ident ( &mut $slf:ident , $arg:ident : $ArgTy:ty ) -> $RetTy:ty;
        walk = $walk:expr;
        map($match_one:ident) = $map:expr;
    ) => {
        pub struct $PatternFolder<'a, 'hir: 'a, 'gcx: 'tcx + 'a, 'tcx: 'a, F>
                where F: FnMut($Pat, Bindings) -> $Pat {
            pattern: $Pat,
            init_mcx: MatchCtxt<'a, 'hir, 'gcx, 'tcx>,
            callback: F,
        }

        impl<'a, 'hir, 'gcx, 'tcx, F> Folder for $PatternFolder<'a, 'hir, 'gcx, 'tcx, F>
                where F: FnMut($Pat, Bindings) -> $Pat {
            fn $fold_thing(&mut $slf, $arg: $ArgTy) -> $RetTy {
                let $arg = $walk;
                let mut $match_one = |x| {
                    if let Ok(mcx) = $slf.init_mcx.clone_match(&$slf.pattern, &x) {
                        ($slf.callback)(x, mcx.bindings)
                    } else {
                        x
                    }
                };
                $map
            }
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

    fn fold_expr(&mut self, e: P<Expr>) -> P<Expr>;
    walk = e.map(|e| fold::noop_fold_expr(e, self));
    map(match_one) = match_one(e);
}

gen_pattern_impl! {
    pattern = P<Ty>;
    folder = TyPatternFolder;

    fn fold_ty(&mut self, t: P<Ty>) -> P<Ty>;
    walk = fold::noop_fold_ty(t, self);
    map(match_one) = match_one(t);
}

gen_pattern_impl! {
    pattern = Stmt;
    folder = StmtPatternFolder;

    fn fold_stmt(&mut self, s: Stmt) -> SmallVector<Stmt>;
    walk = fold::noop_fold_stmt(s, self);
    map(match_one) = s.move_map(match_one);
}

pub struct MultiStmtPatternFolder<'a, 'hir: 'a, 'gcx: 'tcx + 'a, 'tcx: 'a, F>
        where F: FnMut(Vec<Stmt>, Bindings) -> Vec<Stmt> {
    pattern: Vec<Stmt>,
    init_mcx: MatchCtxt<'a, 'hir, 'gcx, 'tcx>,
    callback: F,
}

impl<'a, 'hir, 'gcx, 'tcx, F> Folder for MultiStmtPatternFolder<'a, 'hir, 'gcx, 'tcx, F>
        where F: FnMut(Vec<Stmt>, Bindings) -> Vec<Stmt> {
    fn fold_block(&mut self, b: P<Block>) -> P<Block> {
        assert!(self.pattern.len() > 0);

        let b = fold::noop_fold_block(b, self);

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

        if last == 0 {
            b
        } else {
            new_stmts.extend_from_slice(&b.stmts[last ..]);
            b.map(|b| Block { stmts: new_stmts, ..b })
        }
    }
}

pub fn match_multi_stmt(mcx: &mut MatchCtxt, pattern: &[Stmt], target: &[Stmt]) -> Option<usize> {
    if pattern.len() == 0 {
        return Some(0);
    }

    if is_multi_stmt_glob(mcx, &pattern[0]) {
        let name = pattern[0].pattern_symbol().unwrap();
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
    let sym = match pattern.pattern_symbol() {
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
pub fn fold_match<P, T, F>(st: &CommandState,
                           cx: &driver::Ctxt,
                           pattern: P,
                           target: T,
                           callback: F) -> <T as Fold>::Result
        where P: Pattern,
              T: Fold,
              F: FnMut(P, Bindings) -> P {
    fold_match_with(MatchCtxt::new(st, cx), pattern, target, callback)
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
