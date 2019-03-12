//! AST pattern matching implementation.
//!
//! The matching in this module allows matching one AST fragment against another fragment of the
//! same type.  The "pattern" fragment can use some special forms to capture parts of the target
//! AST, or to impose additional requirements on the matching.
//!
//! Matching forms:
//!
//!  * `__x`: An ident starting with double underscores will capture the AST matched against it,
//!    and save it in the `Bindings`.  Other idents can also be used if the `MatchCtxt` is
//!    appropriately configured first.
//!
//!    By default, this form captures the largest AST that it encounters.  For example, if the
//!    target AST is `MyStruct`, it will try first to capture the entire Expr, then try the inner
//!    Path, then the innermost Ident.  Normally the first attempt succeeds, but if a type is set
//!    for the ident in the `MatchCtxt`, then it will only capture that type.
//!
//!    For itemlikes, a lone ident can't be used as a pattern because it's not a valid itemlike.
//!    Use a zero-argument macro invocation `__x!()` instead.
//!
//!  * `marked!(x [, label])`: Matches `x` only if the node is marked with the given label.  The
//!    label defaults to "target" if omitted.
//!
//!  * `def!(path)`: Matches a path `Expr` or `Ty` that refers to a definition whose absolute path
//!    is `path`.  Specifically, the path of the definition is converted back to an AST using the
//!    `reflect` module, and the new AST is matched against `path`.
//!
//!  * `typed!(x, ty)`: Matches an `Expr` or `Ty` whose resolved type matches `ty`.  Specifically,
//!    the resolved type of the node is converted back to an AST using the `reflect` module, and
//!    the new AST is matched against `ty`.
//!
//!  * `cast!(x)`: Matches the `Expr`s `x`, `x as __t`, `x as __t as __u`, etc.

use rustc::hir::def_id::DefId;
use rustc::session::Session;
use smallvec::SmallVec;
use std::cmp;
use std::result;
use std::path::PathBuf;
use syntax::ast::{Block, Expr, ExprKind, Ident, Item, Label, Pat, Path, Stmt, Ty};
use syntax::mut_visit::{self, MutVisitor};
use syntax::parse::parser::{Parser, PathStyle};
use syntax::parse::token::Token;
use syntax::parse::{self, PResult};
use syntax::ptr::P;
use syntax::symbol::Symbol;
use syntax::tokenstream::TokenStream;
use syntax_pos::FileName;

use crate::ast_manip::util::PatternSymbol;
use crate::ast_manip::{MutVisit, GetNodeId, remove_paren};
use crate::command::CommandState;
use crate::driver::{self, emit_and_panic};
use crate::RefactorCtxt;
use crate::reflect;
use c2rust_ast_builder::IntoSymbol;

mod bindings;
mod impls;
mod subst;

pub use self::bindings::{Bindings, BindingTypes, Type as BindingType, parse_bindings};
pub use self::subst::Subst;


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

    /// A `typed!` macro failed to match because the type of the target expression was not
    /// available.
    TypeUnavailable,

    BadSpecialPattern(Symbol),
}

/// Pattern-matching context.  Stores configuration that affects pattern matching behavior, and
/// collects bindings captured during the match.
#[derive(Clone)]
pub struct MatchCtxt<'a, 'tcx: 'a> {
    pub bindings: Bindings,
    pub types: BindingTypes,
    st: &'a CommandState,
    cx: &'a RefactorCtxt<'a, 'tcx>,
    pub debug: bool,
}

impl<'a, 'tcx> MatchCtxt<'a, 'tcx> {
    pub fn new(st: &'a CommandState,
               cx: &'a RefactorCtxt<'a, 'tcx>) -> MatchCtxt<'a, 'tcx> {
        MatchCtxt {
            bindings: Bindings::new(),
            types: BindingTypes::new(),
            st: st,
            cx: cx,
            debug: false,
        }
    }


    pub fn parse_expr(&mut self, src: &str) -> P<Expr> {
        let (mut p, bt) = make_bindings_parser(self.cx.session(), "<expr>", src);
        match p.parse_expr() {
            Ok(mut expr) => {
                self.types.merge(bt);
                remove_paren(&mut expr);
                expr
            }
            Err(db) => emit_and_panic(db, "expr"),
        }
    }

    pub fn parse_pat(&mut self, src: &str) -> P<Pat> {
        let (mut p, bt) = make_bindings_parser(self.cx.session(), "<pat>", src);
        match p.parse_pat(None) {
            Ok(mut pat) => {
                self.types.merge(bt);
                remove_paren(&mut pat);
                pat
            }
            Err(db) => emit_and_panic(db, "pat"),
        }
    }

    pub fn parse_ty(&mut self, src: &str) -> P<Ty> {
        let (mut p, bt) = make_bindings_parser(self.cx.session(), "<ty>", src);
        match p.parse_ty() {
            Ok(mut ty) => {
                self.types.merge(bt);
                remove_paren(&mut ty);
                ty
            }
            Err(db) => emit_and_panic(db, "ty"),
        }
    }

    pub fn parse_stmts(&mut self, src: &str) -> Vec<Stmt> {
        // TODO: rustc no longer exposes `parse_full_stmt`. `parse_block` is a hacky
        // workaround that may cause suboptimal error messages.
        let (mut p, bt) = make_bindings_parser(self.cx.session(), "<stmt>", &format!("{{ {} }}", src));
        match p.parse_block() {
            Ok(blk) => {
                self.types.merge(bt);
                let mut stmts = blk.into_inner().stmts;
                for s in stmts.iter_mut() {
                    remove_paren(s);
                }
                stmts
            }
            Err(db) => emit_and_panic(db, "stmts"),
        }
    }

    pub fn parse_items(&mut self, src: &str) -> Vec<P<Item>> {
        let (mut p, bt) = make_bindings_parser(self.cx.session(), "<item>", src);
        let mut items = Vec::new();
        loop {
            match p.parse_item() {
                Ok(Some(mut item)) => {
                    remove_paren(&mut item);
                    items.push(item);
                }
                Ok(None) => break,
                Err(db) => emit_and_panic(db, "items"),
            }
        }
        self.types.merge(bt);
        items
    }

    /// Try to match `target` against `pat`, updating `self.bindings` with the results.
    pub fn try_match<T: TryMatch>(&mut self, pat: &T, target: &T) -> Result<()> {
        let r = pat.try_match(target, self);
        r
    }

    /// Build a new `MatchCtxt`, and try to match `target` against `pat` in that context.
    pub fn from_match<T: TryMatch>(st: &'a CommandState,
                                   cx: &'a RefactorCtxt<'a, 'tcx>,
                                   pat: &T,
                                   target: &T) -> Result<MatchCtxt<'a, 'tcx>> {
        let mut m = MatchCtxt::new(st, cx);
        m.try_match(pat, target)?;
        Ok(m)
    }

    /// Clone this context and try to perform a match in the clone, returning `Ok` if it succeeds.
    pub fn clone_match<T: TryMatch>(&self, pat: &T, target: &T)
                                    -> Result<MatchCtxt<'a, 'tcx>> {
        let mut m = self.clone();
        m.try_match(pat, target)?;
        Ok(m)
    }

    pub fn set_type<S: IntoSymbol>(&mut self, name: S, ty: BindingType) {
        let name = name.into_symbol();
        if let Some(old_ty) = self.bindings.get_type(name) {
            assert!(ty == old_ty || ty == BindingType::Unknown || old_ty == BindingType::Unknown,
                    "tried to set type of {:?} to {:?}, but it already has a value of type {:?}",
                    name, ty, old_ty);
        }

        self.types.set_type(name, ty)
    }

    fn is_opt_binding<P: PatternSymbol>(&self, pattern: &P) -> bool {
        let sym = match pattern.pattern_symbol() {
            Some(x) => x,
            None => return false,
        };
        match self.types.get(&sym) {
            Some(&bindings::Type::Optional(_)) => true,
            _ => false,
        }
    }

    fn capture_opt_none<P: PatternSymbol>(&mut self, pattern: &P) -> Result<()> {
        let sym = match pattern.pattern_symbol() {
            Some(x) => x,
            None => panic!("should never reach this"),
        };
        match self.types.get(&sym) {
            Some(&bindings::Type::Optional(_)) => {
                let ok = self.bindings.try_add_none(sym);
                if ok { Ok(()) } else { Err(Error::NonlinearMismatch) }
            }
            bt @ _ => panic!("expected optional binding, got {:?}", bt)
        }
    }
    /// Try to capture an ident.  Returns `Ok(true)` if it captured, `Ok(false)` if `pattern` is
    /// not a capturing pattern, or `Err(_)` if capturing failed.
    pub fn maybe_capture_ident(&mut self, pattern: &Ident, target: &Ident) -> Result<bool> {
        let sym = match pattern.pattern_symbol() {
            Some(x) => x,
            None => return Ok(false),
        };

        // This is a valid ident pattern if it was explicitly given type "ident".  Or, if its name
        // starts with "__", then it's a valid pattern for any binding type.
        match self.types.get(&sym) {
            Some(&bindings::Type::Optional(bindings::Type::Ident)) => {
                let ok = self.bindings.try_add(sym, Some(target.clone()));
                let res = if ok { Ok(true) } else { Err(Error::NonlinearMismatch) };
                return res;
            }

            Some(&bindings::Type::Ident) => {},
            Some(&bindings::Type::Unknown) => {},
            None if sym.as_str().starts_with("__") => {},
            _ => return Ok(false),
        }

        let ok = self.bindings.try_add(sym, target.clone());
        if ok { Ok(true) } else { Err(Error::NonlinearMismatch) }
    }

    pub fn maybe_capture_label(&mut self, pattern: &Label, target: &Label) -> Result<bool> {
        let sym = match pattern.pattern_symbol() {
            Some(x) => x,
            None => return Ok(false),
        };

        // Labels use lifetime syntax, but are `Ident`s instead of `Lifetime`s.
        match self.types.get(&sym) {
            Some(&bindings::Type::Optional(bindings::Type::Ident)) => {
                let ok = self.bindings.try_add(sym, Some(target.ident.clone()));
                let res = if ok { Ok(true) } else { Err(Error::NonlinearMismatch) };
                return res;
            }

            Some(&bindings::Type::Ident) => {},
            Some(&bindings::Type::Unknown) => {},
            None if sym.as_str().starts_with("'__") => {},
            _ => return Ok(false),
        }

        let ok = self.bindings.try_add(sym, target.ident.clone());
        if ok { Ok(true) } else { Err(Error::NonlinearMismatch) }
    }

    pub fn maybe_capture_path(&mut self, pattern: &Path, target: &Path) -> Result<bool> {
        let sym = match pattern.pattern_symbol() {
            Some(x) => x,
            None => return Ok(false),
        };

        match self.types.get(&sym) {
            Some(&bindings::Type::Path) => {},
            Some(&bindings::Type::Unknown) => {},
            None if sym.as_str().starts_with("__") => {},
            _ => return Ok(false),
        }

        let ok = self.bindings.try_add(sym, target.clone());
        if ok { Ok(true) } else { Err(Error::NonlinearMismatch) }
    }

    pub fn maybe_capture_expr(&mut self, pattern: &Expr, target: &Expr) -> Result<bool> {
        let sym = match pattern.pattern_symbol() {
            Some(x) => x,
            None => return Ok(false),
        };

        match self.types.get(&sym) {
            Some(&bindings::Type::Optional(bindings::Type::Expr)) => {
                let ok = self.bindings.try_add(sym, Some(P(target.clone())));
                let res = if ok { Ok(true) } else { Err(Error::NonlinearMismatch) };
                return res;
            }

            Some(&bindings::Type::Expr) => {},
            Some(&bindings::Type::Unknown) => {},
            None if sym.as_str().starts_with("__") => {},
            _ => return Ok(false),
        }

        let ok = self.bindings.try_add(sym, P(target.clone()));
        if ok { Ok(true) } else { Err(Error::NonlinearMismatch) }
    }

    pub fn maybe_capture_pat(&mut self, pattern: &Pat, target: &Pat) -> Result<bool> {
        let sym = match pattern.pattern_symbol() {
            Some(x) => x,
            None => return Ok(false),
        };

        match self.types.get(&sym) {
            Some(&bindings::Type::Pat) => {},
            Some(&bindings::Type::Unknown) => {},
            None if sym.as_str().starts_with("__") => {},
            _ => return Ok(false),
        }

        let ok = self.bindings.try_add(sym, P(target.clone()));
        if ok { Ok(true) } else { Err(Error::NonlinearMismatch) }
    }

    pub fn maybe_capture_ty(&mut self, pattern: &Ty, target: &Ty) -> Result<bool> {
        let sym = match pattern.pattern_symbol() {
            Some(x) => x,
            None => return Ok(false),
        };

        match self.types.get(&sym) {
            Some(&bindings::Type::Optional(bindings::Type::Ty)) => {
                let ok = self.bindings.try_add(sym, Some(P(target.clone())));
                let res = if ok { Ok(true) } else { Err(Error::NonlinearMismatch) };
                return res;
            }

            Some(&bindings::Type::Ty) => {},
            Some(&bindings::Type::Unknown) => {},
            None if sym.as_str().starts_with("__") => {},
            _ => return Ok(false),
        }

        let ok = self.bindings.try_add(sym, P(target.clone()));
        if ok { Ok(true) } else { Err(Error::NonlinearMismatch) }
    }

    pub fn maybe_capture_stmt(&mut self, pattern: &Stmt, target: &Stmt) -> Result<bool> {
        let sym = match pattern.pattern_symbol() {
            Some(x) => x,
            None => return Ok(false),
        };

        match self.types.get(&sym) {
            Some(&bindings::Type::Stmt) => {},
            Some(&bindings::Type::Unknown) => {},
            None if sym.as_str().starts_with("__") => {},
            _ => return Ok(false),
        }

        let ok = self.bindings.try_add(sym, target.clone());
        if ok { Ok(true) } else { Err(Error::NonlinearMismatch) }
    }

    // If you want to be able to capture more types of nodes with `__x` / `__x!()` forms, then add
    // another method here, add a new `TryMatch` impl in `matcher_impls`, and mark the AST type
    // with `#[match=custom]` in ast.txt.  You may also need to add a new `PatternSymbol` impl in
    // util.rs.


    /// Handle the `marked!(...)` matching form.
    pub fn do_marked<T, F>(&mut self,
                           tts: &TokenStream,
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

    /// Core implementation of the `def!(...)` matching form.
    fn do_def_impl(&mut self,
                   tts: &TokenStream,
                   style: PathStyle,
                   opt_def_id: Option<DefId>) -> Result<()> {
        let mut p = Parser::new(&self.cx.session().parse_sess,
                                tts.clone().into(),
                                None, false, false);
        let path_pattern = p.parse_path(style).unwrap();

        let def_id = match_or!([opt_def_id] Some(x) => x;
                               return Err(Error::DefMismatch));
        // TODO: We currently ignore the QSelf.  This means `<S as T>::f` gets matched as just
        // `T::f`.  This would be a little annoying to fix, since `parse_qpath` is private.
        let (_qself, def_path) = reflect::reflect_def_path(self.cx.ty_ctxt(), def_id);

        if self.debug {
            eprintln!("def!(): trying to match pattern {:?} against AST {:?}",
                      path_pattern, def_path);
        }
        if self.try_match(&path_pattern, &def_path).is_err() {
            return Err(Error::DefMismatch);
        }

        Ok(())
    }

    /// Handle the `def!(...)` matching form for exprs.
    pub fn do_def_expr(&mut self, tts: &TokenStream, target: &Expr) -> Result<()> {
        let opt_def_id = self.cx.try_resolve_expr(target);
        self.do_def_impl(tts, PathStyle::Expr, opt_def_id)
    }

    /// Handle the `def!(...)` matching form for exprs.
    pub fn do_def_ty(&mut self, tts: &TokenStream, target: &Ty) -> Result<()> {
        let opt_def_id = self.cx.try_resolve_ty(target);
        self.do_def_impl(tts, PathStyle::Type, opt_def_id)
    }

    /// Handle the `typed!(...)` matching form.
    pub fn do_typed<T, F>(&mut self,
                          tts: &TokenStream,
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

        let tcx_ty = self.cx.opt_node_type(target.get_node_id())
            .ok_or(Error::TypeUnavailable)?;
        let ast_ty = reflect::reflect_tcx_ty(self.cx.ty_ctxt(), tcx_ty);

        if self.debug {
            eprintln!("typed!(): trying to match pattern {:?} against AST {:?}",
                      ty_pattern, ast_ty);
        }
        if self.try_match(&ty_pattern, &ast_ty).is_err() {
            return Err(Error::WrongType);
        }

        self.try_match(&pattern, target)
    }

    pub fn do_cast<F>(&mut self, tts: &TokenStream, func: F, target: &Expr) -> Result<()>
            where F: for<'b> FnOnce(&mut Parser<'b>) -> PResult<'b, P<Expr>> {
        let ts: TokenStream = tts.clone().into();
        let pattern = driver::run_parser_tts(self.cx.session(), ts.into_trees().collect(), func);

        let mut target = target;
        loop {
            // Try to match `pattern` with `target`.  On error, if `target` is a cast expression,
            // try again underneath the cast.
            let old_bnd = self.bindings.clone();
            let err = match self.try_match::<Expr>(&pattern, target) {
                Ok(()) => return Ok(()),
                Err(err) => err,
            };
            self.bindings = old_bnd;

            target = match target.node {
                ExprKind::Cast(ref e, _) => e,
                _ => return Err(err),
            };
        }
    }
}

fn make_bindings_parser<'a>(sess: &'a Session, name: &str, src: &str) -> (Parser<'a>, BindingTypes) {
    let (ts, _) =
        parse::parse_stream_from_source_str(FileName::Real(PathBuf::from(name)),
                                            src.to_owned(),
                                            &sess.parse_sess,
                                            None);
    let (ts, bt) = parse_bindings(ts);
    (parse::stream_to_parser(&sess.parse_sess, ts), bt)
}

pub trait TryMatch {
    fn try_match(&self, target: &Self, mcx: &mut MatchCtxt) -> Result<()>;
}



/// Trait for AST types that can be used as patterns in a search-and-replace (`mut_visit_match`).
pub trait Pattern<V>: TryMatch+Sized {
    fn visit<'a, 'tcx, T, F>(
        self,
        _init_mcx: MatchCtxt<'a, 'tcx>,
        _callback: F,
        _target: &mut T,
    )
    where T: MutVisit,
          F: FnMut(&mut V, MatchCtxt<'a, 'tcx>) {}

    fn flat_map<'a, 'tcx, T, F>(
        self,
        _init_mcx: MatchCtxt<'a, 'tcx>,
        _callback: F,
        _target: &mut T,
    )
    where T: MutVisit,
          F: FnMut(V, MatchCtxt<'a, 'tcx>) -> SmallVec<[V; 1]> {}
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
        /// Automatically generated `Folder` implementation, for use by `Pattern`.
        pub struct $PatternFolder<'a, 'tcx: 'a, F>
                where F: FnMut(&mut $Pat, MatchCtxt<'a, 'tcx>) {
            pattern: $Pat,
            init_mcx: MatchCtxt<'a, 'tcx>,
            callback: F,
        }

        impl<'a, 'tcx, F> MutVisitor for $PatternFolder<'a, 'tcx, F>
                where F: FnMut(&mut $Pat, MatchCtxt<'a, 'tcx>) {
            #[allow(unused_mut)]
            fn $fold_thing(&mut $slf, $arg: $ArgTy) -> $RetTy {
                let $arg = $walk;
                let mut $match_one = |x: &mut $ArgTy| {
                    if let Ok(mcx) = $slf.init_mcx.clone_match(&$slf.pattern, &x) {
                        ($slf.callback)(x, mcx)
                    }
                };
                $map
            }
        }

        impl Pattern<$Pat> for $Pat {
            fn visit<'a, 'tcx, T, F>(
                self,
                init_mcx: MatchCtxt<'a, 'tcx>,
                callback: F,
                target: &mut T,
            )
            where T: MutVisit,
                  F: FnMut(&mut Self, MatchCtxt<'a, 'tcx>)
            {
                let mut f = $PatternFolder {
                    pattern: self,
                    init_mcx: init_mcx,
                    callback: callback,
                };
                target.visit(&mut f)
            }
        }
    };
    (
        pattern = $Pat:ty;
        folder = $PatternFolder:ident;

        // Capture the ident "self" from the outer context, so it can be used in the expressions.
        fn $fold_thing:ident ( &mut $slf:ident , $arg:ident : &mut $ArgTy:ty );
        walk = $walk:expr;
        map($match_one:ident) = $map:expr;
    ) => {
        /// Automatically generated `Folder` implementation, for use by `Pattern`.
        pub struct $PatternFolder<'a, 'tcx: 'a, F>
                where F: FnMut(&mut $Pat, MatchCtxt<'a, 'tcx>) {
            pattern: $Pat,
            init_mcx: MatchCtxt<'a, 'tcx>,
            callback: F,
        }

        impl<'a, 'tcx, F> MutVisitor for $PatternFolder<'a, 'tcx, F>
            where F: FnMut(&mut $Pat, MatchCtxt<'a, 'tcx>)
        {
            #[allow(unused_mut)]
            fn $fold_thing(&mut $slf, $arg: &mut $ArgTy) {
                $walk;
                let mut $match_one = |x: &mut $ArgTy| {
                    if let Ok(mcx) = $slf.init_mcx.clone_match(&$slf.pattern, &x) {
                        ($slf.callback)(x, mcx);
                    }
                };
                $map
            }
        }

        impl Pattern<$Pat> for $Pat {
            fn visit<'a, 'tcx, T, F>(
                self,
                init_mcx: MatchCtxt<'a, 'tcx>,
                callback: F,
                target: &mut T,
            )
            where T: MutVisit,
                  F: FnMut(&mut Self, MatchCtxt<'a, 'tcx>)
            {
                let mut f = $PatternFolder {
                    pattern: self,
                    init_mcx: init_mcx,
                    callback: callback,
                };
                target.visit(&mut f)
            }
        }
    };
}

gen_pattern_impl! {
    // AST node type.
    pattern = P<Expr>;
    // Name to use for the search-and-replace folder.
    folder = ExprPatternFolder;

    // Signature of the corresponding `Folder` method.
    fn visit_expr(&mut self, e: &mut P<Expr>);
    // Expr that runs the default `Folder` action for this node type.  Can refer to the argument of
    // the `Folder` method using the name that appears in the signature above.
    walk = mut_visit::noop_visit_expr(e, self);
    // Expr that runs the callback on the result of the `walk` expression.  This is parameterized
    // by the `match_one` closure.
    map(match_one) = match_one(e);
}

gen_pattern_impl! {
    pattern = P<Ty>;
    folder = TyPatternFolder;

    fn visit_ty(&mut self, t: &mut P<Ty>);
    walk = mut_visit::noop_visit_ty(t, self);
    map(match_one) = match_one(t);
}

gen_pattern_impl! {
    pattern = Stmt;
    folder = StmtPatternFolder;

    fn flat_map_stmt(&mut self, s: Stmt) -> SmallVec<[Stmt; 1]>;
    walk = mut_visit::noop_flat_map_stmt(s, self);
    map(match_one) = { let mut s = s; s.iter_mut().for_each(match_one); s };
}


// Implementation of multi-statement matching.

/// Custom `Folder` for multi-statement `Pattern`s.
pub struct MultiStmtPatternFolder<'a, 'tcx: 'a, F>
        where F: FnMut(&mut Vec<Stmt>, MatchCtxt<'a, 'tcx>) {
    pattern: Vec<Stmt>,
    init_mcx: MatchCtxt<'a, 'tcx>,
    callback: F,
}

impl<'a, 'tcx, F> MutVisitor for MultiStmtPatternFolder<'a, 'tcx, F>
        where F: FnMut(&mut Vec<Stmt>, MatchCtxt<'a, 'tcx>) {
    fn visit_block(&mut self, b: &mut P<Block>) {
        assert!(self.pattern.len() > 0);

        mut_visit::noop_visit_block(b, self);

        let mut new_stmts = Vec::with_capacity(b.stmts.len());
        let mut last = 0;

        let mut i = 0;
        while i < b.stmts.len() {
            let mut mcx = self.init_mcx.clone();
            let result = match_multi_stmt(&mut mcx, &self.pattern, &b.stmts[i..]);
            if let Some(consumed) = result {
                new_stmts.extend_from_slice(&b.stmts[last .. i]);

                let mut consumed_stmts = b.stmts[i .. i + consumed].to_owned();
                (self.callback)(&mut consumed_stmts, mcx);
                new_stmts.extend(consumed_stmts);

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

        if last != 0 {
            new_stmts.extend_from_slice(&b.stmts[last ..]);
            b.stmts = new_stmts;
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
                let ok = mcx.bindings.try_add(name, target[..i].to_owned());
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
        Some(&bindings::Type::MultiStmt) => {}, // FIXME: match Unknown too???
        None if sym.as_str().starts_with("__m_") => {},
        _ => return false,
    }

    true
}

impl Pattern<Vec<Stmt>> for Vec<Stmt> {
    fn visit<'a, 'tcx, T, F>(
        self,
        init_mcx: MatchCtxt<'a, 'tcx>,
        callback: F,
        target: &mut T,
    ) where T: MutVisit,
            F: FnMut(&mut Vec<Stmt>, MatchCtxt<'a, 'tcx>)
    {
        let mut f = MultiStmtPatternFolder {
            pattern: self,
            init_mcx: init_mcx,
            callback: callback,
        };
        target.visit(&mut f)
    }
}


/// Find every match for `pattern` within `target`, and rewrite each one by invoking `callback`.
pub fn mut_visit_match<P, T, F>(st: &CommandState,
                           cx: &RefactorCtxt,
                           pattern: P,
                           target: &mut T,
                           callback: F)
        where P: Pattern<P>,
              T: MutVisit,
              F: FnMut(&mut P, MatchCtxt) {
    mut_visit_match_with(MatchCtxt::new(st, cx), pattern, target, callback)
}

/// Find every match for `pattern` within `target`, and rewrite each one by invoking `callback`.
pub fn mut_visit_match_with<'a, 'tcx, P, T, V, F>(
    init_mcx: MatchCtxt<'a, 'tcx>,
    pattern: P,
    target: &mut T,
    callback: F,
)
where P: Pattern<V>,
      T: MutVisit,
      F: FnMut(&mut V, MatchCtxt<'a, 'tcx>)
{
    pattern.visit(init_mcx, callback, target)
}

pub fn flat_map_match_with<'a, 'tcx, P, T, V, F>(
    init_mcx: MatchCtxt<'a, 'tcx>,
    pattern: P,
    target: &mut T,
    callback: F,
)
where P: Pattern<V>,
      T: MutVisit,
      F: FnMut(V, MatchCtxt<'a, 'tcx>) -> SmallVec<[V; 1]>
{
    pattern.flat_map(init_mcx, callback, target)
}

/// Find the first place where `pattern` matches under initial context `init_mcx`, and return the
/// resulting `Bindings`.
pub fn find_first_with<P, T>(init_mcx: MatchCtxt,
                             pattern: P,
                             target: &mut T) -> Option<Bindings>
        where P: Pattern<P>, T: MutVisit {
    let mut result = None;
    mut_visit_match_with(init_mcx, pattern, target, |_p, mcx| {
        if result.is_none() {
            result = Some(mcx.bindings);
        }
    });
    result
}

/// Find the first place where `pattern` matches, and return the resulting `Bindings`.
pub fn find_first<P, T>(st: &CommandState,
                        cx: &RefactorCtxt,
                        pattern: P,
                        target: &mut T) -> Option<Bindings>
        where P: Pattern<P>, T: MutVisit {
    find_first_with(MatchCtxt::new(st, cx), pattern, target)
}

// TODO: find a better place to put this
/// Replace all instances of expression `pat` with expression `repl`.
pub fn replace_expr<T: MutVisit>(st: &CommandState,
                             cx: &RefactorCtxt,
                             ast: &mut T,
                             pat: &str,
                             repl: &str) {
    let mut mcx = MatchCtxt::new(st, cx);
    let pat = mcx.parse_expr(pat);
    let repl = mcx.parse_expr(repl);
    // TODO: Make Subst modify in place
    mut_visit_match_with(mcx, pat, ast, |x, mcx| *x = repl.clone().subst(st, cx, &mcx.bindings))
}

/// Replace all instances of the statement sequence `pat` with `repl`.
pub fn replace_stmts<T: MutVisit>(st: &CommandState,
                              cx: &RefactorCtxt,
                              ast: &mut T,
                              pat: &str,
                              repl: &str) {
    let mut mcx = MatchCtxt::new(st, cx);
    let pat = mcx.parse_stmts(pat);
    let repl = mcx.parse_stmts(repl);
    // TODO: Make Subst modify in place
    mut_visit_match_with(mcx, pat, ast, |x, mcx| *x = repl.clone().subst(st, cx, &mcx.bindings))
}
