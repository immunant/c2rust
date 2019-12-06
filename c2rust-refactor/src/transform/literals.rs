use ena::unify as ut;
use rustc::{hir, ty};
use rustc::hir::def::{Res, DefKind};
use rustc_data_structures::sync::Lrc;
use syntax::ast::*;
use syntax::parse::token;
use syntax::ptr::P;
use syntax::symbol::Symbol;
use syntax::visit::{self, Visitor};

use std::collections::HashMap;
use std::marker::PhantomData;

use crate::ast_manip::MutVisitNodes;
use crate::command::{CommandState, Registry};
use crate::driver::Phase;
use crate::transform::Transform;
use crate::transform::casts::sym_token_kind;
use crate::RefactorCtxt;


/// # `bytestr_to_str` Command
///
/// Usage: `bytestr_to_str`
///
/// Marks: `target`
///
/// Convert bytestring literal expressions marked `target` to string literal
/// expressions.
///
/// Note the mark must be placed on the expression, as it is currently difficult to
/// mark a literal node.
pub struct ByteStrToStr;

impl Transform for ByteStrToStr {
    fn transform(&self, krate: &mut Crate, st: &CommandState, _cx: &RefactorCtxt) {
        MutVisitNodes::visit(krate, |e: &mut P<Expr>| {
            if !st.marked(e.id, "target") {
                return;
            }

            match &mut e.kind {
                ExprKind::Lit(l) => {
                    match l.kind {
                        LitKind::ByteStr(ref bs) => {
                            let s = String::from_utf8((**bs).clone()).unwrap();
                            l.kind = LitKind::Str(Symbol::intern(&s), StrStyle::Cooked);
                            l.token.kind = token::LitKind::Str;
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        })
    }
}


/// # `remove_null_terminator` Command
///
/// Usage: `remove_null_terminator`
///
/// Marks: `target`
///
/// Remove a trailing `\0` character from marked string and bytestring literal
/// expressions.
///
/// Note the mark must be placed on the expression, as it is currently difficult to
/// mark a literal node.
pub struct RemoveNullTerminator;

fn strip_null(s: &mut Symbol) {
    assert!(s.as_str().ends_with('\0'));
    let end = s.as_str().len() - 1;
    *s = Symbol::intern(&s.as_str()[..end]);
}

impl Transform for RemoveNullTerminator {
    fn transform(&self, krate: &mut Crate, st: &CommandState, _cx: &RefactorCtxt) {
        MutVisitNodes::visit(krate, |e: &mut P<Expr>| {
            if !st.marked(e.id, "target") {
                return;
            }

            match &mut e.kind {
                ExprKind::Lit(l) => {
                    match &mut l.kind {
                        LitKind::ByteStr(bs) => {
                            if bs.last() == Some(&0) {
                                Lrc::get_mut(bs).unwrap().pop();
                                strip_null(&mut l.token.symbol);
                            }
                        }
                        LitKind::Str(ref mut s, _style) => {
                            if s.as_str().ends_with('\0') {
                                strip_null(s);
                            }
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        });
    }
}


/// # `remove_literal_suffixes` Command
///
/// Usage: `remove_literal_suffixes`
///
/// Remove suffixes from literals in cases where Rust type inference will infer
/// the correct types anyway. For example, in `1u64 + 2u64` at most one of the
/// literals needs a suffix.
pub struct RemoveLiteralSuffixes;

fn remove_suffix(lit: &Lit) -> Option<Lit> {
    match lit.kind {
        LitKind::Int(x, _) => Some(Lit {
            token: token::Lit { suffix: None, ..lit.token },
            kind: LitKind::Int(x, LitIntType::Unsuffixed),
            span: lit.span,
        }),

        LitKind::Float(sym, _) => match sym_token_kind(sym) {
            token::LitKind::Float => Some(Lit {
                token: token::Lit { suffix: None, ..lit.token },
                kind: LitKind::FloatUnsuffixed(sym),
                span: lit.span,
            }),

            // We can't remove suffixes on integer-like floats,
            // since that can cause a typeck error, e.g.,
            // `3f64 + 5` is not valid Rust code
            token::LitKind::Integer => None,

            k @ _ => panic!("unexpected token kind: {:?}", k)
        }

        _ => None
    }
}

impl Transform for RemoveLiteralSuffixes {
    fn transform(&self, krate: &mut Crate, _st: &CommandState, cx: &RefactorCtxt) {
        let mut uv = UnifyVisitor {
            cx,
            unif: ut::UnificationTable::new(),
            lit_nodes: HashMap::new(),
        };
        visit::walk_crate(&mut uv, &krate);

        MutVisitNodes::visit(krate, |e: &mut P<Expr>| {
            let id = e.id;
            match &mut e.kind {
                ExprKind::Lit(ref mut lit) => {
                    let key = match_or!([uv.lit_nodes.get(&id)]
                                        Some(x) => x; return);
                    let source = uv.unif.probe_value(*key);
                    match source {
                        LitTySource::Actual(_) => {
                            // We have a type somewhere else, so remove
                            // the suffix from this literal
                            if let Some(new_lit) = remove_suffix(&lit) {
                                *lit = new_lit;
                            }
                        }

                        LitTySource::Suffix(ty) => {
                            assert!(lit.kind.is_suffixed());
                            // We have a set of literals with no other type source,
                            // so the first one (this one) gets a suffix to make it typed,
                            // and the rest lose their suffixes
                            // TODO: if our current literal set is all floats,
                            // and there is one that's integer-like and we can't remove
                            // its suffix (see above), and at the same time it's safe to
                            // remove the suffix on the current one, then we should do that
                            uv.unif.unify_var_value(*key, LitTySource::Actual(ty))
                                .expect("failed to unify");

                            // Special case: if `ty` is `i32` or `f64`,
                            // then we can remove the suffix, since those
                            // are the default inference types
                            match ty.kind {
                                ty::TyKind::Int(IntTy::I32) |
                                ty::TyKind::Float(FloatTy::F64) => {
                                    if let Some(new_lit) = remove_suffix(&lit) {
                                        *lit = new_lit;
                                    }
                                }
                                _ => {}
                            }
                        }

                        LitTySource::None => {}
                    };
                }
                _ => {}
            }
        });
    }

    fn min_phase(&self) -> Phase {
        Phase::Phase3
    }
}

struct UnifyVisitor<'a, 'tcx: 'a> {
    cx: &'a RefactorCtxt<'a, 'tcx>,
    unif: ut::UnificationTable<ut::InPlace<LitTyKey<'tcx>>>,
    lit_nodes: HashMap<NodeId, LitTyKey<'tcx>>,
}

impl<'a, 'tcx> UnifyVisitor<'a, 'tcx> {
    fn new_key(&mut self, source: LitTySource<'tcx>) -> LitTyKey<'tcx> {
        self.unif.new_key(source)
    }

    /// Directly convert an `ast::Ty` to a `ty::Ty`, without any type inference
    /// or going through the typeck tables.
    fn ast_ty_to_source(&self, ty: &Ty) -> LitTySource<'tcx> {
        let node = match_or!([self.cx.hir_map().find(ty.id)] Some(x) => x;
                             return LitTySource::None);
        let t = match_or!([node] hir::Node::Ty(t) => t;
                          return LitTySource::None);
        LitTySource::from_hir_ty(t, self.cx.ty_ctxt())
    }

    fn visit_expr_unify(&mut self, ex: &Expr, key: LitTyKey<'tcx>) {
        let tcx = self.cx.ty_ctxt();
        match ex.kind {
            // TODO: handle Box<T> for unifiable inner types

            ExprKind::Array(ref exprs) => {
                let inner_key = self.new_key(LitTySource::None);
                for e in exprs {
                    self.visit_expr_unify(e, inner_key);
                }
                // TODO: also unify `inner_key` by peeling a layer off `key`
            }

            ExprKind::Call(ref callee, ref args) => {
                self.visit_expr(callee);
                // TODO: check if generic
                for _arg in args {
                    // TODO: get argument type
                    // TODO: unify argument with expression
                }
                // TODO: unify with callee return type
            }

            ExprKind::MethodCall(ref _segment, ref _args) => {
                // TODO: same as `Call`
            }

            // TODO: handle `Tup`; for that, we'd need to keep track
            // of nested types inside `TyKind::Tup` and unify them,
            // in a similar way to `LabeledTy` from `analysis::labeled_ty`

            ExprKind::Binary(ref op, ref lhs, ref rhs) => {
                // FIXME: handle overloads
                use BinOpKind::*;
                match op.node {
                    Add | Sub | Mul | Div | Rem | BitXor | BitAnd | BitOr => {
                        self.visit_expr_unify(lhs, key);
                        self.visit_expr_unify(rhs, key);
                    }

                    Shl | Shr => {
                        self.visit_expr_unify(lhs, key);
                        self.visit_expr(rhs);
                    }

                    Eq | Ne | Lt | Le | Gt | Ge => {
                        // Unify the lhs and rhs with each other, but not with the result
                        let inner_key = self.new_key(LitTySource::None);
                        self.visit_expr_unify(lhs, inner_key);
                        self.visit_expr_unify(rhs, inner_key);
                    }

                    And | Or => {
                        self.visit_expr(lhs);
                        self.visit_expr(rhs);
                    }
                }
            }

            ExprKind::Unary(ref op, ref e) => match op {
                UnOp::Not | UnOp::Neg => self.visit_expr_unify(e, key),
                _ => self.visit_expr(e)
            }

            ExprKind::Lit(ref lit) => {
                if ex.id != DUMMY_NODE_ID && lit.kind.is_suffixed() {
                    let source = LitTySource::from_lit_expr(ex, tcx);
                    let lit_key = self.new_key(source);
                    self.unif.unify_var_var(key, lit_key)
                        .expect("Actual(to unify");
                    self.lit_nodes.insert(ex.id, lit_key);
                }
            }

            ExprKind::Cast(ref e, ref ty) => {
                let source = self.ast_ty_to_source(ty);
                self.unif.unify_var_value(key, source)
                    .expect("failed to unify");

                self.visit_expr(e);
                self.visit_ty(ty);
            }

            ExprKind::Type(ref e, ref ty) => {
                let source = self.ast_ty_to_source(ty);
                self.unif.unify_var_value(key, source)
                    .expect("failed to unify");

                self.visit_expr_unify(e, key);
                self.visit_ty(ty);
            }

            // TODO: handle `Let` from `if/while let`

            ExprKind::If(ref cond, ref block, ref r#else) => {
                self.visit_expr(cond);
                self.visit_block_unify(block, key);
                if let Some(r#else) = r#else {
                    self.visit_expr_unify(r#else, key);
                }
            }

            // TODO: unify loops

            ExprKind::Match(ref e, ref arms) => {
                let pat_key = self.new_key(LitTySource::None);
                self.visit_expr_unify(e, pat_key);
                for arm in arms {
                    self.visit_pat_unify(&arm.pat, pat_key);
                    if let Some(ref guard) = arm.guard {
                        self.visit_expr(guard);
                    }
                    self.visit_expr_unify(&arm.body, key);
                    for attr in &arm.attrs {
                        self.visit_attribute(attr);
                    }
                }
            }

            // TODO: handle `Closure`

            ExprKind::Block(ref block, _) => {
                self.visit_block_unify(block, key);
            }

            // TODO: handle `Async`/`Await`

            ExprKind::Assign(ref lhs, ref rhs) => {
                let inner_key = self.new_key(LitTySource::None);
                self.visit_expr_unify(lhs, inner_key);
                self.visit_expr_unify(rhs, inner_key);
            }

            ExprKind::AssignOp(ref op, ref lhs, ref rhs) => {
                // FIXME: handle overloads
                use BinOpKind::*;
                match op.node {
                    Add | Sub | Mul | Div | Rem | BitXor | BitAnd | BitOr => {
                        let inner_key = self.new_key(LitTySource::None);
                        self.visit_expr_unify(lhs, inner_key);
                        self.visit_expr_unify(rhs, inner_key);
                    }

                    _ => {
                        self.visit_expr(lhs);
                        self.visit_expr(rhs);
                    }
                }
            }

            ExprKind::Field(ref e, ident) => {
                self.visit_expr(e);
                self.visit_ident(ident);
                if let Some(struct_ty) = self.cx.opt_adjusted_node_type(e.id) {
                    if let ty::TyKind::Adt(def, _) = struct_ty.kind {
                        for field in def.all_fields() {
                            if field.ident == ident {
                                let hir_id = tcx.hir().as_local_hir_id(field.did);
                                let field_source = if let Some(hir_id) = hir_id {
                                    // Field is defined in our crate, look it up
                                    match tcx.hir().get(hir_id) {
                                        hir::Node::Field(field) =>
                                            LitTySource::from_hir_ty(&field.ty, tcx),
                                        _ => LitTySource::None
                                    }
                                } else {
                                    // Field is defined in another crate
                                    LitTySource::from_ty(tcx.type_of(field.did))
                                };
                                self.unif.unify_var_value(key, field_source)
                                    .expect("failed to unify")
                            }
                        }
                    }
                    // TODO: handle tuples
                }
            }

            ExprKind::Index(ref e, ref idx) => {
                // FIXME: we should unify with the inner type of the aggregate,
                // but that's too complicated right now
                self.visit_expr(e);

                // We unify `idx` with `usize`, but only after making sure
                // that it's the expected type; we do this in a hacky way:
                // we assume that if the type of the expression was inferred
                // to `usize` with suffixes, then it will stay the same without
                // them; this should generally be true, since the expected type
                // is inferred from the parameter of `Index`/`IndexMut`
                let mut idx_source = LitTySource::None;
                if let Some(idx_ty) = self.cx.opt_node_type(idx.id) {
                    match idx_ty.kind {
                        ty::TyKind::Int(IntTy::Isize) |
                        ty::TyKind::Uint(UintTy::Usize) => {
                            idx_source = LitTySource::from_ty(idx_ty);
                        }
                        _ => {}
                    }
                }

                let idx_key = self.new_key(idx_source);
                self.visit_expr_unify(idx, idx_key);
            }

            ExprKind::Range(ref start, ref end, _) => {
                let inner_key = self.new_key(LitTySource::None);
                if let Some(start) = start {
                    self.visit_expr_unify(start, inner_key);
                }
                if let Some(end) = end {
                    self.visit_expr_unify(end, inner_key);
                }
            }

            ExprKind::Path(..) => {
                visit::walk_expr(self, ex);
                // Try to resolve this path to a local variable
                if let Some(res) = self.cx.try_resolve_expr_hir(ex) {
                    match res {
                        Res::Local(id) => {
                            // This is a local variable that may have a type,
                            // try to get that type and unify it
                            let pid = tcx.hir().get_parent_node(id);
                            let node = match_or!([tcx.hir().find(pid)]
                                                 Some(x) => x; return);
                            let l = match_or!([node] hir::Node::Local(l) => l; return);
                            let lty = match_or!([l.ty] Some(ref lty) => lty; return);
                            let source = LitTySource::from_hir_ty(lty, tcx);
                            self.unif.unify_var_value(key, source)
                                .expect("failed to unify");
                        }

                        Res::Def(_, _did) | Res::SelfCtor(_did) => {
                            // TODO: this is a static, const or external
                        }

                        _ => {}
                    }
                }
            }

            // TODO: handle `AddrOf` by keeping track of pointers
            //
            // TODO: unify `Break` with return values
            //
            // TODO: unify `Ret` with function return type
            //
            // TODO: unify non-generic `Struct`

            // TODO: handle `Repeat` by handling aggregates

            ExprKind::Paren(ref e) => self.visit_expr_unify(e, key),

            // TODO: handle `Try`
            //
            // TODO: handle `Yield`

            _ => visit::walk_expr(self, ex)
        }
    }

    fn visit_block_unify(&mut self, b: &Block, key: LitTyKey<'tcx>) {
        if let Some((last, stmts)) = b.stmts.split_last() {
            for stmt in stmts {
                self.visit_stmt(stmt);
            }
            if let StmtKind::Expr(ref e) = last.kind {
                self.visit_expr_unify(e, key);
            }
        }
    }

    fn visit_pat_unify(&mut self, p: &Pat, key: LitTyKey<'tcx>) {
        match p.kind {
            PatKind::Or(ref pats) => {
                for pat in pats {
                    self.visit_pat_unify(pat, key);
                }
            }

            PatKind::Lit(ref e) => {
                self.visit_expr_unify(e, key);
            }

            PatKind::Range(ref start, ref end, _) => {
                let inner_key = self.new_key(LitTySource::None);
                self.visit_expr_unify(start, inner_key);
                self.visit_expr_unify(end, inner_key);
            }

            PatKind::Paren(ref pat) => self.visit_pat_unify(pat, key),

            // TODO: handle more pattern types once we have hierarchical keys

            _ => visit::walk_pat(self, p)
        }
    }

    // TODO: handle `Field`
    //
    // TODO: handle `AnonConst`
}

impl<'ast, 'a, 'tcx> Visitor<'ast> for UnifyVisitor<'a, 'tcx> {
    fn visit_expr(&mut self, ex: &'ast Expr) {
        let key = self.new_key(LitTySource::None);
        self.visit_expr_unify(ex, key);
    }

    fn visit_local(&mut self, l: &'ast Local) {
        for attr in l.attrs.iter() {
            self.visit_attribute(attr);
        }
        self.visit_pat(&l.pat);

        let source = if let Some(ref ty) = l.ty {
            self.visit_ty(ty);
            self.ast_ty_to_source(ty)
        } else {
            LitTySource::None
        };
        if let Some(ref init) = l.init {
            let key = self.new_key(source);
            self.visit_expr_unify(init, key);
        }
    }

    fn visit_item(&mut self, i: &'ast Item) {
        match i.kind {
            ItemKind::Static(ref ty, _, ref init) |
            ItemKind::Const(ref ty, ref init) => {
                let source = self.ast_ty_to_source(ty);
                let key = self.new_key(source);
                self.visit_ty(ty);
                self.visit_expr_unify(init, key);
            }

            _ => visit::walk_item(self, i)
        }
    }

    fn visit_arm(&mut self, _arm: &'ast Arm) {
        panic!("Arm nodes should be handled in visit_expr_unify");
    }

    fn visit_pat(&mut self, p: &'ast Pat) {
        let key = self.new_key(LitTySource::None);
        self.visit_pat_unify(p, key);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct LitTyKey<'tcx>(u32, PhantomData<LitTySource<'tcx>>);

impl<'tcx> ut::UnifyKey for LitTyKey<'tcx> {
    type Value = LitTySource<'tcx>;
    fn index(&self) -> u32 { self.0 }
    fn from_index(i: u32) -> Self { Self(i, PhantomData) }
    fn tag() -> &'static str { "LitTyKey" }
}

/// Source for the type information for the current `LitTyKey`.
#[derive(Debug, Clone, Copy)]
enum LitTySource<'tcx> {
    /// No type information for this key.
    None,

    /// This type came from a suffix. If we get all our type information from
    /// suffixes, that means we need to keep one literal suffix around to maintain
    /// the type, and throw out all the rest.
    Suffix(ty::Ty<'tcx>),

    /// We got this type from an actual value, e.g., a structure field or
    /// an explicitly typed local. If we get one of these, we can throw out
    /// the suffixes for all literals in this key.
    Actual(ty::Ty<'tcx>),
}

impl<'tcx> LitTySource<'tcx> {
    /// Directly convert a `hir::Ty` to `ty::Ty`
    fn from_hir_ty(ty: &hir::Ty, tcx: ty::TyCtxt<'tcx>) -> LitTySource<'tcx> {
        let path = match_or!([ty.kind]
                             hir::TyKind::Path(hir::QPath::Resolved(None, ref path)) => path;
                             return LitTySource::None);
        // TODO: handle TypeRelative paths
        // TODO: handle fully-qualified paths
        match path.res {
            Res::Def(DefKind::TyAlias, did) => {
                // TODO: check generics
                let hir_id = if let Some(hir_id) = tcx.hir().as_local_hir_id(did) {
                    hir_id
                } else {
                    // The type is from outside the local crate,
                    // which we don't touch, so we can just get it from rustc
                    // FIXME: is this always correct, and do we always want to do it???
                    let ty = tcx.at(path.span).type_of(did);
                    return LitTySource::from_ty(ty);
                };

                let item = match_or!([tcx.hir().get(hir_id)]
                                     hir::Node::Item(item) => item;
                                     return LitTySource::None);
                // TODO: handle `ImplItem`
                let (ty, _) =
                    match_or!([item.kind]
                              hir::ItemKind::TyAlias(ref ty, ref generics) => (ty, generics);
                              return LitTySource::None);
                // TODO: check generics
                Self::from_hir_ty(ty, tcx)
            }

            Res::PrimTy(prim_ty) => match prim_ty {
                // TODO: check generics
                hir::Int(it) => LitTySource::Actual(tcx.mk_mach_int(it)),
                hir::Uint(uit) => LitTySource::Actual(tcx.mk_mach_uint(uit)),
                hir::Float(ft) => LitTySource::Actual(tcx.mk_mach_float(ft)),
                _ => LitTySource::None
            }

            // TODO: when we have a tree-of-sources, handle them here

            _ => LitTySource::None
        }
    }

    fn from_ty(ty: ty::Ty<'tcx>) -> Self {
        match ty.kind {
            ty::TyKind::Int(_) |
            ty::TyKind::Uint(_) |
            ty::TyKind::Float(_) => LitTySource::Actual(ty),

            _ => LitTySource::None
        }
    }

    fn from_lit_expr(e: &Expr, tcx: ty::TyCtxt<'tcx>) -> Self {
        let lit = match e.kind {
            ExprKind::Lit(ref lit) => lit,
            _ => panic!("expected ExprKind::Lit, got {:?}", e)
        };
        match lit.kind {
            LitKind::Int(_, LitIntType::Signed(int_ty)) =>
                LitTySource::Suffix(tcx.mk_mach_int(int_ty)),

            LitKind::Int(_, LitIntType::Unsigned(uint_ty)) =>
                LitTySource::Suffix(tcx.mk_mach_uint(uint_ty)),

            LitKind::Float(_, float_ty) =>
                LitTySource::Suffix(tcx.mk_mach_float(float_ty)),

            _ => LitTySource::None
        }
    }
}

impl<'tcx> ut::UnifyValue for LitTySource<'tcx> {
    type Error = (ty::Ty<'tcx>, ty::Ty<'tcx>);

    fn unify_values(value1: &Self, value2: &Self) -> Result<Self, Self::Error> {
        use LitTySource::*;
        match (value1, value2) {
            // Check for mismatched types
            (Actual(ty1), Actual(ty2)) |
            (Actual(ty1), Suffix(ty2)) |
            (Suffix(ty1), Actual(ty2)) |
            (Suffix(ty1), Suffix(ty2)) if ty1 != ty2 => Err((ty1, ty2)),

            (Actual(ty), _) | (_, Actual(ty)) => Ok(Actual(ty)),
            (Suffix(ty), _) | (_, Suffix(ty)) => Ok(Suffix(ty)),
            (None, None) => Ok(None),
        }
    }
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;
    reg.register("bytestr_to_str", |_args| mk(ByteStrToStr));
    reg.register("remove_null_terminator", |_args| mk(RemoveNullTerminator));
    reg.register("remove_literal_suffixes", |_| mk(RemoveLiteralSuffixes));
}

