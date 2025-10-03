use rustc_arena::DroplessArena;
use ena::unify as ut;
use rustc_hir as hir;
use rustc_hir::def::Res;
use rustc_data_structures::sync::Lrc;
use rustc_ast::*;
use rustc_ast::token;
use rustc_ast::ptr::P;
use rustc_middle::ty;
use rustc_span::symbol::Symbol;
use rustc_ast::visit::{self, Visitor};
use rustc_span::Span;
use rustc_type_ir::sty;

use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;

use crate::ast_manip::MutVisitNodes;
use crate::command::{CommandState, Registry};
use crate::driver::Phase;
use crate::match_or;
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
                            let s = String::from_utf8(bs.to_vec()).unwrap();
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
                            let ms = &mut Lrc::get_mut(bs).unwrap();
                            if let Some((last, rest)) = ms.split_last_mut() {
                                if *last == 0 {
                                    *ms = rest;
                                    strip_null(&mut l.token.symbol);
                                }
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
                kind: LitKind::Float(sym, LitFloatType::Unsuffixed),
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
        let arena = DroplessArena::default();
        let mut uv = UnifyVisitor {
            cx,
            arena: &arena,
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

                        LitTySource::Suffix(ty, needs_suffix) => {
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
                            match (needs_suffix, &ty.kind()) {
                                (false, sty::TyKind::Int(ty::IntTy::I32)) |
                                (false, sty::TyKind::Float(ty::FloatTy::F64)) => {
                                    if let Some(new_lit) = remove_suffix(&lit) {
                                        *lit = new_lit;
                                    }
                                }
                                _ => {}
                            }
                        }

                        LitTySource::Unknown(needs_suffix) => {
                            // We should never wind up here
                            assert!(!needs_suffix);
                        }
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

struct UnifyVisitor<'a, 'kt, 'tcx: 'a + 'kt> {
    cx: &'a RefactorCtxt<'a, 'tcx>,
    arena: &'kt DroplessArena,
    unif: ut::UnificationTable<ut::InPlace<LitTyKey<'tcx>>>,
    lit_nodes: HashMap<NodeId, LitTyKey<'tcx>>,
}

impl<'a, 'kt, 'tcx> UnifyVisitor<'a, 'kt, 'tcx> {
    fn new_empty_node(&mut self) -> LitTyKeyTree<'kt, 'tcx> {
        self.arena.alloc(Cell::new(LitTyKeyNode::Empty))
    }

    fn new_leaf(&mut self, source: LitTySource<'tcx>) -> LitTyKeyTree<'kt, 'tcx> {
        let key = self.unif.new_key(source);
        self.arena.alloc(Cell::new(LitTyKeyNode::Leaf(key)))
    }

    fn new_node(&mut self, kts: &[LitTyKeyTree<'kt, 'tcx>]) -> LitTyKeyTree<'kt, 'tcx> {
        let kt_slice = if kts.is_empty() {
            // `alloc_slice` panics if we pass it an empty slice
            &[][..]
        } else {
            self.arena.alloc_slice(kts)
        };
        self.arena.alloc(Cell::new(LitTyKeyNode::Node(kt_slice)))
    }

    /// Replaces an existing node with a `Leaf`.
    fn replace_with_leaf(
        &mut self,
        leaf: LitTyKeyTree<'kt, 'tcx>,
        source: LitTySource<'tcx>,
    ) {
        let key = self.unif.new_key(source);
        leaf.set(LitTyKeyNode::Leaf(key));
    }

    /// Replaces an existing node with a `Node`.
    fn replace_with_node(
        &mut self,
        leaf: LitTyKeyTree<'kt, 'tcx>,
        kts: &[LitTyKeyTree<'kt, 'tcx>]
    ) {
        let kt_slice = if kts.is_empty() {
            // `alloc_slice` panics if we pass it an empty slice
            &[][..]
        } else {
            self.arena.alloc_slice(kts)
        };
        leaf.set(LitTyKeyNode::Node(kt_slice));
    }

    fn unify_key_trees_internal(
        &mut self,
        kt1: LitTyKeyTree<'kt, 'tcx>,
        kt2: LitTyKeyTree<'kt, 'tcx>,
        seen: &mut HashSet<(usize, usize)>,
    ) {
        // Prevent infinite recursion
        let hash_key = (kt1 as *const _ as usize,
                        kt2 as *const _ as usize);
        if seen.contains(&hash_key) {
            return;
        }
        seen.insert(hash_key);

        match (kt1.get(), kt2.get()) {
            (LitTyKeyNode::Node(ref ch1),
             LitTyKeyNode::Node(ref ch2)) if ch1.len() == ch2.len() => {
                for (ch1_kt, ch2_kt) in ch1.iter().zip(ch2.iter()) {
                    self.unify_key_trees_internal(ch1_kt, ch2_kt, seen);
                }
            }

            (LitTyKeyNode::Leaf(l1), LitTyKeyNode::Leaf(l2)) => {
                self.unif.unify_var_var(l1, l2).expect("failed to unify");
            }

            (LitTyKeyNode::Empty, _) | (_, LitTyKeyNode::Empty) => {}

            (kt1 @ _, kt2 @ _) => panic!("mismatched key trees: {:?} != {:?}", kt1, kt2)
        }
    }

    fn unify_key_trees(
        &mut self,
        kt1: LitTyKeyTree<'kt, 'tcx>,
        kt2: LitTyKeyTree<'kt, 'tcx>,
    ) {
        let mut seen = HashSet::new();
        self.unify_key_trees_internal(kt1, kt2, &mut seen);
    }

    /// Directly convert an `ast::Ty` to a `ty::Ty`, without any type inference
    /// or going through the typeck tables.
    fn ast_ty_to_key_tree(&mut self, ty: &Ty) -> LitTyKeyTree<'kt, 'tcx> {
        let node = match_or!([self.cx.hir_map().find(ty.id)] Some(x) => x;
                             return self.new_empty_node());
        let t = match_or!([node] hir::Node::Ty(t) => t;
                          return self.new_empty_node());
        self.hir_ty_to_key_tree(t)
    }

    /// Directly convert a `hir::Ty` to a `LitTyKeyTree`
    fn hir_ty_to_key_tree(&mut self, ty: &hir::Ty) -> LitTyKeyTree<'kt, 'tcx> {
        // TODO: use the HirId cache???
        match ty.kind {
            hir::TyKind::Path(hir::QPath::Resolved(_, ref path)) => {
                self.res_to_key_tree(path.res, path.span)
            }
            // TODO: handle TypeRelative paths

            hir::TyKind::Slice(ref ty) |
            hir::TyKind::Array(ref ty, _) |
            hir::TyKind::Ptr(hir::MutTy { ref ty, .. }) |
            hir::TyKind::Rptr(_, hir::MutTy { ref ty, .. }) => {
                let ty_kt = self.hir_ty_to_key_tree(ty);
                self.new_node(&[ty_kt])
            }

            hir::TyKind::Tup(ref elems) => {
                let ch = elems.iter()
                    .map(|ty| self.hir_ty_to_key_tree(ty))
                    .collect::<Vec<_>>();
                self.new_node(&ch)
            }

            _ => self.new_empty_node()
        }
    }

    fn res_to_key_tree(&mut self, res: Res, span: Span) -> LitTyKeyTree<'kt, 'tcx> {
        // TODO: use the HirId cache???
        let tcx = self.cx.ty_ctxt();
        match res {
            Res::Def(_, did) => {
                self.def_id_to_key_tree(did, span)
            }

            Res::PrimTy(prim_ty) => {
                let source = match prim_ty {
                    // TODO: check generics
                    hir::PrimTy::Int(it) => LitTySource::Actual(tcx.mk_mach_int(ty::int_ty(it))),
                    hir::PrimTy::Uint(uit) => LitTySource::Actual(tcx.mk_mach_uint(ty::uint_ty(uit))),
                    hir::PrimTy::Float(ft) => LitTySource::Actual(tcx.mk_mach_float(ty::float_ty(ft))),
                    _ => LitTySource::Unknown(false)
                };
                self.new_leaf(source)
            }

            // We don't want to unify with `Self`
            Res::SelfTy { .. } => self.new_empty_node(),

            Res::Local(id) => {
                // This is a local variable that may have a type,
                // try to get that type and unify it
                let pid = self.cx.hir_map().get_parent_node(id);
                let node = match_or!([tcx.hir().find(pid)]
                                     Some(x) => x;
                                     return self.new_empty_node());
                let l = match_or!([node] hir::Node::Local(l) => l;
                                  return self.new_empty_node());
                let lty = match_or!([l.ty] Some(ref lty) => lty;
                                    return self.new_empty_node());
                // FIXME: this local reference might be a sub-binding,
                // and not the entire local; in that case, we need
                // to split up the key tree
                self.hir_ty_to_key_tree(lty)
            }
            // TODO: handle `SelfCtor`

            _ => self.new_empty_node()
        }
    }

    fn def_id_to_key_tree(
        &mut self,
        did: hir::def_id::DefId,
        sp: Span,
    ) -> LitTyKeyTree<'kt, 'tcx> {
        let tcx = self.cx.ty_ctxt();
        let ty = tcx.at(sp).type_of(did);
        self.ty_to_key_tree(ty, true)
    }

    fn ty_to_key_tree_internal(
        &mut self,
        ty: ty::Ty<'tcx>,
        mach_actual: bool,
        seen: &mut HashMap<(ty::Ty<'tcx>, bool), LitTyKeyTree<'kt, 'tcx>>,
    ) -> LitTyKeyTree<'kt, 'tcx> {
        // We memoize the result to eliminate infinite recursion
        let hash_key = (ty, mach_actual);
        if let Some(kt) = seen.get(&hash_key) {
            return kt;
        }
        let new_node = self.new_empty_node();
        seen.insert(hash_key, new_node);

        let tcx = self.cx.ty_ctxt();
        let mut fn_sig_to_key_tree = |fn_sig: ty::PolyFnSig<'tcx>, sig_mach: bool| {
            let ch = fn_sig
                .skip_binder()
                .inputs_and_output
                .iter()
                .map(|ty| self.ty_to_key_tree_internal(ty, sig_mach, seen))
                .collect::<Vec<_>>();
            self.replace_with_node(new_node, &ch);
        };

        match ty.kind() {
            sty::TyKind::Int(_) |
            sty::TyKind::Uint(_) |
            sty::TyKind::Float(_) => {
                let source = if mach_actual {
                    LitTySource::Actual(ty)
                } else {
                    LitTySource::Unknown(false)
                };
                self.replace_with_leaf(new_node, source);
            }

            // TODO: we used to be able to handle Rc/Arc as well
            sty::TyKind::Adt(ref adt_ref, ref substs) if adt_ref.is_box() =>{
                // Ignore the actual structure for these types, and just
                // use the inner type as the single child
                let inner_ty = substs.type_at(0);
                let ty_kt = self.ty_to_key_tree_internal(inner_ty, mach_actual, seen);
                self.replace_with_node(new_node, &[ty_kt]);
            }

            sty::TyKind::Adt(ref adt_def, ref substs) => {
                let ch = adt_def.all_fields()
                    .map(|field| self.ty_to_key_tree_internal(field.ty(tcx, substs), mach_actual, seen))
                    .collect::<Vec<_>>();
                self.replace_with_node(new_node, &ch);
            }

            sty::TyKind::Str => {
                let u8_ty = tcx.types.u8;
                let u8_kt = self.ty_to_key_tree_internal(u8_ty, true, seen);
                self.replace_with_node(new_node, &[u8_kt]);
            }

            sty::TyKind::Array(ty, _) |
            sty::TyKind::Slice(ty) |
            sty::TyKind::RawPtr(ty::TypeAndMut { ty, .. }) |
            sty::TyKind::Ref(_, ty, _) => {
                let ty_kt = self.ty_to_key_tree_internal(*ty, mach_actual, seen);
                self.replace_with_node(new_node, &[ty_kt]);
            }

            sty::TyKind::FnDef(def_id, _) => {
                // Since we're using the original signature
                // and not performing any substitutions,
                // it's safe to include the machine types
                fn_sig_to_key_tree(tcx.fn_sig(def_id), true);
            }
            sty::TyKind::FnPtr(fn_sig) => {
                fn_sig_to_key_tree(*fn_sig, mach_actual);
            }

            // TODO: Closure

            sty::TyKind::Tuple(ref elems) => {
                let ch = elems.iter()
                    .map(|ty| self.ty_to_key_tree_internal(ty, mach_actual, seen))
                    .collect::<Vec<_>>();
                self.replace_with_node(new_node, &ch);
            }

            // We particularly want to leave this one as `Empty`,
            // because we unifying type parameters and removing suffixes
            // might change their type during compilation, which
            // could cause compilation errors (especially for `self`), e.g.,
            // `1u32.wrapping_add(2)` can't ever be rewritten to
            // `1.wrapping_add(2)` since the latter fails to compile
            sty::TyKind::Param(_) => {}

            // All the others are irrelevant
            _ => {}
        }
        new_node
    }

    fn ty_to_key_tree(&mut self, ty: ty::Ty<'tcx>, mach_actual: bool) -> LitTyKeyTree<'kt, 'tcx> {
        let mut seen = HashMap::new();
        self.ty_to_key_tree_internal(ty, mach_actual, &mut seen)
    }

    fn expr_ty_to_key_tree(&mut self, ex: &Expr) -> LitTyKeyTree<'kt, 'tcx> {
        if let Some(ty) = self.cx.opt_node_type(ex.id) {
            self.ty_to_key_tree(ty, false)
        } else {
            self.new_empty_node()
        }
    }

    fn unify_expr_child(&mut self, e: &Expr, kt: LitTyKeyTree<'kt, 'tcx>) {
        if let Some(ch_kt) = kt.get().children() {
            assert!(ch_kt.len() == 1);
            self.visit_expr_unify(e, ch_kt[0]);
        } else {
            self.visit_expr(e);
        }
    }

    fn visit_expr_unify(&mut self, ex: &Expr, kt: LitTyKeyTree<'kt, 'tcx>) {
        let tcx = self.cx.ty_ctxt();
        match ex.kind {
            ExprKind::Box(ref e) => self.unify_expr_child(e, kt),

            ExprKind::Array(ref exprs) => {
                // We really want the subexpressions to at least unify with
                // each other, so we need to either get the key tree
                // from the caller, or create a new common one for all subexpressions
                let elem_kt = if let Some(ch) = kt.get().children() {
                    assert!(ch.len() == 1);
                    ch[0]
                } else {
                    exprs.first()
                        .and_then(|e| self.cx.opt_node_type(e.id))
                        .map(|ty| self.ty_to_key_tree(ty, false))
                        .unwrap_or_else(|| self.new_empty_node())

                };
                for e in exprs {
                    self.visit_expr_unify(e, elem_kt);
                }
            }

            ExprKind::Call(ref callee, ref args) => {
                let callee_key_tree = self.expr_ty_to_key_tree(callee);
                self.visit_expr_unify(callee, callee_key_tree);
                // TODO: check if generic
                if let Some(&[ref input_key_trees @ .., output_key_tree]) = callee_key_tree.get().children() {
                    for (arg_expr, arg_key_tree) in args.iter().zip(input_key_trees.iter()) {
                        self.visit_expr_unify(arg_expr, arg_key_tree);
                    }
                    self.unify_key_trees(kt, output_key_tree);
                } else {
                    for arg in args {
                        self.visit_expr(arg);
                    }
                }
            }

            ExprKind::MethodCall(ref segment, ref args, ref _span) => {
                let hir_id = self.cx.hir_map().node_to_hir_id(ex.id);
                let parent = self.cx.hir_map().get_parent_item(hir_id);
                let body = self.cx.hir_map().body_owned_by(parent);
                let tables = tcx.typeck_body(body);
                let did = tables.type_dependent_def_id(hir_id).unwrap();
                let callee_key_tree = self.def_id_to_key_tree(did, ex.span);

                // Hacky fix for the way rustc gives us method types:
                // we don't get the parametric `Self` as the type of
                // the `self` argument, instead we get the actual type,
                // and we can't unify with that since it can break stuff
                if let Some(ch) = callee_key_tree.get().children() {
                    ch[0].set(LitTyKeyNode::Empty);
                }

                self.visit_path_segment(ex.span, segment);
                if let Some(&[ref input_key_trees @ .., output_key_tree]) = callee_key_tree.get().children() {
                    for (arg_expr, arg_key_tree) in args.iter().zip(input_key_trees.iter()) {
                        self.visit_expr_unify(arg_expr, arg_key_tree);
                    }
                    self.unify_key_trees(kt, output_key_tree);
                } else {
                    for arg in args {
                        self.visit_expr(arg);
                    }
                }
            }

            ExprKind::Tup(ref exprs) => {
                if let Some(ch_kt) = kt.get().children() {
                    assert!(ch_kt.len() == exprs.len());
                    for (e, ch_kt) in exprs.iter().zip(ch_kt.iter()) {
                        self.visit_expr_unify(e, ch_kt);
                    }
                } else {
                    for e in exprs {
                        self.visit_expr(e);
                    }
                }
            }

            ExprKind::Binary(ref op, ref lhs, ref rhs) => {
                // FIXME: handle overloads
                use BinOpKind::*;
                match op.node {
                    Add | Sub | Mul | Div | Rem | BitXor | BitAnd | BitOr => {
                        self.visit_expr_unify(lhs, kt);
                        self.visit_expr_unify(rhs, kt);
                    }

                    Shl | Shr => {
                        self.visit_expr_unify(lhs, kt);
                        self.visit_expr(rhs);
                    }

                    Eq | Ne | Lt | Le | Gt | Ge => {
                        // Unify the lhs and rhs with each other, but not with the result
                        let inner_key_tree = self.expr_ty_to_key_tree(lhs);
                        self.visit_expr_unify(lhs, inner_key_tree);
                        self.visit_expr_unify(rhs, inner_key_tree);
                    }

                    And | Or => {
                        self.visit_expr(lhs);
                        self.visit_expr(rhs);
                    }
                }
            }

            ExprKind::Unary(ref op, ref e) => match op {
                UnOp::Not | UnOp::Neg => self.visit_expr_unify(e, kt),
                UnOp::Deref => {
                    // We're doing this one inside-out: the inner
                    // expression unifies with the outer key tree,
                    // and the outer expression unifies with the child
                    let ptr_key_tree = self.expr_ty_to_key_tree(e);
                    self.visit_expr_unify(e, ptr_key_tree);
                    if let Some(e_ty) = self.cx.opt_node_type(e.id) {
                        if e_ty.builtin_deref(true).is_some() {
                            if let Some(ch) = ptr_key_tree.get().children() {
                                assert!(ch.len() == 1);
                                self.unify_key_trees(kt, ch[0]);
                            }
                        } else {
                            // TODO: handle other types that implement `Deref`
                        }
                    }
                }
            }

            ExprKind::Lit(ref lit) => {
                if ex.id != DUMMY_NODE_ID && lit.kind.is_suffixed() {
                    let source = LitTySource::from_lit_expr(ex, tcx);
                    let lit_key_tree = self.new_leaf(source);
                    self.unify_key_trees(kt, lit_key_tree);
                    self.lit_nodes.insert(ex.id, lit_key_tree.get().as_key());
                }
            }

            ExprKind::Cast(ref e, ref ty) => {
                let ty_key_tree = self.ast_ty_to_key_tree(ty);
                self.unify_key_trees(kt, ty_key_tree);

                let e_key_tree = self.expr_ty_to_key_tree(e);
                if let LitTyKeyNode::Leaf(l) = e_key_tree.get() {
                    if self.cx.opt_node_type(ty.id).map(|ty| ty.is_scalar()).unwrap_or(false) {
                        // Special case: if we're casting a literal to a scalar,
                        // rustc will infer the type of the literal if it's unsuffixed,
                        // so we really need to keep the suffix
                        self.unif.unify_var_value(l, LitTySource::Unknown(true))
                            .expect("failed to unify");
                    }
                }
                self.visit_expr_unify(e, e_key_tree);
                self.visit_ty(ty);
            }

            ExprKind::Type(ref e, ref ty) => {
                let ty_key_tree = self.ast_ty_to_key_tree(ty);
                self.unify_key_trees(kt, ty_key_tree);

                self.visit_expr_unify(e, kt);
                self.visit_ty(ty);
            }

            // TODO: handle `Let` from `if/while let`

            ExprKind::If(ref cond, ref block, ref r#else) => {
                self.visit_expr(cond);
                self.visit_block_unify(block, kt);
                if let Some(r#else) = r#else {
                    self.visit_expr_unify(r#else, kt);
                }
            }

            // TODO: unify loops

            ExprKind::Match(ref e, ref arms) => {
                let pat_key_tree = self.expr_ty_to_key_tree(e);
                self.visit_expr_unify(e, pat_key_tree);
                for arm in arms {
                    self.visit_pat_unify(&arm.pat, pat_key_tree);
                    if let Some(ref guard) = arm.guard {
                        self.visit_expr(guard);
                    }
                    self.visit_expr_unify(&arm.body, kt);
                    for attr in &arm.attrs {
                        self.visit_attribute(attr);
                    }
                }
            }

            // TODO: handle `Closure`

            ExprKind::Block(ref block, _) => {
                self.visit_block_unify(block, kt);
            }

            // TODO: handle `Async`/`Await`

            ExprKind::Assign(ref lhs, ref rhs, ref _span) => {
                let inner_key_tree = self.expr_ty_to_key_tree(lhs);
                self.visit_expr_unify(lhs, inner_key_tree);
                self.visit_expr_unify(rhs, inner_key_tree);
            }

            ExprKind::AssignOp(ref op, ref lhs, ref rhs) => {
                // FIXME: handle overloads
                use BinOpKind::*;
                match op.node {
                    Add | Sub | Mul | Div | Rem | BitXor | BitAnd | BitOr => {
                        let inner_key_tree = self.expr_ty_to_key_tree(lhs);
                        self.visit_expr_unify(lhs, inner_key_tree);
                        self.visit_expr_unify(rhs, inner_key_tree);
                    }

                    _ => {
                        self.visit_expr(lhs);
                        self.visit_expr(rhs);
                    }
                }
            }

            ExprKind::Field(ref e, ident) => {
                let inner_key_tree = self.expr_ty_to_key_tree(e);
                self.visit_expr_unify(e, inner_key_tree);
                self.visit_ident(ident);
                if let Some(struct_ty) = self.cx.opt_adjusted_node_type(e.id) {
                    let ch = inner_key_tree.get().children();
                    match (ch, &struct_ty.kind()) {
                        (None, _) => {}
                        (Some(ch), sty::TyKind::Adt(def, _)) => {
                            let v = &def.non_enum_variant();
                            assert!(ch.len() == v.fields.len());
                            let idx = tcx.find_field_index(ident, v).unwrap();
                            self.unify_key_trees(kt, ch[idx]);
                        }
                        (Some(ch), sty::TyKind::Tuple(ref tys)) => {
                            assert!(ch.len() == tys.len());
                            if let Ok(idx) = ident.as_str().parse::<usize>() {
                                self.unify_key_trees(kt, ch[idx]);
                            }
                        }
                        _ => panic!("expected Adt/Tuple, got {:?}", struct_ty)
                    }
                }
            }

            ExprKind::Index(ref e, ref idx) => {
                let e_key_tree = self.expr_ty_to_key_tree(e);
                self.visit_expr_unify(e, e_key_tree);

                // If the lhs type is array/slice/`str`, we can unify the result
                // of the `ExprKind::Index` with the inner type of the base
                if let Some(e_ty) = self.cx.opt_node_type(e.id) {
                    use sty::TyKind::*;
                    if let Array(..) | Slice(_) | Str = e_ty.kind() {
                        if let Some(ch) = e_key_tree.get().children() {
                            assert!(ch.len() == 1);
                            self.unify_key_trees(kt, ch[0]);
                        }
                    } else {
                        // TODO: check for `Index`/`IndexMut`
                    }
                }

                // We unify `idx` with `usize`, but only after making sure
                // that it's the expected type; we do this in a hacky way:
                // we assume that if the type of the expression was inferred
                // to `usize` with suffixes, then it will stay the same without
                // them; this should generally be true, since the expected type
                // is inferred from the parameter of `Index`/`IndexMut`
                let idx_key_tree = 'block: {
                    if let Some(idx_ty) = self.cx.opt_node_type(idx.id) {
                        if idx_ty.is_ptr_sized_integral() {
                            break 'block self.ty_to_key_tree(idx_ty, true);
                        }
                    }
                    self.expr_ty_to_key_tree(idx)
                };
                self.visit_expr_unify(idx, idx_key_tree);
            }

            ExprKind::Range(ref start, ref end, _) => {
                match (start, end) {
                    (Some(start), Some(end)) => {
                        let inner_key_tree = self.expr_ty_to_key_tree(start);
                        self.visit_expr_unify(start, inner_key_tree);
                        self.visit_expr_unify(end, inner_key_tree);
                    }
                    (Some(e), None) | (None, Some(e)) => self.visit_expr(e),
                    (None, None) => {}
                }
            }

            ExprKind::Path(..) => {
                visit::walk_expr(self, ex);
                if let Some(res) = self.cx.try_resolve_expr_hir(ex) {
                    let path_key_tree = self.res_to_key_tree(res, ex.span);
                    self.unify_key_trees(kt, path_key_tree);
                }
                // TODO: handle TypeRelative paths
            }

            ExprKind::AddrOf(_, _, ref e) => self.unify_expr_child(e, kt),

            // TODO: unify `Break` with return values
            //
            // TODO: unify `Ret` with function return type
            //
            // TODO: unify non-generic `Struct`

            ExprKind::Repeat(ref e, ref count) => {
                self.unify_expr_child(e, kt);
                self.visit_anon_const(count);
            }

            ExprKind::Paren(ref e) => self.visit_expr_unify(e, kt),

            // TODO: handle `Try`
            //
            // TODO: handle `Yield`

            _ => visit::walk_expr(self, ex)
        }
    }

    fn visit_block_unify(&mut self, b: &Block, kt: LitTyKeyTree<'kt, 'tcx>) {
        if let Some((last, stmts)) = b.stmts.split_last() {
            for stmt in stmts {
                self.visit_stmt(stmt);
            }
            if let StmtKind::Expr(ref e) = last.kind {
                self.visit_expr_unify(e, kt);
            }
        }
    }

    fn unify_pat_children(
        &mut self,
        pats: &Vec<P<Pat>>,
        ch: &'kt [LitTyKeyTree<'kt, 'tcx>],
    ) {
        let mut ich = 0;
        for pat in pats {
            let is_rest = self.visit_pat_unify(pat, ch[ich]);
            if is_rest {
                // Encountered a `..`, skip over the corresponding children
                assert!(pats.len() <= ch.len());
                ich += ch.len() - pats.len();
            }
            ich += 1;
        }
        assert!(ich == ch.len());
    }

    /// Visit a `Pat` node, unifying sub-nodes along the way.
    /// Returns `true` if this node is a `Rest`.
    fn visit_pat_unify(&mut self, p: &Pat, kt: LitTyKeyTree<'kt, 'tcx>) -> bool {
        match p.kind {
            PatKind::Ident(_, ident, Some(ref pat)) => {
                // `ref? mut? ident @ pat`, handle it as `pat`
                self.visit_ident(ident);
                return self.visit_pat_unify(pat, kt);
            }

            //FIXME: these are not correct
            //PatKind::Struct(ref path, ref fields, _) => {
            //    self.visit_path(path, p.id);
            //    let res = self.cx.try_resolve_pat_hir(p);
            //    if let Some(res) = res {
            //        let path_key_tree = self.res_to_key_tree(res, path.span);
            //        self.unify_key_trees(kt, path_key_tree);
            //    }

            //    let v = self.cx
            //        .opt_adjusted_node_type(p.id)
            //        .and_then(|ty| ty.ty_adt_def())
            //        .and_then(|def| res.map(|res| def.variant_of_res(res)));
            //    for field in fields {
            //        self.visit_ident(field.ident);
            //        // TODO: unify with type of field
            //        if let (Some(ch), Some(idx)) = (
            //            kt.get().children(),
            //            v.and_then(|v| tcx.find_field_index(field.ident, v)),
            //        ) {
            //            self.visit_pat_unify(&field.pat, ch[idx]);
            //        } else {
            //            self.visit_pat(&field.pat);
            //        }
            //        for attr in field.attrs.iter() {
            //            self.visit_attribute(attr);
            //        }
            //    }
            //}

            //PatKind::TupleStruct(ref path, ref pats) => {
            //    self.visit_path(path, p.id);
            //    let res = self.cx.try_resolve_pat_hir(p);
            //    if let Some(res) = res {
            //        let path_key_tree = self.res_to_key_tree(res, path.span);
            //        self.unify_key_trees(kt, path_key_tree);
            //    }

            //    // FIXME: get correct field index
            //    let ch = self.cx.opt_node_type(p.id)
            //        .map(|ty| self.ty_to_key_tree(ty, false))
            //        .and_then(|kt| kt.get().children());
            //    if let Some(ch) = ch {
            //        self.unify_pat_children(pats, ch);
            //    } else {
            //        for pat in pats {
            //            self.visit_pat(pat);
            //        }
            //    };
            //}

            PatKind::Or(ref pats) => {
                for pat in pats {
                    let _is_rest = self.visit_pat_unify(pat, kt);
                    // None of the above should ever be `Rest` nodes,
                    // but we should sanity-check
                    assert!(!_is_rest);
                }
            }

            PatKind::Path(..) => {
                visit::walk_pat(self, p);
                if let Some(res) = self.cx.try_resolve_pat_hir(p) {
                    let path_key_tree = self.res_to_key_tree(res, p.span);
                    self.unify_key_trees(kt, path_key_tree);
                }
            }

            PatKind::Tuple(ref pats) => {
                if let Some(ch) = kt.get().children() {
                    self.unify_pat_children(pats, ch);
                } else {
                    for pat in pats {
                        self.visit_pat(pat);
                    }
                };
            }

            PatKind::Box(ref inner) | PatKind::Ref(ref inner, _) => {
                if let Some(ch) = kt.get().children() {
                    assert!(ch.len() == 1);
                    self.visit_pat_unify(inner, ch[0]);
                } else {
                    self.visit_pat(inner);
                }
            }

            PatKind::Lit(ref e) => self.visit_expr_unify(e, kt),

            PatKind::Range(ref start, ref end, _) => {
                // TODO: approximate???
                match (start, end) {
                    (Some(start), Some(end)) => {
                        let inner_key_tree = self.expr_ty_to_key_tree(start);
                        self.visit_expr_unify(start, inner_key_tree);
                        self.visit_expr_unify(end, inner_key_tree);
                    }
                    (Some(e), None) | (None, Some(e)) => self.visit_expr(e),
                    (None, None) => {}
                }
            }

            PatKind::Slice(ref pats) => {
                // See comment for `ExprKind::Array`
                let elem_kt = if let Some(ch) = kt.get().children() {
                    assert!(ch.len() == 1);
                    ch[0]
                } else {
                    pats.first()
                        .and_then(|pat| self.cx.opt_node_type(pat.id))
                        .map(|ty| self.ty_to_key_tree(ty, false))
                        .unwrap_or_else(|| self.new_empty_node())
                };
                for pat in pats {
                    self.visit_pat_unify(pat, elem_kt);
                }
            }

            PatKind::Rest => return true,

            PatKind::Paren(ref pat) => return self.visit_pat_unify(pat, kt),

            // TODO: handle `MacCall`? Do we need to?

            _ => visit::walk_pat(self, p)
        };
        false
    }

    // TODO: handle `Field`
    //
    // TODO: handle `AnonConst`
}

impl<'ast, 'a, 'kt, 'tcx> Visitor<'ast> for UnifyVisitor<'a, 'kt, 'tcx> {
    fn visit_expr(&mut self, ex: &'ast Expr) {
        let key_tree = self.expr_ty_to_key_tree(ex);
        self.visit_expr_unify(ex, key_tree);
    }

    fn visit_local(&mut self, l: &'ast Local) {
        for attr in l.attrs.iter() {
            self.visit_attribute(attr);
        }
        self.visit_pat(&l.pat);
        if let Some(ref ty) = l.ty {
            self.visit_ty(ty);
        }
        if let Some(ref init) = l.kind.init() {
            let key_tree = if let Some(ref ty) = l.ty {
                self.ast_ty_to_key_tree(ty)
            } else {
                self.expr_ty_to_key_tree(init)
            };
            self.visit_expr_unify(init, key_tree);
        }
    }

    fn visit_item(&mut self, i: &'ast Item) {
        match i.kind {
            ItemKind::Static(ref ty, _, Some(ref init)) |
            ItemKind::Const(_, ref ty, Some(ref init)) => {
                let key_tree = self.ast_ty_to_key_tree(ty);
                self.visit_ty(ty);
                self.visit_expr_unify(init, key_tree);
            }

            _ => visit::walk_item(self, i)
        }
    }

    fn visit_arm(&mut self, _arm: &'ast Arm) {
        panic!("Arm nodes should be handled in visit_expr_unify");
    }

    fn visit_pat(&mut self, p: &'ast Pat) {
        let key_tree = if let Some(ty) = self.cx.opt_node_type(p.id) {
            self.ty_to_key_tree(ty, false)
        } else {
            self.new_empty_node()
        };
        self.visit_pat_unify(p, key_tree);
    }
}

#[derive(Debug, Clone, Copy)]
enum LitTyKeyNode<'kt, 'tcx: 'kt> {
    /// Empty node, ignored during unification. Used for types
    /// where we intentionally don't want to unify, e.g., `Param`,
    /// and when we can't determine a structure.
    Empty,

    /// Internal node with children, corresponding to aggregate types
    /// and other aggregate-like types, e.g., `FnDef`, that have
    /// multiple inner types.
    Node(&'kt [LitTyKeyTree<'kt, 'tcx>]),

    /// Leaf node containing a unification key. Mostly corresponds
    /// to scalar Rust types.
    Leaf(LitTyKey<'tcx>),
}

type LitTyKeyTree<'kt, 'tcx> = &'kt Cell<LitTyKeyNode<'kt, 'tcx>>;

impl<'kt, 'tcx: 'kt> LitTyKeyNode<'kt, 'tcx> {
    fn as_key(&self) -> LitTyKey<'tcx> {
        match self {
            Self::Leaf(key) => *key,
            _ => panic!("expected leaf, found: {:?}", self)
        }
    }

    fn children(&self) -> Option<&'kt [LitTyKeyTree<'kt, 'tcx>]> {
        match self {
            Self::Node(ch) => Some(ch),
            Self::Empty => None,
            _ => panic!("expected node, found: {:?}", self)
        }
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
    /// No type information for this key. The first field is set
    /// to `true` if the suffix needs to be preserved, e.g.,
    /// this literal is used in a method call or cast to scalar.
    Unknown(bool),

    /// This type came from a suffix. If we get all our type information from
    /// suffixes, that means we need to keep one literal suffix around to maintain
    /// the type, and throw out all the rest. The first field is set to `true`
    /// if the suffix needs to be preserved (see above).
    Suffix(ty::Ty<'tcx>, bool),

    /// We got this type from an actual value, e.g., a structure field or
    /// an explicitly typed local. If we get one of these, we can throw out
    /// the suffixes for all literals in this key.
    Actual(ty::Ty<'tcx>),
}

impl<'tcx> LitTySource<'tcx> {
    fn from_lit_expr(e: &Expr, tcx: ty::TyCtxt<'tcx>) -> Self {
        let lit = match e.kind {
            ExprKind::Lit(ref lit) => lit,
            _ => panic!("expected ExprKind::Lit, got {:?}", e)
        };
        match lit.kind {
            LitKind::Int(_, LitIntType::Signed(int_ty)) =>
                LitTySource::Suffix(tcx.mk_mach_int(ty::int_ty(int_ty)), false),

            LitKind::Int(_, LitIntType::Unsigned(uint_ty)) =>
                LitTySource::Suffix(tcx.mk_mach_uint(ty::uint_ty(uint_ty)), false),

            LitKind::Float(_, LitFloatType::Suffixed(float_ty)) =>
                LitTySource::Suffix(tcx.mk_mach_float(ty::float_ty(float_ty)), false),

            _ => LitTySource::Unknown(false)
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
            (Actual(ty1), Suffix(ty2, _)) |
            (Suffix(ty1, _), Actual(ty2)) |
            (Suffix(ty1, _), Suffix(ty2, _)) if ty1 != ty2 => Err((*ty1, *ty2)),

            (Suffix(ty, n1), Suffix(_, n2)) |
            (Suffix(ty, n1), Unknown(n2)) |
            (Unknown(n1), Suffix(ty, n2)) => Ok(Suffix(*ty, *n1 || *n2)),

            (Actual(ty), _) | (_, Actual(ty)) => Ok(Actual(*ty)),
            (Unknown(n1), Unknown(n2)) => Ok(Unknown(*n1 || *n2)),
        }
    }
}

pub fn register_commands(reg: &mut Registry) {
    use super::mk;
    reg.register("bytestr_to_str", |_args| mk(ByteStrToStr));
    reg.register("remove_null_terminator", |_args| mk(RemoveNullTerminator));
    reg.register("remove_literal_suffixes", |_| mk(RemoveLiteralSuffixes));
}

