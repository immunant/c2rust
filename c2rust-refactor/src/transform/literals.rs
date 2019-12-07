use arena::SyncDroplessArena;
use ena::unify as ut;
use rustc::{hir, ty};
use rustc::hir::def::{Res, DefKind};
use rustc_data_structures::sync::Lrc;
use syntax::ast::*;
use syntax::parse::token;
use syntax::ptr::P;
use syntax::symbol::Symbol;
use syntax::visit::{self, Visitor};
use syntax_pos::Span;

use std::cell::Cell;
use std::collections::{HashMap, HashSet};
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
        let arena = SyncDroplessArena::default();
        let mut uv = UnifyVisitor {
            cx,
            arena: &arena,
            unif: ut::UnificationTable::new(),
            lit_nodes: HashMap::new(),
            def_id_key_tree_cache: HashMap::new(),
            ty_key_tree_cache: HashMap::new(),
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

struct UnifyVisitor<'a, 'kt, 'tcx: 'a + 'kt> {
    cx: &'a RefactorCtxt<'a, 'tcx>,
    arena: &'kt SyncDroplessArena,
    unif: ut::UnificationTable<ut::InPlace<LitTyKey<'tcx>>>,
    lit_nodes: HashMap<NodeId, LitTyKey<'tcx>>,
    def_id_key_tree_cache: HashMap<hir::def_id::DefId, LitTyKeyTree<'kt, 'tcx>>,
    ty_key_tree_cache: HashMap<(ty::Ty<'tcx>, bool), LitTyKeyTree<'kt, 'tcx>>,
}

impl<'a, 'kt, 'tcx> UnifyVisitor<'a, 'kt, 'tcx> {
    fn new_leaf(&mut self, source: LitTySource<'tcx>) -> LitTyKeyTree<'kt, 'tcx> {
        let key = self.unif.new_key(source);
        self.arena.alloc(Cell::new(LitTyKeyNode::Leaf(key)))
    }

    fn new_none_leaf(&mut self) -> LitTyKeyTree<'kt, 'tcx> {
        self.new_leaf(LitTySource::None)
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

            // FIXME
            // (kt1 @ _, kt2 @ _) => panic!("mismatched key trees: {:?} != {:?}", kt1, kt2)
            _ => {}
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
                             return self.new_none_leaf());
        let t = match_or!([node] hir::Node::Ty(t) => t;
                          return self.new_none_leaf());
        self.hir_ty_to_key_tree(t)
    }

    /// Directly convert a `hir::Ty` to a `LitTyKeyTree`
    fn hir_ty_to_key_tree(&mut self, ty: &hir::Ty) -> LitTyKeyTree<'kt, 'tcx> {
        match ty.kind {
            hir::TyKind::Path(hir::QPath::Resolved(_, ref path)) => {
                self.hir_path_to_key_tree(path)
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

            _ => self.new_none_leaf()
        }
    }

    fn hir_path_to_key_tree(&mut self, path: &hir::Path) -> LitTyKeyTree<'kt, 'tcx> {
        let tcx = self.cx.ty_ctxt();
        match path.res {
            Res::Def(def_kind, did) => {
                self.def_id_to_key_tree(def_kind, did, path.span)
            }

            Res::PrimTy(prim_ty) => {
                let source = match prim_ty {
                    // TODO: check generics
                    hir::Int(it) => LitTySource::Actual(tcx.mk_mach_int(it)),
                    hir::Uint(uit) => LitTySource::Actual(tcx.mk_mach_uint(uit)),
                    hir::Float(ft) => LitTySource::Actual(tcx.mk_mach_float(ft)),
                    _ => LitTySource::None
                };
                self.new_leaf(source)
            }

            // TODO: handle `SelfTy`???

            Res::Local(id) => {
                // This is a local variable that may have a type,
                // try to get that type and unify it
                let pid = tcx.hir().get_parent_node(id);
                let node = match_or!([tcx.hir().find(pid)]
                                     Some(x) => x;
                                     return self.new_none_leaf());
                let l = match_or!([node] hir::Node::Local(l) => l;
                                  return self.new_none_leaf());
                let lty = match_or!([l.ty] Some(ref lty) => lty;
                                    return self.new_none_leaf());
                self.hir_ty_to_key_tree(lty)
            }
            // TODO: handle `SelfCtor`

            _ => self.new_none_leaf()
        }
    }

    fn def_id_to_key_tree(
        &mut self,
        def_kind: DefKind,
        did: hir::def_id::DefId,
        sp: Span,
    ) -> LitTyKeyTree<'kt, 'tcx> {
        if let Some(key_tree) = self.def_id_key_tree_cache.get(&did) {
            return key_tree;
        }

        let tcx = self.cx.ty_ctxt();
        let hir_id = if let Some(hir_id) = tcx.hir().as_local_hir_id(did) {
            hir_id
        } else {
            // The type is from outside the local crate,
            // which we don't touch, so we can just get it from rustc
            // FIXME: is this always correct, and do we always want to do it???
            let ty = tcx.at(sp).type_of(did);
            let key_tree = self.ty_to_key_tree(ty, true);
            self.def_id_key_tree_cache.insert(did, key_tree);
            return key_tree;
        };

        let new_node = self.new_none_leaf();
        self.def_id_key_tree_cache.insert(did, new_node);
        match def_kind {
            DefKind::TyAlias => {
                let item = match_or!([tcx.hir().get(hir_id)]
                                     hir::Node::Item(item) => item;
                                     panic!("expected Item node"));
                let (ty, _) =
                    match_or!([item.kind]
                              hir::ItemKind::TyAlias(ref ty, ref generics) => (ty, generics);
                              panic!("expected TyAlias, got {:?}", item));
                // TODO: check generics
                new_node.set(self.hir_ty_to_key_tree(ty).get());
            }

            DefKind::Struct | DefKind::Union | DefKind::Enum => {
                let item = match_or!([tcx.hir().get(hir_id)]
                                     hir::Node::Item(item) => item;
                                     panic!("expected Item node"));
                let ch = match item.kind {
                    hir::ItemKind::Struct(ref def, _) |
                    hir::ItemKind::Union(ref def, _) => {
                        def.fields()
                            .iter()
                            .map(|field| self.hir_ty_to_key_tree(&field.ty))
                            .collect::<Vec<_>>()
                    }

                    hir::ItemKind::Enum(ref def, _) => {
                        // FIXME: I think this matches the `ty::Ty` order,
                        // but should double-check somehow
                        def.variants
                            .iter()
                            .flat_map(|v| v.data.fields())
                            .map(|field| self.hir_ty_to_key_tree(&field.ty))
                            .collect::<Vec<_>>()
                    }

                    _ => panic!("expected Struct/Union/Enum, got {:?}", item)
                };
                self.replace_with_node(new_node, &ch);
            }

            // TODO: handle `Variant`???

            DefKind::Fn => {
                let decl = match tcx.hir().get(hir_id) {
                    hir::Node::Item(ref item) => {
                        match_or!([item.kind]
                                  hir::ItemKind::Fn(ref decl, ..) => decl;
                                  panic!("expected ItemKind::Fn, got {:?}", item))
                    }
                    hir::Node::ForeignItem(ref item) => {
                        match_or!([item.kind]
                                  hir::ForeignItemKind::Fn(ref decl, ..) => decl;
                                  panic!("expected ForeignItemKind::Fn, got {:?}", item))
                    }
                    n @ _ => panic!("expected Item/ForeignItem, got {:?}", n)
                };

                let output_key_tree = match decl.output {
                    hir::FunctionRetTy::DefaultReturn(_) => self.new_none_leaf(),
                    hir::FunctionRetTy::Return(ref ty) => self.hir_ty_to_key_tree(ty),
                };

                let ch = decl.inputs
                    .iter()
                    .map(|ty| self.hir_ty_to_key_tree(ty))
                    .chain(std::iter::once(output_key_tree))
                    .collect::<Vec<_>>();

                self.replace_with_node(new_node, &ch);
            }

            DefKind::Const => {
                let item = match_or!([tcx.hir().get(hir_id)]
                                     hir::Node::Item(item) => item;
                                     panic!("expected Item node"));
                let ty = match_or!([item.kind]
                                   hir::ItemKind::Const(ref ty, _) => ty;
                                   panic!("expected ItemKind::Const, got {:?}", item));
                new_node.set(self.hir_ty_to_key_tree(ty).get());
            }

            DefKind::Static => {
                let ty = match tcx.hir().get(hir_id) {
                    hir::Node::Item(ref item) => {
                        match_or!([item.kind]
                                  hir::ItemKind::Static(ref ty, ..) => ty;
                                  panic!("expected ItemKind::Static, got {:?}", item))
                    }
                    hir::Node::ForeignItem(ref item) => {
                        match_or!([item.kind]
                                  hir::ForeignItemKind::Static(ref ty, ..) => ty;
                                  panic!("expected ForeignItemKind::Static, got {:?}", item))
                    }
                    n @ _ => panic!("expected Item/ForeignItem, got {:?}", n)
                };
                new_node.set(self.hir_ty_to_key_tree(ty).get());
            }

            // TODO: handle `Method`???

            _ => {}
        };
        new_node
    }

    fn ty_to_key_tree(&mut self, ty: ty::Ty<'tcx>, allow_mach: bool) -> LitTyKeyTree<'kt, 'tcx> {
        // We memoize the result both to improve performance and
        // eliminate infinite recursion
        let hash_key = (ty, allow_mach);
        if let Some(kt) = self.ty_key_tree_cache.get(&hash_key) {
            return kt;
        }
        let new_node = self.new_none_leaf();
        self.ty_key_tree_cache.insert(hash_key, new_node);

        let tcx = self.cx.ty_ctxt();
        let mut fn_sig_to_key_tree = |fn_sig: ty::PolyFnSig<'tcx>, sig_mach: bool| {
            let ch = fn_sig
                .skip_binder()
                .inputs_and_output
                .iter()
                .map(|ty| self.ty_to_key_tree(ty, sig_mach))
                .collect::<Vec<_>>();
            self.replace_with_node(new_node, &ch);
        };

        match ty.kind {
            ty::TyKind::Int(_) |
            ty::TyKind::Uint(_) |
            ty::TyKind::Float(_) if allow_mach => {
                self.replace_with_leaf(new_node, LitTySource::Actual(ty));
            }

            ty::TyKind::Adt(ref adt_ref, ref substs)
            if adt_ref.is_box() || adt_ref.is_rc() || adt_ref.is_arc() => {
                // Ignore the actual structure for these types, and just
                // use the inner type as the single child
                let inner_ty = substs.type_at(0);
                let ty_kt = self.ty_to_key_tree(inner_ty, allow_mach);
                self.replace_with_node(new_node, &[ty_kt]);
            }

            ty::TyKind::Adt(ref adt_def, ref substs) => {
                let ch = adt_def.all_fields()
                    .map(|field| self.ty_to_key_tree(field.ty(tcx, substs), allow_mach))
                    .collect::<Vec<_>>();
                self.replace_with_node(new_node, &ch);
            }

            ty::TyKind::Str => {
                let u8_ty = tcx.mk_mach_uint(UintTy::U8);
                let u8_kt = self.ty_to_key_tree(u8_ty, true);
                self.replace_with_node(new_node, &[u8_kt]);
            }

            ty::TyKind::Array(ty, _) |
            ty::TyKind::Slice(ty) |
            ty::TyKind::RawPtr(ty::TypeAndMut { ty, .. }) |
            ty::TyKind::Ref(_, ty, _) => {
                let ty_kt = self.ty_to_key_tree(ty, allow_mach);
                self.replace_with_node(new_node, &[ty_kt]);
            }

            ty::TyKind::FnDef(def_id, _) => {
                // Since we're using the original signature
                // and not performing any substitutions,
                // it's safe to include the machine types
                fn_sig_to_key_tree(tcx.fn_sig(def_id), true);
            }
            ty::TyKind::FnPtr(fn_sig) => {
                fn_sig_to_key_tree(fn_sig, allow_mach);
            }

            // TODO: Closure

            ty::TyKind::Tuple(ref elems) => {
                let ch = elems.types()
                    .map(|ty| self.ty_to_key_tree(ty, allow_mach))
                    .collect::<Vec<_>>();
                self.replace_with_node(new_node, &ch);
            }

            // All the others are irrelevant
            _ => {}
        }
        new_node
    }

    fn expr_ty_to_key_tree(&mut self, ex: &Expr) -> LitTyKeyTree<'kt, 'tcx> {
        if let Some(ty) = self.cx.opt_node_type(ex.id) {
            self.ty_to_key_tree(ty, false)
        } else {
            self.new_none_leaf()
        }
    }

    fn visit_expr_unify(&mut self, ex: &Expr, kt: LitTyKeyTree<'kt, 'tcx>) {
        let tcx = self.cx.ty_ctxt();
        match ex.kind {
            ExprKind::Box(ref e) => {
                assert!(kt.get().children().len() == 1);
                let inner_key_tree = kt.get().children()[0];
                self.visit_expr_unify(e, inner_key_tree);
            }

            ExprKind::Array(ref exprs) => {
                assert!(kt.get().children().len() == 1);
                let inner_key_tree = kt.get().children()[0];
                for e in exprs {
                    self.visit_expr_unify(e, inner_key_tree);
                }
            }

            ExprKind::Call(ref callee, ref args) => {
                let callee_key_tree = self.expr_ty_to_key_tree(callee);
                self.visit_expr_unify(callee, callee_key_tree);
                // TODO: check if generic
                if let &[ref input_key_trees @ .., output_key_tree] = callee_key_tree.get().children() {
                    for (arg_expr, arg_key_tree) in args.iter().zip(input_key_trees.iter()) {
                        self.visit_expr_unify(arg_expr, arg_key_tree);
                    }
                    self.unify_key_trees(kt, output_key_tree);
                } else {
                    panic!("invalid key tree for call: {:?}", ex);
                }
            }

            ExprKind::MethodCall(ref segment, ref args) => {
                self.visit_path_segment(ex.span, segment);
                // FIXME: we have to skip the `self` argument,
                // since literals without suffixes are sometimes invalid,
                // e.g., `1.wrapping_add(2)`
                for arg in args.iter().skip(1) {
                    self.visit_expr(arg);
                    // TODO: unify arguments and return type, just like in `Call`
                }
            }

            ExprKind::Tup(ref exprs) => {
                assert!(kt.get().children().len() == exprs.len());
                for (ch, ch_kt) in exprs.iter().zip(kt.get().children().iter()) {
                    self.visit_expr_unify(ch, ch_kt);
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
                            assert!(ptr_key_tree.get().children().len() == 1);
                            let inner_key_tree = ptr_key_tree.get().children()[0];
                            self.unify_key_trees(kt, inner_key_tree);
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

                self.visit_expr(e);
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

            ExprKind::Assign(ref lhs, ref rhs) => {
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
                //TMP TODO: build the key_tree for the expr, then unify
                //the element for the current field with `kt`
                self.visit_expr(e);
                self.visit_ident(ident);
                if let Some(struct_ty) = self.cx.opt_adjusted_node_type(e.id) {
                    if let Some(def) = struct_ty.ty_adt_def() {
                        for field in def.all_fields() {
                            if field.ident == ident {
                                let hir_id = tcx.hir().as_local_hir_id(field.did);
                                let field_key_tree = if let Some(hir_id) = hir_id {
                                    // Field is defined in our crate, look it up
                                    match tcx.hir().get(hir_id) {
                                        hir::Node::Field(field) =>
                                            self.hir_ty_to_key_tree(&field.ty),
                                        _ => self.new_none_leaf()
                                    }
                                } else {
                                    // Field is defined in another crate
                                    self.ty_to_key_tree(tcx.type_of(field.did), true)
                                };
                                self.unify_key_trees(kt, field_key_tree);
                            }
                        }
                    }
                    // TODO: handle tuples; pretty easy with the key trees
                }
            }

            ExprKind::Index(ref e, ref idx) => {
                let e_key_tree = self.expr_ty_to_key_tree(e);
                self.visit_expr_unify(e, e_key_tree);

                // If the lhs type is array/slice/`str`, we can unify the result
                // of the `ExprKind::Index` with the inner type of the base
                if let Some(e_ty) = self.cx.opt_node_type(e.id) {
                    use ty::TyKind::*;
                    if let Array(..) | Slice(_) | Str = e_ty.kind {
                        assert!(e_key_tree.get().children().len() == 1);
                        let inner_key_tree = e_key_tree.get().children()[0];
                        self.unify_key_trees(kt, inner_key_tree);
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
                    self.new_none_leaf()
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
                let node = self.cx.hir_map().find(ex.id).unwrap();
				let e = match_or!([node] hir::Node::Expr(e) => e;
                                  panic!("expected Expr, got {:?}", node));
				let qpath = match_or!([e.kind] hir::ExprKind::Path(ref q) => q;
                                      panic!("expected ExprKind::Path, got {:?}", e.kind));
                if let hir::QPath::Resolved(_, ref path) = qpath {
                    let path_key_tree = self.hir_path_to_key_tree(path);
                    self.unify_key_trees(kt, path_key_tree);
                }
                // TODO: handle TypeRelative paths
            }

            ExprKind::AddrOf(_, ref e) => {
                assert!(kt.get().children().len() == 1);
                let inner_key_tree = kt.get().children()[0];
                self.visit_expr_unify(e, inner_key_tree);
            }

            // TODO: unify `Break` with return values
            //
            // TODO: unify `Ret` with function return type
            //
            // TODO: unify non-generic `Struct`

            ExprKind::Repeat(ref e, _) => {
                assert!(kt.get().children().len() == 1);
                let inner_key_tree = kt.get().children()[0];
                self.visit_expr_unify(e, inner_key_tree);
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

    fn visit_pat_unify(&mut self, p: &Pat, kt: LitTyKeyTree<'kt, 'tcx>) {
        match p.kind {
            PatKind::Or(ref pats) => {
                for pat in pats {
                    self.visit_pat_unify(pat, kt);
                }
            }

            PatKind::Lit(ref e) => {
                self.visit_expr_unify(e, kt);
            }

            PatKind::Range(ref start, ref end, _) => {
                // TODO: approximate???
                let inner_key_tree = self.expr_ty_to_key_tree(start);
                self.visit_expr_unify(start, inner_key_tree);
                self.visit_expr_unify(end, inner_key_tree);
            }

            PatKind::Paren(ref pat) => self.visit_pat_unify(pat, kt),

            // TODO: handle more pattern types once we have hierarchical keys

            _ => visit::walk_pat(self, p)
        }
    }

    // TODO: handle `Field`
    //
    // TODO: handle `AnonConst`
}

impl<'ast, 'a, 'kt, 'tcx> Visitor<'ast> for UnifyVisitor<'a, 'kt, 'tcx> {
    fn visit_expr(&mut self, ex: &'ast Expr) {
        // TODO: approximate structure
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
        if let Some(ref init) = l.init {
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
            ItemKind::Static(ref ty, _, ref init) |
            ItemKind::Const(ref ty, ref init) => {
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
        // TODO: approximate structure
        let key_tree = self.new_none_leaf();
        self.visit_pat_unify(p, key_tree);
    }
}

#[derive(Debug, Clone, Copy)]
enum LitTyKeyNode<'kt, 'tcx: 'kt> {
    Node(&'kt [LitTyKeyTree<'kt, 'tcx>]),
    Leaf(LitTyKey<'tcx>),
}

type LitTyKeyTree<'kt, 'tcx> = &'kt Cell<LitTyKeyNode<'kt, 'tcx>>;

impl<'kt, 'tcx: 'kt> LitTyKeyNode<'kt, 'tcx> {
    fn as_key(&self) -> LitTyKey<'tcx> {
        match self {
            Self::Leaf(key) => *key,
            Self::Node(ch) => panic!("expected leaf, found node: {:?}", ch)
        }
    }

    fn children(&self) -> &'kt [LitTyKeyTree<'kt, 'tcx>] {
        match self {
            Self::Node(ch) => ch,
            Self::Leaf(key) => panic!("expected node, found leaf: {:?}", key)
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

