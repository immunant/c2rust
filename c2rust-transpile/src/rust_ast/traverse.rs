use syn::*;

/// Traverse the AST in pre-order, which also happens to be the order of subtrees in the
/// pretty-printed output.
pub trait Traversal: Sized {
    fn traverse_stmt(&mut self, s: Stmt) -> Stmt {
        traverse_stmt_def(self, s)
    }

    fn traverse_expr(&mut self, s: Expr) -> Expr {
        traverse_expr_def(self, s)
    }

    fn traverse_trait_item(&mut self, ti: TraitItem) -> TraitItem {
        traverse_trait_item_def(self, ti)
    }

    fn traverse_impl_item(&mut self, ii: ImplItem) -> ImplItem {
        traverse_impl_item_def(self, ii)
    }

    fn traverse_block(&mut self, b: Block) -> Block {
        traverse_block_def(self, b)
    }

    fn traverse_local(&mut self, l: Local) -> Local {
        traverse_local_def(self, l)
    }

    fn traverse_arm(&mut self, a: Arm) -> Arm {
        traverse_arm_def(self, a)
    }

    fn traverse_field(&mut self, f: FieldValue) -> FieldValue {
        traverse_field_def(self, f)
    }

    fn traverse_mod(&mut self, m: ItemMod) -> ItemMod {
        traverse_mod_def(self, m)
    }

    fn traverse_foreign_mod(&mut self, m: ItemForeignMod) -> ItemForeignMod {
        traverse_foreign_mod_def(self, m)
    }

    fn traverse_item(&mut self, i: Item) -> Item {
        traverse_item_def(self, i)
    }

    fn traverse_foreign_item(&mut self, i: ForeignItem) -> ForeignItem {
        i
    }
}

/// Apply a `Traversal` to an AST node.
trait Traversable {
    fn traverse<T: Traversal>(self, t: &mut T) -> Self;
}

macro_rules! traversable_impl {
    ( $traversable_ty:ty, $method_name:ident ) => {
        impl Traversable for $traversable_ty {
            fn traverse<T: Traversal>(self, t: &mut T) -> Self {
                t.$method_name(self)
            }
        }
    };
}

traversable_impl!(Stmt, traverse_stmt);
traversable_impl!(Expr, traverse_expr);
traversable_impl!(TraitItem, traverse_trait_item);
traversable_impl!(ImplItem, traverse_impl_item);
traversable_impl!(Block, traverse_block);
traversable_impl!(Local, traverse_local);
traversable_impl!(Arm, traverse_arm);
traversable_impl!(FieldValue, traverse_field);
traversable_impl!(ItemMod, traverse_mod);
traversable_impl!(ItemForeignMod, traverse_foreign_mod);
traversable_impl!(Item, traverse_item);
traversable_impl!(ForeignItem, traverse_foreign_item);

impl<A: Traversable> Traversable for Vec<A> {
    fn traverse<T: Traversal>(self, t: &mut T) -> Self {
        self.into_iter().map(|x| x.traverse(t)).collect()
    }
}

impl<A: Clone + Traversable, B> Traversable for syn::punctuated::Punctuated<A, B> {
    fn traverse<T: Traversal>(mut self, t: &mut T) -> Self {
        self.iter_mut().for_each(|x| *x = x.clone().traverse(t));
        self
    }
}

impl<A: Traversable> Traversable for Option<A> {
    fn traverse<T: Traversal>(self, t: &mut T) -> Self {
        self.map(|x| x.traverse(t))
    }
}

impl<A, B: Traversable> Traversable for Option<(A, B)> {
    fn traverse<T: Traversal>(self, t: &mut T) -> Self {
        self.map(|x| (x.0, x.1.traverse(t)))
    }
}

impl<A: Traversable + 'static> Traversable for Box<A> {
    fn traverse<T: Traversal>(self, t: &mut T) -> Self {
        Box::new((*self).traverse(t))
    }
}

pub fn traverse_stmt_def<W: Traversal>(walk: &mut W, s: Stmt) -> Stmt {
    match s {
        Stmt::Local(p_local) => Stmt::Local(p_local.traverse(walk)),
        Stmt::Item(p_item) => Stmt::Item(p_item.traverse(walk)),
        Stmt::Expr(p_expr, semi) => Stmt::Expr(p_expr.traverse(walk), semi),
        Stmt::Macro(p_macro) => Stmt::Macro(p_macro),
    }
}

pub fn traverse_expr_def<W: Traversal>(walk: &mut W, e: Expr) -> Expr {
    match e {
        Expr::Array(e) => Expr::Array(ExprArray {
            elems: e.elems.traverse(walk),
            ..e
        }),
        Expr::Call(e) => Expr::Call(ExprCall {
            func: e.func.traverse(walk),
            args: e.args.traverse(walk),
            ..e
        }),
        Expr::MethodCall(e) => Expr::MethodCall(ExprMethodCall {
            receiver: e.receiver.traverse(walk),
            args: e.args.traverse(walk),
            ..e
        }),
        Expr::Tuple(e) => Expr::Tuple(ExprTuple {
            elems: e.elems.traverse(walk),
            ..e
        }),
        Expr::Binary(e) => Expr::Binary(ExprBinary {
            left: e.left.traverse(walk),
            right: e.right.traverse(walk),
            ..e
        }),
        Expr::Unary(e) => Expr::Unary(ExprUnary {
            expr: e.expr.traverse(walk),
            ..e
        }),
        Expr::Cast(e) => Expr::Cast(ExprCast {
            expr: e.expr.traverse(walk),
            ..e
        }),
        Expr::Let(e) => Expr::Let(ExprLet {
            expr: e.expr.traverse(walk),
            ..e
        }),
        Expr::If(e) => Expr::If(ExprIf {
            cond: e.cond.traverse(walk),
            then_branch: e.then_branch.traverse(walk),
            else_branch: e.else_branch.traverse(walk),
            ..e
        }),
        Expr::While(e) => Expr::While(ExprWhile {
            cond: e.cond.traverse(walk),
            body: e.body.traverse(walk),
            ..e
        }),
        Expr::ForLoop(e) => Expr::ForLoop(ExprForLoop {
            expr: e.expr.traverse(walk),
            body: e.body.traverse(walk),
            ..e
        }),
        Expr::Loop(e) => Expr::Loop(ExprLoop {
            body: e.body.traverse(walk),
            ..e
        }),
        Expr::Match(e) => Expr::Match(ExprMatch {
            expr: e.expr.traverse(walk),
            arms: e.arms.traverse(walk),
            ..e
        }),
        Expr::Closure(e) => Expr::Closure(ExprClosure {
            body: e.body.traverse(walk),
            ..e
        }),
        Expr::Block(e) => Expr::Block(ExprBlock {
            block: e.block.traverse(walk),
            ..e
        }),
        Expr::Assign(e) => Expr::Assign(ExprAssign {
            left: e.left.traverse(walk),
            right: e.right.traverse(walk),
            ..e
        }),
        Expr::Field(e) => Expr::Field(ExprField {
            base: e.base.traverse(walk),
            ..e
        }),
        Expr::Index(e) => Expr::Index(ExprIndex {
            expr: e.expr.traverse(walk),
            index: e.index.traverse(walk),
            ..e
        }),
        Expr::Range(e) => Expr::Range(ExprRange {
            start: e.start.traverse(walk),
            end: e.end.traverse(walk),
            ..e
        }),
        Expr::Path(e) => Expr::Path(ExprPath { ..e }),
        Expr::Reference(e) => Expr::Reference(ExprReference {
            expr: e.expr.traverse(walk),
            ..e
        }),
        Expr::Break(e) => Expr::Break(ExprBreak {
            expr: e.expr.traverse(walk),
            ..e
        }),
        Expr::Continue(e) => Expr::Continue(ExprContinue { ..e }),
        Expr::Return(e) => Expr::Return(ExprReturn {
            expr: e.expr.traverse(walk),
            ..e
        }),
        //Expr::InlineAsm(e) => Expr::InlineAsm(ExprInlineAsm { asm, ..e }),
        Expr::Macro(e) => Expr::Macro(ExprMacro { ..e }),
        Expr::Struct(e) => Expr::Struct(ExprStruct {
            fields: e.fields.traverse(walk),
            rest: e.rest.traverse(walk),
            ..e
        }),
        Expr::Repeat(e) => Expr::Repeat(ExprRepeat {
            expr: e.expr.traverse(walk),
            len: e.len.traverse(walk),
            ..e
        }),
        Expr::Paren(e) => Expr::Paren(ExprParen {
            expr: e.expr.traverse(walk),
            ..e
        }),
        Expr::Try(e) => Expr::Try(ExprTry {
            expr: e.expr.traverse(walk),
            ..e
        }),
        Expr::Yield(e) => Expr::Yield(ExprYield {
            expr: e.expr.traverse(walk),
            ..e
        }),
        Expr::Lit(e) => Expr::Lit(ExprLit { ..e }),
        Expr::Async(e) => Expr::Async(ExprAsync {
            block: e.block.traverse(walk),
            ..e
        }),
        Expr::TryBlock(e) => Expr::TryBlock(ExprTryBlock {
            block: e.block.traverse(walk),
            ..e
        }),
        Expr::Unsafe(e) => Expr::Unsafe(ExprUnsafe {
            block: e.block.traverse(walk),
            ..e
        }),
        Expr::Verbatim(_tokens) => unimplemented!(),
        Expr::Await(e) => Expr::Await(ExprAwait {
            base: e.base.traverse(walk),
            ..e
        }),
        _ => unimplemented!(),
    }
}

pub fn traverse_trait_item_def<W: Traversal>(walk: &mut W, ti: TraitItem) -> TraitItem {
    match ti {
        TraitItem::Const(ti) => TraitItem::Const(TraitItemConst {
            default: ti.default.traverse(walk),
            ..ti
        }),
        TraitItem::Fn(ti) => TraitItem::Fn(TraitItemFn {
            default: ti.default.traverse(walk),
            ..ti
        }),
        TraitItem::Type(ti) => TraitItem::Type(TraitItemType { ..ti }),
        TraitItem::Macro(ti) => TraitItem::Macro(TraitItemMacro { ..ti }),
        _ => unimplemented!(),
    }
}

pub fn traverse_impl_item_def<W: Traversal>(walk: &mut W, ii: ImplItem) -> ImplItem {
    match ii {
        ImplItem::Const(ii) => ImplItem::Const(ImplItemConst {
            expr: ii.expr.traverse(walk),
            ..ii
        }),
        ImplItem::Fn(ii) => ImplItem::Fn(ImplItemFn {
            block: ii.block.traverse(walk),
            ..ii
        }),
        ImplItem::Type(ii) => ImplItem::Type(ImplItemType { ..ii }),
        ImplItem::Macro(ii) => ImplItem::Macro(ImplItemMacro { ..ii }),
        _ => unimplemented!(),
    }
}

pub fn traverse_block_def<W: Traversal>(walk: &mut W, mut b: Block) -> Block {
    b.stmts = b.stmts.traverse(walk);
    b
}

pub fn traverse_local_def<W: Traversal>(walk: &mut W, mut l: Local) -> Local {
    l.init = l.init.map(|x| LocalInit {
        expr: x.expr.traverse(walk),
        ..x
    });
    l
}

pub fn traverse_local_init_def<W: Traversal>(walk: &mut W, mut l: LocalInit) -> LocalInit {
    l.expr = l.expr.traverse(walk);
    l
}

pub fn traverse_arm_def<W: Traversal>(walk: &mut W, mut a: Arm) -> Arm {
    a.guard = a.guard.traverse(walk);
    a.body = a.body.traverse(walk);
    a
}

pub fn traverse_field_def<W: Traversal>(walk: &mut W, mut f: FieldValue) -> FieldValue {
    f.expr = f.expr.traverse(walk);
    f
}

pub fn traverse_mod_def<W: Traversal>(walk: &mut W, mut m: ItemMod) -> ItemMod {
    m.content = m.content.traverse(walk);
    m
}

pub fn traverse_foreign_mod_def<W: Traversal>(
    walk: &mut W,
    mut m: ItemForeignMod,
) -> ItemForeignMod {
    m.items = m.items.traverse(walk);
    m
}

pub fn traverse_item_def<W: Traversal>(walk: &mut W, i: Item) -> Item {
    match i {
        Item::Static(item) => Item::Static(ItemStatic {
            expr: item.expr.traverse(walk),
            ..item
        }),
        Item::Const(item) => Item::Const(ItemConst {
            expr: item.expr.traverse(walk),
            ..item
        }),
        Item::Fn(item) => Item::Fn(ItemFn {
            block: item.block.traverse(walk),
            ..item
        }),
        Item::Mod(item) => Item::Mod(ItemMod {
            content: item.content.traverse(walk),
            ..item
        }),
        Item::ForeignMod(item) => Item::ForeignMod(ItemForeignMod {
            items: item.items.traverse(walk),
            ..item
        }),
        Item::Trait(item) => Item::Trait(ItemTrait {
            items: item.items.traverse(walk),
            ..item
        }),
        Item::Impl(item) => Item::Impl(ItemImpl {
            items: item.items.traverse(walk),
            ..item
        }),
        Item::Use(item) => Item::Use(ItemUse { ..item }),
        Item::ExternCrate(item) => Item::ExternCrate(ItemExternCrate { ..item }),
        //Item::GlobalAsm(ItemGlobalAsm { u}) => Item::GlobalAsm(ItemGlobalAsm { u}),
        Item::Type(item) => Item::Type(ItemType { ..item }),
        Item::Enum(item) => Item::Enum(ItemEnum { ..item }),
        Item::Struct(item) => Item::Struct(ItemStruct { ..item }),
        Item::Union(item) => Item::Union(ItemUnion { ..item }),
        Item::TraitAlias(item) => Item::TraitAlias(ItemTraitAlias { ..item }),
        Item::Macro(item) => Item::Macro(ItemMacro { ..item }),
        _ => unimplemented!(),
    }
}
