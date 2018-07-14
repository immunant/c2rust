use syntax::ast::*;

/// Traverse the AST in pre-order, which also happens to be the order of subtrees in the
/// pretty-printed output.
pub trait Traverse: Sized {
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

    fn traverse_field(&mut self, f: Field) -> Field {
        traverse_field_def(self, f)
    }

    fn traverse_mod(&mut self, m: Mod) -> Mod {
        traverse_mod_def(self, m)
    }

    fn traverse_foreign_mod(&mut self, m: ForeignMod) -> ForeignMod {
        traverse_foreign_mod_def(self, m)
    }

    fn traverse_item(&mut self, i: Item) -> Item {
        traverse_item_def(self, i)
    }

    fn traverse_foreign_item(&mut self, i: ForeignItem) -> ForeignItem {
        i
    }
}

pub fn traverse_stmt_def<W: Traverse>(walk: &mut W, mut s: Stmt) -> Stmt {
    s.node = match s.node {
        StmtKind::Local(p_local) => StmtKind::Local(p_local.map(|l| walk.traverse_local(l))),
        StmtKind::Item(p_item) => StmtKind::Item(p_item.map(|item| walk.traverse_item(item))),
        StmtKind::Expr(p_expr) => StmtKind::Expr(p_expr.map(|expr| walk.traverse_expr(expr))),
        StmtKind::Semi(p_expr) => StmtKind::Semi(p_expr.map(|expr| walk.traverse_expr(expr))),
        StmtKind::Mac(m) => StmtKind::Mac(m),
    };
    s
}

pub fn traverse_expr_def<W: Traverse>(walk: &mut W, mut e: Expr) -> Expr {
    e.node = match e.node {
        ExprKind::Box(p_expr) => ExprKind::Box(p_expr.map(|expr| walk.traverse_expr(expr))),
        ExprKind::Array(vec_p_expr) => ExprKind::Array(vec_p_expr
            .into_iter()
            .map(|p_expr| p_expr.map(|expr| walk.traverse_expr(expr)))
            .collect()
        ),
        ExprKind::Call(p_expr, vec_p_expr) => ExprKind::Call(
            p_expr.map(|expr| walk.traverse_expr(expr)),
            vec_p_expr
                .into_iter()
                .map(|p_expr| p_expr.map(|expr| walk.traverse_expr(expr)))
                .collect(),
        ),
        ExprKind::MethodCall(meth, vec_p_expr) => ExprKind::MethodCall(meth, vec_p_expr
            .into_iter()
            .map(|p_expr| p_expr.map(|expr| walk.traverse_expr(expr)))
            .collect()
        ),
        ExprKind::Tup(vec_p_expr) => ExprKind::Tup(vec_p_expr
            .into_iter()
            .map(|p_expr| p_expr.map(|expr| walk.traverse_expr(expr)))
            .collect()
        ),
        ExprKind::Binary(op, lhs, rhs) => ExprKind::Binary(
            op,
            lhs.map(|l| walk.traverse_expr(l)),
            rhs.map(|r| walk.traverse_expr(r)),
        ),
        ExprKind::Unary(op, p_expr) => ExprKind::Unary(
            op,
            p_expr.map(|expr| walk.traverse_expr(expr)),
        ),
        ExprKind::Cast(p_expr, t) => ExprKind::Cast(
            p_expr.map(|expr| walk.traverse_expr(expr)),
            t,
        ),
        ExprKind::Type(p_expr, t) => ExprKind::Type(
            p_expr.map(|expr| walk.traverse_expr(expr)),
            t,
        ),
        ExprKind::If(p_cond, p_block_thn, p_expr_els_opt) => ExprKind::If(
            p_cond.map(|expr| walk.traverse_expr(expr)),
            p_block_thn.map(|block_thn| walk.traverse_block(block_thn)),
            p_expr_els_opt
                .map(|p_expr_els| p_expr_els.map(|expr_els| walk.traverse_expr(expr_els))),
        ),
        ExprKind::IfLet(pats, p_cond, p_block_thn, p_expr_els_opt) => ExprKind::IfLet(
            pats,
            p_cond.map(|expr| walk.traverse_expr(expr)),
            p_block_thn.map(|block_thn| walk.traverse_block(block_thn)),
            p_expr_els_opt
                .map(|p_expr_els| p_expr_els.map(|expr_els| walk.traverse_expr(expr_els))),
        ),
        ExprKind::While(p_cond, p_block_body, lbl) => ExprKind::While(
            p_cond.map(|expr| walk.traverse_expr(expr)),
            p_block_body.map(|block_body| walk.traverse_block(block_body)),
            lbl,
        ),
        ExprKind::WhileLet(pats, p_cond, p_block_body, lbl) => ExprKind::WhileLet(
            pats,
            p_cond.map(|expr| walk.traverse_expr(expr)),
            p_block_body.map(|block_body| walk.traverse_block(block_body)),
            lbl,
        ),
        ExprKind::ForLoop(pat, p_cond, p_block_body, lbl) => ExprKind::ForLoop(
            pat,
            p_cond.map(|expr| walk.traverse_expr(expr)),
            p_block_body.map(|block_body| walk.traverse_block(block_body)),
            lbl,
        ),
        ExprKind::Loop(p_block_body, lbl) => ExprKind::Loop(
            p_block_body.map(|block_body| walk.traverse_block(block_body)),
            lbl,
        ),
        ExprKind::Match(p_cond, vec_arm) => ExprKind::Match(
            p_cond.map(|expr| walk.traverse_expr(expr)),
            vec_arm.into_iter().map(|arm| walk.traverse_arm(arm)).collect(),
        ),
        ExprKind::Closure(cap, mov, fn_decl, p_expr, s) => ExprKind::Closure(
            cap,
            mov,
            fn_decl,
            p_expr.map(|expr| walk.traverse_expr(expr)),
            s,
        ),
        ExprKind::Block(p_block, lbl) => ExprKind::Block(
            p_block.map(|blk| walk.traverse_block(blk)),
            lbl
        ),
        ExprKind::Catch(p_block) => ExprKind::Catch(
            p_block.map(|blk| walk.traverse_block(blk)),
        ),
        ExprKind::Assign(lhs, rhs) => ExprKind::Assign(
            lhs.map(|l| walk.traverse_expr(l)),
            rhs.map(|r| walk.traverse_expr(r)),
        ),
        ExprKind::AssignOp(op, lhs, rhs) => ExprKind::AssignOp(
            op,
            lhs.map(|l| walk.traverse_expr(l)),
            rhs.map(|r| walk.traverse_expr(r)),
        ),
        ExprKind::Field(p_expr, f) => ExprKind::Field(
            p_expr.map(|expr| walk.traverse_expr(expr)),
            f,
        ),
        ExprKind::Index(lhs, rhs) => ExprKind::Index(
            lhs.map(|l| walk.traverse_expr(l)),
            rhs.map(|r| walk.traverse_expr(r)),
        ),
        ExprKind::Range(lhs_opt, rhs_opt, rl) => ExprKind::Range(
            lhs_opt.map(|lhs| lhs.map(|l| walk.traverse_expr(l))),
            rhs_opt.map(|rhs| rhs.map(|r| walk.traverse_expr(r))),
            rl,
        ),
        ExprKind::Path(qself,p) => ExprKind::Path(qself,p),
        ExprKind::AddrOf(m, p_expr) => ExprKind::AddrOf(
            m,
            p_expr.map(|expr| walk.traverse_expr(expr)),
        ),
        ExprKind::Break(lbl, opt_p_expr) => ExprKind::Break(
            lbl,
            opt_p_expr.map(|p_expr| p_expr.map(|expr| walk.traverse_expr(expr))),
        ),
        ExprKind::Continue(lbl) => ExprKind::Continue(lbl),
        ExprKind::Ret(opt_p_expr) => ExprKind::Ret(
            opt_p_expr.map(|p_expr| p_expr.map(|expr| walk.traverse_expr(expr))),
        ),
        ExprKind::InlineAsm(asm) => ExprKind::InlineAsm(asm),
        ExprKind::Mac(mac) => ExprKind::Mac(mac),
        ExprKind::Struct(p, vec_field, opt_p_expr) => ExprKind::Struct(
            p,
            vec_field.into_iter().map(|field| walk.traverse_field(field)).collect(),
            opt_p_expr.map(|p_expr| p_expr.map(|expr| walk.traverse_expr(expr))),
        ),
        ExprKind::Repeat(p_expr, c) => ExprKind::Repeat(
            p_expr.map(|expr| walk.traverse_expr(expr)),
            c,
        ),
        ExprKind::Paren(p_expr) => ExprKind::Paren(p_expr.map(|expr| walk.traverse_expr(expr))),
        ExprKind::Try(p_expr) => ExprKind::Try(p_expr.map(|expr| walk.traverse_expr(expr))),
        ExprKind::Yield(opt_p_expr) => ExprKind::Yield(
            opt_p_expr.map(|p_expr| p_expr.map(|expr| walk.traverse_expr(expr)))
        ),
        ExprKind::Lit(l) => ExprKind::Lit(l),
        ExprKind::ObsoleteInPlace(lhs, rhs) => ExprKind::ObsoleteInPlace(
            lhs.map(|l| walk.traverse_expr(l)),
            rhs.map(|r| walk.traverse_expr(r)),
        ),
    };
    e
}

pub fn traverse_trait_item_def<W: Traverse>(walk: &mut W, mut ti: TraitItem) -> TraitItem {
    ti.node = match ti.node {
        TraitItemKind::Const(ty, opt_p_expr) => TraitItemKind::Const(
            ty,
            opt_p_expr.map(|p_expr| p_expr.map(|expr| walk.traverse_expr(expr))),
        ),
        TraitItemKind::Method(sig, opt_p_block) => TraitItemKind::Method(
            sig,
            opt_p_block.map(|p_block| p_block.map(|block| walk.traverse_block(block))),
        ),
        TraitItemKind::Type(bds, t) => TraitItemKind::Type(bds, t),
        TraitItemKind::Macro(mac) => TraitItemKind::Macro(mac),
    };
    ti
}

pub fn traverse_impl_item_def<W: Traverse>(walk: &mut W, mut ii: ImplItem) -> ImplItem {
    ii.node = match ii.node {
        ImplItemKind::Const(ty, p_expr) => ImplItemKind::Const(
            ty,
            p_expr.map(|expr| walk.traverse_expr(expr)),
        ),
        ImplItemKind::Method(sig, p_block) => ImplItemKind::Method(
            sig,
            p_block.map(|block| walk.traverse_block(block)),
        ),
        ImplItemKind::Type(t) => ImplItemKind::Type(t),
        ImplItemKind::Macro(mac) => ImplItemKind::Macro(mac),
    };
    ii
}

pub fn traverse_block_def<W: Traverse>(walk: &mut W, mut b: Block) -> Block {
    b.stmts = b.stmts.into_iter().map(|stmt| walk.traverse_stmt(stmt)).collect();
    b
}

pub fn traverse_local_def<W: Traverse>(walk: &mut W, mut l: Local) -> Local {
    l.init = l.init.map(|p_expr| p_expr.map(|expr| walk.traverse_expr(expr)));
    l
}

pub fn traverse_arm_def<W: Traverse>(walk: &mut W, mut a: Arm) -> Arm {
    a.guard = a.guard.map(|p_expr| p_expr.map(|expr| walk.traverse_expr(expr)));
    a.body = a.body.map(|expr| walk.traverse_expr(expr));
    a
}

pub fn traverse_field_def<W: Traverse>(walk: &mut W, mut f: Field) -> Field {
    f.expr = f.expr.map(|expr| walk.traverse_expr(expr));
    f
}

pub fn traverse_mod_def<W: Traverse>(walk: &mut W, mut m: Mod) -> Mod {
    m.items = m.items.into_iter().map(|p_i| p_i.map(|i| walk.traverse_item(i))).collect();
    m
}

pub fn traverse_foreign_mod_def<W: Traverse>(walk: &mut W, mut m: ForeignMod) -> ForeignMod {
    m.items = m.items.into_iter().map(|fi| walk.traverse_foreign_item(fi)).collect();
    m
}

pub fn traverse_item_def<W: Traverse>(walk: &mut W, mut i: Item) -> Item {
    i.node = match i.node {
        ItemKind::Static(ty, mu, p_expr) => {
            let p_expr = p_expr.map(|expr| walk.traverse_expr(expr));
            ItemKind::Static(ty, mu, p_expr)
        },
        ItemKind::Const(ty, p_expr) => ItemKind::Const(
            ty,
            p_expr.map(|expr| walk.traverse_expr(expr)),
        ),
        ItemKind::Fn(fn_decl, uns, cons, header, gen, p_block) => {
            let p_block = p_block.map(|block| walk.traverse_block(block));
            ItemKind::Fn(fn_decl, uns, cons, header, gen, p_block)
        }
        ItemKind::Mod(m) => ItemKind::Mod(walk.traverse_mod(m)),
        ItemKind::ForeignMod(fm) => ItemKind::ForeignMod(walk.traverse_foreign_mod(fm)),
        ItemKind::Trait(a, u, gen, bds, trait_items) => {
            let trait_items = trait_items
                .into_iter()
                .map(|ti| walk.traverse_trait_item(ti))
                .collect();
            ItemKind::Trait(a, u, gen, bds, trait_items)
        }
        ItemKind::Impl(u, p, d, gen, tr, ty, impl_items) => {
            let impl_items = impl_items.into_iter().map(|ti| walk.traverse_impl_item(ti)).collect();
            ItemKind::Impl(u, p, d, gen, tr, ty, impl_items)
        }
        ItemKind::Use(u) => ItemKind::Use(u),
        ItemKind::ExternCrate(u) => ItemKind::ExternCrate(u),
        ItemKind::GlobalAsm(u) => ItemKind::GlobalAsm(u),
        ItemKind::Ty(l,r) => ItemKind::Ty(l,r),
        ItemKind::Enum(l,r) => ItemKind::Enum(l,r),
        ItemKind::Struct(l,r) => ItemKind::Struct(l,r),
        ItemKind::Union(l,r) => ItemKind::Union(l,r),
        ItemKind::TraitAlias(l,r) => ItemKind::TraitAlias(l,r),
        ItemKind::Mac(m) => ItemKind::Mac(m),
        ItemKind::MacroDef(m) => ItemKind::MacroDef(m),
    };
    i
}
