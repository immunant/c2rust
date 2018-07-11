
use syntax::ast;

use syntax::ext::base::ExtCtxt;
use syntax::ext::quote::rt::{ExtParseUtils};
use syntax::ptr::P;

use xcfg;

fn djb2_hash(s: &str) -> u32 {
    s.bytes().fold(5381u32, |h, c| h.wrapping_mul(33).wrapping_add(c as u32))
}

pub trait CrossCheckBuilder {
    fn build_ident_xcheck(&self, cx: &ExtCtxt, tag_str: &str, ident: &ast::Ident) -> Option<ast::Stmt>;
    fn build_xcheck<F>(&self, cx: &ExtCtxt, tag_str: &str, val_ref_str: &str, f: F) -> Option<ast::Stmt>
        where F: FnOnce(ast::Ident, Vec<ast::Stmt>) -> P<ast::Expr>;
}

impl CrossCheckBuilder for xcfg::XCheckType {
    fn build_ident_xcheck(&self, cx: &ExtCtxt, tag_str: &str, ident: &ast::Ident) -> Option<ast::Stmt> {
        self.build_xcheck(cx, tag_str, &"$INVALID$", |tag, pre_hash_stmts| {
            assert!(pre_hash_stmts.is_empty());
            let id = djb2_hash(&*ident.name.as_str()) as u64;
            quote_expr!(cx, Some(($tag, $id)))
        })
    }

    // Allow clients to specify the id or name manually, like this:
    // #[cross_check(name = "foo")]
    // #[cross_check(id = 0x12345678)]
    fn build_xcheck<F>(&self, cx: &ExtCtxt, tag_str: &str,
                       val_ref_str: &str, f: F) -> Option<ast::Stmt>
            where F: FnOnce(ast::Ident, Vec<ast::Stmt>) -> P<ast::Expr> {
        let tag = ast::Ident::from_str(tag_str);
        let check = match *self {
            xcfg::XCheckType::Default => f(tag, vec![]),
            xcfg::XCheckType::AsType(ref ty_str) => {
                let val_ref_ident = ast::Ident::from_str(val_ref_str);
                let ty = cx.parse_tts(ty_str.clone());
                let val_cast   = quote_stmt!(cx, let __c2rust_cast_val = *$val_ref_ident as $ty);
                let val_update = quote_stmt!(cx, let $val_ref_ident = &__c2rust_cast_val);
                let stmts = val_cast.into_iter().chain(val_update.into_iter())
                    .collect::<Vec<_>>();
                f(tag, stmts)
            },

            xcfg::XCheckType::None |
            xcfg::XCheckType::Disabled => quote_expr!(cx, None),
            xcfg::XCheckType::Fixed(id) => quote_expr!(cx, Some(($tag, $id))),
            xcfg::XCheckType::Djb2(ref s) => {
                let id = djb2_hash(s) as u64;
                quote_expr!(cx, Some(($tag, $id)))
            },
            xcfg::XCheckType::Custom(ref s) => {
                // TODO: allow the custom expr to return an Option???
                let custom_expr = cx.parse_expr(s.clone());
                quote_expr!(cx, Some(($tag, $custom_expr)))
            },
        };
        quote_stmt!(cx, {
            use cross_check_runtime::xcheck::$tag;
            cross_check_iter!($check.into_iter())
        })
    }
}
