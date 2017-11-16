
use syntax::ast;

use syntax::ext::base::ExtCtxt;
use syntax::ext::quote::rt::{ExtParseUtils};
use syntax::ptr::P;

use std::convert::TryInto;

use xcfg;

fn djb2_hash(s: &str) -> u32 {
    s.bytes().fold(5381u32, |h, c| h.wrapping_mul(33).wrapping_add(c as u32))
}

pub trait CrossCheckHash {
    fn get_ident_hash(&self, cx: &ExtCtxt, ident: &ast::Ident) -> Option<P<ast::Expr>>;
    fn get_hash<F>(&self, cx: &ExtCtxt, f: F) -> Option<P<ast::Expr>>
        where F: FnOnce() -> Option<P<ast::Expr>>;
}

impl CrossCheckHash for xcfg::XCheckType {
    fn get_ident_hash(&self, cx: &ExtCtxt, ident: &ast::Ident) -> Option<P<ast::Expr>> {
        self.get_hash(cx, || {
            let id = djb2_hash(&*ident.name.as_str()) as u64;
            Some(quote_expr!(cx, $id))
        })
    }

    // Allow clients to specify the id or name manually, like this:
    // #[cross_check(name = "foo")]
    // #[cross_check(id = 0x12345678)]
    fn get_hash<F>(&self, cx: &ExtCtxt, f: F) -> Option<P<ast::Expr>>
            where F: FnOnce() -> Option<P<ast::Expr>> {
        match *self {
            xcfg::XCheckType::Default => f(),
            xcfg::XCheckType::None |
            xcfg::XCheckType::Disabled => None,
            xcfg::XCheckType::Fixed(id) => Some(quote_expr!(cx, $id)),
            xcfg::XCheckType::Djb2(ref s) => {
                let id = djb2_hash(s) as u64;
                Some(quote_expr!(cx, $id))
            },
            xcfg::XCheckType::Custom(ref s) => Some(cx.parse_expr(s.clone())),
        }
    }
}

fn parse_xcheck_type(name: &'static str,
                     arg: &xcfg::attr::ArgValue) -> xcfg::XCheckType {
    match name {
        "default"  => xcfg::XCheckType::Default,
        "none"     => xcfg::XCheckType::None,
        "disabled" => xcfg::XCheckType::Disabled,

        "djb2" => xcfg::XCheckType::Djb2(String::from(arg.as_str())),
        "fixed" => {
            match *arg {
                // TODO: handle LitKind::Str

                xcfg::attr::ArgValue::Int(id128) => {
                    if let Ok(id64) = id128.try_into() {
                        xcfg::XCheckType::Fixed(id64)
                    } else {
                        panic!("invalid u32 for cross_check id: {}", id128)
                    }
                },

                _ => panic!("invalid literal for cross_check id: {:?}", arg)
            }
        },
        "custom" => xcfg::XCheckType::Custom(String::from(arg.as_str())),
        _ => panic!("unknown cross-check type: {}", name)
     }
}

pub fn parse_xcheck_arglist(args: &xcfg::attr::ArgList<'static>) -> Option<xcfg::XCheckType> {
    if args.len() > 1 {
        panic!("expected single argument for cross-check type attribute");
    }
    args.iter().next()
        .map(|(name, ref arg)| parse_xcheck_type(name, arg))
}

pub fn parse_xcheck_arg(arg: &xcfg::attr::ArgValue<'static>) -> Option<xcfg::XCheckType> {
    match *arg {
        xcfg::attr::ArgValue::Nothing => None,
        xcfg::attr::ArgValue::List(ref l) => parse_xcheck_arglist(l),
        _ => panic!("unexpected argument to all_args():{:?}", *arg)
    }
}
