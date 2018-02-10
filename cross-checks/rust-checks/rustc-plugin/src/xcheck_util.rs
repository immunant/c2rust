
use syntax::ast;

use syntax::ext::base::ExtCtxt;
use syntax::ext::quote::rt::{ExtParseUtils};
use syntax::ptr::P;

use std::convert::TryInto;

use xcfg;
use xcfg::attr::{ArgValue, ArgList};

fn djb2_hash(s: &str) -> u32 {
    s.bytes().fold(5381u32, |h, c| h.wrapping_mul(33).wrapping_add(c as u32))
}

pub trait CrossCheckBuilder {
    fn build_ident_xcheck(&self, cx: &ExtCtxt, tag_str: &str, ident: &ast::Ident) -> Option<ast::Stmt>;
    fn build_xcheck<F>(&self, cx: &ExtCtxt, tag_str: &str, f: F) -> Option<ast::Stmt>
        where F: FnOnce(ast::Ident) -> P<ast::Expr>;
}

impl CrossCheckBuilder for xcfg::XCheckType {
    fn build_ident_xcheck(&self, cx: &ExtCtxt, tag_str: &str, ident: &ast::Ident) -> Option<ast::Stmt> {
        self.build_xcheck(cx, tag_str, |tag| {
            let id = djb2_hash(&*ident.name.as_str()) as u64;
            quote_expr!(cx, Some(($tag, $id)))
        })
    }

    // Allow clients to specify the id or name manually, like this:
    // #[cross_check(name = "foo")]
    // #[cross_check(id = 0x12345678)]
    fn build_xcheck<F>(&self, cx: &ExtCtxt, tag_str: &str, f: F) -> Option<ast::Stmt>
            where F: FnOnce(ast::Ident) -> P<ast::Expr> {
        let tag = ast::Ident::from_str(tag_str);
        let check = match *self {
            xcfg::XCheckType::Default => f(tag),
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

fn parse_xcheck_type(name: &'static str, arg: &ArgValue) -> xcfg::XCheckType {
    match name {
        "default"  => xcfg::XCheckType::Default,
        "none"     => xcfg::XCheckType::None,
        "disabled" => xcfg::XCheckType::Disabled,

        "djb2" => xcfg::XCheckType::Djb2(String::from(arg.as_str())),
        "fixed" => {
            match *arg {
                // TODO: handle LitKind::Str

                ArgValue::Int(id128) => {
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

pub fn parse_xcheck_arglist(args: &ArgList<'static>) -> Option<xcfg::XCheckType> {
    if args.len() > 1 {
        panic!("expected single argument for cross-check type attribute");
    }
    args.iter().next()
        .map(|(name, ref arg)| parse_xcheck_type(name, arg))
}

pub fn parse_xcheck_arg(arg: &ArgValue<'static>) -> Option<xcfg::XCheckType> {
    match *arg {
        ArgValue::Nothing => None,
        ArgValue::List(ref l) => parse_xcheck_arglist(l),
        _ => panic!("unexpected argument to all_args():{:?}", *arg)
    }
}
