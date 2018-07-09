
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

pub fn parse_xcheck_arglist(args: &ArgList<'static>, or_default: bool) -> Option<xcfg::XCheckType> {
    if args.len() > 1 {
        panic!("expected single argument for cross-check type attribute");
    }
    args.iter().next()
        .map(|(name, ref arg)| parse_xcheck_type(name, arg))
        .or(if or_default { Some(xcfg::XCheckType::Default) } else { None })
}

pub fn parse_xcheck_arg(arg: &ArgValue<'static>, or_default: bool) -> Option<xcfg::XCheckType> {
    let res = match *arg {
        ArgValue::Nothing => None,
        ArgValue::List(ref l) => parse_xcheck_arglist(l, or_default),
        _ => panic!("unexpected argument to all_args():{:?}", *arg)
    };
    res.or(if or_default { Some(xcfg::XCheckType::Default) } else { None })
}

pub fn parse_attr_config(item_xcfg: &mut xcfg::ItemConfig, mi: &ast::MetaItem) {
    assert!(mi.name() == "cross_check");
    match item_xcfg {
        &mut xcfg::ItemConfig::Defaults(ref mut d) => parse_defaults_attr_config(d, mi),
        &mut xcfg::ItemConfig::Function(ref mut f) => parse_function_attr_config(f, mi),
        &mut xcfg::ItemConfig::Struct(ref mut s)   => parse_struct_attr_config(s, mi),
        _ => panic!("unexpected item: {:#?}", item_xcfg)
    }
}

fn parse_defaults_attr_config(d: &mut xcfg::DefaultsConfig, mi: &ast::MetaItem) {
    let args = xcfg::attr::get_syntax_item_args(mi);
    for (name, arg) in args.iter() {
        match *name {
            "disabled" | "none" => d.disable_xchecks = Some(true),
            "enabled"  | "yes"  => d.disable_xchecks = Some(false),
            "entry"    => d.entry    = parse_xcheck_arg(&arg, true),
            "exit"     => d.exit     = parse_xcheck_arg(&arg, true),
            "all_args" => d.all_args = parse_xcheck_arg(&arg, true),
            "ret"      => d.ret      = parse_xcheck_arg(&arg, true),
            _ => panic!("unexpected cross_check item: {}", name)
        }
    }
}

fn parse_function_attr_config(f: &mut xcfg::FunctionConfig, mi: &ast::MetaItem) {
    let args = xcfg::attr::get_syntax_item_args(mi);
    for (name, arg) in args.iter() {
        match *name {
            "disabled" | "none" => f.disable_xchecks = Some(true),
            "enabled"  | "yes"  => f.disable_xchecks = Some(false),
            "entry"    => f.entry    = parse_xcheck_arg(&arg, true),
            "exit"     => f.exit     = parse_xcheck_arg(&arg, true),
            "all_args" => f.all_args = parse_xcheck_arg(&arg, true),
            "ret"      => f.ret      = parse_xcheck_arg(&arg, true),
            "args"     => {
                // Parse per-argument cross-check types
                f.args.extend(arg.as_list().iter().filter_map(|(name, arg)| {
                    if let xcfg::attr::ArgValue::List(ref l) = *arg {
                        let arg_xcheck = parse_xcheck_arglist(l, false)
                            .expect(&format!("expected valid cross-check type \
                                              for argument: {}", name));
                        Some((String::from(*name), arg_xcheck))
                    } else { None }
                }));
            }
            "ahasher"  => f.ahasher  = Some(String::from(arg.as_str())),
            "shasher"  => f.shasher  = Some(String::from(arg.as_str())),
            // TODO: handle entry_extra and exit_extra for Function
            _ => panic!("unexpected cross_check item: {}", name)
        }
    }
}

fn parse_struct_attr_config(s: &mut xcfg::StructConfig, mi: &ast::MetaItem) {
    let args = xcfg::attr::get_syntax_item_args(mi);
    for (name, arg) in args.iter() {
        match *name {
            "disabled" | "none" => s.disable_xchecks = Some(true),
            "enabled"  | "yes"  => s.disable_xchecks = Some(false),
            "ahasher"  => s.ahasher  = Some(String::from(arg.as_str())),
            "shasher"  => s.shasher  = Some(String::from(arg.as_str())),
            "field_hasher" => s.field_hasher = Some(String::from(arg.as_str())),
            "custom_hash"  => s.custom_hash  = Some(String::from(arg.as_str())),
            _ => panic!("unexpected cross_check item: {}", name)
        }
    }
}
