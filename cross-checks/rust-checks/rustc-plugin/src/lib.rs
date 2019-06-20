#![feature(plugin_registrar, rustc_private)]

extern crate rustc_plugin;
extern crate syntax;

// Unused: #[macro_use]
extern crate matches;

extern crate serde;
extern crate serde_yaml;

#[macro_use]
extern crate smallvec;

extern crate c2rust_xcheck_config as xcfg;

use rustc_plugin::Registry;

use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::iter;
use std::path::PathBuf;

use smallvec::SmallVec;

use syntax::ast;
use syntax::ext::base::{Annotatable, ExtCtxt, MultiItemModifier, SyntaxExtension};
use syntax::ext::build::AstBuilder;
use syntax::mut_visit::{self, ExpectOne, MutVisitor};
use syntax::parse::{token, parse_stream_from_source_str, new_parser_from_source_str, ParseSess};
use syntax::print::pprust;
use syntax::ptr::P;
use syntax::source_map::{FileLoader, FileName, RealFileLoader, Span, Spanned, DUMMY_SP};
use syntax::symbol::{Symbol, keywords};
use syntax::tokenstream::{TokenTree, TokenStream, TokenStreamBuilder};

mod default_config;

trait AstExtBuilder {
    fn expr_u64(&self, sp: Span, u: u64) -> P<ast::Expr>;

    fn expr_mac(&self, sp: Span, path: ast::Path, delim: ast::MacDelimiter,
                tts: TokenStream) -> P<ast::Expr>;
    fn item_mac(&self, sp: Span, path: ast::Path, delim: ast::MacDelimiter,
                tts: TokenStream) -> P<ast::Item>;
    fn stmt_mac(&self, sp: Span, path: ast::Path, delim: ast::MacDelimiter,
                tts: TokenStream, style: ast::MacStmtStyle,
                attrs: Vec<ast::Attribute>) -> ast::Stmt;

    fn expr_mac_fn(&self, sp: Span, path: ast::Path, args: Vec<token::Nonterminal>) -> P<ast::Expr>;
    fn item_mac_fn(&self, sp: Span, path: ast::Path, args: Vec<token::Nonterminal>) -> P<ast::Item>;
    fn stmt_mac_fn(&self, sp: Span, path: ast::Path, args: Vec<token::Nonterminal>) -> ast::Stmt;

    fn args_to_tts(&self, sp: Span, args: Vec<token::Nonterminal>) -> TokenStream;
}

impl<'a> AstExtBuilder for ExtCtxt<'a> {
    fn expr_u64(&self, sp: Span, u: u64) -> P<ast::Expr> {
        self.expr_lit(sp, ast::LitKind::Int(u as u128,
                                            ast::LitIntType::Unsigned(ast::UintTy::U64)))
    }

    fn expr_mac(&self, span: Span, path: ast::Path, delim: ast::MacDelimiter,
                tts: TokenStream) -> P<ast::Expr> {
        let node = ast::Mac_ { path, delim, tts };
        self.expr(span, ast::ExprKind::Mac(Spanned { node, span }))
    }

    fn item_mac(&self, span: Span, path: ast::Path, delim: ast::MacDelimiter,
                tts: TokenStream) -> P<ast::Item> {
        let name = keywords::Invalid.ident();
        let node = ast::Mac_ { path, delim, tts };
        let attrs = vec![];
        self.item(span, name, attrs, ast::ItemKind::Mac(Spanned { node, span }))
    }

    fn stmt_mac(&self, span: Span, path: ast::Path, delim: ast::MacDelimiter,
                tts: TokenStream, style: ast::MacStmtStyle,
                attrs: Vec<ast::Attribute>) -> ast::Stmt {
        let node = ast::Mac_ { path, delim, tts };
        let node = ast::StmtKind::Mac(P((Spanned { node, span }, style, attrs.into())));
        ast::Stmt {
            id: ast::DUMMY_NODE_ID,
            span,
            node
        }
    }

    fn expr_mac_fn(&self, sp: Span, path: ast::Path, args: Vec<token::Nonterminal>) -> P<ast::Expr> {
        let arg_tts = self.args_to_tts(sp, args);
        self.expr_mac(sp, path, ast::MacDelimiter::Parenthesis, arg_tts)
    }

    fn item_mac_fn(&self, sp: Span, path: ast::Path, args: Vec<token::Nonterminal>) -> P<ast::Item> {
        let arg_tts = self.args_to_tts(sp, args);
        self.item_mac(sp, path, ast::MacDelimiter::Parenthesis, arg_tts)
    }

    fn stmt_mac_fn(&self, sp: Span, path: ast::Path, args: Vec<token::Nonterminal>) -> ast::Stmt {
        let arg_tts = self.args_to_tts(sp, args);
        self.stmt_mac(sp, path, ast::MacDelimiter::Parenthesis,
                      arg_tts, ast::MacStmtStyle::Semicolon, vec![])
    }

    fn args_to_tts(&self, sp: Span, args: Vec<token::Nonterminal>) -> TokenStream {
        let mut tsb = TokenStreamBuilder::new();
        let mut add_comma = false;
        for arg in args {
            if add_comma {
                tsb.push(TokenTree::Token(DUMMY_SP, token::Token::Comma));
            } else {
                add_comma = true;
            }
            let arg_str = pprust::nonterminal_to_string(&arg);
            let arg_file_name = FileName::Custom(String::from("c2rust-xcheck-macro"));
            let arg_tokens = parse_stream_from_source_str(arg_file_name, arg_str,
                                                          self.parse_sess, Some(sp));
            tsb.push(arg_tokens);
        }
        tsb.build()
    }
}

fn djb2_hash(s: &str) -> u32 {
    s.bytes()
        .fold(5381u32, |h, c| h.wrapping_mul(33).wrapping_add(c.into()))
}

trait CrossCheckBuilder {
    fn build_ident_xcheck(
        &self,
        cx: &ExtCtxt,
        exp: &CrossCheckExpander,
        tag_str: &str,
        ident: ast::Ident,
    ) -> ast::Stmt;
    fn build_xcheck<F>(
        &self,
        cx: &ExtCtxt,
        exp: &CrossCheckExpander,
        tag_str: &str,
        val_ref_ident: ast::Ident,
        f: F,
    ) -> ast::Stmt
    where
        F: FnOnce(P<ast::Expr>, Vec<ast::Stmt>) -> P<ast::Expr>;
}

fn parse_ty(file_name: &str, ty_str: &str, sess: &ParseSess) -> P<ast::Ty> {
    let file_name = FileName::Custom(file_name.to_owned());
    let mut p = new_parser_from_source_str(sess, file_name, ty_str.to_owned());
    p.parse_ty()
        .expect(&format!("failed to parse type: {}", ty_str))
}

impl CrossCheckBuilder for xcfg::XCheckType {
    fn build_ident_xcheck(
        &self,
        cx: &ExtCtxt,
        exp: &CrossCheckExpander,
        tag_str: &str,
        ident: ast::Ident,
    ) -> ast::Stmt {
        let invalid_ident = ast::Ident::from_str("__c2rust_invalid").gensym();
        self.build_xcheck(cx, exp, tag_str, invalid_ident, |tag, pre_hash_stmts| {
            assert!(pre_hash_stmts.is_empty());
            let name = &*ident.name.as_str();
            let id = djb2_hash(name);
            exp.insert_djb2_name(id, String::from(name));

            let id_expr = cx.expr_u32(DUMMY_SP, id);
            cx.expr_some(DUMMY_SP, cx.expr_tuple(DUMMY_SP, vec![tag, id_expr]))
        })
    }

    // Allow clients to specify the id or name manually, like this:
    // #[cross_check(name = "foo")]
    // #[cross_check(id = 0x12345678)]
    fn build_xcheck<F>(
        &self,
        cx: &ExtCtxt,
        exp: &CrossCheckExpander,
        tag_str: &str,
        val_ref_ident: ast::Ident,
        f: F,
    ) -> ast::Stmt
    where
        F: FnOnce(P<ast::Expr>, Vec<ast::Stmt>) -> P<ast::Expr>,
    {
        let tag_path =
            cx.path(DUMMY_SP, vec![
                    cx.ident_of("c2rust_xcheck_runtime"),
                    cx.ident_of("xcheck"),
                    cx.ident_of(tag_str)
            ]);
        let tag_expr = cx.expr_path(tag_path);
        let xcheck = match *self {
            xcfg::XCheckType::Default => f(tag_expr, vec![]),
            xcfg::XCheckType::AsType(ref ty_str) => {
                let var_ident = cx.ident_of("__c2rust_cast_val").gensym();
                // let __c2rust_cast_val = *$val_ref_ident as $ty;
                let val_cast_let = {
                    let ref_expr = cx.expr_ident(DUMMY_SP, val_ref_ident.clone());
                    let ref_deref = cx.expr_deref(DUMMY_SP, ref_expr);
                    let ty = parse_ty("c2rust-xcheck-astype", &ty_str, cx.parse_sess);
                    let val_cast = cx.expr_cast(DUMMY_SP, ref_deref, ty);
                    cx.stmt_let(DUMMY_SP, false, var_ident.clone(), val_cast)
                };
                // let $val_ref_ident = &__c2rust_cast_val;
                let val_update = {
                    let var_ref = cx.expr_addr_of(DUMMY_SP, cx.expr_ident(DUMMY_SP, var_ident));
                    cx.stmt_let(DUMMY_SP, false, val_ref_ident, var_ref)
                };

                f(tag_expr, vec![val_cast_let, val_update])
            }

            xcfg::XCheckType::None | xcfg::XCheckType::Disabled => cx.expr_none(DUMMY_SP),
            xcfg::XCheckType::Fixed(id) => {
                let id = cx.expr_u64(DUMMY_SP, id);
                cx.expr_some(DUMMY_SP, cx.expr_tuple(DUMMY_SP, vec![tag_expr, id]))
            }
            xcfg::XCheckType::Djb2(ref s) => {
                let id = djb2_hash(s);
                exp.insert_djb2_name(id, s.clone());

                let id_expr = cx.expr_u32(DUMMY_SP, id);
                cx.expr_some(DUMMY_SP, cx.expr_tuple(DUMMY_SP, vec![tag_expr, id_expr]))
            }
            xcfg::XCheckType::Custom(ref s) => {
                let file_name = FileName::Custom(String::from("c2rust-xcheck-custom"));
                let mut p = new_parser_from_source_str(cx.parse_sess, file_name,
                                                       s.clone());
                let custom_expr = p.parse_expr()
                    .expect(&format!("failed to parse expr: {}", s));
                cx.expr_some(DUMMY_SP, cx.expr_tuple(DUMMY_SP, vec![tag_expr, custom_expr]))
            }
        };
        let xcheck_path =
            cx.path_ident(DUMMY_SP, cx.ident_of("cross_check_iter"));
        let xcheck_arg =
            cx.expr_method_call(DUMMY_SP, xcheck, cx.ident_of("into_iter"), vec![]);
        cx.stmt_mac_fn(DUMMY_SP, xcheck_path, vec![token::NtExpr(xcheck_arg)])
    }
}

trait AstItemScope {
    fn push_ast_item(
        &mut self,
        item: &ast::Item,
        mi: Option<&ast::MetaItem>,
        external_config: &xcfg::Config,
        cx: &ExtCtxt,
    ) -> usize;
}

impl AstItemScope for xcfg::scopes::ScopeStack {
    /// Push a Rust AST item to a xcfg::scopes::ScopeStack
    fn push_ast_item(
        &mut self,
        item: &ast::Item,
        mi: Option<&ast::MetaItem>,
        external_config: &xcfg::Config,
        cx: &ExtCtxt,
    ) -> usize {
        let (span, item_kind, item_xcfg) = match item.node {
            ast::ItemKind::Fn(..) => (
                item.span,
                Some(xcfg::scopes::ItemKind::Function),
                Some(xcfg::ItemConfig::Function(Default::default())),
            ),
            ast::ItemKind::Enum(..) | ast::ItemKind::Struct(..) | ast::ItemKind::Union(..) => (
                item.span,
                Some(xcfg::scopes::ItemKind::Struct),
                Some(xcfg::ItemConfig::Struct(Default::default())),
            ),
            ast::ItemKind::Impl(..) => (
                item.span,
                Some(xcfg::scopes::ItemKind::Impl),
                Some(xcfg::ItemConfig::Struct(Default::default())),
            ),
            ast::ItemKind::Mod(ref m) => (m.inner, None, None),
            _ => (item.span, None, None),
        };
        let file_name = cx.source_map().span_to_filename(span);
        let file_name = file_name.to_string();

        // Check if there are any file-level defaults, and if so, apply them
        let mut pushed_count = 0usize;
        pushed_count += {
            let file_defaults_config = self.push_file(external_config, &file_name);
            if file_defaults_config.is_some() {
                1
            } else {
                0
            }
        };
        if let Some(ik) = item_kind {
            // If the item is an impl for a type, e.g.:
            // `impl T { ... }`, then we take its name
            // from the type, not from the identifier
            let item_name_str = item.ident.name.as_str();
            let item_name = match item.node {
                ast::ItemKind::Impl(.., ref ty, _) => {
                    // FIXME: handle generics in the type
                    Cow::from(pprust::ty_to_string(ty))
                }
                _ => Cow::from(&*item_name_str),
            };

            // We have either a #[cross_check] attribute or external config
            // TODO: order???
            let mut item_xcfg = item_xcfg.unwrap();
            if let Some(ref mi) = mi {
                xcfg::attr::syntax::parse_attr_config(&mut item_xcfg, mi);
            }
            let xcheck_attr = find_cross_check_attr(&item.attrs);
            if let Some(ref attr) = xcheck_attr {
                let mi = attr.parse_meta(cx.parse_sess).unwrap();
                xcfg::attr::syntax::parse_attr_config(&mut item_xcfg, &mi);
            }
            self.push_item(ik, &file_name, &*item_name, &[item_xcfg], &[]);
            pushed_count += 1;
        }
        pushed_count
    }
}

enum AttrValue {
    Nothing,
    Str(String),
}

type AttrMap = HashMap<&'static str, AttrValue>;

struct CrossChecker<'a, 'cx: 'a, 'exp> {
    expander: &'exp CrossCheckExpander,
    cx: &'a mut ExtCtxt<'cx>,
    scope_stack: xcfg::scopes::ScopeStack,
    default_ahasher: P<ast::Ty>,
    default_shasher: P<ast::Ty>,

    // New items to add at the next item boundary
    pending_items: Vec<P<ast::Item>>,

    // Index of the next field in this scope (if the scope is a structure)
    // We use this to keep track of the index/ident of the next field
    // in a tuple
    field_idx_stack: Vec<usize>,

    // Whether to skip calling build_new_scope() on the first scope.
    // We set this to true for #[cross_check(...)] invocations caused
    // by macro expansions, since the compiler passes the attribute to us
    // in mi: &MetaItem and not in the item's actual attribute list,
    // so we need to skip parsing the latter.
    skip_first_scope: bool,
}

fn find_cross_check_attr(attrs: &[ast::Attribute]) -> Option<&ast::Attribute> {
    attrs.iter().find(|attr| attr.check_name("cross_check"))
}

impl<'a, 'cx, 'exp> CrossChecker<'a, 'cx, 'exp> {
    fn new(
        expander: &'exp CrossCheckExpander,
        cx: &'a mut ExtCtxt<'cx>,
        scope_stack: xcfg::scopes::ScopeStack,
        skip_first_scope: bool,
    ) -> CrossChecker<'a, 'cx, 'exp> {

        let default_ahasher =
            parse_ty("c2rust-xcheck-defaults",
                     "::c2rust_xcheck_runtime::hash::jodyhash::JodyHasher",
                     cx.parse_sess);
        let default_shasher =
            parse_ty("c2rust-xcheck-defaults",
                     "::c2rust_xcheck_runtime::hash::simple::SimpleHasher",
                     cx.parse_sess);
        CrossChecker {
            expander,
            cx,
            scope_stack,
            default_ahasher,
            default_shasher,
            pending_items: vec![],
            field_idx_stack: vec![],
            skip_first_scope,
        }
    }

    #[inline]
    fn config(&self) -> &xcfg::scopes::ScopeConfig {
        self.scope_stack.last()
    }

    // Get the ahasher/shasher pair
    fn get_hasher_pair(&self) -> (P<ast::Ty>, P<ast::Ty>) {
        let ahasher = if let Some(ref ahasher_str) = self.config().inherited.ahasher.as_ref() {
            parse_ty("c2rust-xcheck-hasher", ahasher_str, self.cx.parse_sess)
        } else {
            self.default_ahasher.clone()
        };
        let shasher = if let Some(ref shasher_str) = self.config().inherited.shasher.as_ref() {
            parse_ty("c2rust-xcheck-hasher", shasher_str, self.cx.parse_sess)
        } else {
            self.default_shasher.clone()
        };
        (ahasher, shasher)
    }

    // Get the cross-check block for this argument
    fn build_arg_xcheck(&self, arg: &ast::Arg) -> ast::Stmt {
        match arg.pat.node {
            ast::PatKind::Ident(_, ref ident, _) => {
                // Parameter pattern is just an identifier,
                // so we can reference it directly by name
                let arg_idx = xcfg::FieldIndex::Str(ident.name.to_string());
                let arg_xcheck_cfg = self
                    .config()
                    .function_config()
                    .args
                    .get(&arg_idx)
                    .unwrap_or(&self.config().inherited.all_args);
                // FIXME: no gensym()???
                let val_ref_ident = self.cx.ident_of("__c2rust_val_ref").gensym();
                arg_xcheck_cfg.build_xcheck(
                    self.cx,
                    self.expander,
                    "FUNCTION_ARG_TAG",
                    val_ref_ident,
                    |tag, pre_hash_stmts| {
                        // By default, we use cross_check_hash
                        // to hash the value of the identifier
                        let (ahasher, shasher) = self.get_hasher_pair();
                        let mac_path =
                            self.cx.path_ident(DUMMY_SP, self.cx.ident_of("__c2rust_emit_xcheck"));
                        let mut mac_args = vec![
                            token::NtExpr(tag),
                            token::NtIdent(ident.clone(), false),
                            token::NtIdent(val_ref_ident, false),
                            token::NtTy(ahasher),
                            token::NtTy(shasher)
                        ];
                        mac_args.extend(pre_hash_stmts.into_iter().map(|stmt| token::NtStmt(stmt)));
                        self.cx.expr_mac_fn(DUMMY_SP, mac_path, mac_args)
                    },
                )
            }
            _ => unimplemented!("unknown argument: {:#?}", arg),
        }
    }

    // Create the arguments for #[cross_check_hash]
    fn build_hash_attr_args(&self) -> AttrMap {
        let mut res = AttrMap::new();
        if let Some(ref ahasher) = self.config().inherited.ahasher.as_ref() {
            res.insert("ahasher", AttrValue::Str(ahasher.to_string()));
        }
        if let Some(ref shasher) = self.config().inherited.shasher.as_ref() {
            res.insert("shasher", AttrValue::Str(shasher.to_string()));
        }
        let struct_config = self.config().struct_config();
        if let Some(ref field_hasher) = struct_config.field_hasher.as_ref() {
            res.insert("field_hasher", AttrValue::Str(field_hasher.to_string()));
        }
        if let Some(ref custom_hash) = struct_config.custom_hash.as_ref() {
            res.insert("custom_hash", AttrValue::Str(custom_hash.to_string()));
        }
        match struct_config.custom_hash_format.as_ref() {
            Some(xcfg::CustomHashFormat::Function) => {
                res.insert("custom_hash_format", AttrValue::Str("function".to_string()));
            }
            Some(xcfg::CustomHashFormat::Expression) => {
                res.insert("custom_hash_format", AttrValue::Str("expression".to_string()));
            }
            Some(xcfg::CustomHashFormat::Extern) => {
                res.insert("custom_hash_format", AttrValue::Str("extern".to_string()));
            }
            None => {}
        }
        res
    }

    fn convert_hash_attr_map(&self, sp: Span, map: AttrMap) -> ast::MetaItem {
        let args = map.into_iter()
            .map(|(arg, val)| {
                let arg_name = self.cx.name_of(arg);
                match val {
                    AttrValue::Nothing => self.cx.meta_list_item_word(sp, arg_name),
                    AttrValue::Str(s) => {
                        let arg_lit = ast::LitKind::Str(Symbol::intern(&s), ast::StrStyle::Cooked);
                        let arg_mi = self.cx.meta_name_value(sp, arg_name, arg_lit);
                        ast::NestedMetaItem::MetaItem(arg_mi)
                    }
                }
            })
            .collect();
        self.cx.meta_list(sp, self.cx.name_of("cross_check_hash"), args)
    }

    fn build_extra_xchecks(&self, extra_xchecks: &[xcfg::ExtraXCheck]) -> Vec<ast::Stmt> {
        extra_xchecks
            .iter()
            .map(|ex| {
                // TODO: allow the custom functions to return Option or an iterator???
                let file_name = FileName::Custom(String::from("c2rust-xcheck-custom"));
                let mut p = new_parser_from_source_str(self.cx.parse_sess, file_name,
                                                       ex.custom.clone());
                let expr = p.parse_expr()
                    .expect(&format!("failed to parse expr: {}", ex.custom));
                let tag_str = match ex.tag {
                    xcfg::XCheckTag::Unknown => "UNKNOWN_TAG",
                    xcfg::XCheckTag::FunctionEntry => "FUNCTION_ENTRY_TAG",
                    xcfg::XCheckTag::FunctionExit => "FUNCTION_EXIT_TAG",
                    xcfg::XCheckTag::FunctionArg => "FUNCTION_ARG_TAG",
                    xcfg::XCheckTag::FunctionReturn => "FUNCTION_RETURN_TAG",
                };
                let tag_ident = self.cx.ident_of(tag_str);
                let mac_path =
                    self.cx.path_ident(DUMMY_SP, self.cx.ident_of("cross_check_raw"));
                self.cx.stmt_mac_fn(DUMMY_SP, mac_path,
                                    vec![token::NtIdent(tag_ident, false),
                                         token::NtExpr(expr)])
            })
            .collect::<Vec<ast::Stmt>>()
    }

    fn build_function_xchecks(
        &mut self,
        fn_ident: ast::Ident,
        fn_decl: &ast::FnDecl,
        block: P<ast::Block>,
    ) -> P<ast::Block> {
        let checked_block = if self.config().inherited.enabled {
            // Emit the following block: {
            //     $entry_xcheck
            //     $arg_xchecks
            //     $entry_extra_xchecks
            //     let $result_ident = (|| -> $result_ty { $block })();
            //     $exit_xcheck
            //     $result_xcheck
            //     $exit_extra_xchecks
            //     $result_ident
            // }
            //
            // Add the cross-check to the beginning of the function
            // TODO: only add the checks to C abi functions???
            let mut new_stmts = vec![];
            let cfg = &self.config();
            let entry_xcheck = cfg.inherited.entry.build_ident_xcheck(
                self.cx,
                self.expander,
                "FUNCTION_ENTRY_TAG",
                fn_ident,
            );
            new_stmts.push(entry_xcheck);

            // Insert cross-checks for function arguments
            new_stmts.extend(fn_decl.inputs.iter()
                .map(|ref arg| self.build_arg_xcheck(arg)));

            // Insert extra entry xchecks
            let fcfg = &cfg.function_config();
            let entry_extra_xchecks = self.build_extra_xchecks(&fcfg.entry_extra);
            new_stmts.extend(entry_extra_xchecks);

            // Copy and adjust the old body
            // `let $result_ident = (|| -> $result_ty { $block })();`
            let body_span = block.span;
            let body_fn_decl = self.cx.fn_decl(vec![], fn_decl.output.clone());
            let body_lambda =
                self.cx.lambda_fn_decl(body_span, body_fn_decl,
                                       self.cx.expr_block(block),
                                       body_span);
            let body_lambda_call =
                self.cx.expr_call(body_span, body_lambda, vec![]);
            // FIXME: should this be gensym()???
            // Without it, xchecks can access it, which may be desirable
            let result_ident = self.cx.ident_of("__c2rust_fn_result");
            let result_let =
                self.cx.stmt_let(body_span, false, result_ident, body_lambda_call);
            new_stmts.push(result_let);

            let exit_xcheck = cfg.inherited.exit.build_ident_xcheck(
                self.cx,
                self.expander,
                "FUNCTION_EXIT_TAG",
                fn_ident,
            );
            new_stmts.push(exit_xcheck);

            let val_ref_ident = self.cx.ident_of("__c2rust_val_ref").gensym();
            let result_xcheck = cfg.inherited.ret.build_xcheck(
                self.cx,
                self.expander,
                "FUNCTION_RETURN_TAG",
                val_ref_ident,
                |tag, pre_hash_stmts| {
                    // By default, we use cross_check_hash
                    // to hash the value of the identifier
                    let (ahasher, shasher) = self.get_hasher_pair();
                    let mac_path =
                        self.cx.path_ident(DUMMY_SP, self.cx.ident_of("__c2rust_emit_xcheck"));
                    let mut mac_args = vec![
                        token::NtExpr(tag),
                        token::NtIdent(result_ident, false),
                        token::NtIdent(val_ref_ident, false),
                        token::NtTy(ahasher),
                        token::NtTy(shasher)
                    ];
                    mac_args.extend(pre_hash_stmts.into_iter().map(|stmt| token::NtStmt(stmt)));
                    self.cx.expr_mac_fn(DUMMY_SP, mac_path, mac_args)
                },
            );
            new_stmts.push(result_xcheck);

            // Insert the final exit cross-checks
            let exit_extra_xchecks = self.build_extra_xchecks(&fcfg.exit_extra);
            new_stmts.extend(exit_extra_xchecks);

            // Return the result
            let result_expr = self.cx.expr_ident(DUMMY_SP, result_ident);
            let result_stmt = self.cx.stmt_expr(result_expr);
            new_stmts.push(result_stmt);

            // We're done, return the new block
            self.cx.block(body_span, new_stmts)
        } else {
            block
        };
        // TODO: Add our typedefs to the beginning of each function;
        // whatever the configuration says, we should always add these
        checked_block
    }

    fn build_union_hash(&mut self, union_ident: ast::Ident) -> P<ast::Item> {
        let custom_hash_opt = &self.config().struct_config().custom_hash;
        let custom_hash_format = &self.config().struct_config().custom_hash_format;
        let mac_path =
            self.cx.path_ident(DUMMY_SP, self.cx.ident_of("__c2rust_impl_union_hash"));
        let mut mac_args = vec![token::NtIdent(union_ident, false)];
        if let Some(ref custom_hash) = *custom_hash_opt {
            // User provided a custom hash function, pass it to the macro
            match custom_hash_format {
                None | Some(xcfg::CustomHashFormat::Function) => {
                    let (ahasher, shasher) = self.get_hasher_pair();
                    let hash_fn_ident = self.cx.ident_of(custom_hash);
                    mac_args.push(token::NtIdent(self.cx.ident_of("Function"), false));
                    mac_args.push(token::NtIdent(hash_fn_ident, false));
                    mac_args.push(token::NtTy(ahasher));
                    mac_args.push(token::NtTy(shasher));
                }
                Some(xcfg::CustomHashFormat::Expression) => {
                    let file_name = FileName::Custom(String::from("c2rust-xcheck-custom"));
                    let mut p = new_parser_from_source_str(self.cx.parse_sess, file_name,
                                                           custom_hash.clone());
                    let expr = p.parse_expr()
                        .expect(&format!("failed to parse expr: {}", custom_hash));
                    mac_args.push(token::NtIdent(self.cx.ident_of("Expression"), false));
                    mac_args.push(token::NtExpr(expr));
                }
                Some(xcfg::CustomHashFormat::Extern) => {
                    let hash_fn_ident = self.cx.ident_of(custom_hash);
                    mac_args.push(token::NtIdent(self.cx.ident_of("Extern"), false));
                    mac_args.push(token::NtIdent(hash_fn_ident, false));
                }
            }
        } else {
            // TODO: emit warning
            mac_args.push(token::NtIdent(self.cx.ident_of("Default"), false));
        };
        self.cx.item_mac_fn(DUMMY_SP, mac_path, mac_args)
    }

    #[cfg(feature = "c-hash-functions")]
    fn build_type_c_hash_function(
        &mut self,
        ty_ident: ast::Ident,
        ty_suffix: &str,
    ) -> Option<P<ast::Item>> {
        let hash_fn_name = format!("__c2rust_hash_{}_{}", ty_ident, ty_suffix);
        let hash_fn_ident = self.cx.ident_of(&hash_fn_name);
        let hash_fn_section = format!(".gnu.linkonce.t.{}", hash_fn_name);
        let hash_fn_sec_meta =
            self.cx.meta_name_value(DUMMY_SP, self.cx.name_of("link_section"),
                                    ast::LitKind::Str(hash_fn_section, ast::StrStyle::Cooked));

        // Check if function has already been emitted;
        // FIXME: should this check be optional (compile-time feature)???
        if !self
            .expander
            .c_hash_functions
            .borrow_mut()
            .insert(hash_fn_name)
        {
            return None;
        }

        let (ahasher, shasher) = self.get_hasher_pair();
        let mac_path =
            self.cx.path_ident(DUMMY_SP, self.cx.ident_of("__c2rust_export_extern_hash"));
        let mac_args = vec![
            token::NtIdent(ty_ident, false),
            token::NtIdent(hash_fn_ident, false),
            token::NtMeta(hash_fn_sec_meta),
            token::NtTy(ahasher),
            token::NtTy(shasher)
        ];
        Some(self.cx.item_mac_fn(DUMMY_SP, mac_path, mac_args))
    }

    fn internal_visit_item(&mut self, item: &mut P<ast::Item>) {
        let span = item.span;
        let ident = item.ident;
        let mut new_attrs = vec![];
        match item.node {
            ast::ItemKind::Fn(ref fn_decl, _, _, ref mut block) => {
                *block = self.build_function_xchecks(ident, fn_decl, block.clone());
            }
            ast::ItemKind::Union(_, _) => {
                let union_hash_impl = self.build_union_hash(ident);
                self.pending_items.push(union_hash_impl);
                #[cfg(feature = "c-hash-functions")]
                {
                    let c_hash_func = self.build_type_c_hash_function(ident, "struct");
                    self.pending_items.extend(c_hash_func.into_iter());
                }
            }
            ast::ItemKind::Enum(_, _) | ast::ItemKind::Struct(_, _) => {
                // Prepend #[derive(CrossCheckHash)] automatically
                // to every structure definition
                if self.config().inherited.enabled {
                    let xcheck_hash_derive_attr = {
                        let xch_sym = self.cx.name_of("CrossCheckHash");
                        let xch = self.cx.meta_list_item_word(span, xch_sym);
                        let attr = self.cx.meta_list(span, Symbol::intern("derive"), vec![xch]);
                        self.cx.attribute(span, attr)
                    };
                    new_attrs.push(xcheck_hash_derive_attr);

                    let attr_args = self.build_hash_attr_args();
                    if !attr_args.is_empty() {
                        let attr_args = self.convert_hash_attr_map(span, attr_args);
                        let xcheck_hash_attr = self.cx.attribute(span, attr_args);
                        new_attrs.push(xcheck_hash_attr);
                    }
                    #[cfg(feature = "c-hash-functions")]
                    {
                        let c_hash_func =
                            self.build_type_c_hash_function(ident, "struct");
                        self.pending_items.extend(c_hash_func.into_iter());
                    }
                }
            }
            ast::ItemKind::Mac(_) => {
                if !cfg!(feature = "expand_macros") {
                    self.expander
                        .insert_macro_scope(span, self.config().clone());
                }
            }
            _ => {}
        }
        item.attrs.extend(new_attrs.into_iter());
    }

    // Parse the #[cross_check(...)] attribute and turn it into a XCheck
    fn parse_field_attr(&self, attrs: &[ast::Attribute]) -> Option<xcfg::XCheckType> {
        let xcheck_attr = find_cross_check_attr(attrs);
        xcheck_attr.and_then(|attr| {
            attr.parse_meta(self.cx.parse_sess).ok().and_then(|mi| {
                let args = xcfg::attr::syntax::get_item_args(&mi);
                xcfg::attr::syntax::parse_xcheck_arglist(&args, false)
            })
        })
    }
}

impl<'a, 'cx, 'exp> MutVisitor for CrossChecker<'a, 'cx, 'exp> {
    // Mutate functions that handle macro expansion
    fn flat_map_item(&mut self, item: P<ast::Item>) -> SmallVec<[P<ast::Item>; 1]> {
        if cfg!(feature = "expand-macros") {
            //if let ast::ItemKind::Mac(_) = item.node {
            //    return self
            //        .cx
            //        .expander()
            //        .fold_item(item)
            //        .into_iter()
            //        .flat_map(|item| self.fold_item(item).into_iter())
            //        .collect();
            //}
            unimplemented!("expand-macros not implemented");
        }
        let mut res = if self.skip_first_scope {
            // If skip_first_scope is true, skip building a new scope
            // (see the comment for skip_first_scope in CrossChecker above)
            self.skip_first_scope = false;
            let mut folded_items = mut_visit::noop_flat_map_item(item, self);
            for item in folded_items.iter_mut() {
                self.internal_visit_item(item);
            }
            folded_items
        } else {
            let new_scopes = self.scope_stack.push_ast_item(
                &item,
                None,
                &self.expander.external_config,
                self.cx,
            );
            let mut folded_items = mut_visit::noop_flat_map_item(item, self);
            for item in folded_items.iter_mut() {
                self.internal_visit_item(item);
            }
            self.scope_stack.pop_multi(new_scopes);
            folded_items
        };
        // Add the pending items
        res.extend(self.pending_items.drain(..));
        res
    }

    fn flat_map_impl_item(&mut self, item: ast::ImplItem) -> SmallVec<[ast::ImplItem; 1]> {
        match (item.node, item.defaultness) {
            (ast::ImplItemKind::Method(sig, body), defaultness) => {
                // FIXME: this is a bit hacky: we forcibly build a fake
                // Item::Fn with the same signature and body as our method,
                // then add cross-checks to that one
                let fake_item = ast::Item {
                    ident: item.ident,
                    attrs: item.attrs,
                    id: item.id,
                    vis: item.vis,
                    span: item.span,
                    tokens: item.tokens,
                    node: ast::ItemKind::Fn(sig.decl, sig.header, item.generics, body),
                };
                let folded_flat_items = self.flat_map_item(P(fake_item));
                folded_flat_items.into_iter().map(move |ffi| {
                    let ast::Item { ident, attrs, id, node, vis, span, tokens } = ffi.into_inner();
                    let (sig, generics, body) = match node {
                        ast::ItemKind::Fn(decl, header, generics, body) => {
                            let sig = ast::MethodSig { header, decl };
                            (sig, generics, body)
                        }
                        n => panic!("unexpected folded item node: {:?}", n),
                    };
                    let node = ast::ImplItemKind::Method(sig, body);
                    ast::ImplItem { ident, attrs, id, vis, span, tokens, defaultness, generics, node }
                })
                .collect()
            },
            (node, defaultness) => {
                let item = ast::ImplItem { node, defaultness, ..item };
                mut_visit::noop_flat_map_impl_item(item, self)
            }
        }
    }

    fn flat_map_stmt(&mut self, s: ast::Stmt) -> SmallVec<[ast::Stmt; 1]> {
        if cfg!(feature = "expand-macros") {
            //if let ast::StmtKind::Mac(_) = s.node {
            //    return self
            //        .cx
            //        .expander()
            //        .fold_stmt(s)
            //        .into_iter()
            //        .flat_map(|stmt| self.fold_stmt(stmt).into_iter())
            //        .collect();
            //}
            unimplemented!("expand-macros not implemented");
        } else {
            self.expander
                .insert_macro_scope(s.span, self.config().clone());
        }

        let folded_stmt = mut_visit::noop_flat_map_stmt(s, self);
        folded_stmt
            .into_iter()
            .flat_map(|s| {
                let new_stmt = match s.node {
                    ast::StmtKind::Local(ref local) => {
                        let attr = find_cross_check_attr(&*local.attrs);
                        // TODO: check that the cross_check attr is "yes"
                        attr.and_then(|_| {
                            // TODO: only add cross-checks for initialized locals???
                            // (in other words, check local.init.is_some())
                            match local.pat.node {
                                ast::PatKind::Ident(_, ident, _) => {
                                    let (ahasher, shasher) = self.get_hasher_pair();
                                    let ident_expr = self.cx.expr_ident(ident.span, ident);
                                    let mac_path =
                                        self.cx.path_ident(DUMMY_SP, self.cx.ident_of("cross_check_value"));
                                    let mut mac_args = vec![
                                        token::NtIdent(self.cx.ident_of("UNKNOWN_TAG"), false),
                                        token::NtExpr(ident_expr),
                                        token::NtTy(ahasher),
                                        token::NtTy(shasher),
                                    ];
                                    let xcheck =
                                        self.cx.stmt_mac_fn(DUMMY_SP, mac_path, mac_args);
                                    Some(xcheck)
                                }
                                // TODO: handle more pattern types
                                _ => None,
                            }
                        })
                    }
                    _ => None,
                };
                iter::once(s)
                    .chain(new_stmt.into_iter())
                    .collect::<Vec<_>>()
            })
            .collect()
    }

    fn visit_variant_data(&mut self, vdata: &mut ast::VariantData) {
        self.field_idx_stack.push(0);
        mut_visit::noop_visit_variant_data(vdata, self);
        self.field_idx_stack.pop();
    }

    fn visit_struct_field(&mut self, sf: &mut ast::StructField) {
        mut_visit::noop_visit_struct_field(sf, self);

        // Get the name of the field; the compiler should give us the name
        // if the field is in a Struct. If it's in a Tuple, we use
        // the field index, which we need to compute ourselves from field_idx_stack.
        let sf_name = sf.ident
            .map(|ident| xcfg::FieldIndex::Str(ident.name.to_string()))
            .unwrap_or_else(|| {
                // We use field_idx_stack to keep track of the index/name
                // of the fields inside a VariantData
                let idx = self.field_idx_stack.last_mut().unwrap();
                let res = xcfg::FieldIndex::Int(*idx);
                *idx += 1;
                res
            });

        let sf_attr_xcheck = self.parse_field_attr(&sf.attrs);
        let sf_xcfg_xcheck = self.config().struct_config().fields.get(&sf_name);
        let sf_xcheck = sf_xcfg_xcheck.or_else(|| sf_attr_xcheck.as_ref());
        let hash_attr = sf_xcheck.and_then(|sf_xcheck| {
            match *sf_xcheck {
                xcfg::XCheckType::Default => None,

                xcfg::XCheckType::AsType(ref ty) => {
                    let mut attrs = AttrMap::new();
                    attrs.insert("as_type", AttrValue::Str(ty.clone()));
                    let attr_args = self.convert_hash_attr_map(sf.span, attrs);
                    Some(self.cx.attribute(sf.span, attr_args))
                }

                xcfg::XCheckType::None | xcfg::XCheckType::Disabled => {
                    let mut attrs = AttrMap::new();
                    attrs.insert("none", AttrValue::Nothing);
                    let attr_args = self.convert_hash_attr_map(sf.span, attrs);
                    Some(self.cx.attribute(sf.span, attr_args))
                }

                xcfg::XCheckType::Djb2(_) => unimplemented!(),

                xcfg::XCheckType::Fixed(id) => {
                    // FIXME: we're passing the id in as a string because
                    // that's how derive-macros parses it
                    let mut attrs = AttrMap::new();
                    let sid = format!("{}", id);
                    attrs.insert("fixed_hash", AttrValue::Str(sid));
                    let attr_args = self.convert_hash_attr_map(sf.span, attrs);
                    Some(self.cx.attribute(sf.span, attr_args))
                }

                xcfg::XCheckType::Custom(ref s) => {
                    let mut attrs = AttrMap::new();
                    attrs.insert("custom_hash", AttrValue::Str(s.clone()));
                    let attr_args = self.convert_hash_attr_map(sf.span, attrs);
                    Some(self.cx.attribute(sf.span, attr_args))
                }
            }
        });

        // Remove #[cross_check] from attributes, then append #[cross_check_hash]
        sf.attrs.retain(|attr| !attr.check_name("cross_check"));
        sf.attrs.extend(hash_attr.into_iter());
    }

    fn visit_expr(&mut self, expr: &mut P<ast::Expr>) {
        if cfg!(feature = "expand-macros") {
            //if let ast::ExprKind::Mac(_) = expr.node {
            //    return self
            //        .cx
            //        .expander()
            //        .fold_expr(expr)
            //        .map(|e| fold::noop_fold_expr(e, self));
            //}
            unimplemented!("expand-macros not implemented");
        } else {
            self.expander
                .insert_macro_scope(expr.span, self.config().clone());
        }
        mut_visit::noop_visit_expr(expr, self);
    }

    fn flat_map_foreign_item(&mut self, ni: ast::ForeignItem) -> SmallVec<[ast::ForeignItem; 1]> {
        let folded_items = mut_visit::noop_flat_map_foreign_item(ni, self);
        folded_items
            .into_iter()
            .map(|folded_ni| {
                if let ast::ForeignItemKind::Ty = folded_ni.node {
                    // Foreign type, implement CrossCheckHash for it
                    // This is implemented as a call to the `__c2rust_hash_T` function
                    // TODO: include ahasher/shasher into the function name
                    // TODO: configure this via attribute&external configuration
                    //       * option to disable CrossCheckHash altogether
                    //       * option to use a custom function
                    let ty_name = folded_ni.ident;
                    let ty_suffix = &"struct"; // FIXME
                    let hash_fn_name = format!("__c2rust_hash_{}_{}", ty_name, ty_suffix);
                    let hash_fn_ident = self.cx.ident_of(&hash_fn_name);
                    let mac_path =
                        self.cx.path_ident(DUMMY_SP, self.cx.ident_of("__c2rust_import_extern_hash"));
                    let mac_args = vec![
                        token::NtIdent(ty_name, false),
                        token::NtIdent(hash_fn_ident, false),
                    ];
                    let hash_impl_item = self.cx.item_mac_fn(DUMMY_SP, mac_path, mac_args);
                    self.pending_items.push(hash_impl_item);
                };
                folded_ni
            })
            .collect()
    }

    fn visit_mac(&mut self, mac: &mut ast::Mac) {
        mut_visit::noop_visit_mac(mac, self);
    }
}

#[derive(Default)]
struct CrossCheckExpander {
    // Arguments passed to plugin
    // TODO: pre-parse them???
    external_config: xcfg::Config,
    macro_scopes: RefCell<HashMap<Span, xcfg::scopes::ScopeConfig>>,

    // Reverse mapping from djb2 hashes of names to the originals
    djb2_names: RefCell<HashMap<u32, HashSet<String>>>,
    djb2_names_files: Vec<PathBuf>,

    // List of already emitted C ABI hash functions,
    // used to prevent the emission of duplicates
    #[cfg(feature = "c-hash-functions")]
    c_hash_functions: RefCell<HashSet<String>>,
}

impl CrossCheckExpander {
    fn new(args: &[ast::NestedMetaItem]) -> CrossCheckExpander {
        let mut exp = Self::default();
        exp.external_config = Self::parse_config_files(args);
        exp.djb2_names_files = Self::parse_djb2_names_files(args);
        exp
    }

    fn parse_config_files(args: &[ast::NestedMetaItem]) -> xcfg::Config {
        // Parse the hard-coded default configuration, then apply
        // the items from the files on top of it
        let dcfg = xcfg::parse_string(&default_config::DEFAULT_CONFIG)
            .expect("could not parse default config");

        // Parse arguments of the form
        // #[plugin(c2rust_xcheck_plugin(config_file = "..."))]
        let fl = RealFileLoader;
        args.iter()
            .filter(|nmi| nmi.check_name("config_file"))
            .map(|mi| mi.value_str().expect("invalid string for config_file"))
            .map(|fsym| PathBuf::from(&*fsym.as_str()))
            .map(|fp| {
                fl.abs_path(&fp)
                    .unwrap_or_else(|| panic!("invalid path to config file: {:?}", fp))
            })
            .map(|fp| {
                fl.read_file(&fp)
                    .unwrap_or_else(|e| panic!("could not read config file {:?}: {}", fp, e))
            })
            // TODO: use a Reader to read&parse each configuration file
            // without storing its contents in an intermediate String buffer???
            .map(|fd| xcfg::parse_string(&fd).expect("could not parse config file"))
            .fold(dcfg, |acc, fc| acc.merge(fc))
    }

    fn parse_djb2_names_files(args: &[ast::NestedMetaItem]) -> Vec<PathBuf> {
        let fl = RealFileLoader;
        args.iter()
            .filter(|nmi| nmi.check_name("djb2_names_file"))
            .map(|mi| mi.value_str().expect("invalid string for config_file"))
            .map(|fsym| PathBuf::from(&*fsym.as_str()))
            .map(|fp| {
                fl.abs_path(&fp)
                    .unwrap_or_else(|| panic!("invalid path to djb2 names file: {:?}", fp))
            })
            .collect()
    }

    fn insert_macro_scope(&self, sp: Span, config: xcfg::scopes::ScopeConfig) {
        self.macro_scopes.borrow_mut().insert(sp, config);
    }

    fn find_span_scope(&self, sp: Span) -> Option<xcfg::scopes::ScopeConfig> {
        let macro_scopes = self.macro_scopes.borrow();
        macro_scopes.get(&sp).cloned().or_else(|| {
            sp.ctxt()
                .outer()
                .expn_info()
                .and_then(|ei| self.find_span_scope(ei.call_site))
        })
    }

    fn insert_djb2_name(&self, djb2: u32, name: String) {
        self.djb2_names
            .borrow_mut()
            .entry(djb2)
            .or_default()
            .insert(name);
    }

    fn write_djb2_names(&mut self) {
        let djb2_names = &*self.djb2_names.borrow();
        for fp in &self.djb2_names_files {
            // TODO: should probably read the existing file,
            // and merge our names into the existing contents
            let mut f = fs::File::create(fp)
                .unwrap_or_else(|e| panic!("could not create djb2 names file {:?}: {}", fp, e));
            serde_yaml::to_writer(f, djb2_names).unwrap_or_else(|e| {
                panic!("could not write YAML to djb2 names file {:?}: {}", fp, e)
            });
        }
    }
}

impl Drop for CrossCheckExpander {
    fn drop(&mut self) {
        self.write_djb2_names()
    }
}

impl MultiItemModifier for CrossCheckExpander {
    fn expand(
        &self,
        cx: &mut ExtCtxt,
        sp: Span,
        mi: &ast::MetaItem,
        item: Annotatable,
    ) -> Vec<Annotatable> {
        match item {
            Annotatable::Item(i) => {
                let span_scope = self.find_span_scope(sp);
                // If we're seeing #![cross_check] at the top of the crate or a module,
                // create a fresh configuration and perform a folding; otherwise, just
                // ignore this expansion and let the higher level one do everything
                let ni = match (&i.node, span_scope) {
                    (&ast::ItemKind::Mod(_), None) => {
                        let mut scope_stack = xcfg::scopes::ScopeStack::default();
                        // Parse the top-level attribute configuration
                        let mut top_xcfg = xcfg::ItemConfig::Defaults(Default::default());
                        xcfg::attr::syntax::parse_attr_config(&mut top_xcfg, &mi);
                        scope_stack.last_mut().parse_xcfg_config(&top_xcfg);
                        // Build the scope config for this item
                        scope_stack.push_ast_item(&i, Some(mi), &self.external_config, cx);
                        CrossChecker::new(self, cx, scope_stack, true)
                            .flat_map_item(i)
                            .expect_one("too many items returned")
                    }
                    (_, Some(scope)) => {
                        // If this #[cross_check(...)] expansion is caused by a
                        // macro expansion, handle it here
                        let mut scope_stack = xcfg::scopes::ScopeStack::from_scope(scope);
                        scope_stack.push_ast_item(&i, Some(mi), &self.external_config, cx);
                        // TODO: if build_item_scope returns None, keep scope_config
                        CrossChecker::new(self, cx, scope_stack, true)
                            .flat_map_item(i)
                            .expect_one("too many items returned")
                    }
                    (_, None) => i,
                };
                Annotatable::Item(ni).into()
            }
            // TODO: handle TraitItem
            // TODO: handle ImplItem
            _ => panic!("unexpected item: {:?}", item),
        }
    }
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    let ecc = CrossCheckExpander::new(reg.args());
    reg.register_syntax_extension(
        Symbol::intern("cross_check"),
        SyntaxExtension::MultiModifier(Box::new(ecc)),
    );
}
