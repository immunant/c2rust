#![feature(plugin_registrar, quote, rustc_private, try_from)]

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
use std::collections::{HashSet, HashMap};
use std::fs;
use std::path::PathBuf;

use smallvec::SmallVec;

use syntax::ast;
use syntax::ext::base::{SyntaxExtension, ExtCtxt, Annotatable, MultiItemModifier};
use syntax::ext::quote::rt::{ToTokens, ExtParseUtils};
use syntax::source_map::{Span, FileLoader, RealFileLoader};
use syntax::fold::{self, Folder, ExpectOne};
use syntax::symbol::Symbol;
use syntax::print::pprust;
use syntax::ptr::P;
use syntax::tokenstream::TokenTree;

mod default_config;

fn djb2_hash(s: &str) -> u32 {
    s.bytes().fold(5381u32, |h, c| h.wrapping_mul(33).wrapping_add(c as u32))
}

trait CrossCheckBuilder {
    fn build_ident_xcheck(&self, cx: &ExtCtxt, exp: &CrossCheckExpander,
                          tag_str: &str, ident: ast::Ident) -> Option<ast::Stmt>;
    fn build_xcheck<F>(&self, cx: &ExtCtxt, exp: &CrossCheckExpander,
                       tag_str: &str, val_ref_str: &str, f: F) -> Option<ast::Stmt>
        where F: FnOnce(ast::Ident, Vec<ast::Stmt>) -> P<ast::Expr>;
}

impl CrossCheckBuilder for xcfg::XCheckType {
    fn build_ident_xcheck(&self, cx: &ExtCtxt, exp: &CrossCheckExpander,
                          tag_str: &str, ident: ast::Ident) -> Option<ast::Stmt> {
        self.build_xcheck(cx, exp, tag_str, &"$INVALID$", |tag, pre_hash_stmts| {
            assert!(pre_hash_stmts.is_empty());
            let name = &*ident.name.as_str();
            let id = djb2_hash(name) as u64;
            exp.insert_djb2_name(id as u32, String::from(name));
            quote_expr!(cx, Some(($tag, $id)))
        })
    }

    // Allow clients to specify the id or name manually, like this:
    // #[cross_check(name = "foo")]
    // #[cross_check(id = 0x12345678)]
    fn build_xcheck<F>(&self, cx: &ExtCtxt, exp: &CrossCheckExpander,
                       tag_str: &str, val_ref_str: &str, f: F) -> Option<ast::Stmt>
        where F: FnOnce(ast::Ident, Vec<ast::Stmt>) -> P<ast::Expr>
    {
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
                exp.insert_djb2_name(id as u32, s.clone());
                quote_expr!(cx, Some(($tag, $id)))
            },
            xcfg::XCheckType::Custom(ref s) => {
                // TODO: allow the custom expr to return an Option???
                let custom_expr = cx.parse_expr(s.clone());
                quote_expr!(cx, Some(($tag, $custom_expr)))
            },
        };
        quote_stmt!(cx, {
            #[allow(unused_imports)]
            use c2rust_xcheck_runtime::xcheck::$tag;
            cross_check_iter!($check.into_iter())
        })
    }
}

trait AstItemScope {
    fn push_ast_item(&mut self, item: &ast::Item,
                     mi: Option<&ast::MetaItem>,
                     external_config: &xcfg::Config,
                     cx: &ExtCtxt) -> usize;
}

impl AstItemScope for xcfg::scopes::ScopeStack {
    /// Push a Rust AST item to a xcfg::scopes::ScopeStack
    fn push_ast_item(&mut self, item: &ast::Item,
                     mi: Option<&ast::MetaItem>,
                     external_config: &xcfg::Config,
                     cx: &ExtCtxt) -> usize {
        let (span, item_kind, item_xcfg) = match item.node {
            ast::ItemKind::Fn(..)     =>
                (item.span, Some(xcfg::scopes::ItemKind::Function),
                 Some(xcfg::ItemConfig::Function(Default::default()))),
            ast::ItemKind::Enum(..)   |
            ast::ItemKind::Struct(..) |
            ast::ItemKind::Union(..)  =>
                (item.span, Some(xcfg::scopes::ItemKind::Struct),
                 Some(xcfg::ItemConfig::Struct(Default::default()))),
            ast::ItemKind::Impl(..)   =>
                (item.span, Some(xcfg::scopes::ItemKind::Impl),
                 Some(xcfg::ItemConfig::Struct(Default::default()))),
            ast::ItemKind::Mod(ref m) => (m.inner,   None, None),
            _                         => (item.span, None, None)
        };
        let file_name = cx.source_map().span_to_filename(span);
        let file_name = file_name.to_string();

        // Check if there are any file-level defaults, and if so, apply them
        let mut pushed_count = 0usize;
        pushed_count += {
            let file_defaults_config = self.push_file(external_config, &file_name);
            if file_defaults_config.is_some() { 1 } else { 0 }
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
                _ => Cow::from(&*item_name_str)
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
            self.push_item(ik, &file_name, &*item_name,
                           &[item_xcfg], &[]);
            pushed_count += 1;
        }
        pushed_count
    }
}

struct CrossChecker<'a, 'cx: 'a, 'exp> {
    expander: &'exp CrossCheckExpander,
    cx: &'a mut ExtCtxt<'cx>,
    scope_stack: xcfg::scopes::ScopeStack,
    default_ahasher: Vec<TokenTree>,
    default_shasher: Vec<TokenTree>,

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
    fn new(expander: &'exp CrossCheckExpander,
           cx: &'a mut ExtCtxt<'cx>,
           scope_stack: xcfg::scopes::ScopeStack,
           skip_first_scope: bool) -> CrossChecker<'a, 'cx, 'exp> {
        let default_ahasher = {
            let q = quote_ty!(cx, ::c2rust_xcheck_runtime::hash::jodyhash::JodyHasher);
            q.to_tokens(cx)
        };
        let default_shasher = {
            let q = quote_ty!(cx, ::c2rust_xcheck_runtime::hash::simple::SimpleHasher);
            q.to_tokens(cx)
        };
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
    fn get_hasher_pair(&self) -> (Vec<TokenTree>, Vec<TokenTree>) {
        let ahasher = if let Some(ahasher_str) = self.config().inherited.ahasher.clone() {
            self.cx.parse_tts(ahasher_str)
        } else {
            self.default_ahasher.clone()
        };
        let shasher = if let Some(shasher_str) = self.config().inherited.shasher.clone() {
            self.cx.parse_tts(shasher_str)
        } else {
            self.default_shasher.clone()
        };
        (ahasher, shasher)
    }

    // Get the cross-check block for this argument
    fn build_arg_xcheck(&self, arg: &ast::Arg) -> Option<ast::Stmt> {
        match arg.pat.node {
            ast::PatKind::Ident(_, ref ident, _) => {
                // Parameter pattern is just an identifier,
                // so we can reference it directly by name
                let arg_idx = xcfg::FieldIndex::from_str(&*ident.name.as_str());
                let arg_xcheck_cfg = self.config().function_config()
                    .args.get(&arg_idx)
                    .unwrap_or(&self.config().inherited.all_args);
                arg_xcheck_cfg.build_xcheck(self.cx, self.expander,
                                            "FUNCTION_ARG_TAG", "val_ref",
                                            |tag, pre_hash_stmts| {
                    // By default, we use cross_check_hash
                    // to hash the value of the identifier
                    let (ahasher, shasher) = self.get_hasher_pair();
                    quote_expr!(self.cx, {
                        #[allow(unused_imports)]
                        use c2rust_xcheck_runtime::hash::CrossCheckHash as XCH;
                        let val_ref = &$ident;
                        $pre_hash_stmts
                        let hash = XCH::cross_check_hash::<$ahasher, $shasher>(val_ref);
                        hash.map(|hash| ($tag, hash))
                    })
                })
            }
            _ => unimplemented!("unknown argument: {:#?}", arg)
        }
    }

    // Create the arguments for #[cross_check_hash]
    // FIXME: we need to store them as strings, since there
    // doesn't seem to be a good way to create NestedMetaItems
    fn build_hash_attr_args(&self) -> Vec<String> {
        let mut res: Vec<String> = vec![];
        if let Some(ref ahasher) = self.config().inherited.ahasher.as_ref() {
            let mi = format!("ahasher=\"{}\"", ahasher);
            res.push(mi);
        }
        if let Some(ref shasher) = self.config().inherited.shasher.as_ref() {
            let mi = format!("shasher=\"{}\"", shasher);
            res.push(mi);
        }
        let struct_config = self.config().struct_config();
        if let Some(ref field_hasher) = struct_config.field_hasher.as_ref() {
            let mi = format!("field_hasher=\"{}\"", field_hasher);
            res.push(mi);
        }
        if let Some(ref custom_hash) = struct_config.custom_hash.as_ref() {
            let mi = format!("custom_hash=\"{}\"", custom_hash);
            res.push(mi);
        }
        match struct_config.custom_hash_format.as_ref() {
            Some(xcfg::CustomHashFormat::Function) =>
                res.push("custom_hash_format=\"function\"".to_string()),
            Some(xcfg::CustomHashFormat::Expression) =>
                res.push("custom_hash_format=\"expression\"".to_string()),
            Some(xcfg::CustomHashFormat::Extern) =>
                res.push("custom_hash_format=\"extern\"".to_string()),
            None => {}
        }
        res
    }

    fn build_extra_xchecks(&self, extra_xchecks: &[xcfg::ExtraXCheck]) -> Vec<ast::Stmt> {
        extra_xchecks.iter().flat_map(|ex| {
            // TODO: allow the custom functions to return Option or an iterator???
            let expr = self.cx.parse_expr(ex.custom.clone());
            let tag_str = match ex.tag {
                xcfg::XCheckTag::Unknown        => "UNKNOWN_TAG",
                xcfg::XCheckTag::FunctionEntry  => "FUNCTION_ENTRY_TAG",
                xcfg::XCheckTag::FunctionExit   => "FUNCTION_EXIT_TAG",
                xcfg::XCheckTag::FunctionArg    => "FUNCTION_ARG_TAG",
                xcfg::XCheckTag::FunctionReturn => "FUNCTION_RETURN_TAG",
            };
            let tag = ast::Ident::from_str(tag_str);
            quote_stmt!(self.cx, cross_check_raw!($tag, $expr))
        }).collect::<Vec<ast::Stmt>>()
    }

    fn build_function_xchecks(&mut self, fn_ident: ast::Ident,
                              fn_decl: &ast::FnDecl,
                              block: P<ast::Block>) -> P<ast::Block> {
        let checked_block = if self.config().inherited.enabled {
            // Add the cross-check to the beginning of the function
            // TODO: only add the checks to C abi functions???
            let cfg = &self.config();
            let entry_xcheck = cfg.inherited.entry
                .build_ident_xcheck(self.cx, self.expander,
                                    "FUNCTION_ENTRY_TAG", fn_ident);
            let exit_xcheck = cfg.inherited.exit
                .build_ident_xcheck(self.cx, self.expander,
                                    "FUNCTION_EXIT_TAG", fn_ident);
            // Insert cross-checks for function arguments
            let arg_xchecks = fn_decl.inputs.iter()
                .flat_map(|ref arg| self.build_arg_xcheck(arg))
                .collect::<Vec<ast::Stmt>>();
            let result_xcheck = cfg.inherited.ret
                .build_xcheck(self.cx, self.expander,
                              "FUNCTION_RETURN_TAG", "val_ref",
                              |tag, pre_hash_stmts| {
                // By default, we use cross_check_hash
                // to hash the value of the identifier
                let (ahasher, shasher) = self.get_hasher_pair();
                quote_expr!(self.cx, {
                    #[allow(unused_imports)]
                    use c2rust_xcheck_runtime::hash::CrossCheckHash as XCH;
                    let val_ref = &__c2rust_fn_result;
                    $pre_hash_stmts
                    let hash = XCH::cross_check_hash::<$ahasher, $shasher>(val_ref);
                    hash.map(|hash| ($tag, hash))
                })
            });

            let fcfg = &cfg.function_config();
            let entry_extra_xchecks = self.build_extra_xchecks(&fcfg.entry_extra);
            let exit_extra_xchecks = self.build_extra_xchecks(&fcfg.exit_extra);
            // Extract the result type from the function signature,
            // so we can attach it to the __c2rust_fn_body closure
            let result_ty = match fn_decl.output {
                ast::FunctionRetTy::Default(_) => quote_ty!(self.cx, ()),
                ast::FunctionRetTy::Ty(ref ty) => ty.clone(),
            };
            quote_block!(self.cx, {
                $entry_xcheck
                $arg_xchecks
                $entry_extra_xchecks
                let mut __c2rust_fn_body = || -> $result_ty { $block };
                let __c2rust_fn_result = __c2rust_fn_body();
                $exit_xcheck
                $result_xcheck
                $exit_extra_xchecks
                __c2rust_fn_result
            })
        } else {
            block
        };
        // Add our typedefs to the beginning of each function;
        // whatever the configuration says, we should always add these
        let (ahasher, shasher) = self.get_hasher_pair();
        quote_block!(self.cx, {
            #[allow(dead_code)]
            mod cross_check_types {
                pub type DefaultAggHasher    = $ahasher;
                pub type DefaultSimpleHasher = $shasher;
            };
            $checked_block
        })
    }

    fn build_union_hash(&mut self, union_ident: ast::Ident) -> P<ast::Item> {
        let custom_hash_opt = &self.config().struct_config().custom_hash;
        let custom_hash_format = &self.config().struct_config().custom_hash_format;
        let hash_body = if let Some(ref custom_hash) = *custom_hash_opt {
            // User provided a custom hash function, use it
            match custom_hash_format {
                None |
                Some(xcfg::CustomHashFormat::Function) => {
                    let (ahasher, shasher) = self.get_hasher_pair();
                    let hash_fn = ast::Ident::from_str(custom_hash);
                    quote_expr!(self.cx, $hash_fn::<$ahasher, $shasher>(&self, _depth))
                }
                Some(xcfg::CustomHashFormat::Expression) => {
                    self.cx.parse_expr(custom_hash.clone())
                }
                Some(xcfg::CustomHashFormat::Extern) => {
                    let hash_fn = ast::Ident::from_str(custom_hash);
                    quote_expr!(self.cx, {
                        extern {
                            #[no_mangle]
                            fn $hash_fn(_: *const (), _: usize) -> u64;
                        }
                        unsafe { $hash_fn(self as *const (), _depth) }
                    })
                }
            }
        } else {
            // TODO: emit warning
            quote_expr!(self.cx, {
                if _depth == 0 {
                    ::c2rust_xcheck_runtime::hash::LEAF_RECORD_HASH
                } else {
                    ::c2rust_xcheck_runtime::hash::ANY_UNION_HASH
                }
            })
        };
        quote_item!(self.cx,
            impl ::c2rust_xcheck_runtime::hash::CrossCheckHash for $union_ident {
                #[inline]
                fn cross_check_hash_depth<HA, HS>(&self, _depth: usize) -> u64
                    where HA: ::c2rust_xcheck_runtime::hash::CrossCheckHasher,
                          HS: ::c2rust_xcheck_runtime::hash::CrossCheckHasher
                {
                    $hash_body
                }
            }
        ).unwrap_or_else(|| {
            panic!("unable to implement CrossCheckHash for union '{}'",
                   union_ident.to_string())
        })
    }

    #[cfg(feature="c-hash-functions")]
    fn build_type_c_hash_function(&mut self, ty_ident: ast::Ident,
                                  ty_suffix: &str) -> Option<P<ast::Item>> {
        let hash_fn_name = format!("__c2rust_hash_{}_{}", ty_ident, ty_suffix);
        let hash_fn = ast::Ident::from_str(&hash_fn_name);
        let hash_fn_section = format!(".gnu.linkonce.t.{}", hash_fn_name);

        // Check if function has already been emitted;
        // FIXME: should this check be optional (compile-time feature)???
        if !self.expander.c_hash_functions.borrow_mut().insert(hash_fn_name) {
            return None;
        }

        let (ahasher, shasher) = self.get_hasher_pair();
        Some(quote_item!(self.cx,
            #[no_mangle]
            #[link_section = $hash_fn_section]
            pub unsafe extern "C" fn $hash_fn(x: *mut $ty_ident, depth: usize) -> u64 {
                #[allow(unused_imports)]
                use ::c2rust_xcheck_runtime::hash::CrossCheckHash;
                (*x).cross_check_hash_depth::<$ahasher, $shasher>(depth)
            }
        ).unwrap_or_else(|e| {
            panic!("unable to implement C ABI hash function for type '{}': {}",
                   ty_ident.to_string(), e)
        }))
    }

    fn internal_fold_item_simple(&mut self, item: ast::Item) -> ast::Item {
        let folded_item = fold::noop_fold_item_simple(item, self);
        match folded_item.node {
            ast::ItemKind::Fn(fn_decl, header, generics, block) => {
                let checked_block = self.build_function_xchecks(
                    folded_item.ident, &*fn_decl, block);
                let checked_fn = ast::ItemKind::Fn(
                    fn_decl,
                    header,
                    generics,
                    checked_block);
                // Build and return the replacement function item
                ast::Item {
                    node: checked_fn,
                    ..folded_item
                }
            }
            ast::ItemKind::Union(_, _) => {
                let union_hash_impl = self.build_union_hash(folded_item.ident);
                self.pending_items.push(union_hash_impl);
                #[cfg(feature="c-hash-functions")]
                {
                    let c_hash_func = self.build_type_c_hash_function(folded_item.ident,
                                                                      "struct");
                    self.pending_items.extend(c_hash_func.into_iter());
                }
                folded_item
            }
            ast::ItemKind::Enum(_, _) |
            ast::ItemKind::Struct(_, _) => {
                // Prepend #[derive(CrossCheckHash)] automatically
                // to every structure definition
                let mut item_attrs = folded_item.attrs;
                if self.config().inherited.enabled {
                    let xcheck_hash_derive_attr = quote_attr!(self.cx, #[derive(CrossCheckHash)]);
                    item_attrs.push(xcheck_hash_derive_attr);

                    let attr_args = self.cx.parse_tts(self.build_hash_attr_args().join(","));
                    if !attr_args.is_empty() {
                        let xcheck_hash_attr = quote_attr!(self.cx, #[cross_check_hash($attr_args)]);
                        item_attrs.push(xcheck_hash_attr);
                    }
                    #[cfg(feature="c-hash-functions")]
                    {
                        let c_hash_func = self.build_type_c_hash_function(folded_item.ident,
                                                                          "struct");
                        self.pending_items.extend(c_hash_func.into_iter());
                    }
                }
                ast::Item {
                    attrs: item_attrs,
                    ..folded_item
                }
            }
            ast::ItemKind::Mac(_) => {
                if !cfg!(feature = "expand_macros") {
                    self.expander.insert_macro_scope(folded_item.span, self.config().clone());
                }
                folded_item
            }
            _ => folded_item
        }
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

impl<'a, 'cx, 'exp> Folder for CrossChecker<'a, 'cx, 'exp> {
    fn fold_item_simple(&mut self, item: ast::Item) -> ast::Item {
        if self.skip_first_scope {
            // If skip_first_scope is true, skip building a new scope
            // (see the comment for skip_first_scope in CrossChecker above)
            self.skip_first_scope = false;
            self.internal_fold_item_simple(item)
        } else {
            let new_scopes =
                self.scope_stack.push_ast_item(&item, None,
                                               &self.expander.external_config,
                                               self.cx);
            let new_item = self.internal_fold_item_simple(item);
            self.scope_stack.pop_multi(new_scopes);
            new_item
        }
    }

    fn fold_impl_item(&mut self, item: ast::ImplItem) -> SmallVec<[ast::ImplItem; 1]> {
        match item.node {
            ast::ImplItemKind::Method(sig, body) => {
                // FIXME: this is a bit hacky: we forcibly build a fake
                // Item::Fn with the same signature and body as our method,
                // then add cross-checks to that one
                let fake_item = ast::Item {
                    ident:  item.ident,
                    attrs:  item.attrs,
                    id:     item.id,
                    vis:    item.vis,
                    span:   item.span,
                    tokens: item.tokens,
                    node: ast::ItemKind::Fn(sig.decl, sig.header,
                                            item.generics, body)
                };
                let folded_fake_item = self.fold_item_simple(fake_item);
                let (folded_sig, folded_generics, folded_body) = match folded_fake_item.node {
                    ast::ItemKind::Fn(decl, header, generics, body) => {
                        let sig = ast::MethodSig { header, decl };
                        // TODO: call noop_fold_method_sig on sig???
                        (sig, generics, body)
                    }
                    n => panic!("unexpected folded item node: {:?}", n)
                };
                smallvec![ast::ImplItem {
                    ident:  folded_fake_item.ident,
                    attrs:  folded_fake_item.attrs,
                    id:     folded_fake_item.id,
                    vis:    folded_fake_item.vis,
                    span:   folded_fake_item.span,
                    tokens: folded_fake_item.tokens,
                    defaultness: item.defaultness,
                    generics: folded_generics,
                    node: ast::ImplItemKind::Method(folded_sig, folded_body)
                }]
            }
            _ => fold::noop_fold_impl_item(item, self)
        }
    }

    fn fold_stmt(&mut self, s: ast::Stmt) -> SmallVec<[ast::Stmt; 1]> {
       if cfg!(feature = "expand-macros") {
           if let ast::StmtKind::Mac(_) = s.node {
               return self.cx.expander().fold_stmt(s)
                   .into_iter()
                   .flat_map(|stmt| self.fold_stmt(stmt).into_iter())
                   .collect();
           }
       } else {
           self.expander.insert_macro_scope(s.span, self.config().clone());
       }

       let folded_stmt = fold::noop_fold_stmt(s, self);
       folded_stmt.into_iter().flat_map(|s| {
           let new_stmt = match s.node {
               ast::StmtKind::Local(ref local) => {
                   let attr = find_cross_check_attr(&*local.attrs);
                   // TODO: check that the cross_check attr is "yes"
                   attr.and_then(|_| {
                       // TODO: only add cross-checks for initialized locals???
                       // (in other words, check local.init.is_some())
                       match local.pat.node {
                           ast::PatKind::Ident(_, ident, _) => {
                               Some(quote_stmt!(self.cx, cross_check_value!($ident)).unwrap())
                           },
                           // TODO: handle more pattern types
                           _ => None
                       }
                   })
               },
               _ => None
           };
           Some(s).into_iter()
                  .chain(new_stmt.into_iter())
                  .collect::<Vec<_>>()
       }).collect()
    }

    fn fold_variant_data(&mut self, vdata: ast::VariantData) -> ast::VariantData {
        self.field_idx_stack.push(0);
        let res = fold::noop_fold_variant_data(vdata, self);
        self.field_idx_stack.pop();
        res
    }

    fn fold_struct_field(&mut self, sf: ast::StructField) -> ast::StructField {
        let folded_sf = fold::noop_fold_struct_field(sf, self);

        // Get the name of the field; the compiler should give us the name
        // if the field is in a Struct. If it's in a Tuple, we use
        // the field index, which we need to compute ourselves from field_idx_stack.
        let sf_name = folded_sf.ident
            .map(|ident| xcfg::FieldIndex::from_str(&*ident.name.as_str()))
            .unwrap_or_else(|| {
                // We use field_idx_stack to keep track of the index/name
                // of the fields inside a VariantData
                let idx = self.field_idx_stack.last_mut().unwrap();
                let res = xcfg::FieldIndex::Int(*idx);
                *idx += 1;
                res
            });

        let sf_attr_xcheck = self.parse_field_attr(&folded_sf.attrs);
        let sf_xcfg_xcheck = self.config().struct_config().fields.get(&sf_name);
        let sf_xcheck = sf_xcfg_xcheck.or_else(|| sf_attr_xcheck.as_ref());
        let hash_attr = sf_xcheck.and_then(|sf_xcheck| {
            match *sf_xcheck {
                xcfg::XCheckType::Default => None,

                xcfg::XCheckType::AsType(ref ty) =>
                    Some(quote_attr!(self.cx, #[cross_check_hash(as_type=$ty)])),

                xcfg::XCheckType::None |
                xcfg::XCheckType::Disabled =>
                    Some(quote_attr!(self.cx, #[cross_check_hash(none)])),

                xcfg::XCheckType::Djb2(_) => unimplemented!(),

                xcfg::XCheckType::Fixed(id) => {
                    // FIXME: we're passing the id in as a string because
                    // that's how derive-macros parses it
                    let sid = format!("{}", id);
                    Some(quote_attr!(self.cx, #[cross_check_hash(fixed_hash=$sid)]))
                },

                xcfg::XCheckType::Custom(ref s) =>
                    Some(quote_attr!(self.cx, #[cross_check_hash(custom_hash=$s)])),
            }
        });

        // Remove #[cross_check] from attributes, then append #[cross_check_hash]
        let sf_attrs = folded_sf.attrs.into_iter()
            .filter(|attr| !attr.check_name("cross_check"))
            .chain(hash_attr.into_iter())
            .collect();
        ast::StructField {
            attrs: sf_attrs,
            ..folded_sf
        }
    }

    // Fold functions that handle macro expansion
    fn fold_item(&mut self, item: P<ast::Item>) -> SmallVec<[P<ast::Item>; 1]> {
        if cfg!(feature = "expand-macros") {
            if let ast::ItemKind::Mac(_) = item.node {
                return self.cx.expander().fold_item(item)
                    .into_iter()
                    .flat_map(|item| self.fold_item(item).into_iter())
                    .collect();
            }
        }
        let mut res = fold::noop_fold_item(item, self);
        // Add the pending items
        res.extend(self.pending_items.drain(..));
        res
    }

    fn fold_expr(&mut self, expr: P<ast::Expr>) -> P<ast::Expr> {
        if cfg!(feature = "expand-macros") {
            if let ast::ExprKind::Mac(_) = expr.node {
                return self.cx.expander().fold_expr(expr)
                    .map(|e| fold::noop_fold_expr(e, self));
            }
        } else {
           self.expander.insert_macro_scope(expr.span, self.config().clone());
        }
        expr.map(|e| fold::noop_fold_expr(e, self))
    }

    // TODO: fold_block???

    fn fold_foreign_item(&mut self, ni: ast::ForeignItem) -> SmallVec<[ast::ForeignItem; 1]> {
        let folded_items = fold::noop_fold_foreign_item(ni, self);
        folded_items.into_iter().map(|folded_ni| {
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
                let hash_fn = ast::Ident::from_str(&hash_fn_name);
                let hash_impl_item = quote_item!(self.cx,
                    impl ::c2rust_xcheck_runtime::hash::CrossCheckHash for $ty_name {
                        #[inline]
                        fn cross_check_hash_depth<HA, HS>(&self, depth: usize) -> u64
                            where HA: ::c2rust_xcheck_runtime::hash::CrossCheckHasher,
                                  HS: ::c2rust_xcheck_runtime::hash::CrossCheckHasher
                        {
                            extern {
                                #[no_mangle]
                                fn $hash_fn(_: *const $ty_name, _: usize) -> u64;
                            }
                            unsafe { $hash_fn(self as *const $ty_name, depth) }
                        }
                    }
                ).unwrap_or_else(|| {
                    panic!("unable to implement CrossCheckHash for foreign type '{}'", ty_name)
                });
                self.pending_items.push(hash_impl_item);
            };
            folded_ni
        }).collect()
    }

    fn fold_mac(&mut self, mac: ast::Mac) -> ast::Mac {
       mac
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
    #[cfg(feature="c-hash-functions")]
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
            .map(|fp| fl.abs_path(&fp)
                        .unwrap_or_else(|| {
                            panic!("invalid path to config file: {:?}", fp)
                        }))
            .map(|fp| fl.read_file(&fp)
                        .unwrap_or_else(|e| {
                            panic!("could not read config file {:?}: {}", fp, e)
                        }))
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
            .map(|fp| fl.abs_path(&fp)
                        .unwrap_or_else(|| {
                            panic!("invalid path to djb2 names file: {:?}", fp)
                        }))
            .collect()
    }

    fn insert_macro_scope(&self, sp: Span, config: xcfg::scopes::ScopeConfig) {
        self.macro_scopes.borrow_mut().insert(sp, config);
    }

    fn find_span_scope(&self, sp: Span) -> Option<xcfg::scopes::ScopeConfig> {
        let macro_scopes = self.macro_scopes.borrow();
        macro_scopes.get(&sp).cloned().or_else(|| {
            sp.ctxt().outer().expn_info().and_then(|ei| {
                self.find_span_scope(ei.call_site)
            })
        })
    }

    fn insert_djb2_name(&self, djb2: u32, name: String) {
        self.djb2_names.borrow_mut()
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
                .unwrap_or_else(|e| {
                    panic!("could not create djb2 names file {:?}: {}", fp, e)
                });
            serde_yaml::to_writer(f, djb2_names)
                .unwrap_or_else(|e| {
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
    fn expand(&self,
              cx: &mut ExtCtxt,
              sp: Span,
              mi: &ast::MetaItem,
              item: Annotatable) -> Vec<Annotatable> {
        match item {
            Annotatable::Item(i) => {
                let span_scope = self.find_span_scope(sp);
                // If we're seeing #![cross_check] at the top of the crate or a module,
                // create a fresh configuration and perform a folding; otherwise, just
                // ignore this expansion and let the higher level one do everything
                let ni = match (&i.node, span_scope) {
                    (&ast::ItemKind::Mod(_), None) => {
                        let mut scope_stack = xcfg::scopes::ScopeStack::new();
                        // Parse the top-level attribute configuration
                        let mut top_xcfg = xcfg::ItemConfig::Defaults(Default::default());
                        xcfg::attr::syntax::parse_attr_config(&mut top_xcfg, &mi);
                        scope_stack.last_mut().parse_xcfg_config(&top_xcfg);
                        // Build the scope config for this item
                        scope_stack.push_ast_item(&i, Some(mi),
                                                  &self.external_config, cx);
                        CrossChecker::new(self, cx, scope_stack, true)
                            .fold_item(i)
                            .expect_one("too many items returned")
                    }
                    (_, Some(scope)) => {
                        // If this #[cross_check(...)] expansion is caused by a
                        // macro expansion, handle it here
                        let mut scope_stack = xcfg::scopes::ScopeStack::from_scope(scope);
                        scope_stack.push_ast_item(&i, Some(mi),
                                                  &self.external_config, cx);
                        // TODO: if build_item_scope returns None, keep scope_config
                        CrossChecker::new(self, cx, scope_stack, true)
                            .fold_item(i)
                            .expect_one("too many items returned")
                    }
                    (_, None) => i
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
        SyntaxExtension::MultiModifier(Box::new(ecc)));
}
