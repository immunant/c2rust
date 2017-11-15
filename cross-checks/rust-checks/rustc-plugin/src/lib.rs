#![feature(plugin_registrar, quote, rustc_private, try_from)]

extern crate rustc_plugin;
extern crate syntax;

#[macro_use]
extern crate matches;

extern crate cross_check_config as xcfg;

use rustc_plugin::Registry;
use syntax::ast;
use syntax::fold;

use std::cell::Cell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::path::PathBuf;
use std::rc::Rc;

use syntax::ext::base::{SyntaxExtension, ExtCtxt, Annotatable, MultiItemModifier};
use syntax::ext::quote::rt::{ToTokens, ExtParseUtils};
use syntax::codemap::{Span, FileLoader, RealFileLoader};
use syntax::fold::Folder;
use syntax::symbol::Symbol;
use syntax::print::pprust;
use syntax::ptr::P;
use syntax::tokenstream::TokenTree;
use syntax::util::small_vector::SmallVector;

fn djb2_hash(s: &str) -> u32 {
    s.bytes().fold(5381u32, |h, c| h.wrapping_mul(33).wrapping_add(c as u32))
}

trait CrossCheckHash {
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

fn parse_xcheck_arglist(args: &xcfg::attr::ArgList<'static>) -> Option<xcfg::XCheckType> {
    if args.len() > 1 {
        panic!("expected single argument for cross-check type attribute");
    }
    args.iter().next()
        .map(|(name, ref arg)| parse_xcheck_type(name, arg))
}

fn parse_xcheck_arg(arg: &xcfg::attr::ArgValue<'static>) -> Option<xcfg::XCheckType> {
    match *arg {
        xcfg::attr::ArgValue::Nothing => None,
        xcfg::attr::ArgValue::List(ref l) => parse_xcheck_arglist(l),
        _ => panic!("unexpected argument to all_args():{:?}", *arg)
    }
}

#[derive(Clone)]
struct InheritedCheckConfig {
    // Whether cross-checks are enabled overall
    enabled: bool,

    // Overrides for ahasher/shasher
    ahasher: Option<Vec<TokenTree>>,
    shasher: Option<Vec<TokenTree>>,
}

impl Default for InheritedCheckConfig {
    fn default() -> InheritedCheckConfig {
        InheritedCheckConfig {
            enabled: true,
            ahasher: None,
            shasher: None,
        }
    }
}

#[derive(Clone, Default)]
struct ScopeCheckConfig {
    // Cross-check configuration inherited from parent
    inherited: Rc<InheritedCheckConfig>,

    // The main xcheck for this item (entry point for function, contents for structure)
    main_xcheck: xcfg::XCheckType,

    // Cross-check types for subcomponents: function arguments or structure fields
    sub_xchecks: HashMap<xcfg::FieldIndex, xcfg::XCheckType>,

    // Item-specific configuration starts here
    // ---------------------------------------
    // Function item configuration
    all_args_xcheck: xcfg::XCheckType,

    // Structure item configuration
    field_hasher: Option<String>,
}

#[derive(Debug, PartialEq, Eq)]
enum AttrScope {
    Top,
    Struct,
    Function,
}

impl AttrScope {
    fn from_item(item: &ast::Item) -> AttrScope {
        match item.node {
            ast::ItemKind::Fn(..) => AttrScope::Function,
            ast::ItemKind::Enum(..) |
            ast::ItemKind::Struct(..) |
            ast::ItemKind::Union(..) => AttrScope::Struct,

            _ => panic!("found #[cross_check] attribute on invalid item")
        }
    }
}

impl ScopeCheckConfig {
    fn new() -> ScopeCheckConfig {
        ScopeCheckConfig {
            all_args_xcheck: xcfg::XCheckType::None,
            ..Default::default()
        }
    }

    fn inherit(&self) -> Self {
        ScopeCheckConfig {
            inherited: Rc::clone(&self.inherited),
            ..ScopeCheckConfig::new()
        }
    }

    fn parse_attr_config(&mut self, cx: &ExtCtxt, mi: &ast::MetaItem,
                         scope: AttrScope) {
        assert!(mi.name == "cross_check");
        let args = xcfg::attr::get_syntax_item_args(mi);
        for (name, arg) in args.iter() {
            match *name {
                "disabled" |
                "none" => {
                    Rc::make_mut(&mut self.inherited).enabled = false
                }
                "enabled" |
                "yes" => {
                    Rc::make_mut(&mut self.inherited).enabled = true
                }
                "ahasher" => {
                    Rc::make_mut(&mut self.inherited).ahasher =
                        Some(cx.parse_tts(String::from(arg.as_str())));
                }
                "shasher" => {
                    Rc::make_mut(&mut self.inherited).shasher =
                        Some(cx.parse_tts(String::from(arg.as_str())));
                }

                // Function-specific attributes
                "entry" if scope == AttrScope::Function => {
                    self.main_xcheck = parse_xcheck_arg(&arg)
                        .unwrap_or(xcfg::XCheckType::Default);
                }

                "all_args" if scope == AttrScope::Function => {
                    // Enable cross-checking for arguments
                    self.all_args_xcheck = parse_xcheck_arg(&arg)
                        .unwrap_or(xcfg::XCheckType::Default);
                }

                "args" if scope == AttrScope::Function => {
                    // Parse per-argument cross-check types
                    self.sub_xchecks.extend(arg.as_list().iter().filter_map(|(name, arg)| {
                        if let xcfg::attr::ArgValue::List(ref l) = *arg {
                            let arg_xcheck = parse_xcheck_arglist(l)
                                .expect(&format!("expected valid cross-check type \
                                                  for argument: {}", name));
                            Some((xcfg::FieldIndex::from_str(name), arg_xcheck))
                        } else { None }
                    }));
                }

                // Structure-specific attributes
                "custom_hash" if scope == AttrScope::Struct => {
                    let s = String::from(arg.as_str());
                    self.main_xcheck = xcfg::XCheckType::Custom(s);
                },

                "field_hasher" if scope == AttrScope::Struct => {
                    self.field_hasher = Some(String::from(arg.as_str()));
                }

                name@_ => panic!("unknown cross_check item: {}", name)
            }
        }
    }

    fn parse_xcfg_config(&mut self, cx: &ExtCtxt, xcfg: &xcfg::ItemConfig) {
        macro_rules! parse_optional_field {
            // Field for the current scope
            (>$self_name:ident, $parent:ident, $xcfg_name:ident, $new_value:expr) => (
                if let Some(ref $xcfg_name) = $parent.$xcfg_name {
                    self.$self_name = $new_value;
                }
            );
            // Inherited field
            (^$self_name:ident, $parent:ident, $xcfg_name:ident, $new_value:expr) => (
                if let Some(ref $xcfg_name) = $parent.$xcfg_name {
                    Rc::make_mut(&mut self.inherited).$self_name = $new_value;
                }
            )
        }
        match *xcfg {
            xcfg::ItemConfig::Function(ref func) => {
                parse_optional_field!(^enabled,         func, disable_xchecks, !disable_xchecks);
                parse_optional_field!(>main_xcheck,     func, entry,           entry.clone());
                parse_optional_field!(>all_args_xcheck, func, all_args,        all_args.clone());
                self.sub_xchecks.extend(func.args.iter().map(|(k, v)| {
                    (xcfg::FieldIndex::from_str(k), v.clone())
                }));
                // TODO: add a way for the external config to reset these to default
                parse_optional_field!(^ahasher, func, ahasher, Some(cx.parse_tts(ahasher.clone())));
                parse_optional_field!(^shasher, func, shasher, Some(cx.parse_tts(shasher.clone())));
                // TODO: parse more fields: exit, ret
            },

            xcfg::ItemConfig::Struct(ref struc) => {
                parse_optional_field!(>main_xcheck,  struc, custom_hash,  xcfg::XCheckType::Custom(custom_hash.clone()));
                parse_optional_field!(>field_hasher, struc, field_hasher, Some(field_hasher.clone()));
                self.sub_xchecks.extend(struc.fields.clone().into_iter());
                // TODO: add a way for the external config to reset these to default
                parse_optional_field!(^ahasher, struc, ahasher, Some(cx.parse_tts(ahasher.clone())));
                parse_optional_field!(^shasher, struc, shasher, Some(cx.parse_tts(shasher.clone())));
            },
            _ => ()
        }
    }
}

#[derive(Clone)]
struct ScopeConfig<'xcfg> {
    file_name: Rc<String>, // FIXME: this should be a &str
    items: Option<Rc<xcfg::NamedItemList<'xcfg>>>,
    check_config: ScopeCheckConfig,

    // Index of the next field in this scope (if the scope is a structure)
    // We use this to keep track of the index/ident of the next field
    // in a tuple
    field_idx: Cell<usize>,
}

impl<'xcfg> ScopeConfig<'xcfg> {
    fn new(cfg: &'xcfg xcfg::Config, file_name: &str,
           ccc: ScopeCheckConfig) -> ScopeConfig<'xcfg> {
        ScopeConfig {
            file_name: Rc::new(String::from(file_name)),
            items: cfg.get_file_items(file_name)
                      .map(xcfg::NamedItemList::new)
                      .map(Rc::new),
            check_config: ccc,
            field_idx: Cell::new(0),
        }
    }

    fn get_item_config(&self, item: &str) -> Option<&'xcfg xcfg::ItemConfig> {
        self.items
            .as_ref()
            .and_then(|nil| nil.name_map.get(item))
            .cloned()
    }

    fn from_item(&self, item_config: Option<&'xcfg xcfg::ItemConfig>,
                 ccc: ScopeCheckConfig) -> Self {
        ScopeConfig {
            file_name: self.file_name.clone(),
            items: item_config.and_then(xcfg::ItemConfig::nested_items)
                              .map(xcfg::NamedItemList::new)
                              .map(Rc::new),
            check_config: ccc,
            field_idx: Cell::new(0),
        }
    }

    fn same_file(&self, file_name: &str) -> bool {
        self.file_name.as_ref() == file_name
    }
}

struct CrossChecker<'a, 'cx: 'a, 'xcfg> {
    cx: &'a ExtCtxt<'cx>,
    external_config: &'xcfg xcfg::Config,
    scope_stack: Vec<ScopeConfig<'xcfg>>,
    default_ahasher: Vec<TokenTree>,
    default_shasher: Vec<TokenTree>,
}

fn find_cross_check_attr(attrs: &[ast::Attribute]) -> Option<&ast::Attribute> {
    attrs.iter().find(|attr| attr.check_name("cross_check"))
}

impl<'a, 'cx, 'xcfg> CrossChecker<'a, 'cx, 'xcfg> {
    #[inline]
    fn last_scope(&self) -> &ScopeConfig<'xcfg> {
        self.scope_stack.last().unwrap()
    }

    #[inline]
    fn config(&self) -> &ScopeCheckConfig {
        &self.last_scope().check_config
    }

    fn build_new_scope(&self, item: &ast::Item) -> ScopeConfig<'xcfg> {
        // We have either a #[cross_check] attribute
        // or external config, so create a new ScopeCheckConfig
        let mut new_config = self.config().inherit();
        // TODO: order???
        let xcheck_attr = find_cross_check_attr(&item.attrs);
        if let Some(ref attr) = xcheck_attr {
            let mi = attr.parse_meta(self.cx.parse_sess).unwrap();
            new_config.parse_attr_config(self.cx, &mi, AttrScope::from_item(item));
        };

        let last_scope = self.last_scope();
        let item_xcfg_config = {
            let item_name = item.ident.name.as_str();
            last_scope.get_item_config(&*item_name)
        };
        if let Some(ref xcfg) = item_xcfg_config {
            new_config.parse_xcfg_config(self.cx, xcfg);
        };

        let span = match item.node {
            ast::ItemKind::Mod(ref m) => m.inner,
            _ => item.span
        };
        let mod_file_name = self.cx.codemap().span_to_filename(span);
        if !last_scope.same_file(&mod_file_name) {
            // We should only ever get a file name mismatch
            // at the top of a module
            assert_matches!(item.node, ast::ItemKind::Mod(_));
            ScopeConfig::new(self.external_config, &mod_file_name, new_config)
        } else {
            last_scope.from_item(item_xcfg_config, new_config)
        }
    }

    // Get the ahasher/shasher pair
    fn get_hasher_pair(&self) -> (&Vec<TokenTree>, &Vec<TokenTree>) {
        (self.config().inherited.ahasher.as_ref().unwrap_or(self.default_ahasher.as_ref()),
         self.config().inherited.shasher.as_ref().unwrap_or(self.default_shasher.as_ref()))
    }

    // Get the cross-check block for this argument
    fn build_arg_xcheck(&self, arg: &ast::Arg) -> Option<P<ast::Block>> {
        match arg.pat.node {
            ast::PatKind::Ident(_, ref ident, _) => {
                // Parameter pattern is just an identifier,
                // so we can reference it directly by name
                let arg_idx = xcfg::FieldIndex::from_str(&*ident.node.name.as_str());
                let arg_xcheck_cfg = self.config().sub_xchecks.get(&arg_idx)
                    .unwrap_or(&self.config().all_args_xcheck);
                arg_xcheck_cfg.get_hash(self.cx, || {
                    // By default, we use cross_check_hash
                    // to hash the value of the identifier
                    let (ahasher, shasher) = self.get_hasher_pair();
                    Some(quote_expr!(self.cx, {
                        use cross_check_runtime::hash::CrossCheckHash as XCH;
                        XCH::cross_check_hash::<$ahasher, $shasher>(&$ident)
                    }))
                }).map(|val| quote_block!(self.cx, {
                    cross_check_raw!(FUNCTION_ARG_TAG, $val)
                }))
            }
            _ => unimplemented!()
        }
    }

    // Create the arguments for #[cross_check_hash]
    // FIXME: we need to store them as strings, since there
    // doesn't seem to be a good way to create NestedMetaItems
    fn build_hash_attr_args(&self) -> Vec<String> {
        let mut res: Vec<String> = vec![];
        if let Some(ref ahasher) = self.config().inherited.ahasher {
            let ahasher_str = pprust::tts_to_string(
                &ahasher.to_tokens(self.cx));
            let mi = format!("ahasher=\"{}\"", ahasher_str);
            res.push(mi);
        }
        if let Some(ref shasher) = self.config().inherited.shasher {
            let shasher_str = pprust::tts_to_string(
                &shasher.to_tokens(self.cx));
            let mi = format!("shasher=\"{}\"", shasher_str);
            res.push(mi);
        }
        if let Some(ref field_hasher) = self.config().field_hasher {
            let mi = format!("field_hasher=\"{}\"", field_hasher);
            res.push(mi);
        }
        match self.config().main_xcheck {
            xcfg::XCheckType::Default => (),
            xcfg::XCheckType::Custom(ref s) => {
                let mi = format!("custom_hash=\"{}\"", s);
                res.push(mi);
            }
            ref xc@_ => panic!("invalid cross-check type for structure:{:?}", xc)
        }
        res
    }

    fn internal_fold_item_simple(&mut self, item: ast::Item) -> ast::Item {
        let folded_item = fold::noop_fold_item_simple(item, self);
        match folded_item.node {
            ast::ItemKind::Fn(fn_decl, unsafety, constness, abi, generics, block) => {
                let fn_ident = folded_item.ident;
                let checked_block = if self.config().inherited.enabled {
                    // Add the cross-check to the beginning of the function
                    // TODO: only add the checks to C abi functions???
                    let entry_xcheck = self.config().main_xcheck
                        .get_ident_hash(self.cx, &fn_ident)
                        .map(|hash| quote_stmt!(self.cx, cross_check_raw!(FUNCTION_ENTRY_TAG, $hash);))
                        .unwrap_or_default();
                    // Insert cross-checks for function arguments
                    let arg_xchecks = fn_decl.inputs.iter()
                        .flat_map(|ref arg| self.build_arg_xcheck(arg))
                        .collect::<Vec<P<ast::Block>>>();
                    quote_block!(self.cx, {
                        $entry_xcheck
                        $arg_xchecks
                        $block
                    })
                } else {
                    block
                };

                // Add our typedefs to the beginning of each function;
                // whatever the configuration says, we should always add these
                let block_with_types = {
                    let (ahasher, shasher) = self.get_hasher_pair();
                    quote_block!(self.cx, {
                        #[allow(dead_code)]
                        mod cross_check_types {
                            pub type DefaultAggHasher    = $ahasher;
                            pub type DefaultSimpleHasher = $shasher;
                        };
                        $checked_block
                    })
                };
                let checked_fn = ast::ItemKind::Fn(
                    fn_decl,
                    unsafety,
                    constness,
                    abi,
                    generics,
                    block_with_types);
                // Build and return the replacement function item
                ast::Item {
                    node: checked_fn,
                    ..folded_item
                }
            }
            ast::ItemKind::Enum(_, _) |
            ast::ItemKind::Struct(_, _) |
            ast::ItemKind::Union(_, _) => {
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
                }
                ast::Item {
                    attrs: item_attrs,
                    ..folded_item
                }
            }
            _ => folded_item
        }
    }

    // Parse the #[cross_check(...)] attribute and turn it into a XCheck
    fn parse_field_attr(&self, attrs: &[ast::Attribute]) -> Option<xcfg::XCheckType> {
        let xcheck_attr = find_cross_check_attr(attrs);
        xcheck_attr.and_then(|attr| {
            attr.parse_meta(self.cx.parse_sess).ok().and_then(|mi| {
                let args = xcfg::attr::get_syntax_item_args(&mi);
                parse_xcheck_arglist(&args)
            })
        })
    }
}

impl<'a, 'cx, 'xcfg> Folder for CrossChecker<'a, 'cx, 'xcfg> {
    fn fold_item_simple(&mut self, item: ast::Item) -> ast::Item {
        let new_scope = self.build_new_scope(&item);
        self.scope_stack.push(new_scope);
        let new_item = self.internal_fold_item_simple(item);
        self.scope_stack.pop();
        new_item
    }

    fn fold_stmt(&mut self, s: ast::Stmt) -> SmallVector<ast::Stmt> {
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
        self.last_scope().field_idx.set(0);
        fold::noop_fold_variant_data(vdata, self)
    }

    fn fold_struct_field(&mut self, sf: ast::StructField) -> ast::StructField {
        let folded_sf = fold::noop_fold_struct_field(sf, self);

        // Get the name of the field; the compiler should give us the name
        // if the field is in a Struct. If it's in a Tuple, we use
        // the field index, which we need to compute ourselves from field_idx.
        let sf_name = folded_sf.ident
            .map(|ident| xcfg::FieldIndex::from_str(&*ident.name.as_str()))
            .unwrap_or_else(|| {
                // We use field_idx to keep track of the index/name
                // of the fields inside a VariantData
                let idx = self.last_scope().field_idx.get();
                self.last_scope().field_idx.set(idx + 1);
                xcfg::FieldIndex::Int(idx)
            });

        let sf_attr_xcheck = self.parse_field_attr(&folded_sf.attrs);
        let sf_xcfg_xcheck = self.config().sub_xchecks.get(&sf_name);
        let sf_xcheck = sf_xcfg_xcheck.or(sf_attr_xcheck.as_ref());
        let hash_attr = sf_xcheck.and_then(|sf_xcheck| {
            match *sf_xcheck {
                xcfg::XCheckType::Default => None,

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

    fn fold_mac(&mut self, mac: ast::Mac) -> ast::Mac {
        mac
    }
}

struct CrossCheckExpander {
    // Arguments passed to plugin
    // TODO: pre-parse them???
    external_config: xcfg::Config,
}

impl CrossCheckExpander {
    fn new(args: &[ast::NestedMetaItem]) -> CrossCheckExpander {
        CrossCheckExpander {
            external_config: CrossCheckExpander::parse_config_files(args),
        }
    }

    fn parse_config_files(args: &[ast::NestedMetaItem]) -> xcfg::Config {
        // Parse arguments of the form
        // #[plugin(cross_check_plugin(config_file = "..."))]
        let fl = RealFileLoader;
        args.iter()
            .filter(|nmi| nmi.check_name("config_file"))
            .map(|mi| mi.value_str().expect("invalid string for config_file"))
            .map(|fsym| PathBuf::from(&*fsym.as_str()))
            .map(|fp| fl.abs_path(&fp)
                        .expect(&format!("invalid path to config file: {:?}", fp)))
            .map(|fp| fl.read_file(&fp)
                        .expect(&format!("could not read config file: {:?}", fp)))
            // TODO: use a Reader to read&parse each configuration file
            // without storing its contents in an intermediate String buffer???
            .map(|fd| xcfg::parse_string(&fd).expect("could not parse config file"))
            .fold(Default::default(), |acc, fc| acc.merge(fc))
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
                // If we're seeing #![cross_check] at the top of the crate or a module,
                // create a fresh configuration and perform a folding; otherwise, just
                // ignore this expansion and let the higher level one do everything
                let ni = match i.node {
                    ast::ItemKind::Mod(_) => {
                        let mut top_config = ScopeCheckConfig::new();
                        top_config.parse_attr_config(cx, mi, AttrScope::Top);
                        let top_file_name = cx.codemap().span_to_filename(sp);
                        let top_scope = ScopeConfig::new(&self.external_config,
                                                         &top_file_name,
                                                         top_config);
                        CrossChecker {
                            cx: cx,
                            external_config: &self.external_config,
                            scope_stack: vec![top_scope],
                            default_ahasher: quote_ty!(cx, ::cross_check_runtime::hash::jodyhash::JodyHasher).to_tokens(cx),
                            default_shasher: quote_ty!(cx, ::cross_check_runtime::hash::simple::SimpleHasher).to_tokens(cx),
                        }.fold_item(i)
                         .expect_one("too many items returned")
                    }
                    _ => i
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
    // TODO: parse args
    reg.register_syntax_extension(
        Symbol::intern("cross_check"),
        SyntaxExtension::MultiModifier(Box::new(ecc)));
}
