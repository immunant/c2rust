
use syntax::ast;

use std::rc::Rc;

use syntax::ext::base::ExtCtxt;
use syntax::ext::quote::rt::ExtParseUtils;
use syntax::tokenstream::TokenTree;

use std::collections::HashMap;

use xcfg;
use xcheck_util;

#[derive(Clone)]
pub struct InheritedCheckConfig {
    // Whether cross-checks are enabled overall
    pub enabled: bool,

    // Overrides for ahasher/shasher
    pub ahasher: Option<Vec<TokenTree>>,
    pub shasher: Option<Vec<TokenTree>>,
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

pub struct FunctionCheckConfig {
    pub entry: xcfg::XCheckType,
    pub all_args: xcfg::XCheckType,
    pub args: HashMap<xcfg::FieldIndex, xcfg::XCheckType>,
}

// We want all_args set to None, so we need a custom Default implementation
impl Default for FunctionCheckConfig {
    fn default() -> FunctionCheckConfig {
        FunctionCheckConfig {
            entry: xcfg::XCheckType::Default,
            all_args: xcfg::XCheckType::None,
            args: Default::default(),
        }
    }
}

#[derive(Default)]
pub struct StructCheckConfig {
    pub custom_hash: Option<String>,
    pub field_hasher: Option<String>,
    pub fields: HashMap<xcfg::FieldIndex, xcfg::XCheckType>,
}

pub enum ItemCheckConfig {
    // Top-level configuration
    Top,

    // Function item configuration
    Function(FunctionCheckConfig),

    // Structure item configuration
    Struct(StructCheckConfig),

    // Other items (for now, this shouldn't really occur)
    Other,
}

pub struct ScopeCheckConfig {
    // Cross-check configuration inherited from parent
    pub inherited: Rc<InheritedCheckConfig>,

    // Configuration for this item
    pub item: ItemCheckConfig,
}

impl ScopeCheckConfig {
    pub fn new() -> ScopeCheckConfig {
        ScopeCheckConfig {
            inherited: Default::default(),
            item: ItemCheckConfig::Top,
        }
    }

    pub fn inherit(&self, item: &ast::Item) -> Self {
        let item_config = match item.node {
            ast::ItemKind::Fn(..) => ItemCheckConfig::Function(Default::default()),
            ast::ItemKind::Enum(..) |
            ast::ItemKind::Struct(..) |
            ast::ItemKind::Union(..) => ItemCheckConfig::Struct(Default::default()),
            _ => ItemCheckConfig::Other,
        };
        ScopeCheckConfig {
            inherited: Rc::clone(&self.inherited),
            item: item_config,
        }
    }

    // Getters for various options
    pub fn function_config(&self) -> &FunctionCheckConfig {
        if let ItemCheckConfig::Function(ref func) = self.item {
            func
        } else {
            panic!("expected function item configuration");
        }
    }

    pub fn struct_config(&self) -> &StructCheckConfig {
        if let ItemCheckConfig::Struct(ref struc) = self.item {
            struc
        } else {
            panic!("expected structure item configuration");
        }
    }

    pub fn parse_attr_config(&mut self, cx: &ExtCtxt, mi: &ast::MetaItem) {
        assert!(mi.name == "cross_check");
        let args = xcfg::attr::get_syntax_item_args(mi);
        for (name, arg) in args.iter() {
            match (*name, &mut self.item) {
                ("disabled", _) |
                ("none", _) => {
                    Rc::make_mut(&mut self.inherited).enabled = false
                }
                ("enabled", _) |
                ("yes", _) => {
                    Rc::make_mut(&mut self.inherited).enabled = true
                }
                ("ahasher", _) => {
                    Rc::make_mut(&mut self.inherited).ahasher =
                        Some(cx.parse_tts(String::from(arg.as_str())));
                }
                ("shasher", _) => {
                    Rc::make_mut(&mut self.inherited).shasher =
                        Some(cx.parse_tts(String::from(arg.as_str())));
                }

                // Function-specific attributes
                ("entry", &mut ItemCheckConfig::Function(ref mut func)) => {
                    func.entry = xcheck_util::parse_xcheck_arg(&arg)
                        .unwrap_or(xcfg::XCheckType::Default);
                }

                ("all_args", &mut ItemCheckConfig::Function(ref mut func)) => {
                    // Enable cross-checking for arguments
                    func.all_args = xcheck_util::parse_xcheck_arg(&arg)
                        .unwrap_or(xcfg::XCheckType::Default);
                }

                ("args", &mut ItemCheckConfig::Function(ref mut func)) => {
                    // Parse per-argument cross-check types
                    func.args.extend(arg.as_list().iter().filter_map(|(name, arg)| {
                        if let xcfg::attr::ArgValue::List(ref l) = *arg {
                            let arg_xcheck = xcheck_util::parse_xcheck_arglist(l)
                                .expect(&format!("expected valid cross-check type \
                                                  for argument: {}", name));
                            Some((xcfg::FieldIndex::from_str(name), arg_xcheck))
                        } else { None }
                    }));
                }

                // Structure-specific attributes
                ("custom_hash", &mut ItemCheckConfig::Struct(ref mut struc)) => {
                    struc.custom_hash = Some(String::from(arg.as_str()));
                },

                ("field_hasher", &mut ItemCheckConfig::Struct(ref mut struc)) => {
                    struc.field_hasher = Some(String::from(arg.as_str()));
                }

                (name@_, _) => panic!("unknown cross_check item: {}", name)
            }
        }
    }

    pub fn parse_xcfg_config(&mut self, cx: &ExtCtxt, xcfg: &xcfg::ItemConfig) {
        macro_rules! parse_optional_field {
            // Field for the current scope
            (>$self_name:ident, $self_parent:ident, $xcfg_parent:ident, $xcfg_name:ident, $new_value:expr) => (
                if let Some(ref $xcfg_name) = $xcfg_parent.$xcfg_name {
                    $self_parent.$self_name = $new_value;
                }
            );
            // Inherited field
            (^$self_name:ident, $xcfg_parent:ident, $xcfg_name:ident, $new_value:expr) => (
                if let Some(ref $xcfg_name) = $xcfg_parent.$xcfg_name {
                    Rc::make_mut(&mut self.inherited).$self_name = $new_value;
                }
            )
        }
        match (&mut self.item, xcfg) {
            (&mut ItemCheckConfig::Function(ref mut self_func), &xcfg::ItemConfig::Function(ref xcfg_func)) => {
                // Inherited fields
                parse_optional_field!(^enabled, xcfg_func, disable_xchecks, !disable_xchecks);
                // TODO: add a way for the external config to reset these to default
                parse_optional_field!(^ahasher, xcfg_func, ahasher, Some(cx.parse_tts(ahasher.clone())));
                parse_optional_field!(^shasher, xcfg_func, shasher, Some(cx.parse_tts(shasher.clone())));
                // Function-specific fields
                parse_optional_field!(>entry,    self_func, xcfg_func, entry,    entry.clone());
                parse_optional_field!(>all_args, self_func, xcfg_func, all_args, all_args.clone());
                self_func.args.extend(xcfg_func.args.iter().map(|(k, v)| {
                    (xcfg::FieldIndex::from_str(k), v.clone())
                }));
                // TODO: parse more fields: exit, ret
            },

            (&mut ItemCheckConfig::Struct(ref mut self_struc), &xcfg::ItemConfig::Struct(ref xcfg_struc)) => {
                // Inherited fields
                // TODO: add a way for the external config to reset these to default
                parse_optional_field!(^ahasher, xcfg_struc, ahasher, Some(cx.parse_tts(ahasher.clone())));
                parse_optional_field!(^shasher, xcfg_struc, shasher, Some(cx.parse_tts(shasher.clone())));
                // Structure-specific fields
                parse_optional_field!(>custom_hash,  self_struc, xcfg_struc, custom_hash,  Some(custom_hash.clone()));
                parse_optional_field!(>field_hasher, self_struc, xcfg_struc, field_hasher, Some(field_hasher.clone()));
                self_struc.fields.extend(xcfg_struc.fields.clone().into_iter());
            },
            (_, _) => ()
        }
    }
}
