
use syntax::ast;

use std::rc::Rc;

use syntax::ext::base::ExtCtxt;
use syntax::ext::quote::rt::ExtParseUtils;
use syntax::tokenstream::TokenTree;

use std::collections::HashMap;

use xcfg;
use xcheck_util;

#[derive(Debug, Clone)]
pub struct InheritedCheckConfig {
    // Whether cross-checks are enabled overall
    pub enabled: bool,

    // Function-specific overrides
    pub entry: xcfg::XCheckType,
    pub exit: xcfg::XCheckType,
    pub all_args: xcfg::XCheckType,
    pub ret: xcfg::XCheckType,

    // Overrides for ahasher/shasher
    pub ahasher: Option<Vec<TokenTree>>,
    pub shasher: Option<Vec<TokenTree>>,
}

impl Default for InheritedCheckConfig {
    fn default() -> InheritedCheckConfig {
        InheritedCheckConfig {
            enabled: true,
            entry: xcfg::XCheckType::Default,
            exit: xcfg::XCheckType::Default,
            all_args: xcfg::XCheckType::None,
            ret: xcfg::XCheckType::Disabled,  // FIXME: Default
            ahasher: None,
            shasher: None,
        }
    }
}

#[derive(Debug)]
pub struct FunctionCheckConfig {
    pub args: HashMap<xcfg::FieldIndex, xcfg::XCheckType>,
    pub entry_extra: Vec<xcfg::ExtraXCheck>,
    pub exit_extra: Vec<xcfg::ExtraXCheck>,
}

// We want all_args set to None, so we need a custom Default implementation
impl Default for FunctionCheckConfig {
    fn default() -> FunctionCheckConfig {
        FunctionCheckConfig {
            args: Default::default(),
            entry_extra: Default::default(),
            exit_extra: Default::default(),
        }
    }
}

#[derive(Debug, Default)]
pub struct StructCheckConfig {
    pub custom_hash: Option<String>,
    pub field_hasher: Option<String>,
    pub fields: HashMap<xcfg::FieldIndex, xcfg::XCheckType>,
}

#[derive(Debug)]
pub enum ItemCheckConfig {
    // Top-level configuration
    Top,

    // Per-file defaults
    FileDefaults,

    // Function item configuration
    Function(FunctionCheckConfig),

    // Structure item configuration
    Struct(StructCheckConfig),

    // Other items (for now, this shouldn't really occur)
    Other,
}

#[derive(Debug)]
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

    pub fn from_item(item: &ast::Item, inherited: Rc<InheritedCheckConfig>) -> Self {
        let item_config = match item.node {
            ast::ItemKind::Fn(..) => ItemCheckConfig::Function(Default::default()),
            ast::ItemKind::Enum(..) |
            ast::ItemKind::Struct(..) |
            ast::ItemKind::Union(..) => ItemCheckConfig::Struct(Default::default()),
            _ => ItemCheckConfig::Other,
        };
        ScopeCheckConfig {
            inherited: inherited,
            item: item_config,
        }
    }

    pub fn inherit(&self, item: &ast::Item) -> Self {
        Self::from_item(item, Rc::clone(&self.inherited))
    }

    pub fn new_file(&self) -> Self {
        ScopeCheckConfig {
            inherited: Rc::clone(&self.inherited),
            item: ItemCheckConfig::FileDefaults,
        }
    }

    // Getters for various options
    pub fn function_config(&self) -> &FunctionCheckConfig {
        if let ItemCheckConfig::Function(ref func) = self.item {
            func
        } else {
            panic!("expected function item configuration, \
                    found: {:?}", self.item);
        }
    }

    pub fn struct_config(&self) -> &StructCheckConfig {
        if let ItemCheckConfig::Struct(ref struc) = self.item {
            struc
        } else {
            panic!("expected structure item configuration, \
                    found: {:?}", self.item);
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
                ("entry", &mut ItemCheckConfig::FileDefaults) |
                ("entry", &mut ItemCheckConfig::Function(_)) => {
                    Rc::make_mut(&mut self.inherited).entry =
                        xcheck_util::parse_xcheck_arg(&arg)
                        .unwrap_or(xcfg::XCheckType::Default);
                }

                ("exit", &mut ItemCheckConfig::FileDefaults) |
                ("exit", &mut ItemCheckConfig::Function(_)) => {
                    Rc::make_mut(&mut self.inherited).exit =
                        xcheck_util::parse_xcheck_arg(&arg)
                        .unwrap_or(xcfg::XCheckType::Default);
                }

                // TODO: handle file-level defaults
                ("all_args", &mut ItemCheckConfig::FileDefaults) |
                ("all_args", &mut ItemCheckConfig::Function(_)) => {
                    // Enable cross-checking for arguments
                    Rc::make_mut(&mut self.inherited).all_args =
                        xcheck_util::parse_xcheck_arg(&arg)
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

                ("ret", &mut ItemCheckConfig::FileDefaults) |
                ("ret", &mut ItemCheckConfig::Function(_)) => {
                    // Enable cross-checking for arguments
                    Rc::make_mut(&mut self.inherited).ret =
                        xcheck_util::parse_xcheck_arg(&arg)
                        .unwrap_or(xcfg::XCheckType::Default);
                }

                // TODO: handle entry_extra and exit_extra for Function

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
            (&mut ItemCheckConfig::FileDefaults, &xcfg::ItemConfig::Defaults(ref xcfg_defs)) => {
                // Inherited fields
                parse_optional_field!(^enabled,  xcfg_defs, disable_xchecks, !disable_xchecks);
                parse_optional_field!(^entry,    xcfg_defs, entry,    entry.clone());
                parse_optional_field!(^exit,     xcfg_defs, exit,     exit.clone());
                parse_optional_field!(^all_args, xcfg_defs, all_args, all_args.clone());
                parse_optional_field!(^ret,      xcfg_defs, ret,      ret.clone());
            },

            (&mut ItemCheckConfig::Function(ref mut self_func), &xcfg::ItemConfig::Function(ref xcfg_func)) => {
                // Inherited fields
                parse_optional_field!(^enabled,  xcfg_func, disable_xchecks, !disable_xchecks);
                parse_optional_field!(^entry,    xcfg_func, entry,    entry.clone());
                parse_optional_field!(^exit,     xcfg_func, exit,     exit.clone());
                parse_optional_field!(^all_args, xcfg_func, all_args, all_args.clone());
                parse_optional_field!(^ret,      xcfg_func, ret,      ret.clone());
                // TODO: add a way for the external config to reset these to default
                parse_optional_field!(^ahasher, xcfg_func, ahasher, Some(cx.parse_tts(ahasher.clone())));
                parse_optional_field!(^shasher, xcfg_func, shasher, Some(cx.parse_tts(shasher.clone())));
                // Function-specific fields
                self_func.args.extend(xcfg_func.args.iter().map(|(k, v)| {
                    (xcfg::FieldIndex::from_str(k), v.clone())
                }));
                self_func.entry_extra.extend(xcfg_func.entry_extra.iter().cloned());
                self_func.exit_extra.extend(xcfg_func.exit_extra.iter().cloned());
                // TODO: parse more fields: exit, ret
            },

            (&mut ItemCheckConfig::Struct(ref mut self_struc), &xcfg::ItemConfig::Struct(ref xcfg_struc)) => {
                // Inherited fields
                // TODO: add a way for the external config to reset these to default
                parse_optional_field!(^enabled, xcfg_struc, disable_xchecks, !disable_xchecks);
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
