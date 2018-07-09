
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct InheritedConfig {
    // Whether cross-checks are enabled overall
    pub enabled: bool,

    // Function-specific overrides
    pub entry: super::XCheckType,
    pub exit: super::XCheckType,
    pub all_args: super::XCheckType,
    pub ret: super::XCheckType,

    // Overrides for ahasher/shasher
    pub ahasher: Option<String>,
    pub shasher: Option<String>,
}

impl Default for InheritedConfig {
    fn default() -> InheritedConfig {
        InheritedConfig {
            enabled: true,
            entry: super::XCheckType::Default,
            exit: super::XCheckType::Default,
            all_args: super::XCheckType::None,
            ret: super::XCheckType::Default,
            ahasher: None,
            shasher: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionConfig {
    pub args: HashMap<super::FieldIndex, super::XCheckType>,
    pub entry_extra: Vec<super::ExtraXCheck>,
    pub exit_extra: Vec<super::ExtraXCheck>,
}

// We want all_args set to None, so we need a custom Default implementation
impl Default for FunctionConfig {
    fn default() -> FunctionConfig {
        FunctionConfig {
            args: Default::default(),
            entry_extra: Default::default(),
            exit_extra: Default::default(),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct StructConfig {
    pub custom_hash: Option<String>,
    pub field_hasher: Option<String>,
    pub fields: HashMap<super::FieldIndex, super::XCheckType>,
}

#[derive(Debug, Copy, Clone)]
pub enum ItemKind {
    Function,
    Struct,
    Impl,
}

#[derive(Debug, Clone)]
pub enum ItemConfig {
    // Top-level configuration
    Top,

    // Per-file defaults
    FileDefaults,

    // Function item configuration
    Function(FunctionConfig),

    // Structure item configuration
    Struct(StructConfig),

    // `impl` for a structure
    Impl,
}

#[derive(Debug, Clone)]
pub struct ScopeConfig {
    // File containing this scope
    pub file_name: Option<Rc<String>>, // FIXME: this should be a &str

    // Configuration for items contained in this scope
    pub items: Option<super::NamedItemList>,

    // Cross-check configuration inherited from parent
    pub inherited: Rc<InheritedConfig>,

    // Configuration for this item
    pub item: ItemConfig,
}

impl ScopeConfig {
    pub fn new() -> ScopeConfig {
        ScopeConfig {
            file_name: None,
            items: None,
            inherited: Default::default(),
            item: ItemConfig::Top,
        }
    }

    /// Build a FileDefaults ScopeConfig for the given file,
    /// if we have any FileDefaults in the external configuration
    pub fn new_file(&self, external_config: &super::Config,
                    file_name: &str) -> Option<Self> {
        if self.same_file(file_name) {
            return None;
        }
        let file_items = external_config.get_file_items(file_name);
        file_items.map(|file_items| {
            let mut new_config = ScopeConfig {
                file_name: Some(Rc::new(String::from(file_name))),
                items: Some(super::NamedItemList::new(file_items)),
                inherited: Rc::clone(&self.inherited),
                item: ItemConfig::FileDefaults,
            };
            for item in file_items.items().iter() {
                match **item {
                    super::ItemConfig::Defaults(_) => {
                        new_config.parse_xcfg_config(item);
                    }
                    _ => (),
                }
            }
            new_config
        })
    }

    pub fn new_item(&self, item: ItemKind, file_name: &str) -> Self {
        let file_name = if self.same_file(file_name) {
            self.file_name.as_ref().map(Rc::clone)
        } else {
            Some(Rc::new(String::from(file_name)))
        };
        let item_config = match item {
            ItemKind::Function => ItemConfig::Function(Default::default()),
            ItemKind::Struct   => ItemConfig::Struct(Default::default()),
            ItemKind::Impl     => ItemConfig::Impl,
        };
        ScopeConfig {
            file_name: file_name,
            items: Default::default(),
            inherited: Rc::clone(&self.inherited),
            item: item_config,
        }
    }

    fn same_file(&self, file_name: &str) -> bool {
        self.file_name.as_ref()
            .map(|sfn| **sfn == file_name)
            .unwrap_or(false)
    }

    fn get_item_xcfg(&self, item: &str) -> Option<&super::ItemConfig> {
        self.items
            .as_ref()
            .and_then(|nil| nil.name_map.get(item))
            .map(|x| &**x)
    }

    // Getters for various options
    pub fn function_config(&self) -> &FunctionConfig {
        if let ItemConfig::Function(ref func) = self.item {
            func
        } else {
            panic!("expected function item configuration, \
                    found: {:?}", self.item);
        }
    }

    pub fn struct_config(&self) -> &StructConfig {
        if let ItemConfig::Struct(ref struc) = self.item {
            struc
        } else {
            panic!("expected structure item configuration, \
                    found: {:?}", self.item);
        }
    }

    pub fn parse_xcfg_config(&mut self, xcfg: &super::ItemConfig) {
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
            (&mut ItemConfig::Top,          &super::ItemConfig::Defaults(ref xcfg_defs)) |
            (&mut ItemConfig::FileDefaults, &super::ItemConfig::Defaults(ref xcfg_defs)) => {
                // Inherited fields
                parse_optional_field!(^enabled,  xcfg_defs, disable_xchecks, !disable_xchecks);
                parse_optional_field!(^entry,    xcfg_defs, entry,    entry.clone());
                parse_optional_field!(^exit,     xcfg_defs, exit,     exit.clone());
                parse_optional_field!(^all_args, xcfg_defs, all_args, all_args.clone());
                parse_optional_field!(^ret,      xcfg_defs, ret,      ret.clone());
            },

            (&mut ItemConfig::Function(ref mut self_func), &super::ItemConfig::Function(ref xcfg_func)) => {
                // Inherited fields
                parse_optional_field!(^enabled,  xcfg_func, disable_xchecks, !disable_xchecks);
                parse_optional_field!(^entry,    xcfg_func, entry,    entry.clone());
                parse_optional_field!(^exit,     xcfg_func, exit,     exit.clone());
                parse_optional_field!(^all_args, xcfg_func, all_args, all_args.clone());
                parse_optional_field!(^ret,      xcfg_func, ret,      ret.clone());
                // TODO: add a way for the external config to reset these to default
                parse_optional_field!(^ahasher, xcfg_func, ahasher, Some(ahasher.clone()));
                parse_optional_field!(^shasher, xcfg_func, shasher, Some(shasher.clone()));
                // Function-specific fields
                self_func.args.extend(xcfg_func.args.iter().map(|(k, v)| {
                    (super::FieldIndex::from_str(k), v.clone())
                }));
                self_func.entry_extra.extend(xcfg_func.entry_extra.iter().cloned());
                self_func.exit_extra.extend(xcfg_func.exit_extra.iter().cloned());
                // TODO: parse more fields: exit, ret
                if let Some(ref nested_items) = xcfg_func.nested {
                    self.items.get_or_insert_with(Default::default)
                        .extend(super::NamedItemList::new(nested_items));
                }
            },

            (&mut ItemConfig::Struct(ref mut self_struc), &super::ItemConfig::Struct(ref xcfg_struc)) => {
                // Inherited fields
                // TODO: add a way for the external config to reset these to default
                parse_optional_field!(^enabled, xcfg_struc, disable_xchecks, !disable_xchecks);
                parse_optional_field!(^ahasher, xcfg_struc, ahasher, Some(ahasher.clone()));
                parse_optional_field!(^shasher, xcfg_struc, shasher, Some(shasher.clone()));
                // Structure-specific fields
                parse_optional_field!(>custom_hash,  self_struc, xcfg_struc, custom_hash,  Some(custom_hash.clone()));
                parse_optional_field!(>field_hasher, self_struc, xcfg_struc, field_hasher, Some(field_hasher.clone()));
                self_struc.fields.extend(xcfg_struc.fields.clone().into_iter());
                if let Some(ref nested_items) = xcfg_struc.nested {
                    self.items.get_or_insert_with(Default::default)
                        .extend(super::NamedItemList::new(nested_items));
                }
            },

            // Parse the relevant fields for `impl`s
            (&mut ItemConfig::Impl, &super::ItemConfig::Struct(ref xcfg_struc)) => {
                // Inherited fields
                // TODO: add a way for the external config to reset these to default
                parse_optional_field!(^enabled, xcfg_struc, disable_xchecks, !disable_xchecks);
                parse_optional_field!(^ahasher, xcfg_struc, ahasher, Some(ahasher.clone()));
                parse_optional_field!(^shasher, xcfg_struc, shasher, Some(shasher.clone()));
                if let Some(ref nested_items) = xcfg_struc.nested {
                    self.items.get_or_insert_with(Default::default)
                        .extend(super::NamedItemList::new(nested_items));
                }
            },
            p @ (_, _) => panic!(format!("mismatched configuration: {:?}", p))
        }
    }
}

#[derive(Debug, Clone)]
pub struct ScopeStack {
    stack: Vec<ScopeConfig>,
}

impl ScopeStack {
    pub fn new() -> ScopeStack {
        ScopeStack {
            stack: vec![ScopeConfig::new()],
        }
    }

    pub fn from_scope(scope: ScopeConfig) -> ScopeStack {
        ScopeStack {
            stack: vec![scope],
        }
    }

    pub fn push_file(&mut self, external_config: &super::Config,
                     file_name: &str) -> Option<&ScopeConfig> {
        let file_scope = self.last().new_file(external_config, file_name);
        file_scope.map(move |file_scope| {
            self.stack.push(file_scope);
            self.last()
        })
    }

    pub fn push_item(&mut self, item_kind: ItemKind,
                     file_name: &str, item_name: &str,
                     pre_xcfg: Option<super::ItemConfig>,
                     post_xcfg: Option<super::ItemConfig>) -> &ScopeConfig {
        let new_config = {
            let old_config = self.last();
            let mut new_config = old_config.new_item(item_kind, file_name);
            if let Some(xcfg) = pre_xcfg {
                new_config.parse_xcfg_config(&xcfg);
            }
            if let Some(ref xcfg) = old_config.get_item_xcfg(item_name) {
                new_config.parse_xcfg_config(xcfg);
            };
            if let Some(xcfg) = post_xcfg {
                new_config.parse_xcfg_config(&xcfg);
            }
            new_config
        };
        self.stack.push(new_config);
        self.last()
    }

    #[allow(dead_code)]
    pub fn pop(&mut self) -> Option<ScopeConfig> {
        self.stack.pop()
    }

    pub fn pop_multi(&mut self, cnt: usize) -> Vec<ScopeConfig> {
        assert!(cnt <= self.stack.len(), "too many items to pop from stack!");
        let new_len = self.stack.len() - cnt;
        self.stack.split_off(new_len)
    }

    pub fn last(&self) -> &ScopeConfig {
        self.stack.last().unwrap()
    }

    pub fn last_mut(&mut self) -> &mut ScopeConfig {
        self.stack.last_mut().unwrap()
    }
}
