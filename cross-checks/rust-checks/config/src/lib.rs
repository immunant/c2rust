#![cfg_attr(feature="parse-syntax", feature(rustc_private, try_from))]

#[macro_use]
extern crate serde_derive;

extern crate serde;
extern crate serde_yaml;

extern crate globset;

pub mod attr;
#[cfg(feature="scopes")] pub mod scopes;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Deserialize, Debug, PartialEq, Clone)]
#[serde(rename_all = "snake_case")]
pub enum XCheckType {
    // Basic types
    Default,

    // FIXME: these are aliases of each other and should really
    // only be one value, but I'm not sure how to make serde parse that
    None,
    Disabled,

    // Types with additional parameters
    Fixed(u64),
    Djb2(String),

    // Hash using the default hash for another type
    AsType(String),

    // Compute the cross-check value from an arbitrary Rust expression
    Custom(String),
}

impl XCheckType {
    pub fn is_disabled(&self) -> bool {
        match *self {
            XCheckType::None |
            XCheckType::Disabled => true,
            _ => false
        }
    }
}

impl Default for XCheckType {
    fn default() -> XCheckType {
        XCheckType::Default
    }
}

#[derive(Deserialize, Debug, Clone)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum XCheckTag {
    Unknown,
    FunctionEntry,
    FunctionExit,
    FunctionArg,
    FunctionReturn,
}

impl Default for XCheckTag {
    fn default() -> XCheckTag {
        XCheckTag::Unknown
    }
}

#[derive(Deserialize, Debug, Default, Clone)]
pub struct ExtraXCheck {
    #[serde(default)]
    pub tag: XCheckTag,

    pub custom: String,
}

#[derive(Deserialize, Debug, Default, Clone)]
#[serde(default)]
pub struct DefaultsConfig {
    pub disable_xchecks: Option<bool>,

    pub entry: Option<XCheckType>,
    pub exit: Option<XCheckType>,

    pub all_args: Option<XCheckType>,

    #[serde(rename = "return")]
    pub ret: Option<XCheckType>,
}

impl DefaultsConfig {
    pub fn merge(&mut self, other: &DefaultsConfig) {
        macro_rules! update_field {
            ($field:ident) => {
                if other.$field.is_some() {
                    self.$field = other.$field.clone();
                }
            }
        };
        update_field!(disable_xchecks);
        update_field!(entry);
        update_field!(exit);
        update_field!(all_args);
        update_field!(ret);
    }
}

#[derive(Deserialize, Debug, Default, Clone)]
#[serde(default)]
pub struct FunctionConfig {
    // Name of the function
    // FIXME: where do we get this???
    pub name: String,

    // Overrides for the attribute config items
    pub disable_xchecks: Option<bool>,

    // How to cross-check function entry and exit
    pub entry: Option<XCheckType>,
    pub exit: Option<XCheckType>,

    // The default check for all arguments not in "args"
    pub all_args: Option<XCheckType>,
    // How to cross-check each argument
    pub args: HashMap<String, XCheckType>,

    // How to cross-check the return value
    #[serde(rename = "return")]
    pub ret: Option<XCheckType>,

    // Overrides for the aggregate/simple hashers
    pub ahasher: Option<String>,
    pub shasher: Option<String>,

    // Nested items
    pub nested: Option<ItemList>,

    // Extra cross-checks
    pub entry_extra: Vec<ExtraXCheck>,
    pub exit_extra: Vec<ExtraXCheck>,
}

impl FunctionConfig {
    // Create a copy of the current config, but without
    // the metadata for nested functions, e.g., the "nested" field
    pub fn clone_current(&self) -> FunctionConfig {
        FunctionConfig {
            name: self.name.clone(),
            disable_xchecks: self.disable_xchecks,
            entry: self.entry.clone(),
            exit: self.exit.clone(),
            all_args: self.all_args.clone(),
            args: self.args.clone(),
            ret: self.ret.clone(),
            ahasher: self.ahasher.clone(),
            shasher: self.shasher.clone(),
            nested: Default::default(),
            entry_extra: self.entry_extra.clone(),
            exit_extra: self.exit_extra.clone(),
        }
    }
}

// Index of a structure/aggregate field
// Can be an integer (for tuples) or a string (for structures)
#[derive(Deserialize, Debug, Clone, PartialEq, Eq, Hash)]
#[serde(untagged)]
pub enum FieldIndex {
    Int(usize),
    Str(String),
}

impl FieldIndex {
    pub fn from_str(s: &str) -> FieldIndex {
        FieldIndex::Str(String::from(s))
    }
}

#[derive(Deserialize, Debug, Clone, Copy)]
#[serde(rename_all = "snake_case")]
pub enum CustomHashFormat {
    Function,
    Expression,
    Extern,
}

impl CustomHashFormat {
    pub fn from_str(s: &str) -> CustomHashFormat {
        match s {
            "function"   => CustomHashFormat::Function,
            "expression" => CustomHashFormat::Expression,
            "extern"     => CustomHashFormat::Extern,
            _ => panic!("unexpected custom_hash_format: {}", s)
        }
    }
}

#[derive(Deserialize, Debug, Default, Clone)]
#[serde(default)]
pub struct StructConfig {
    pub name: String,

    // Overrides for the attribute config items
    pub disable_xchecks: Option<bool>,

    // Overrides for ahasher/shasher
    pub ahasher: Option<String>,
    pub shasher: Option<String>,

    // Replacement hasher for this structure
    pub field_hasher: Option<String>,

    // Custom hash function to call to hash this structure
    pub custom_hash: Option<String>,
    pub custom_hash_format: Option<CustomHashFormat>,

    pub fields: HashMap<FieldIndex, XCheckType>,

    // Nested items; in this context, it means
    // methods implemented in impl's
    pub nested: Option<ItemList>,
}

#[derive(Deserialize, Debug, Clone)]
#[serde(tag = "item", rename_all = "lowercase")]
pub enum ItemConfig {
    Defaults(DefaultsConfig),
    Function(FunctionConfig),
    Struct(StructConfig),
    Value,   // TODO
    Closure, // TODO
}

impl ItemConfig {
    fn name(&self) -> Option<&str> {
        match *self {
            ItemConfig::Function(FunctionConfig { ref name, .. }) => Some(&name[..]),
            ItemConfig::Struct(StructConfig { ref name, .. }) => Some(&name[..]),
            _ => None
        }
    }

    pub fn nested_items(&self) -> Option<&ItemList> {
        match *self {
            ItemConfig::Function(FunctionConfig { ref nested, .. }) => nested.as_ref(),
            ItemConfig::Struct(StructConfig { ref nested, .. }) => nested.as_ref(),
            // TODO: other cases
            _ => None
        }
    }
}

pub type ItemConfigRef = Rc<ItemConfig>;

#[derive(Deserialize, Debug, Default, Clone)]
pub struct ItemList(Vec<ItemConfigRef>);

impl ItemList {
    pub fn items(&self) -> &[ItemConfigRef] {
        &self.0[..]
    }
}

#[derive(Debug, Default, Clone)]
pub struct NamedItemList {
    // FIXME: _items is unused; do we really need it???
    pub name_map: HashMap<String, Vec<ItemConfigRef>>,
}

impl NamedItemList {
    pub fn new(items: &ItemList) -> NamedItemList {
        let mut map: HashMap<String, Vec<_>> = HashMap::new();
        for item in items.0.iter() {
            if let Some(item_name) = item.name().map(String::from) {
                map.entry(item_name)
                    .or_default()
                    .push(ItemConfigRef::clone(item));
            }
        }
        NamedItemList {
            name_map: map,
        }
    }

    pub fn extend(&mut self, other: NamedItemList) {
        self.name_map.extend(other.name_map.into_iter());
    }
}

#[derive(Deserialize, Debug, Default, Clone)]
pub struct FileConfig(ItemList);

#[derive(Deserialize, Debug, Default, Clone)]
pub struct ExtFileConfig {
    file: String,

    #[serde(default)]
    priority: isize,

    items: FileConfig,
}


#[derive(Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum RootConfig {
    /// Filename-to-config mapping
    NameMap(HashMap<String, FileConfig>),

    /// Vector of ExtFileConfig elements
    ExtVector(Vec<ExtFileConfig>),
}

impl Default for RootConfig {
    fn default() -> RootConfig {
        RootConfig::NameMap(Default::default())
    }
}

impl RootConfig {
    fn merge(self, other: Self) -> Self {
        match (self, other) {
            (RootConfig::NameMap(mut map_self), RootConfig::NameMap(map_other)) => {
                for (file_name, cfg) in map_other.into_iter() {
                    // FIXME: check for duplicates???
                    (map_self.entry(file_name.clone())
                           .or_insert_with(Default::default)
                           .0).0.extend((cfg.0).0);
                };
                RootConfig::NameMap(map_self)
            },
            (RootConfig::ExtVector(mut ev_self), RootConfig::ExtVector(ev_other)) => {
                ev_self.extend(ev_other.into_iter());
                RootConfig::ExtVector(ev_self)
            }
            p @ (_, _) => p.0.into_ext_vector().merge(p.1.into_ext_vector())
        }
    }

    fn into_ext_vector(self) -> Self {
        match self {
            RootConfig::NameMap(map_self) => {
                // Convert the NameMap into an ordered Vec
                // WARNING: the elements are emitted in random order
                RootConfig::ExtVector(map_self.into_iter()
                    .map(|(file, cfg)| ExtFileConfig {
                        file,
                        priority: 0,
                        items: cfg,
                    }).collect())
            },
            r @ RootConfig::ExtVector(_) => r
        }
    }
}

#[derive(Debug, Clone)]
pub struct Config {
    root: RootConfig,

    /// GlobSet used by ExtVector to match each source file
    glob_set: RefCell<globset::GlobSet>,
}

impl Default for Config {
    fn default() -> Config {
        let glob_set = globset::GlobSetBuilder::new().build().unwrap();
        Config {
            root: RootConfig::default(),
            glob_set: RefCell::new(glob_set),
        }
    }
}

impl Config {
    pub fn new(root: RootConfig) -> Config {
        Config {
            root,
            ..Default::default()
        }
    }

    fn rebuild_glob_set(&self, files: &[ExtFileConfig]) {
        if self.glob_set.borrow().len() == files.len() {
            // File vector hasn't changed
            return;
        }

        let mut gsb = globset::GlobSetBuilder::new();
        for file in files {
            let glob = globset::Glob::new(&file.file)
                .unwrap_or_else(|e| panic!("error creating glob for file '{}': {}", file.file, e));
            gsb.add(glob);
        }
        self.glob_set.replace(gsb.build().unwrap());
    }

    pub fn get_file_items(&self, file: &str) -> ItemList {
        match self.root {
            RootConfig::NameMap(ref m) => {
                m.get(file).map(|fc| fc.0.clone())
                    .unwrap_or_default()
            },
            RootConfig::ExtVector(ref files) => {
                self.rebuild_glob_set(files);
                // Return all items that match this file,
                // sorted by increasing file priority
                let mut items = self.glob_set.borrow()
                    .matches(file)
                    .into_iter()
                    .map(|idx| (files[idx].priority, idx))
                    .collect::<Vec<_>>();
                items.sort();
                let item_list = items.into_iter()
                    .flat_map(|(_, idx)| files[idx].items.0.items())
                    .map(ItemConfigRef::clone)
                    .collect();
                ItemList(item_list)
            }
        }
    }

    pub fn merge(self, other: Self) -> Self {
        Self {
            root: self.root.merge(other.root),
            ..Default::default()
        }
    }
}

#[derive(Debug)]
pub enum Error {
    YAML(serde_yaml::Error),
}

pub fn parse_string(s: &str) -> Result<Config, Error> {
    serde_yaml::from_str::<RootConfig>(s)
        .map_err(Error::YAML)
        .map(Config::new)
}

#[cfg(test)]
mod tests {
    use super::*;

    use serde;
    use serde_yaml;

    fn parse_test_yaml<T: serde::de::DeserializeOwned>(y: &str) -> T {
        let mut yaml_str = String::from("---\n");
        yaml_str.push_str(y);
        serde_yaml::from_str(&yaml_str).unwrap()
    }

    #[test]
    fn test_types() {
        assert_eq!(parse_test_yaml::<XCheckType>("default"),
                   XCheckType::Default);
        assert_eq!(parse_test_yaml::<XCheckType>("none"),
                   XCheckType::None);
        assert_eq!(parse_test_yaml::<XCheckType>("disabled"),
                   XCheckType::Disabled);
        assert_eq!(parse_test_yaml::<XCheckType>("{ \"fixed\": 1234 }"),
                   XCheckType::Fixed(1234));
        assert_eq!(parse_test_yaml::<XCheckType>("{ \"djb2\": \"foo\" }"),
                   XCheckType::Djb2(String::from("foo")));
    }

    #[test]
    fn test_function() {
        // TODO
    }
}
