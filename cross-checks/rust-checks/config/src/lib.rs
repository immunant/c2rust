
#[macro_use]
extern crate serde_derive;

extern crate serde;
extern crate serde_yaml;

use std::collections::HashMap;

#[derive(Deserialize, Debug, PartialEq, Clone)]
#[serde(rename_all = "lowercase")]
pub enum XCheckType {
    // Basic types
    Default,
    Skip,

    // Types with additional parameters
    Fixed(u64),
    Djb2(String),

    // Compute the cross-check value from an arbitrary Rust expression
    Custom(String),
}

impl Default for XCheckType {
    fn default() -> XCheckType {
        XCheckType::Default
    }
}

#[derive(Deserialize, Debug, Default)]
#[serde(default)]
pub struct FunctionConfig {
    // Name of the function
    // FIXME: where do we get this???
    pub name: String,

    // Overrides for the attribute config items
    pub no_xchecks: Option<bool>,

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
    nested: Option<ItemList>,
}

impl FunctionConfig {
    // Create a copy of the current config, but without
    // the metadata for nested functions, e.g., the "nested" field
    pub fn clone_current(&self) -> FunctionConfig {
        FunctionConfig {
            name: self.name.clone(),
            no_xchecks: self.no_xchecks,
            entry: self.entry.clone(),
            exit: self.exit.clone(),
            all_args: self.all_args.clone(),
            args: self.args.clone(),
            ret: self.ret.clone(),
            ahasher: self.ahasher.clone(),
            shasher: self.shasher.clone(),
            nested: Default::default(),
        }
    }
}

#[derive(Deserialize, Debug)]
#[serde(tag = "item", rename_all = "lowercase")]
pub enum ItemConfig {
    Function(FunctionConfig),
    Struct,  // TODO
    Value,   // TODO
    Closure, // TODO
}

impl ItemConfig {
    fn name(&self) -> Option<&str> {
        match *self {
            ItemConfig::Function(FunctionConfig { ref name, .. }) => Some(&name[..]),
            _ => None
        }
    }

    pub fn nested_items(&self) -> Option<&ItemList> {
        match *self {
            ItemConfig::Function(FunctionConfig { ref nested, .. }) => nested.as_ref(),
            // TODO: other cases
            _ => None
        }
    }
}

#[derive(Deserialize, Debug, Default)]
pub struct ItemList(Vec<ItemConfig>);

pub struct NamedItemList<'a> {
    // FIXME: _items is unused; do we really need it???
    _items: &'a ItemList,
    pub name_map: HashMap<&'a str, &'a ItemConfig>,
}

impl<'a> NamedItemList<'a> {
    pub fn new(items: &'a ItemList) -> NamedItemList<'a> {
        let map = items.0.iter()
            .filter_map(|item| item.name().map(|name| (name, item)))
            .collect();
        NamedItemList {
            _items: items,
            name_map: map,
        }
    }
}

#[derive(Deserialize, Debug, Default)]
pub struct FileConfig(ItemList);

#[derive(Deserialize, Debug, Default)]
pub struct Config(HashMap<String, FileConfig>);

impl Config {
    pub fn get_file_config(&self, file: &str) -> Option<&FileConfig> {
        self.0.get(file)
    }

    pub fn get_file_items(&self, file: &str) -> Option<&ItemList> {
        self.get_file_config(file).map(|fc| &fc.0)
    }

    pub fn merge(mut self, other: Self) -> Self {
        for (file_name, cfg) in other.0.into_iter() {
            // FIXME: check for duplicates???
            (self.0.entry(file_name.clone())
                   .or_insert(Default::default())
                   .0).0.extend((cfg.0).0);
        }
        self
    }
}

pub fn parse_string(s: &str) -> Result<Config, String> {
    serde_yaml::from_str(s).map_err(|e| format!("serde_yaml error: {}", e))
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
        assert_eq!(parse_test_yaml::<XCheckType>("skip"),
                   XCheckType::Skip);
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
