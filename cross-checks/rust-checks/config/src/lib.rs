
#[macro_use]
extern crate serde_derive;

extern crate serde;
extern crate serde_yaml;

use std::collections::HashMap;

#[derive(Deserialize, Debug, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum XCheckBasicType {
    Default,
    Skip,
}

#[derive(Deserialize, Debug, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum XCheckComplexType {
    // Basic types
    // FIXME: ideally, we'd extend or include XCheckBasicType here
    Default,
    Skip,

    // Complex types (which use other fields from TypeInfo below)
    Fixed(u64),
    Djb2(String),
}

#[derive(Deserialize, Debug)]
#[serde(untagged)]
pub enum XCheckType {
    Basic(XCheckBasicType),
    Complex(XCheckComplexType),
}

impl Default for XCheckType {
    fn default() -> XCheckType {
        XCheckType::Basic(XCheckBasicType::Default)
    }
}

#[derive(Deserialize, Debug, Default)]
#[serde(default)]
pub struct FunctionConfig {
    // Name of the function
    // FIXME: where do we get this???
    name: String,

    // Overrides for the attribute config items
    no_xchecks: bool,

    // How to cross-check function entry and exit
    entry: XCheckType,
    exit: XCheckType,

    // How to cross-check each argument
    args: HashMap<String, XCheckType>,

    // How to cross-check the return value
    #[serde(rename = "return")]
    ret: XCheckType,

    // Nested items
    nested: Option<ItemList>,
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
    items: &'a ItemList,
    pub name_map: HashMap<&'a str, &'a ItemConfig>,
}

impl<'a> NamedItemList<'a> {
    pub fn new(items: &'a ItemList) -> NamedItemList<'a> {
        let map = items.0.iter()
            .filter_map(|item| item.name().map(|name| (name, item)))
            .collect();
        NamedItemList {
            items: items,
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
        assert_eq!(parse_test_yaml::<XCheckBasicType>("default"),
                   XCheckBasicType::Default);
        assert_eq!(parse_test_yaml::<XCheckComplexType>("type: skip").ty,
                   XCheckBasicType::Skip);
        assert_eq!(parse_test_yaml::<XCheckComplexType>("{ \"type\": \"default\" }").ty,
                   XCheckBasicType::Default);

        // TODO: test XCheckComplexType
    }

    #[test]
    fn test_function() {
        // TODO
    }
}
