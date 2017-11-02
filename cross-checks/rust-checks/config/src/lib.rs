
#[macro_use]
extern crate serde_derive;

extern crate serde;
extern crate serde_yaml;

use std::cell::{RefCell, Ref};
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
    Fixed,
}

#[derive(Deserialize, Debug)]
pub struct XCheckComplexTypeInfo {
    #[serde(rename = "type")]
    ty: XCheckComplexType,

    #[serde(default)]
    id: Option<u64>,
}

#[derive(Deserialize, Debug)]
#[serde(untagged)]
pub enum XCheckType {
    Basic(XCheckBasicType),
    Complex(XCheckComplexTypeInfo),
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
    xcheck_name: Option<String>,

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
#[serde(tag = "item")]
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
}

#[derive(Deserialize, Debug)]
pub struct ItemList(Vec<ItemConfig>);

pub struct NamedItemList<'a> {
    items: ItemList,
    name_map: RefCell<Option<HashMap<&'a str, &'a ItemConfig>>>,
}

impl<'a> NamedItemList<'a> {
    fn from(items: ItemList) -> NamedItemList<'a> {
        NamedItemList {
            items: items,
            name_map: RefCell::new(None),
        }
    }

    fn get_map(&'a self) -> Ref<HashMap<&'a str, &'a ItemConfig>> {
        if self.name_map.borrow().is_none() {
            let map = self.items.0.iter()
                .filter_map(|item| item.name().map(|name| (name, item)))
                .collect();
            *self.name_map.borrow_mut() = Some(map);
        }
        Ref::map(self.name_map.borrow(), |opt| opt.as_ref().unwrap())
    }
}

#[derive(Deserialize, Debug)]
pub struct FileConfig(ItemList);

#[derive(Deserialize, Debug)]
pub struct Config(HashMap<String, FileConfig>);

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
