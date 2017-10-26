
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
    name: Option<String>,

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
