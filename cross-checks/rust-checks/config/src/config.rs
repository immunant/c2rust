
#[derive(Deserialize, Debug, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum XCheckBasicType {
    Default,
    Skip,
}

#[derive(Deserialize, Debug)]
pub struct XCheckComplexType {
    #[serde(rename = "type")]
    ty: XCheckBasicType,
}

#[derive(Deserialize, Debug)]
#[serde(untagged)]
pub enum XCheckType {
    Basic(XCheckBasicType),
    Complex(XCheckComplexType),
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
}
