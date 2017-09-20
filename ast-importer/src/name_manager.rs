use renamer::Renamer;
use std::collections::HashMap;

pub struct NameManager {
    types: Renamer<u64>,
    idents: Renamer<u64>,
    fields: HashMap<u64, Renamer<u64>>,
}