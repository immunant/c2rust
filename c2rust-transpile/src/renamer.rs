use std::collections::HashMap;
use std::hash::Hash;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign};

struct Scope<T> {
    name_map: HashMap<T, String>,
    used: HashMap<String, Namespaces>,
}

impl<T: Clone + Eq + Hash> Scope<T> {
    pub fn new() -> Self {
        Scope {
            name_map: HashMap::new(),
            used: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: T, val: String) {
        self.name_map.insert(key, val);
    }

    pub fn contains_key(&self, key: &T) -> bool {
        self.name_map.contains_key(key)
    }

    pub fn contains_value(&self, val: &str, ns: Namespaces) -> bool {
        self.used
            .get(val)
            .is_some_and(|val_ns| val_ns.intersects(ns))
    }

    pub fn reserve(&mut self, val: &str, ns: Namespaces) {
        *self.used.entry(val.to_owned()).or_default() |= ns;
    }
}

// Keywords obtained from https://doc.rust-lang.org/reference/keywords.html
#[rustfmt::skip] // Preserve one keyword per line.
pub const RUST_KEYWORDS: &[&str] = &[
    // Strict keywords
    "as",
    "async", // 2018 Edition
    "await", // 2018 Edition
    "break",
    "const",
    "continue",
    "crate",
    "dyn", // 2018 Edition
    "else",
    "enum",
    "extern",
    "false",
    "fn",
    "for",
    "if",
    "impl",
    "in",
    "let",
    "loop",
    "match",
    "mod",
    "move",
    "mut",
    "pub",
    "ref",
    "return",
    "self",
    "Self",
    "static",
    "struct",
    "super",
    "trait",
    "true",
    "type",
    "unsafe",
    "use",
    "where",
    "while",
    // Reserved keywords
    "abstract",
    "become",
    "box",
    "do",
    "final",
    "gen", // 2024 Edition
    "macro",
    "override",
    "priv",
    "try", // 2018 Edition
    "typeof",
    "unsized",
    "virtual",
    "yield",
];

/// Keywords that cannot be expressed as a raw identifier [0] because they can be used as path
/// segments. The list of path segment keywords can be found here [1]. For discussion of this
/// topic see [2].
/// [0] https://doc.rust-lang.org/edition-guide/rust-2018/module-system/raw-identifiers.html
/// [1] https://github.com/rust-lang/rust/blob/e96c36b6f76833388c519561d145492d2c08db4e/compiler/rustc_span/src/symbol.rs#L2892
/// [2] https://internals.rust-lang.org/t/raw-identifiers-dont-work-for-all-identifiers/9094
#[rustfmt::skip]
const PATH_SEGMENT_KEYWORDS: [&str; 4] = [
    "super",
    "self",
    "Self",
    "crate",
];

const PRELUDE_TYPE_NAMESPACE: &[&str] = &[
    "Copy",
    "Send",
    "Sized",
    "Sync",
    "Drop",
    "Fn",
    "FnMut",
    "FnOnce",
    "Box",
    "ToOwned",
    "Clone",
    "PartialEq",
    "PartialOrd",
    "Eq",
    "Ord",
    "AsRef",
    "AsMut",
    "Into",
    "From",
    "Default",
    "Iterator",
    "Extend",
    "IntoIterator",
    "DoubleEndedIterator",
    "ExactSizeIterator",
    "Option",
    "Result",
    "SliceConcatExt",
    "String",
    "ToString",
    "Vec",
    "bool",
    "char",
    "f32",
    "f64",
    "i8",
    "i16",
    "i32",
    "i64",
    "i128",
    "isize",
    "u8",
    "u16",
    "u32",
    "u64",
    "u128",
    "usize",
    "str",
];

#[rustfmt::skip] // Preserve one symbol per line.
const PRELUDE_VALUE_NAMESPACE: &[&str] = &[
    "drop",
    "Some",
    "None",
    "Ok",
    "Err",
];

pub struct Renamer<T> {
    scopes: Vec<Scope<T>>,
    next_fresh: u64,
}

impl<T: Clone + Eq + Hash> Renamer<T> {
    /// Creates a new renaming environment with a single, empty scope. The given set of
    /// reserved names will exclude those names from being chosen as the mangled names from
    /// the insert method.
    pub fn new<'a>(reserved_names: impl IntoIterator<Item = (&'a str, Namespaces)>) -> Self {
        let mut global_scope = Scope::new();

        for (name, ns) in reserved_names {
            global_scope.reserve(name, ns);
        }

        Renamer {
            scopes: vec![global_scope],
            next_fresh: 0,
        }
    }

    pub fn keywords() -> Self {
        let keywords = RUST_KEYWORDS.iter().map(|&name| (name, Namespaces::all()));
        Renamer::new(keywords)
    }

    pub fn keywords_and_prelude() -> Self {
        let keywords = RUST_KEYWORDS.iter().map(|&name| (name, Namespaces::all()));
        let prelude_types = PRELUDE_TYPE_NAMESPACE
            .iter()
            .map(|&name| (name, Namespaces::types()));
        let prelude_values = PRELUDE_VALUE_NAMESPACE
            .iter()
            .map(|&name| (name, Namespaces::values()));
        // TODO: Do not include "main" for most renamings.
        let main = [("main", Namespaces::values())];
        Renamer::new(
            keywords
                .chain(prelude_types)
                .chain(prelude_values)
                .chain(main),
        )
    }

    /// Introduces a new name binding scope
    pub fn add_scope(&mut self) {
        self.scopes.push(Scope::new())
    }

    /// Drops the current name binding scope
    pub fn drop_scope(&mut self) {
        if self.scopes.len() == 1 {
            panic!("Attempting to drop outermost scope")
        }

        self.scopes.pop();
    }

    fn current_scope(&self) -> &Scope<T> {
        self.scopes.last().expect("Expected a scope")
    }

    fn current_scope_mut(&mut self) -> &mut Scope<T> {
        self.scopes.last_mut().expect("Expected a scope")
    }

    /// Is the mangled name currently in use
    fn is_target_used(&self, key: &str, ns: Namespaces) -> bool {
        self.scopes.iter().any(|x| x.contains_value(key, ns))
    }

    /// Assigns a name that doesn't collide with anything in the context of a particular
    /// scope, defaulting to the current scope if None is provided
    fn pick_name_in_scope(
        &mut self,
        basename: &str,
        ns: Namespaces,
        scope: Option<usize>,
    ) -> String {
        let mut target =
            Self::raw_identifier_if_reserved_name(basename).unwrap_or_else(|| basename.to_string());

        for i in 0.. {
            if self.is_target_used(&target, ns) {
                target = format!("{}_{}", basename, i);
            } else {
                break;
            }
        }

        match scope {
            Some(scope_index) => self.scopes[scope_index].reserve(&target, ns),
            None => self.current_scope_mut().reserve(&target, ns),
        }

        target
    }

    pub fn pick_name(&mut self, basename: &str, ns: Namespaces) -> String {
        check_c2rust_name(basename);
        self.pick_name_in_scope(basename, ns, None)
    }

    /// Permanently assign a name that doesn't collide with anything
    /// currently in scope, and also never goes out of scope
    pub fn pick_name_root(&mut self, basename: &str, ns: Namespaces) -> String {
        check_c2rust_name(basename);
        self.pick_name_in_scope(basename, ns, Some(0))
    }

    /// Introduce a new name binding into a particular scope or the current one if None is provided.
    /// If the key is unbound in the scope then Some of the resulting mangled name is returned,
    /// otherwise None.
    fn insert_in_scope(
        &mut self,
        key: T,
        basename: &str,
        ns: Namespaces,
        scope: Option<usize>,
    ) -> Option<String> {
        let contains_key = match scope {
            Some(scope_index) => self.scopes[scope_index].contains_key(&key),
            None => self.current_scope().contains_key(&key),
        };

        if contains_key {
            return None;
        }

        let target = self.pick_name_in_scope(basename, ns, scope);

        match scope {
            Some(scope_index) => self.scopes[scope_index].insert(key, target.clone()),
            None => self.current_scope_mut().insert(key, target.clone()),
        }

        Some(target)
    }

    /// Introduce a new name binding into the current scope. If the key is unbound in
    /// the current scope then Some of the resulting mangled name is returned, otherwise
    /// None.
    pub fn insert(&mut self, key: T, basename: &str, ns: Namespaces) -> Option<String> {
        self.insert_in_scope(key, basename, ns, None)
    }

    /// Introduce a new name binding into the root scope. If the key is unbound in
    /// the root scope then Some of the resulting mangled name is returned, otherwise
    /// None.
    pub fn insert_root(&mut self, key: T, basename: &str, ns: Namespaces) -> Option<String> {
        self.insert_in_scope(key, basename, ns, Some(0))
    }

    /// Assign a name in the current scope without reservation or checking for overlap.
    /// This is intended to be used when one key is going to be merged
    pub fn alias(&mut self, new_key: T, old_key: &T) {
        match self.get(old_key) {
            Some(name) => self.current_scope_mut().insert(new_key, name),
            None => panic!("Failed to overlap name"),
        }
    }

    /// Lookup the given key in all of the scopes returning Some of the matched mangled name
    /// if one exists, otherwise None.
    pub fn get(&self, key: &T) -> Option<String> {
        for scope in self.scopes.iter().rev() {
            if let Some(target) = scope.name_map.get(key) {
                return Some(target.to_string());
            }
        }
        None
    }

    pub fn fresh(&mut self, ns: Namespaces) -> String {
        let fresh = self.next_fresh;
        self.next_fresh += 1;
        self.pick_name(&format!("c2rust_fresh{fresh}"), ns)
    }

    fn raw_identifier_if_reserved_name(basename: &str) -> Option<String> {
        if RUST_KEYWORDS.contains(&basename) && !PATH_SEGMENT_KEYWORDS.contains(&basename) {
            Some(format!("r#{}", basename))
        } else {
            None
        }
    }
}

#[derive(Clone, Copy, Default, PartialEq, Eq)]
pub struct Namespaces {
    pub types: bool,
    pub values: bool,
}

impl Namespaces {
    pub fn none() -> Self {
        Default::default()
    }

    pub fn all() -> Self {
        Self {
            types: true,
            values: true,
        }
    }

    pub fn types() -> Self {
        Self {
            types: true,
            ..Default::default()
        }
    }

    pub fn values() -> Self {
        Self {
            values: true,
            ..Default::default()
        }
    }

    pub fn is_empty(self) -> bool {
        self == Self::default()
    }

    pub fn intersects(self, other: Self) -> bool {
        !(self & other).is_empty()
    }
}

impl BitAnd for Namespaces {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self {
            types: self.types && rhs.types,
            values: self.values && rhs.values,
        }
    }
}

impl BitAndAssign for Namespaces {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}

impl BitOr for Namespaces {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self {
            types: self.types || rhs.types,
            values: self.values || rhs.values,
        }
    }
}

impl BitOrAssign for Namespaces {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

fn check_c2rust_name(basename: &str) {
    assert!(basename.starts_with("c2rust_") || basename.starts_with("C2Rust_"));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple() {
        let mut renamer = Renamer::new([("reserved", Namespaces::all())]);

        let one1 = renamer.insert(1, "one", Namespaces::all()).unwrap();
        let one2 = renamer.get(&1).unwrap();
        assert_eq!(one1, one2);

        let reserved1 = renamer.insert(2, "reserved", Namespaces::all()).unwrap();
        let reserved2 = renamer.get(&2).unwrap();
        assert_eq!(reserved1, "reserved_0");
        assert_eq!(reserved2, "reserved_0");
    }

    #[test]
    fn scoped() {
        let mut renamer = Renamer::new([]);

        let one1 = renamer.insert(10, "one", Namespaces::all()).unwrap();
        renamer.add_scope();

        let one2 = renamer.get(&10).unwrap();
        assert_eq!(one1, one2);

        let one3 = renamer.insert(20, "one", Namespaces::all()).unwrap();
        let one4 = renamer.get(&20).unwrap();
        assert_eq!(one3, one4);
        assert_ne!(one3, one2);

        renamer.drop_scope();

        let one5 = renamer.get(&10).unwrap();
        assert_eq!(one5, one2);
    }

    #[test]
    fn forgets() {
        let mut renamer = Renamer::new([]);
        assert_eq!(renamer.get(&1), None);
        renamer.add_scope();
        renamer.insert(1, "example", Namespaces::all());
        renamer.drop_scope();
        assert_eq!(renamer.get(&1), None);
    }

    #[test]
    fn raw_identifier() {
        let mut renamer = Renamer::new(RUST_KEYWORDS.iter().map(|&name| (name, Namespaces::all())));

        // A reserved keyword that can be expressed as a raw identifier
        let reserved1 = renamer.insert(1, "dyn", Namespaces::all()).unwrap();
        let reserved2 = renamer.get(&1).unwrap();
        assert_eq!(reserved1, "r#dyn");
        assert_eq!(reserved2, "r#dyn");

        // A reserved keyword that is already bound and therefore does not need the "#r" prefix
        let reserved1 = renamer.insert(2, "dyn", Namespaces::all()).unwrap();
        let reserved2 = renamer.get(&2).unwrap();
        assert_eq!(reserved1, "dyn_0");
        assert_eq!(reserved2, "dyn_0");

        // A reserved that cannot be used as a raw identifier because it can be used in a path
        let reserved1 = renamer.insert(3, "self", Namespaces::all()).unwrap();
        let reserved2 = renamer.get(&3).unwrap();
        assert_eq!(reserved1, "self_0");
        assert_eq!(reserved2, "self_0");
    }
}
