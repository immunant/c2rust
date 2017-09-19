use std::collections::HashSet;
use std::hash::Hash;
use bimap::BiMap;
use bimap::Overwritten;

type Scope<T> = BiMap<T, String>;

struct Renamer<T> {
    scopes: Vec<Scope<T>>,
    reserved_names: HashSet<String>,
}

impl<T: ToNameString + Eq + Hash> Renamer<T> {

    /// Creates a new renaming environment with a single, empty scope. The given set of
    /// reserved names will exclude those names from being chosen as the mangled names from
    /// the insert method.
    pub fn new(reserved_names: HashSet<String>) -> Renamer<T> {
        Renamer {
            scopes: vec![BiMap::new()],
            reserved_names,
        }
    }

    /// Introduces a new name binding scope
    pub fn add_scope(&mut self) {
        self.scopes.push(BiMap::new())
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

    fn is_target_used(&self, key: &str) -> bool {
        let key = key.to_string();

        let is_reserved = self.reserved_names.contains(&key);

        is_reserved
            || self.scopes.iter().any(|x| x.get_by_right(&key).is_some())
    }

    /// Introduce a new name binding into the current scope. If the key is unbound in
    /// the current scope then Some of the resulting mangled name is returned, otherwise
    /// None.
    pub fn insert(&mut self, key: T) -> Option<String> {

        if self.current_scope().get_by_left(&key).is_some() {
            return None
        }

        let target0 = key.to_name();
        let mut target = target0.clone();

        for i in 0.. {
            if self.is_target_used(&target) {
                target = format!("{}_{}", target0, i);
            } else {
                break
            }
        }

        if self.current_scope_mut().insert(key, target.clone()).did_overwrite() {
            panic!("Unexpected overwriting occurred")
        }

        Some(target)
    }

    /// Lookup the given key in all of the scopes returning Some of the matched mangled name
    /// if one exists, otherwise None.
    pub fn get(&self, key: T) -> Option<String> {
        for scope in self.scopes.iter().rev() {
            if let Some(target) = scope.get_by_left(&key) {
                return Some(target.to_string())
            }
        }
        None
    }
}

trait ToNameString {
    fn to_name(&self) -> String;
}

impl<'a> ToNameString for &'a str {
    fn to_name(&self) -> String {
        self.to_string()
    }
}

impl ToNameString for str {
    fn to_name(&self) -> String {
        self.to_string()
    }
}

impl ToNameString for String {
    fn to_name(&self) -> String {
        self.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple() {
        let keywords = vec!["reserved"].into_iter().map(str::to_string).collect();
        let mut renamer = Renamer::new(keywords);

        let one1 = renamer.insert("one").unwrap();
        let one2 = renamer.get("one").unwrap();
        assert_eq!(one1, one2);

        let reserved1 = renamer.insert("reserved").unwrap();
        let reserved2 = renamer.get("reserved").unwrap();
        assert_eq!(reserved1, "reserved_0");
        assert_eq!(reserved2, "reserved_0");
    }

    #[test]
    fn scoped() {
        let mut renamer = Renamer::new(HashSet::new());

        let one1 = renamer.insert("one").unwrap();
        renamer.add_scope();

        let one2 = renamer.get("one").unwrap();
        assert_eq!(one1, one2);

        let one3 = renamer.insert("one").unwrap();
        let one4 = renamer.get("one").unwrap();
        assert_eq!(one3, one4);
        assert_ne!(one3, one2);

        renamer.drop_scope();

        let one5 = renamer.get("one").unwrap();
        assert_eq!(one5, one2);
    }

    #[test]
    fn forgets() {
        let mut renamer = Renamer::new(HashSet::new());
        assert_eq!(renamer.get("example"), None);
        renamer.add_scope();
        renamer.insert("example");
        renamer.drop_scope();
        assert_eq!(renamer.get("example"), None);
    }
}