use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;
use std::iter::FromIterator;

struct Scope<T> {
    name_map: HashMap<T, String>,
    used: HashSet<String>,
}

impl<T: Clone + Eq + Hash> Scope<T> {
    pub fn new() -> Self {
        Self::new_with_reserved(HashSet::new())
    }

    pub fn new_with_reserved(reserved: HashSet<String>) -> Self {
        Scope {
            name_map: HashMap::new(),
            used: reserved,
        }
    }

    pub fn insert(&mut self, key: T, val: String) {
        self.name_map.insert(key, val);
    }

    pub fn contains_key(&self, key: &T) -> bool {
        self.name_map.contains_key(key)
    }

    pub fn contains_value(&self, val: &str) -> bool {
        self.used.contains(val)
    }

    pub fn reserve(&mut self, val: String) {
        self.used.insert(val);
    }
}

pub struct Renamer<T> {
    scopes: Vec<Scope<T>>,
    next_fresh: u64,
}

impl<T: Clone + Eq + Hash> Renamer<T> {
    /// Creates a new renaming environment with a single, empty scope. The given set of
    /// reserved names will exclude those names from being chosen as the mangled names from
    /// the insert method.
    pub fn new(reserved_names: &[&str]) -> Self {
        let set: HashSet<String> = HashSet::from_iter(reserved_names.iter().map(|&x| x.to_owned()));
        Renamer {
            scopes: vec![Scope::new_with_reserved(set)],
            next_fresh: 0,
        }
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
    fn is_target_used(&self, key: &str) -> bool {
        let key = key.to_string();

        self.scopes.iter().any(|x| x.contains_value(&key))
    }

    /// Assigns a name that doesn't collide with anything in the context of a particular
    /// scope, defaulting to the current scope if None is provided
    fn pick_name_in_scope(&mut self, basename: &str, scope: Option<usize>) -> String {
        let mut target = basename.to_string();

        for i in 0.. {
            if self.is_target_used(&target) {
                target = format!("{basename}_{i}");
            } else {
                break;
            }
        }

        match scope {
            Some(scope_index) => self.scopes[scope_index].reserve(target.clone()),
            None => self.current_scope_mut().reserve(target.clone()),
        }

        target
    }

    pub fn pick_name(&mut self, basename: &str) -> String {
        self.pick_name_in_scope(basename, None)
    }

    /// Permanently assign a name that doesn't collide with anything
    /// currently in scope, and also never goes out of scope
    pub fn pick_name_root(&mut self, basename: &str) -> String {
        self.pick_name_in_scope(basename, Some(0))
    }

    /// Introduce a new name binding into a particular scope or the current one if None is provided.
    /// If the key is unbound in the scope then Some of the resulting mangled name is returned,
    /// otherwise None.
    fn insert_in_scope(&mut self, key: T, basename: &str, scope: Option<usize>) -> Option<String> {
        let contains_key = match scope {
            Some(scope_index) => self.scopes[scope_index].contains_key(&key),
            None => self.current_scope().contains_key(&key),
        };

        if contains_key {
            return None;
        }

        let target = self.pick_name_in_scope(basename, scope);

        match scope {
            Some(scope_index) => self.scopes[scope_index].insert(key, target.clone()),
            None => self.current_scope_mut().insert(key, target.clone()),
        }

        Some(target)
    }

    /// Introduce a new name binding into the current scope. If the key is unbound in
    /// the current scope then Some of the resulting mangled name is returned, otherwise
    /// None.
    pub fn insert(&mut self, key: T, basename: &str) -> Option<String> {
        self.insert_in_scope(key, basename, None)
    }

    /// Introduce a new name binding into the root scope. If the key is unbound in
    /// the root scope then Some of the resulting mangled name is returned, otherwise
    /// None.
    pub fn insert_root(&mut self, key: T, basename: &str) -> Option<String> {
        self.insert_in_scope(key, basename, Some(0))
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

    pub fn fresh(&mut self) -> String {
        let fresh = self.next_fresh;
        self.next_fresh += 1;
        self.pick_name(&format!("fresh{fresh}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple() {
        let mut renamer = Renamer::new(&["reserved"]);

        let one1 = renamer.insert(1, "one").unwrap();
        let one2 = renamer.get(&1).unwrap();
        assert_eq!(one1, one2);

        let reserved1 = renamer.insert(2, "reserved").unwrap();
        let reserved2 = renamer.get(&2).unwrap();
        assert_eq!(reserved1, "reserved_0");
        assert_eq!(reserved2, "reserved_0");
    }

    #[test]
    fn scoped() {
        let mut renamer = Renamer::new(&[]);

        let one1 = renamer.insert(10, "one").unwrap();
        renamer.add_scope();

        let one2 = renamer.get(&10).unwrap();
        assert_eq!(one1, one2);

        let one3 = renamer.insert(20, "one").unwrap();
        let one4 = renamer.get(&20).unwrap();
        assert_eq!(one3, one4);
        assert_ne!(one3, one2);

        renamer.drop_scope();

        let one5 = renamer.get(&10).unwrap();
        assert_eq!(one5, one2);
    }

    #[test]
    fn forgets() {
        let mut renamer = Renamer::new(&[]);
        assert_eq!(renamer.get(&1), None);
        renamer.add_scope();
        renamer.insert(1, "example");
        renamer.drop_scope();
        assert_eq!(renamer.get(&1), None);
    }
}
