//! Worklist algorithm for finding fixed points.
use log::info;
use std::cell::UnsafeCell;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::marker::PhantomData;
use std::mem;
use std::ops::Index;

/// Map of dependency relationships using internal mutability.
struct DepMap<K> {
    map: UnsafeCell<HashMap<K, HashSet<K>>>,
}

impl<K> DepMap<K>
where
    K: Hash + Eq + ::std::fmt::Debug,
{
    fn new() -> DepMap<K> {
        DepMap {
            map: UnsafeCell::new(HashMap::new()),
        }
    }

    /// Record a new dependency edge, pointing from `src` to `dest`.
    fn insert(&self, src: K, dest: K) {
        info!(" * record dep: {:?} -> {:?}", src, dest);
        unsafe {
            (*self.map.get())
                .entry(src)
                .or_insert_with(HashSet::new)
                .insert(dest);
        }
    }

    /// Iterate over the targets of edges starting at `key`.
    fn for_each<F: FnMut(&K)>(&self, key: &K, mut func: F) {
        unsafe {
            let map = &*self.map.get();
            if let Some(dests) = map.get(key) {
                for dest in dests {
                    func(dest);
                }
            }
        }
    }
}

/// Worklist algorithm context.  `K` is the type of keys, which identify nodes, and `V` is the type
/// of value associated with each node.  Allows indexing to retrieve data from other nodes.
/// Accessing the current node's data triggers a panic.
pub struct Ctxt<'a, K: 'a, V: 'a> {
    rev_deps: &'a DepMap<K>,
    data: &'a HashMap<K, UnsafeCell<V>>,
    _marker0: PhantomData<&'a mut HashMap<K, V>>,
    cur: K,
}

impl<'a, K, V> Ctxt<'a, K, V>
where
    K: Hash + Eq + ::std::fmt::Debug,
{
    fn new(
        rev_deps: &'a DepMap<K>,
        init: &'a mut HashMap<K, V>,
        cur: K,
    ) -> (Ctxt<'a, K, V>, &'a mut V) {
        unsafe {
            let data: &'a HashMap<K, UnsafeCell<V>> = mem::transmute(init);

            let cur_val = &mut *data[&cur].get();

            let ctxt = Ctxt {
                rev_deps,
                data,
                _marker0: PhantomData,
                cur,
            };

            (ctxt, cur_val)
        }
    }
}

impl<'a, K, V> Index<K> for Ctxt<'a, K, V>
where
    K: Hash + Eq + Clone + ::std::fmt::Debug,
{
    type Output = V;
    fn index(&self, key: K) -> &V {
        assert!(key != self.cur, "tried to access current node");
        self.rev_deps.insert(key.clone(), self.cur.clone());
        unsafe { &*self.data[&key].get() }
    }
}

/// Run an iterative worklist algorithm to find a fixed point.  `data` should be pre-initialized
/// with data for each relevant key.  This function will call `update(k, v, ctx)` for each node.
/// Use `ctx[other_k]` to obtain data for other nodes.
///
/// TODO: Right now, indexing panics if `other_k == k`.  If we change the signature of `update` to
/// `FnMut(K, &RefactorCtxt<K, V>) -> V` then we can avoid this issue.  Being unable to mutate the value
/// in-place might be slightly less efficient for some use cases, though.
pub fn iterate<K, V, F>(data: &mut HashMap<K, V>, mut update: F)
where
    K: Hash + Eq + Clone + ::std::fmt::Debug,
    F: FnMut(K, &mut V, &Ctxt<K, V>) -> bool,
{
    let rev_deps = DepMap::new();

    let mut pending = data.keys().cloned().collect::<HashSet<_>>();
    while !pending.is_empty() {
        let cur = pending.iter().next().unwrap().clone();
        pending.remove(&cur);
        info!("DF: update {:?}", cur);

        let changed = {
            let (ctxt, val) = Ctxt::new(&rev_deps, data, cur.clone());
            update(cur.clone(), val, &ctxt)
        };

        if changed {
            info!("  changed; queueing updates...");
            rev_deps.for_each(&cur, |other| {
                info!("    queue {:?}", other);
                pending.insert(other.clone());
            });
        }
    }
}
