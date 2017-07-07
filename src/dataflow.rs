use std::cell::UnsafeCell;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::marker::PhantomData;
use std::mem;
use std::ops::Index;


struct DepMap<K> {
    map: UnsafeCell<HashMap<K, HashSet<K>>>,
}

impl<K> DepMap<K>
        where K: Hash+Eq+::std::fmt::Debug {
    fn new() -> DepMap<K> {
        DepMap {
            map: UnsafeCell::new(HashMap::new()),
        }
    }

    fn insert(&self, src: K, dest: K) {
        println!(" * record dep: {:?} -> {:?}", src, dest);
        unsafe {
            (*self.map.get()).entry(src).or_insert_with(HashSet::new).insert(dest);
        }
    }

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


pub struct Ctxt<'a, K: 'a, V: 'a> {
    rev_deps: &'a DepMap<K>,
    data: &'a HashMap<K, UnsafeCell<V>>,
    _marker0: PhantomData<&'a mut HashMap<K, V>>,
    cur: K,
}

impl<'a, K, V> Ctxt<'a, K, V>
        where K: Hash+Eq+::std::fmt::Debug {
    fn new(rev_deps: &'a DepMap<K>,
           init: &'a mut HashMap<K, V>,
           cur: K) -> (Ctxt<'a, K, V>, &'a mut V) {
        unsafe {
            let data: &'a HashMap<K, UnsafeCell<V>> = mem::transmute(init);

            let cur_val = unsafe { &mut *data[&cur].get() };

            let ctxt = Ctxt {
                rev_deps: rev_deps,
                data: data,
                _marker0: PhantomData,
                cur: cur,
            };

            (ctxt, cur_val)
        }
    }
}

impl<'a, K, V> Index<K> for Ctxt<'a, K, V>
        where K: Hash+Eq+Clone+::std::fmt::Debug {
    type Output = V;
    fn index(&self, key: K) -> &V {
        assert!(key != self.cur, "tried to access current node");
        unsafe {
            self.rev_deps.insert(key.clone(), self.cur.clone());
        }
        unsafe {
            &*self.data[&key].get()
        }
    }
}

pub fn iterate<K, V, F>(data: &mut HashMap<K, V>, mut update: F)
        where K: Hash+Eq+Clone+::std::fmt::Debug,
              F: FnMut(K, &mut V, &Ctxt<K, V>) -> bool {
    let rev_deps = DepMap::new();

    let mut pending = data.keys().map(|x| x.clone()).collect::<HashSet<_>>();
    while pending.len() > 0 {
        let cur = pending.iter().next().unwrap().clone();
        pending.remove(&cur);
        println!("DF: update {:?}", cur);

        let changed = {
            let (ctxt, val) = Ctxt::new(&rev_deps, data, cur.clone());
            update(cur.clone(), val, &ctxt)
        };

        if changed {
            println!("  changed; queueing updates...");
            rev_deps.for_each(&cur, |other| {
                println!("    queue {:?}", other);
                pending.insert(other.clone());
            });
        }
    }
}
