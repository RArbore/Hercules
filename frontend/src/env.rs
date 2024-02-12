use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;

pub struct Env<K, V> {
    table : HashMap<K, Vec<V>>,
    scope : Vec<HashSet<K>>,
    count : usize,
}

impl<K : Eq + Hash + Copy, V> Env<K, V> {
    pub fn new() -> Env<K, V> {
        Env { table : HashMap::new(), scope : vec![], count : 0 }
    }

    pub fn lookup(&self, k : &K) -> Option<&V> {
        match self.table.get(k) {
            None => None,
            Some(l) => Some(&l[l.len()-1]),
        }
    }

    pub fn insert(&mut self, k : K, v : V) {
        if self.scope[self.scope.len()-1].contains(&k) {
            match self.table.get_mut(&k) {
                None => assert!(false, "Internal Failure: Environment Insert"),
                Some(r) => {
                    let last = r.len() - 1;
                    r[last] = v;
                },
            }
        } else {
            let last = self.scope.len() - 1;
            match self.table.get_mut(&k) {
                None => { self.table.insert(k, vec![v]); },
                Some(r) => { r.push(v); },
            }
            self.scope[last].insert(k);
        }
    }

    pub fn openScope(&mut self) {
        self.scope.push(HashSet::new());
    }

    pub fn closeScope(&mut self) {
        match self.scope.pop() {
            None => assert!(false, "Internal Failure: Environment no scope to close"),
            Some(toRemove) => {
                for k in toRemove {
                    match self.table.get_mut(&k) {
                        None => { assert!(false, "Internal Failure: Environment Close Scope"); },
                        Some(r) => { r.pop(); },
                    }
                }
            },
        }
    }

    pub fn uniq(&mut self) -> usize {
        let n = self.count;
        self.count += 1;
        n
    }
}
