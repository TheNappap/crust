use std::{collections::HashMap, hash::Hash};



#[derive(Debug, PartialEq, Clone)]
pub struct FieldMap<K, V> where K: Hash + Eq {
    keys: HashMap<K, usize>,
    values: Vec<V>
}

impl<K: Hash + Eq, V> FieldMap<K, V> {
    pub fn get(&self, k: &K) -> Option<&V> {
        self.keys.get(k).map(|i| &self.values[*i])
    }

    pub fn values(&self) -> impl Iterator<Item = &V> + '_ {
        self.values.iter()
    }
    
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> + '_ {
        field_iter::Iter::<K,V>::new(self)
    }
}

impl<K: Hash + Eq, V> Default for FieldMap<K, V> {
    fn default() -> Self {
        Self { keys: Default::default(), values: Default::default() }
    }
}

impl<K: Hash + Eq, V> Extend<(K,V)> for FieldMap<K, V> {
    fn extend<T: IntoIterator<Item = (K,V)>>(&mut self, iter: T) {
        for (k,v) in iter {
            self.values.push(v);
            self.keys.insert(k, self.values.len()-1);
        }
    }
}

impl<K: Hash + Eq, V> FromIterator<(K,V)> for FieldMap<K, V> {
    fn from_iter<I: IntoIterator<Item=(K,V)>>(iter: I) -> Self {
        let mut keys = HashMap::new();
        let mut values = Vec::new();

        for (k,v) in iter {
            values.push(v);
            keys.insert(k, values.len()-1);
        }

        FieldMap{keys, values}
    }
}

mod field_iter {
    use std::{hash::Hash, ops::Range, collections::HashMap};

    use super::FieldMap;

    pub struct Iter<'map, K, V> where K: Hash + Eq + 'map, V: 'map {
        iter: Range<usize>,
        index_to_key: HashMap<usize, &'map K>,
        field_map: &'map FieldMap<K,V>,
    }
    
    impl<'map, K: Hash + Eq, V> Iter<'map, K, V> {
        pub fn new(field_map: &'map FieldMap<K,V>) -> Iter<'map, K, V> {
            let index_to_key = field_map.keys.iter().map(|(k,i)| (*i,k)).collect();
            Iter::<K,V>{ 
                iter: 0..(field_map.values.len()),
                index_to_key, field_map,
            }
        }
    }

    impl<'map, K: Hash + Eq, V> Iterator for Iter<'map, K, V> {
        type Item = (&'map K,&'map V);
    
        fn next(&mut self) -> Option<Self::Item> {
            let i = self.iter.next()?;
            let key = self.index_to_key.get(&i)?;
            let value = &self.field_map.values[i];
            Some((*key,value))
        }
    }
}