use std::{collections::HashMap, hash::Hash};


#[derive(Debug, PartialEq, Clone)]
pub struct OrderedMap<K, V> where K: Hash + Eq {
    keys: HashMap<K, usize>,
    values: Vec<V>
}

impl<K: Hash + Eq, V> OrderedMap<K, V> {
    pub fn get(&self, k: &K) -> Option<&V> {
        self.keys.get(k).map(|i| &self.values[*i])
    }

    pub fn values(&self) -> impl Iterator<Item = &V> + '_ {
        self.values.iter()
    }
    
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> + '_ {
        field_iter::Iter::<K,V>::new(self)
    }
    
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&K, &mut V)> + '_ {
        field_iter::IterMut::<K,V>::new(self)
    }
}

impl<K: Hash + Eq, V> Default for OrderedMap<K, V> {
    fn default() -> Self {
        Self { keys: Default::default(), values: Default::default() }
    }
}

impl<K: Hash + Eq, V> Extend<(K,V)> for OrderedMap<K, V> {
    fn extend<T: IntoIterator<Item = (K,V)>>(&mut self, iter: T) {
        for (k,v) in iter {
            self.values.push(v);
            self.keys.insert(k, self.values.len()-1);
        }
    }
}

impl<K: Hash + Eq, V> FromIterator<(K,V)> for OrderedMap<K, V> {
    fn from_iter<I: IntoIterator<Item=(K,V)>>(iter: I) -> Self {
        let mut keys = HashMap::new();
        let mut values = Vec::new();

        for (k,v) in iter {
            values.push(v);
            keys.insert(k, values.len()-1);
        }

        OrderedMap{keys, values}
    }
}

mod field_iter {
    use std::{hash::Hash, ops::Range, collections::HashMap};

    use super::OrderedMap;

    pub struct Iter<'map, K, V> where K: Hash + Eq + 'map, V: 'map {
        iter: Range<usize>,
        index_to_key: HashMap<usize, &'map K>,
        values: &'map Vec<V>,
    }
    
    impl<'map, K: Hash + Eq, V> Iter<'map, K, V> {
        pub fn new(field_map: &'map OrderedMap<K,V>) -> Iter<'map, K, V> {
            let index_to_key = field_map.keys.iter().map(|(k,i)| (*i,k)).collect();
            Iter::<K,V>{ 
                iter: 0..(field_map.values.len()),
                index_to_key, values: &field_map.values,
            }
        }
    }

    impl<'map, K: Hash + Eq, V> Iterator for Iter<'map, K, V> {
        type Item = (&'map K,&'map V);
    
        fn next(&mut self) -> Option<Self::Item> {
            let i = self.iter.next()?;
            let key = self.index_to_key.get(&i)?;
            let value = &self.values[i];
            Some((*key,value))
        }
    }
    pub struct IterMut<'map, K, V> where K: Hash + Eq + 'map, V: 'map {
        iter: Range<usize>,
        index_to_key: HashMap<usize, &'map K>,
        values: std::slice::IterMut<'map, V>,
    }
    
    impl<'map, K: Hash + Eq, V> IterMut<'map, K, V> {
        pub fn new(field_map: &'map mut OrderedMap<K,V>) -> IterMut<'map, K, V> {
            let index_to_key = field_map.keys.iter_mut().map(|(k,i)| (*i,k)).collect();
            IterMut::<K,V>{ 
                iter: 0..(field_map.values.len()),
                index_to_key, values: field_map.values.iter_mut(),
            }
        }
    }

    impl<'map, K: Hash + Eq, V> Iterator for IterMut<'map, K, V> {
        type Item = (&'map K,&'map mut V);
    
        fn next(&mut self) -> Option<Self::Item> {
            let i = self.iter.next()?;
            let key = self.index_to_key.get(&i)?;
            let value = self.values.next()?;
            Some((*key,value))
        }
    }
}