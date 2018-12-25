use std::collections::{HashSet};
use std::hash::{Hash, Hasher};


#[derive(Debug, PartialOrd, Eq, Ord, Clone)]
/// An unordered two element tuple that such that (a, b) == (b, a)
pub struct PairSet<T: Ord + PartialEq + Eq + Clone>(pub T, pub T);

impl <T> Hash for PairSet<T> where T: Hash + Ord + Clone{
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        if self.0 < self.1 {
            self.0.hash(state);
            self.1.hash(state);
        } else {
            self.1.hash(state);
            self.0.hash(state);
        }
    }
}

impl <T> PartialEq for PairSet<T> where T: Ord + Clone{
    fn eq(&self, other: &PairSet<T>) -> bool {
        &self.0 == &other.0 && &self.1 == &other.1 || &self.1 == &other.0 && &self.0 == &other.1
    }
}

#[cfg(test)]
mod pair_set_test {
    use super::*;

    #[test]
    fn pairset() {
        assert_eq!(
            PairSet::<&'static str>("test1", "test2"),
            PairSet::<&'static str>("test2", "test1"),
            "Order should not matter"
        );

        assert!(
            !hashset!{PairSet::<&'static str>("test1", "test2")}
            .insert(PairSet::<&'static str>("test2", "test1")),
            "Hashed value should be the same regardless of order"
        )
    }
}

/// Returns the unique pairs of a set of items
pub fn pairs<T: Eq + Hash + Clone + Ord>(items: &HashSet<T>) -> HashSet<PairSet<T>> {
    let mut accum = HashSet::new();

    let mut sorted: Vec<T> = items.clone().into_iter().collect();
    sorted.sort();

    for i in sorted.iter().cloned() {
        for j in sorted.iter().cloned() {
            if i != j && j > i {
                accum.insert(PairSet(i.clone(), j.clone()));
            }
        }
    }
    accum
}

/// Returns the pairs of a set of items
pub fn pairs_from_sets<T: Eq + Hash + Clone + Ord>(items1: HashSet<T>,
                                                   items2: HashSet<T>)
                                                   -> HashSet<PairSet<T>> {
    let mut accum = HashSet::new();

    let mut sorted1: Vec<T> = items1.into_iter().collect();
    sorted1.sort();

    let mut sorted2: Vec<T> = items2.into_iter().collect();
    sorted2.sort();

    for i in sorted1.iter().cloned() {
        for j in sorted2.iter().cloned() {
            if i != j && j > i {
                accum.insert(PairSet(i.clone(), j.clone()));
            }
        }
    }
    accum
}

#[cfg(test)]
mod pairs_test {
    use super::*;

    #[test]
    fn yields_unique_pairs_only() {
        let p1 = "a";
        let p2 = "b";
        let p3 = "c";
        assert_eq!(
            hashset!{PairSet(p1.clone(), p2.clone()),
                     PairSet(p1.clone(), p3.clone()),
                     PairSet(p2.clone(), p3.clone())},
            pairs(&hashset!{p1.clone(), p2.clone(), p3.clone()})
        );
    }

    #[test]
    fn yields_unique_pairs_from_sets() {
        assert_eq!(pairs_from_sets(hashset!{1, 2}, hashset!{3}),
                   hashset!{PairSet(1, 3), PairSet(2, 3)})
    }
}
