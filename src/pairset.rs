use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::iter::FromIterator;

#[derive(Debug, PartialOrd, Eq, Ord, Clone)]
/// An unordered two element tuple that such that (a, b) == (b, a)
pub struct PairSet<T: Ord + PartialEq + Eq + Clone>(pub T, pub T);

impl<T> Hash for PairSet<T>
where
    T: Hash + Ord + Clone,
{
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        if self.0 < self.1 {
            self.0.hash(state);
            self.1.hash(state);
        } else {
            self.1.hash(state);
            self.0.hash(state);
        }
    }
}

impl<T> PartialEq for PairSet<T>
where
    T: Ord + Clone,
{
    fn eq(&self, other: &PairSet<T>) -> bool {
        self.0 == other.0 && self.1 == other.1 || self.1 == other.0 && self.0 == other.1
    }
}

impl<T> PairSet<T>
where
    T: Ord + PartialEq + Eq + Clone,
{
    pub fn to_owned(&self) -> PairSet<T> {
        let PairSet(a, b) = self;
        PairSet(a.to_owned(), b.to_owned())
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
            !hashset! {PairSet::<&'static str>("test1", "test2")}
                .insert(PairSet::<&'static str>("test2", "test1")),
            "Hashed value should be the same regardless of order"
        )
    }
}

/// Returns the unique pairs of a set of items
pub fn pairs<T: Eq + Hash + Clone + Ord>(items: &HashSet<T>) -> HashSet<PairSet<T>> {
    let mut accum = HashSet::new();
    let mut sorted = Vec::from_iter(items.iter().cloned());
    sorted.sort();

    for i in sorted.iter() {
        for j in sorted.iter() {
            if i != j {
                accum.insert(PairSet(i.to_owned(), j.to_owned()));
            }
        }
    }
    accum
}

/// Returns the pairs of a set of items
pub fn pairs_from_sets<T: Eq + Hash + Clone + Ord>(
    items1: HashSet<T>,
    items2: HashSet<T>,
) -> HashSet<PairSet<T>> {
    let mut accum = HashSet::new();

    let mut sorted1 = Vec::from_iter(items1.into_iter());
    sorted1.sort();

    let mut sorted2 = Vec::from_iter(items2.into_iter());
    sorted2.sort();

    for i in sorted1.iter() {
        for j in sorted2.iter() {
            if i != j {
                accum.insert(PairSet(i.to_owned(), j.to_owned()));
            }
        }
    }
    accum
}

#[cfg(test)]
mod pairs_test {
    use super::*;
    use crate::action::Action;
    use crate::proposition::Proposition;

    #[test]
    fn yields_unique_pairs_only() {
        let p1 = "a";
        let p2 = "b";
        let p3 = "c";
        assert_eq!(
            hashset! {PairSet(p1.clone(), p2.clone()),
            PairSet(p1.clone(), p3.clone()),
            PairSet(p2.clone(), p3.clone())},
            pairs(&hashset! {p1.clone(), p2.clone(), p3.clone()})
        );
    }

    #[test]
    fn yields_unique_pairs_from_sets() {
        assert_eq!(
            pairs_from_sets(hashset! {1, 2}, hashset! {3}),
            hashset! {PairSet(1, 3), PairSet(2, 3)}
        );

        assert_eq!(
            pairs_from_sets(hashset! {1}, hashset! {1, 2}),
            hashset! {PairSet(1, 2)}
        );

        assert_eq!(pairs_from_sets(hashset! {}, hashset! {1, 2}), hashset! {});
    }

    #[test]
    fn yields_unique_pairs_from_sets_of_actions() {
        let p1 = Proposition::from("tired");
        let not_p1 = p1.negate();
        let p2 = Proposition::from("dog needs to pee");
        let not_p2 = p2.negate();
        let p3 = Proposition::from("caffeinated");

        let a1 = Action::new(
            String::from("coffee"),
            hashset! {&p1},
            hashset! {&p3, &not_p1},
        );
        let a2 = Action::new(
            String::from("walk dog"),
            hashset! {&p2, &p3},
            hashset! {&not_p2},
        );
        let a3 = Action::new_maintenance(&not_p1);

        assert_eq!(
            pairs_from_sets(hashset! {a1.clone()}, hashset! {a2.clone()}),
            hashset! {PairSet(a1.clone(), a2.clone())}
        );

        assert_eq!(
            pairs_from_sets(hashset! {a3.clone()}, hashset! {a2.clone()}),
            hashset! {PairSet(a3, a2)}
        );
    }
}
