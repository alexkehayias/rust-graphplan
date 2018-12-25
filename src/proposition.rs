use std::fmt;
use std::hash::{Hash,Hasher};


#[derive(Eq, PartialEq, Ord, PartialOrd, Clone)]
pub struct Proposition {
    pub name: &'static str,
    pub negation: bool,
}

impl fmt::Debug for Proposition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}P:{}", if self.negation {"¬"} else {""}, self.name)
    }
}

impl Hash for Proposition {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.negation.hash(state);
        self.name.hash(state);
    }
}

impl Proposition {
    pub fn from_str(name: &'static str) -> Proposition {
        Proposition {name, negation: false}
    }

    pub fn negate(&self) -> Proposition {
        Proposition { name: self.name, negation: !self.negation }
    }

    pub fn is_negation(&self, prop: &Proposition) -> bool {
        prop.name == self.name && prop.negation == !self.negation
    }
}

#[cfg(test)]
mod proposition_test {
    use super::*;

    #[test]
    fn propositions_can_be_negated() {
        // Sanity check
        assert_eq!(Proposition::from_str("test"), Proposition::from_str("test"));
        let p1 = Proposition::from_str("test");

        assert!(false == p1.negation);
        assert!(true == Proposition::from_str("test").negate().negation);

        let p2 = Proposition::from_str("test").negate();

        assert!(
            p2.is_negation(&p1),
            format!("{:?} is not a negation of {:?}", p1, p2)
        );

        assert!(p1.is_negation(&p2));
    }

    #[test]
    fn proposition_hashing_works() {
        let set = hashset!{Proposition::from_str("caffeinated")};
        assert!(set.contains(&Proposition::from_str("caffeinated")));

        let set = hashset!{Proposition::from_str("caffeinated").negate()};
        assert!(set.contains(&Proposition::from_str("caffeinated").negate()));

        let set = hashset!{Proposition::from_str("caffeinated").negate()};
        assert!(!set.contains(&Proposition::from_str("caffeinated")));
    }

}
