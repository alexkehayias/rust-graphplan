use std::fmt;
use std::cmp::{Ordering};
use std::hash::{Hash, Hasher};
use std::collections::{HashSet};
use crate::proposition::Proposition;

#[derive(Eq, PartialEq, Clone)]
pub struct Action {
    pub name: String,
    pub reqs: HashSet<Proposition>,
    pub effects: HashSet<Proposition>,
}

impl fmt::Debug for Action {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // write!(f, "A:{} {{req: {:?} effects: {:?}}}", self.name, self.reqs, self.effects)
        write!(f, "A:{}", self.name)
    }
}

/// Actions are hashed based on their name, that means you can't have
/// two actions of the same name in a HashSet even if they have
/// different reqs and effects
impl Hash for Action {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.name.hash(state);
    }
}

impl Ord for Action {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.name).cmp(&(other.name))
    }
}

impl PartialOrd for Action {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Action {
    pub fn new(name: String, reqs: HashSet<&Proposition>, effects: HashSet<&Proposition>) -> Action {
        Action {
            name: name,
            reqs: reqs.into_iter().map(|i| i.to_owned()).collect(),
            effects: effects.into_iter().map(|i| i.to_owned()).collect()
        }
    }
}
