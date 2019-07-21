use std::cmp::{Ordering};
use std::hash::{Hash, Hasher};
use std::collections::{HashSet, HashMap};
use crate::proposition::Proposition;

#[derive(Hash, Eq, PartialEq, Clone, Debug, Ord, PartialOrd)]
pub enum ActionType<ActionId> {
    Action(ActionId),
    Maintenance(ActionId)
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Action<ActionId: Hash + Clone> {
    pub id: ActionType<ActionId>,
    pub reqs: HashSet<Proposition>,
    pub effects: HashSet<Proposition>,
}

/// Actions are hashed based on their id, that means you can't have
/// two actions of the same id in a HashSet even if they have
/// different reqs and effects
impl<ActionId: Hash + Clone> Hash for Action<ActionId> {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.id.hash(state);
    }
}

impl<ActionId: Ord + Clone + Hash> Ord for Action<ActionId> {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.id).cmp(&(other.id))
    }
}

impl<ActionId: Hash + Ord + Clone> PartialOrd for Action<ActionId> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<ActionId: Hash + Clone> Action<ActionId> {
    pub fn new(id: ActionId, reqs: HashSet<&Proposition>, effects: HashSet<&Proposition>) -> Action<ActionId> {
        Action {
            id: ActionType::Action(id),
            reqs: reqs.into_iter().map(|i| i.to_owned()).collect(),
            effects: effects.into_iter().map(|i| i.to_owned()).collect(),
        }
    }

    pub fn get_action(&self) -> &ActionId {
        match &self.id {
            ActionType::Action(action) => action,
            ActionType::Maintenance(action) => action
        }
    }
}

#[cfg(test)]
mod test_action {
    use super::*;


    #[derive(Debug, Hash, Clone, Eq, PartialEq)]
    enum TestActionId {
        A,
        B,
    }

    #[test]
    fn test_action() {
        let a = Action::new(TestActionId::A, hashset!{}, hashset!{});
        let a2 = Action::new(TestActionId::A, hashset!{}, hashset!{});

        // The same ActionId value results in the same internal ID
        assert_eq!(a, a2);

        // Making a set from duplicate actions removes dupes
        let set = hashset!{a.clone(), a2};
        assert_eq!(set.len(), 1);

        // Actions with different ActionId are not equal
        let b = Action::new(TestActionId::B, hashset!{}, hashset!{});
        assert_ne!(a, b.clone());

        // Maintenance actions for the same ActionId are not equal
        let b2 = Action {
            id: ActionType::Maintenance(TestActionId::B),
            reqs: hashset!{},
            effects: hashset!{},
        };

        assert_ne!(b, b2);
    }
}
