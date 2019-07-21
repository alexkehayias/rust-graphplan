use std::cmp::{Ordering};
use std::hash::{Hash, Hasher};
use std::collections::HashSet;
use crate::proposition::Proposition;

#[derive(Hash, Eq, PartialEq, Clone, Debug, Ord, PartialOrd)]
pub enum ActionType<ActionId> {
    Action(ActionId),
    Maintenance(Proposition)
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
        (self.id).cmp(&other.id)
    }
}

impl<ActionId: Hash + Ord + Clone> PartialOrd for Action<ActionId> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some((self.id).cmp(&other.id))
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

    pub fn new_maintenance(prop: Proposition) -> Action<ActionId> {
        Action {
            id: ActionType::Maintenance(prop.clone()),
            reqs: hashset!{prop.clone()},
            effects: hashset!{prop},
        }
    }

    pub fn get_action(&self) -> &ActionId {
        match &self.id {
            ActionType::Action(action) => action,
            _ => panic!("Attempted to get an action from a maintenance action type"),
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
    fn equality_works() {
        let a = Action::new(TestActionId::A, hashset!{}, hashset!{});
        assert_eq!(a.clone(), a.clone());

        let a2 = Action::new(TestActionId::A, hashset!{}, hashset!{});
        assert_eq!(a, a2);

        let b = Action::new(TestActionId::B, hashset!{}, hashset!{});
        assert_ne!(a, b);
    }

    #[test]
    fn hashing_works() {
        let a = Action::new(TestActionId::A, hashset!{}, hashset!{});
        let a2 = Action::new(TestActionId::A, hashset!{}, hashset!{});
        let set = hashset!{a.clone(), a2};
        assert_eq!(set.len(), 1);
    }


    #[test]
    fn maintenance_action_works() {
        let m: Action<Proposition> = Action::new_maintenance(Proposition::from_str("test"));
        let m2 = Action::new_maintenance(Proposition::from_str("test"));
        assert_eq!(m, m2.clone());

        let m3 = Action::new_maintenance(Proposition::from_str("test2"));
        assert_ne!(m2, m3);
    }
}
