use std::cmp::{Ordering};
use std::fmt::Display;
use std::hash::{Hash, Hasher};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashSet;
use crate::proposition::Proposition;

#[derive(Hash, Eq, PartialEq, Clone, Debug, Ord, PartialOrd)]
pub enum ActionType<'a, ActionId, PropositionId: Display + Hash> {
    Action(ActionId),
    Maintenance(&'a Proposition<PropositionId>)
}

#[derive(Eq, Clone, Debug)]
pub struct Action<'a, ActionId: Hash + Clone, PropositionId: Display + Hash + PartialEq + Eq + Clone> {
    pub id: ActionType<'a, ActionId, PropositionId>,
    pub reqs: HashSet<&'a Proposition<PropositionId>>,
    pub effects: HashSet<&'a Proposition<PropositionId>>,
}

/// Actions are hashed based on their id, that means you can't have
/// two actions of the same id in a HashSet even if they have
/// different reqs and effects
impl<'a, ActionId: Hash + Clone, PropositionId: Display + Hash + PartialEq + Eq + Clone> Hash for Action<'a, ActionId, PropositionId> {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.id.hash(state);
    }
}

impl<'a, ActionId: Hash + Clone, PropositionId: Clone + Eq + Hash + Display> PartialEq for Action<'a, ActionId, PropositionId> {
    fn eq(&self, other: &Self) -> bool {
        let mut hasher_left = DefaultHasher::new();
        let mut hasher_right = DefaultHasher::new();
        self.hash(&mut hasher_left);
        other.hash(&mut hasher_right);
        hasher_left.finish() == hasher_right.finish()
    }
}

impl<'a, ActionId: Ord + Clone + Hash, PropositionId: Ord + PartialEq + Eq + Display + Hash + Clone> Ord for Action<'a, ActionId, PropositionId> {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.id).cmp(&other.id)
    }
}

impl<'a, ActionId: Hash + Ord + Clone, PropositionId: Ord + PartialEq + Eq + Display + Hash + Clone> PartialOrd for Action<'a, ActionId, PropositionId> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some((self.id).cmp(&other.id))
    }
}

impl<'a, ActionId: Hash + Clone, PropositionId: Display + Hash + Clone + PartialEq + Eq> Action<'a, ActionId, PropositionId> {
    pub fn new(id: ActionId,
               reqs: HashSet<&'a Proposition<PropositionId>>,
               effects: HashSet<&'a Proposition<PropositionId>>)
               -> Action<'a, ActionId, PropositionId> {
        Action {
            id: ActionType::Action(id),
            reqs: reqs,
            effects: effects,
        }
    }

    pub fn new_maintenance(prop: &'a Proposition<PropositionId>) -> Action<'a, ActionId, PropositionId> {
        Action {
            id: ActionType::Maintenance(prop),
            reqs: hashset!{prop},
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
        let a: Action<TestActionId, &str> = Action::new(TestActionId::A, hashset!{}, hashset!{});
        assert_eq!(a.clone(), a.clone());

        let a2 = Action::new(TestActionId::A, hashset!{}, hashset!{});
        assert_eq!(a, a2);

        let b = Action::new(TestActionId::B, hashset!{}, hashset!{});
        assert_ne!(a, b);
    }

    #[test]
    fn hashing_works() {
        let a1: Action<TestActionId, &str> = Action::new(TestActionId::A, hashset!{}, hashset!{});
        let a2 = Action::new(TestActionId::A, hashset!{}, hashset!{});
        let set = hashset!{a1, a2};
        assert_eq!(set.len(), 1);
    }

    #[test]
    fn maintenance_action_works() {
        let p1 = Proposition::from("test");
        let p2 = Proposition::from("test2");
        let m1: Action<TestActionId, &str> = Action::new_maintenance(&p1);
        let m2 = Action::new_maintenance(&p1);
        assert_eq!(m1, m2);

        let m3 = Action::new_maintenance(&p2);
        assert_ne!(m2, m3);
    }
}
