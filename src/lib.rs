#![allow(dead_code)]
use std::collections::{HashSet, HashMap, BTreeSet, VecDeque};
use std::iter::FromIterator;
use std::fmt;
use std::cmp::{Ordering};
use std::hash::{Hash,Hasher};
use itertools::Itertools;

#[macro_export]
/// Create a **HashSet** from a list of elements. Implementation
/// copied from the maplit library https://github.com/bluss/maplit
///
/// ## Example
///
/// ```
/// #[macro_use] extern crate graphplan;
/// # fn main() {
///
/// let set = hashset!{"a", "b"};
/// assert!(set.contains("a"));
/// assert!(set.contains("b"));
/// assert!(!set.contains("c"));
/// # }
/// ```
macro_rules! hashset {
    (@single $($x:tt)*) => (());
    (@count $($rest:expr),*) => (<[()]>::len(&[$(hashset!(@single $rest)),*]));

    ($($key:expr,)+) => { hashset!($($key),+) };
    ($($key:expr),*) => {
        {
            let _cap = hashset!(@count $($key),*);
            let mut _set = ::std::collections::HashSet::with_capacity(_cap);
            $(
                let _ = _set.insert($key);
            )*
                _set
        }
    };
}

#[macro_export]
/// Create a **BTreeSet** from a list of elements. Implementation
/// copied from the maplit library https://github.com/bluss/maplit
///
/// ## Example
///
/// ```
/// #[macro_use] extern crate maplit;
/// # fn main() {
///
/// let set = btreeset!{"a", "b"};
/// assert!(set.contains("a"));
/// assert!(set.contains("b"));
/// assert!(!set.contains("c"));
/// # }
/// ```
macro_rules! btreeset {
    ($($key:expr,)+) => (btreeset!($($key),+));

    ( $($key:expr),* ) => {
        {
            let mut _set = ::std::collections::BTreeSet::new();
            $(
                _set.insert($key);
            )*
            _set
        }
    };
}

#[macro_export]
/// Create a **HashMap** from a list of key-value pairs. Implementation
/// copied from the maplit library https://github.com/bluss/maplit
///
/// ## Example
///
/// ```
/// #[macro_use] extern crate maplit;
/// # fn main() {
///
/// let map = hashmap!{
///     "a" => 1,
///     "b" => 2,
/// };
/// assert_eq!(map["a"], 1);
/// assert_eq!(map["b"], 2);
/// assert_eq!(map.get("c"), None);
/// # }
/// ```
macro_rules! hashmap {
    (@single $($x:tt)*) => (());
    (@count $($rest:expr),*) => (<[()]>::len(&[$(hashmap!(@single $rest)),*]));

    ($($key:expr => $value:expr,)+) => { hashmap!($($key => $value),+) };
    ($($key:expr => $value:expr),*) => {
        {
            let _cap = hashmap!(@count $($key),*);
            let mut _map = ::std::collections::HashMap::with_capacity(_cap);
            $(
                let _ = _map.insert($key, $value);
            )*
            _map
        }
    };
}


#[derive(Eq, PartialEq, Ord, PartialOrd, Clone)]
pub struct Proposition {
    name: &'static str,
    negation: bool,
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


#[derive(Eq, PartialEq, Clone)]
pub struct Action {
    name: String,
    reqs: HashSet<Proposition>,
    effects: HashSet<Proposition>,
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
    pub fn new(name: String, reqs: HashSet<Proposition>, effects: HashSet<Proposition>) -> Action {
        Action {name, reqs, effects}
    }
}

pub type ActionLayerData = HashSet<Action>;
pub type PropositionLayerData = HashSet<Proposition>;

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Layer {
    ActionLayer(ActionLayerData),
    PropositionLayer(PropositionLayerData),
}

#[derive(Debug, PartialOrd, Eq, Ord, Clone)]
/// An unordered two element tuple that such that (a, b) == (b, a)
pub struct PairSet<T: Ord + PartialEq + Eq + Clone>(T, T);

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
        let p1 = Proposition::from_str("a");
        let p2 = Proposition::from_str("b");
        let p3 = Proposition::from_str("c");
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

type MutexPairs<T> = HashSet<PairSet<T>>;

impl Layer {
    /// Create a new layer from another. ActionLayer returns a
    /// PropositionLayer and PropositionLayer returns an ActionLayer
    /// # Example
    /// ```
    /// #[macro_use] extern crate graphplan;
    /// use graphplan::Layer;
    /// let prop_layer = Layer::PropositionLayer(hashset!{});
    /// Layer::from_layer(hashset![], prop_layer);
    /// ```
    pub fn from_layer(all_actions: HashSet<Action>, layer: Layer) -> Layer {
        match &layer {
            Layer::ActionLayer(actions) => {
                let mut layer_data = PropositionLayerData::new();
                for a in actions {
                    for e in a.effects.iter() {
                        layer_data.insert(e.clone());
                    }
                }

                Layer::PropositionLayer(layer_data)
            },
            Layer::PropositionLayer(props) => {
                let props_hash = HashSet::from_iter(props.iter().cloned());
                let mut layer_data = ActionLayerData::new();

                for a in all_actions {
                    // If a has all props and no two of its
                    // requirements are labeled as mutually exclusive
                    // push it to the layer

                    // TODO Pass in mutexes and check if no two of its
                    // preconditions are labeled as mutually exclusive
                    if a.reqs.is_subset(&props_hash) {
                        layer_data.insert(a);
                    }
                }

                // Add in maintenance actions for all props
                for p in props {
                    layer_data.insert(
                        Action::new(
                            format!("[maintain] {}{}", if p.negation {"¬"} else {""}, p.name),
                            hashset!{p.clone()},
                            hashset!{p.clone()},
                        )
                    );
                }
                Layer::ActionLayer(layer_data)
            },
        }
    }

    pub fn action_mutexes(actions: HashSet<Action>,
                          mutex_props: Option<MutexPairs<Proposition>>)
                          -> MutexPairs<Action> {
        let mut mutexes = MutexPairs::new();
        let action_pairs = pairs(&actions);

        for PairSet(a1, a2) in action_pairs {
            // Inconsistent effects: The effect of one action is the
            // negation of another
            // - Negate the effects of one of the actions then check
            //   the overlap
            // - If there is any overlap the two actions
            //   are mutex
            let negated_fx: HashSet<Proposition> = a1.effects
                .iter()
                .map(|e| e.negate())
                .collect();
            let inconsistent_fx: HashSet<Proposition> = a2.effects
                .intersection(&negated_fx)
                .map(|i| i.to_owned())
                .collect();
            if !inconsistent_fx.is_empty() {
                mutexes.insert(PairSet(a1.clone(), a2.clone()));
                continue
            }

            // Interference: One action deletes the precondition of
            // another action (they can't be done in parallel then)
            let left_interference: HashSet<Proposition> = a2.reqs
                .intersection(&negated_fx)
                .map(|i| i.to_owned())
                .collect();

            if !left_interference.is_empty() {
                mutexes.insert(PairSet(a1.clone(), a2.clone()));
                continue
            }

            // Since actions are not symetrical (they may have different
            // reqs) we need to check if the right hand side action
            // interferes with left hand side too
            let right_negated_fx: HashSet<Proposition> = a2.clone().effects
                .iter()
                .map(|e| e.negate())
                .collect();
            let right_interference: HashSet<Proposition> = a1.clone().reqs
                .intersection(&right_negated_fx)
                .map(|i| i.to_owned())
                .collect();

            if !right_interference.is_empty() {
                mutexes.insert(PairSet(a1.clone(), a2.clone()));
                continue
            }

            // Competing needs: Action has precondition that is
            // mutex
            // - Generate pairs of Action.reqs and compare them
            //   to proposition mutees
            // - Check for intersection with mutex props
            if let Some(mx_props) = mutex_props.clone() {
                let req_pairs = pairs_from_sets(a1.clone().reqs, a2.clone().reqs);
                let competing_needs: HashSet<PairSet<Proposition>> = req_pairs
                    .intersection(&mx_props)
                    .map(|i| i.to_owned())
                    .collect();

                if !competing_needs.is_empty() {
                    mutexes.insert(PairSet(a1.clone(), a2.clone()));
                }
            }
        }
        mutexes
    }

    /// Returns a set of propositions that are mutually exclusive
    ///
    /// Propositions are mutex if
    /// - They are negations of one another
    /// - All ways of achieving the propositions at are pairwise mutex
    pub fn proposition_mutexes(
        props: HashSet<Proposition>,
        actions: HashSet<Action>,
        mutex_actions: Option<MutexPairs<Action>>,
    ) -> MutexPairs<Proposition> {
        let mut mutexes = MutexPairs::new();

        // Find mutexes due to negation
        for p in props.iter() {
            let not_p = p.negate();
            if props.contains(&not_p) {
                mutexes.insert(PairSet(p.clone(), not_p.clone()));
            }
        }

        // Find mutexes where all ways of achieving p are mutex
        // - Get all uniq pairs of propositions
        // - For each pair, get all uniq action pairs that could output the prop pair
        // - Set difference with the mutex_actions
        // - If there is no difference then the props are mutex
        if let Some(mx_actions) = mutex_actions {
            for PairSet(p1, p2) in pairs(&props) {
                let viable_acts: HashSet<Action> = actions.clone()
                    .into_iter()
                    .filter(|a| a.effects.contains(&p1) || a.effects.contains(&p2))
                    .collect();

                let viable_act_pairs: HashSet<PairSet<Action>> = pairs(&viable_acts).into_iter().collect();

                let diff: HashSet<_> = viable_act_pairs
                    .difference(&mx_actions)
                    .collect();

                if diff.is_empty() && !mx_actions.is_empty() {
                    mutexes.insert(PairSet(p1.clone(), p2.clone()));
                }
            }
        }
        mutexes
    }
}

#[cfg(test)]
mod from_layer_test {
    use super::*;

    #[test]
    fn action_layer_from_proposition_layer() {
        let prop = Proposition::from_str("test");
        let actual = Layer::from_layer(
            hashset!{},
            Layer::PropositionLayer(hashset!{prop.clone()})
        );
        let expected = Layer::ActionLayer(
            hashset!{
                Action::new(
                    String::from("[maintain] test"),
                    hashset!{prop.clone()},
                    hashset!{prop.clone()}
                )
            }
        );
        assert_eq!(expected, actual);
    }
}

#[cfg(test)]
mod mutex_test {
    use super::*;

    #[test]
    fn proposition_mutexes_due_to_negation() {
        let props = hashset!{
            Proposition::from_str("caffeinated"),
            Proposition::from_str("caffeinated").negate(),
            Proposition::from_str("tired"),
        };
        let actions = hashset!{};
        let action_mutexes = MutexPairs::new();
        let expected = hashset!{
            PairSet(Proposition::from_str("caffeinated"),
                    Proposition::from_str("caffeinated").negate())
        };

        assert_eq!(
            expected,
            Layer::proposition_mutexes(
                props,
                actions,
                Some(action_mutexes)
            )
        );
    }

    #[test]
    fn proposition_mutexes_due_to_mutex_actions() {
        let props = hashset!{
            Proposition::from_str("caffeinated"),
            Proposition::from_str("coffee"),
        };
        let actions = hashset!{};
        let action_mutexes = hashset!{
            PairSet(
                Action::new(
                    String::from("drink coffee"),
                    hashset!{Proposition::from_str("coffee")},
                    hashset!{Proposition::from_str("caffeinated"),
                             Proposition::from_str("coffee").negate()},
                ),
                Action::new(
                    String::from("make coffee"),
                    hashset!{},
                    hashset!{Proposition::from_str("coffee")},
                ),
            )
        };
        let expected = hashset!{
            PairSet(Proposition::from_str("caffeinated"),
                    Proposition::from_str("coffee"))
        };
        assert_eq!(
            expected,
            Layer::proposition_mutexes(props, actions, Some(action_mutexes))
        );
    }

    #[test]
    fn action_mutexes_due_to_inconsistent_fx() {
        let a1 = Action::new(
            String::from("drink coffee"),
            hashset!{},
            hashset!{Proposition::from_str("coffee").negate()}
        );

        let a2 = Action::new(
            String::from("make coffee"),
            hashset!{},
            hashset!{Proposition::from_str("coffee")}
        );
        let actions = hashset!{a1.clone(), a2.clone()};
        let props = MutexPairs::new();
        let actual = Layer::action_mutexes(actions, Some(props));

        let mut expected = MutexPairs::new();
        expected.insert(PairSet(a1, a2));

        assert_eq!(expected, actual);
    }

    #[test]
    fn action_mutexes_due_to_interference() {
        let a1 = Action::new(
            String::from("eat sandwich"),
            hashset!{Proposition::from_str("hungry")},
            hashset!{Proposition::from_str("hungry").negate()}
        );

        let a2 = Action::new(
            String::from("eat soup"),
            hashset!{Proposition::from_str("hungry")},
            hashset!{Proposition::from_str("hungry").negate()}
        );
        let actions = hashset!{a1.clone(), a2.clone()};
        let props = MutexPairs::new();
        let actual = Layer::action_mutexes(actions, Some(props));

        let mut expected = MutexPairs::new();
        expected.insert(PairSet(a1, a2));

        assert_eq!(expected, actual);
    }

    #[test]
    fn action_mutexes_due_to_competing_needs() {
        let a1 = Action::new(
            String::from("eat sandwich"),
            hashset!{Proposition::from_str("hungry")},
            hashset!{Proposition::from_str("hungry").negate()}
        );

        let a2 = Action::new(
            String::from("eat soup"),
            hashset!{Proposition::from_str("hungry")},
            hashset!{Proposition::from_str("hungry").negate()}
        );
        let actions = hashset!{a1.clone(), a2.clone()};
        let mut mutex_props = MutexPairs::new();
        mutex_props.insert(
            PairSet(
                Proposition::from_str("hungry"),
                Proposition::from_str("hungry").negate()
            )
        );
        let actual = Layer::action_mutexes(actions, Some(mutex_props));

        let mut expected = MutexPairs::new();
        expected.insert(PairSet(a1, a2));

        assert_eq!(expected, actual);
    }
}

type LayerNumber = usize;
type Solution = Vec<HashSet<Action>>;
type QueryActions = Result<BTreeSet<Action>, String>;

#[derive(Debug)]
pub struct PlanGraph {
    goals: HashSet<Proposition>,
    actions: HashSet<Action>,
    layers: Vec<Layer>,
    mutex_props: HashMap<LayerNumber, MutexPairs<Proposition>>,
    mutex_actions: HashMap<LayerNumber, MutexPairs<Action>>,
}

impl PlanGraph {
    pub fn new(initial_props: HashSet<Proposition>,
               goals: HashSet<Proposition>,
               actions: HashSet<Action>) -> Self {
        // TODO make sure the initial props are not already mutex
        PlanGraph {
            goals: goals,
            actions: actions,
            layers: vec![Layer::PropositionLayer(initial_props)],
            mutex_props: HashMap::new(),
            mutex_actions: HashMap::new()
        }
    }

    pub fn push(&mut self, layer: Layer) {
        self.layers.push(layer.clone())
    }

    /// Extends the plangraph to depth i+1
    /// Inserts another action layer and proposition layer
    pub fn extend(&mut self) {
        let length = self.layers.len();
        if let Some(layer) = self.layers.last().cloned() {
            match layer {
                Layer::ActionLayer(_) => {
                    panic!("Tried to extend a plangraph from an ActionLayer which is not allowed")
                },
                Layer::PropositionLayer(props) => {
                    let mutex_props = self.mutex_props.get(&(length - 1)).cloned();
                    let action_layer = Layer::from_layer(
                        self.actions.clone(),
                        Layer::PropositionLayer(props)
                    );
                    let action_layer_actions = match action_layer.clone() {
                        Layer::ActionLayer(action_data) => Some(action_data),
                        _ => None
                    };
                    let action_mutexes = Layer::action_mutexes(
                        action_layer_actions.clone().unwrap(),
                        mutex_props
                    );
                    self.layers.push(action_layer.clone());
                    self.mutex_actions.insert(self.layers.len(), action_mutexes.clone());

                    let prop_layer = Layer::from_layer(
                        self.actions.clone(),
                        action_layer
                    );
                    let prop_layer_props = match prop_layer.clone() {
                        Layer::PropositionLayer(prop_data) => Some(prop_data),
                        _ => None
                    };
                    let prop_mutexes = Layer::proposition_mutexes(
                        // This shouldn't fail
                        prop_layer_props.unwrap(),
                        // This shouldn't fail
                        action_layer_actions.unwrap(),
                        Some(action_mutexes)
                    );
                    self.layers.push(prop_layer);
                    self.mutex_props.insert(self.layers.len(), prop_mutexes.clone());
                }
            }
        } else {
            panic!("Tried to extend a plangraph that is not initialized. Please use PlanGraph::new instead of instantiating it as a struct.")
        }
    }

    /// Returns the depth of the planning graph
    pub fn depth(&self) -> usize {
        if self.layers.len() > 2 {
            (self.layers.len() - 1) / 2
        } else {
            0
        }
    }

    // Returns the actions at layer index as an ordered set
    pub fn actions_at_layer(&self, index: usize) -> QueryActions {
        self.layers.get(index).map_or(
            Err(format!("Layer {} does not exist", index)),
            |layer| {
                match layer {
                    Layer::ActionLayer(actions) => {
                        let acts = actions
                           .into_iter()
                           .cloned()
                           .collect::<BTreeSet<_>>();
                        Ok(acts)
                    },
                    Layer::PropositionLayer(_) => {
                        Err(format!("Tried to get actions from proposition layer {}",
                                    index))
                    }
                }
            }
        )
    }

    /// A solution is possible if all goals exist in the last
    /// proposition layer and are not mutex
    fn has_possible_solution(&self) -> bool {
        let goals = self.goals.clone();
        let last_layer_idx = self.layers.clone().len() - 1;

        if let Some(prop_layer) = self.layers.get(last_layer_idx) {
            match prop_layer {
                Layer::PropositionLayer(props) => {
                    let all_goals_present = &goals.is_subset(props);
                    let mutexes = self.mutex_props
                        .get(&last_layer_idx)
                        .unwrap_or(&MutexPairs::new())
                        .to_owned();
                    let goal_pairs = pairs(&goals);
                    let mutex_goals: HashSet<_> = mutexes
                        .intersection(&goal_pairs)
                        .collect();
                    *all_goals_present && mutex_goals.is_empty()
                },
                _ => panic!("Tried to check for a solution in a Layer::Action")
            }
        } else {
            false
        }
    }

    /// Searches the planning graph for a solution using the solver if
    /// there is no solution, extends the graph to depth i+1 and tries
    /// to solve again
    pub fn search_with<T>(&mut self, solver: T) -> Option<Solution>
    where T: GraphPlanSolver {
        if self.has_possible_solution() {
            solver.search(self)
        } else {
            // TODO extend the graph and try again until we know there
            // is no solution
            println!("[debug] No solution exists at depth {}", self.depth());
            None
        }
    }

    /// Takes a solution and filters out maintenance actions
    pub fn format_plan(solution: Option<Solution>) -> Option<Solution> {
        solution.map(|steps| steps.iter()
                     .map(|s| s.iter()
                          .filter(|i| !i.name.contains("[maintain]"))
                          .cloned()
                          .collect())
                     .collect()
        )
    }
}

pub trait GraphPlanSolver {
    /// Searches a plangraph for a sequence of collection of actions
    /// that satisfy the goals
    fn search(&self, plangraph: &PlanGraph) -> Option<Solution>;
}

struct SimpleSolver;

impl SimpleSolver {
    fn new() -> SimpleSolver {
        SimpleSolver {}
    }
}

impl SimpleSolver {
    fn action_combinations(goals: Vec<Proposition>,
                           actions: BTreeSet<Action>,
                           mutexes: Option<MutexPairs<Action>>) -> Option<(HashSet<Action>, HashMap<usize, BTreeSet<Action>>)>{
        let mut stack: VecDeque<(
            usize,
        )> = VecDeque::new();
        let mut action_accum = HashSet::new();
        let mut attempts: HashMap<usize, BTreeSet<Action>> = HashMap::new();
        let goal_len = goals.len();
        let init_goal_idx = 0;
        let mut goals_met = false;

        // Add the end goals to the stack
        stack.push_front((init_goal_idx,));

        while let Some((goal_idx, )) = stack.pop_front() {
            let available_actions = if let Some(acts) = attempts.get(&goal_idx) {
                acts.to_owned()
            } else {
                let goal = &goals[goal_idx];
                println!("[debug] Looking for action for goal {:?} in {:?}", goal, actions.clone());
                let mut available = BTreeSet::new();
                // Only actions that produce the goal and are not
                // mutex with any other actions and have not
                // already been attempted in combination with the
                // other attempted actions and are not mutex with
                // any other action
                for a in &actions {
                    // Early continue since the later checks are
                    // more expensive
                    if !a.effects.contains(goal) {
                        continue
                    };

                    if action_accum.contains(a) {
                        available.insert(a.clone());
                        break
                    };

                    // Check if this action is mutex with any of
                    // the other accumulated actions
                    let mut acts = action_accum.clone();
                    acts.insert(a.clone());
                    let pairs = pairs_from_sets(
                        hashset!{a.clone()},
                        acts
                    );
                    let action_mutexes: Vec<PairSet<Action>> = mutexes
                        .clone()
                        .unwrap_or(HashSet::new())
                        .intersection(&pairs)
                        .into_iter()
                        .cloned()
                        .collect();

                    if action_mutexes.is_empty() {
                        available.insert(a.clone());
                    }
                };
                available
            };

            if available_actions.is_empty() {
                if goal_idx == 0 {
                    // Complete fail
                    break;
                }
                // Go back to previous goal
                println!("[debug] Unable to find actions for goal {:?}. Going back to previous goal...", goal_idx);
                stack.push_front((goal_idx - 1,));
            } else {
                // Add the action to the plan and continue
                let next_action = available_actions.iter()
                    .next()
                    .map(|i| {action_accum.insert(i.clone()); i})
                    .unwrap();
                println!("[debug] Selected action {:?}", next_action.clone());

                // Add to previous attempts in case we need to backtrack
                let mut remaining_actions = available_actions.clone();
                remaining_actions.remove(&next_action);
                attempts.insert(goal_idx, remaining_actions);

                // TODO if this action staisfies more than one
                // goal then handle that...

                // Proceed to the next goal
                if goal_idx < goal_len - 1 {
                    stack.push_front((goal_idx + 1,));
                } else {
                    goals_met = true;
                }
            };
        };

        if goals_met {
            Some((action_accum, attempts))
        } else {
            None
        }
    }
}

impl GraphPlanSolver for SimpleSolver {
    fn search<>(&self, plangraph: &PlanGraph) -> Option<Solution> {
        // The implementation is basically a recursive function where
        // we are using a stack and a loop rather than a recursive
        // function. Not sure how recursion works with Rust and the
        // allowed call depth so sticking with this approach for now
        let mut success = false;
        let mut plan = Solution::new();

        // Initialize the loop
        let mut stack: VecDeque<(usize, Vec<Proposition>)> = VecDeque::new();
        let init_goals: Vec<Proposition> = plangraph.goals.clone()
            .into_iter()
            .collect();
        let init_layer_idx = plangraph.layers.clone().len() - 1;
        stack.push_front((init_layer_idx, init_goals));

        while let Some((idx, goals)) = stack.pop_front() {
            println!("[debug] Working on layer {:?} with goals {:?}", idx, goals);
            // Note: This is a btreeset so ordering is guaranteed
            // TODO maybe remove the use of unwrap
            let actions = plangraph.actions_at_layer(idx - 1).unwrap();
            let mutexes = plangraph.mutex_actions.get(&(idx - 1)).cloned();

            if let Some((goal_actions, _attempts)) = SimpleSolver::action_combinations(goals.clone(), actions.clone(), mutexes) {
                println!("[debug] Found actions {:?}", goal_actions);
                // If we are are on the second to last proposition
                // layer, we are done
                if (idx - 2) == 0 {
                    plan.push(goal_actions.clone());
                    println!("[debug] Found plan! {:?}", plan);
                    success = true;
                } else {
                    plan.push(goal_actions.clone());
                    let next_goals = goal_actions.into_iter()
                        .flat_map(|action| action.reqs)
                        .unique()
                        .collect();
                    stack.push_front((idx - 2, next_goals));
                };
            } else {
                println!("[debug] Unable to find actions for goals {:?} from actions {:?}", goals, actions);
            }
        };

        if success {
            // Since this solver goes from the last layer to the
            // first, we need to reverse the plan
            plan.reverse();
            Some(plan)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod simple_solver_test {
    use super::*;

    #[test]
    fn action_combinations_single_goal() {
        let p1 = Proposition::from_str("coffee");
        let p2 = Proposition::from_str("caffeinated");
        let goals = vec![p2.clone()];
        let mut actions = BTreeSet::new();
        let a1 = Action::new(
            String::from("drink coffee"),
            hashset!{p1.clone()},
            hashset!{p2.clone()}
        );
        actions.insert(a1.clone());
        let mutexes = Some(MutexPairs::new());
        let attempts: HashMap<usize, BTreeSet<Action>> = hashmap!{0 => btreeset!{}};
        let expected = Some((hashset!{a1.clone()}, attempts));
        let actual = SimpleSolver::action_combinations(goals, actions, mutexes);
        assert_eq!(expected, actual);
    }

    #[test]
    fn action_combinations_multiple_goals() {
        let p1 = Proposition::from_str("coffee");
        let p2 = Proposition::from_str("caffeinated");
        let p3 = Proposition::from_str("breakfast");
        let p4 = Proposition::from_str("full");
        let goals = vec![p2.clone()];
        let mut actions = BTreeSet::new();
        let a1 = Action::new(
            String::from("drink coffee"),
            hashset!{p1.clone()},
            hashset!{p2.clone()}
        );
        actions.insert(a1.clone());

        let a2 = Action::new(
            String::from("eat breakfast"),
            hashset!{p3.clone()},
            hashset!{p4.clone()}
        );
        actions.insert(a2.clone());

        let mutexes = Some(MutexPairs::new());
        let attempts: HashMap<usize, BTreeSet<Action>> = hashmap!{0 => btreeset!{}};
        let expected = Some((hashset!{a1.clone(), a1.clone()}, attempts));
        let actual = SimpleSolver::action_combinations(goals, actions, mutexes);
        assert_eq!(expected, actual);
    }

    #[test]
    fn solver_works() {
        let p1 = Proposition::from_str("tired");
        let p2 = Proposition::from_str("dog needs to pee");
        let p3 = Proposition::from_str("caffeinated");

        let a1 = Action::new(
            String::from("coffee"),
            hashset!{p1.clone()},
            hashset!{p3.clone(), p1.clone().negate()}
        );

        let a2 = Action::new(
            String::from("walk dog"),
            hashset!{p2.clone(), p3.clone()},
            hashset!{p2.clone().negate()},
        );

        let mut pg = PlanGraph::new(
            hashset!{p1.clone(), p2.clone()},
            hashset!{p1.clone().negate(),
                     p2.clone().negate(),
                     p3.clone()},
            hashset!{a1.clone(), a2.clone()}
        );
        pg.extend();
        pg.extend();
        println!("Plangraph: {:?}", pg);

        let solver = SimpleSolver::new();
        let expected = Some(vec![hashset!{a1.clone()}, hashset!{a2.clone()}]);
        let actual = PlanGraph::format_plan(solver.search(&pg));
        assert_eq!(expected, actual);
    }
}

#[cfg(test)]
/// Prove that BTreeSet preserves ordering
mod btreeset_test {
    use super::*;

    #[test]
    fn is_sorted () {
        let mut b = BTreeSet::new();
        b.insert(2);
        b.insert(3);
        b.insert(1);
        assert_eq!(b.into_iter().collect::<Vec<_>>(), vec![1, 2, 3]);

        let mut b = BTreeSet::new();
        b.insert(1);
        b.insert(2);
        b.insert(3);
        assert_eq!(b.into_iter().collect::<Vec<_>>(), vec![1, 2, 3]);
    }
}

struct GraphPlan<T: GraphPlanSolver> {
    solver: T,
    data: PlanGraph,
}

impl<T: GraphPlanSolver> GraphPlan<T> {
    // fn new(solver: T) -> GraphPlan<T> {
    //     GraphPlan {
    //         solver: solver,
    //         data: PlanGraph::new()
    //     }
    // }
}

#[cfg(test)]
mod integration_test {
    use super::*;

    #[test]
    fn integration() {
        let p1 = Proposition::from_str("tired");
        let p2 = Proposition::from_str("dog needs to pee");
        let p3 = Proposition::from_str("caffeinated");

        let a1 = Action::new(
            String::from("coffee"),
            hashset!{p1.clone()},
            hashset!{p3.clone()}
        );

        let a2 = Action::new(
            String::from("walk dog"),
            hashset!{p2.clone(), p3.clone()},
            hashset!{p2.clone().negate()},
        );

        let mut pg = PlanGraph::new(
            hashset!{p1.clone(), p2.clone(), p3.clone()},
            hashset!{p1.clone().negate(),
                     p2.negate().clone(),
                     p3.clone()},
            hashset!{a1.clone(), a2.clone()}
        );

        let solver = SimpleSolver::new();
        assert!(pg.search_with(solver) != None, "Solution should not be None");
    }
}
