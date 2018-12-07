use std::collections::{HashSet, HashMap};
use std::iter::FromIterator;
use std::fmt;
use std::cmp::{Ordering};
use std::hash::{Hash,Hasher};

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

#[derive(Eq, PartialEq, Ord, PartialOrd, Clone)]
pub struct Proposition {
    name: &'static str,
    negation: bool,
}

impl fmt::Debug for Proposition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
        {
            assert!(false == p1.negation);
        }

        assert!(true == Proposition::from_str("test").negate().negation);

        let p2 = Proposition::from_str("test").negate();
        {
            assert!(
                p2.is_negation(&p1),
                format!("{:?} is not a negation of {:?}", p1, p2)
            );
        }
        {
            assert!(p1.is_negation(&p2));
        }
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "A:{} {{req: {:?} effects: {:?}}}", self.name, self.reqs, self.effects)
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

/// Returns the pairs of a set of items
pub fn pairs<T: Eq + Hash + Clone + Ord>(items: Vec<T>) -> Vec<PairSet<T>> {
    let mut accum = Vec::new();

    // TODO maybe try to borrow this instead?
    let mut sorted = items.clone();
    sorted.sort();

    for i in sorted.iter().cloned() {
        for j in sorted.iter().cloned() {
            if i != j && j > i {
                accum.push(PairSet(i.clone(), j.clone()));
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
            vec![PairSet(p1.clone(), p2.clone()),
                 PairSet(p1.clone(), p3.clone()),
                 PairSet(p2.clone(), p3.clone())],
            pairs(vec![p1.clone(), p2.clone(), p3.clone()])
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
    pub fn from_layer(all_actions: HashSet<Action>,
                      layer: Layer)
                      -> Layer {
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
                {
                    for a in all_actions {
                        // if a has all props push it to layer
                        if a.reqs.is_subset(&props_hash) {
                            layer_data.insert(a);
                        }
                    }
                }

                // Add in maintenance actions for all props
                for p in props {
                    layer_data.insert(
                        Action::new(
                            format!("[maintain] {}", p.name),
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
        let action_vec: Vec<Action> = actions.into_iter().collect();
        // TODO maybe pairs can take any Iterable instead
        let action_pairs = pairs(action_vec);

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
    /// Uses btrees because they are hashable due to insertion order
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
            let prop_vec: Vec<Proposition> = props.into_iter().collect();
            for PairSet(p1, p2) in pairs(prop_vec) {
                let viable_acts: Vec<Action> = actions.clone()
                    .into_iter()
                    .filter(|a| a.effects.contains(&p1) || a.effects.contains(&p2))
                    .collect();

                let viable_act_pairs: HashSet<PairSet<Action>> = pairs(viable_acts).into_iter().collect();

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

type Solution = Vec<Vec<Action>>;

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

    pub fn actions_at_layer(&self, index: usize) -> Result<HashSet<Action>, String> {
        self.layers.get(index).map_or(
            Err(format!("Layer {} does not exist", index)),
            |layer| {
                match layer {
                    Layer::ActionLayer(actions) => Ok(actions.clone()),
                    Layer::PropositionLayer(_) => {
                        Err(format!("Tried to get actions from proposition layer {}",
                                    index))
                    }
                }
            }
        )
    }

    fn has_possible_solution(&self) -> bool {
        let last_layer_idx = self.layers.clone().len() - 1;
        if let Some(prop_layer) = self.layers.get(last_layer_idx) {
            match prop_layer {
                Layer::PropositionLayer(props) => self.goals.is_subset(props),
                _ => false
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
        let mut result = None;
        // TODO maybe make this configurable
        let max_tries = self.actions.len() + 1;
        let mut tries = 0;

        while tries < max_tries {
            if self.has_possible_solution() {
                if let Some(solution) = solver.search(self) {
                    result = Some(solution);
                    break;
                }
            } else {
                tries += 1;
                self.extend();
            }
        }
        result
    }
}

pub trait GraphPlanSolver {
    /// Searches a plangraph for a collection of actions that
    /// satisfies the goals
    fn search(&self, plangraph: &PlanGraph) -> Option<Solution>;
}

struct SimpleSolver;

impl SimpleSolver {
    fn new() -> SimpleSolver {
        SimpleSolver {}
    }
}

impl GraphPlanSolver for SimpleSolver {
    fn search(&self, plangraph: &PlanGraph) -> Option<Solution> {
        let mut stack: Vec<(usize, HashSet<Proposition>, HashSet<Action>, HashSet<Action>)> = Vec::new();
        let mut index = plangraph.layers.len() - 1;
        let mut plan = Solution::new();

        // Add the end goals to the stack
        // Stack is a type of the (layer index, goal props, attempts)
        stack.push((index, plangraph.goals.clone(), HashSet::new(), HashSet::new()));

        while !stack.is_empty() {
            stack.pop().map(|(idx, goals, accum, attempts)| {
                let actions = plangraph.actions_at_layer(idx - 1).unwrap();
                for g in goals {
                    // Only actions that produce the goal and are not
                    // mutex with any other actions and have not
                    // already been attempted in combination with the
                    // other attempted actions and are not mutex with
                    // any other action
                    let available_actions: Vec<Action> = actions.clone()
                        .into_iter()
                        .filter(|a| {
                            let mut acts = accum.clone();
                            acts.insert(a.clone());
                            let mutexes = plangraph.mutex_actions
                                .get(&(idx - 1))
                                .unwrap();
                            let pairs = pairs_from_sets(hashset!{a.clone()}, acts);
                            let action_mutexes: Vec<PairSet<Action>> = mutexes
                                .intersection(&pairs)
                                .into_iter()
                                .cloned()
                                .collect();
                            a.effects.contains(&g) &&
                                !attempts.contains(&a) &&
                                action_mutexes.is_empty()
                        })
                        .collect();
                    if available_actions.is_empty() {
                        break;
                    } else {

                    }
                }
            });
        }

        if plan.is_empty() {
            None
        } else {
            Some(plan)
        }
    }
}

struct GraphPlan<T: GraphPlanSolver> {
    solver: T,
    data: PlanGraph,
}


#[cfg(test)]
mod plangraph_test {
    use super::*;

    #[test]
    fn is_goal_satisfield () {
    }
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
