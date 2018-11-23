#![crate_name = "graphplan"]
use std::collections::HashSet;
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
        Proposition {name: name, negation: false}
    }

    pub fn negate(&self) -> Proposition {
        Proposition { name: self.name, negation: !self.negation }
    }

    pub fn is_negation(&self, prop: &Proposition) -> bool {
        prop.name == self.name && prop.negation == !self.negation
    }
}

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

#[derive(Eq, PartialEq, Clone)]
pub struct Action {
    name: &'static str,
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
    pub fn new(name: &'static str, reqs: HashSet<Proposition>, effects: HashSet<Proposition>) -> Action {
        Action {name: name, reqs: reqs, effects: effects}
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
/// An unordered two element tuple that, when constructed via `new`,
/// guarantees that (a, b) == (b, a)
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
        let left;
        if &self.0 < &self.1 {
            left = (self.0.clone(), self.1.clone());
        } else {
            left = (self.1.clone(), self.0.clone());
        }

        let right;
        if &other.0 < &other.1 {
            right = (other.0.clone(), other.1.clone());
        } else {
            right = (other.1.clone(), other.0.clone());
        }

        left == right
    }
}

#[test]
fn uniq_pair_test() {
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
pub fn pairs_from_sets<T: Eq + Hash + Clone + Ord>(items1: Vec<T>, items2: Vec<T>)
                                                   -> Vec<PairSet<T>> {
    let mut accum = Vec::new();

    let mut sorted1 = items1.clone();
    sorted1.sort();

    let mut sorted2 = items2.clone();
    sorted2.sort();

    for i in sorted1.iter().cloned() {
        for j in sorted2.iter().cloned() {
            if i != j && j > i {
                accum.push(PairSet(i.clone(), j.clone()));
            }
        }
    }
    accum
}

#[test]
pub fn test_pairs() {
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

type MutexPairs<T> = HashSet<PairSet<T>>;

impl Layer {
    /// Create a new layer from another. ActionLayer returns a
    /// PropositionLayer and PropositionLayer returns an ActionLayer
    /// # Example
    /// ```
    /// #[macro_use] extern crate graphplan;
    /// use graphplan::Layer;
    /// let prop_layer = Layer::PropositionLayer(hashset!{});
    /// Layer::from_layer(hashset![], Option::None, prop_layer);
    /// ```
    pub fn from_layer(all_actions: HashSet<Action>,
                      prev_layer: Option<Layer>,
                      layer: Layer)
                      -> Layer {
        match &layer {
            Layer::ActionLayer(actions) => {
                let mut effects = PropositionLayerData::new();
                for a in actions {
                    for e in a.effects.iter() {
                        effects.insert(e.clone());
                    }
                }

                // Add in maintenance action effects for initial props
                if let Some(prop_layer) = prev_layer {
                    match prop_layer {
                        Layer::ActionLayer(_) => (),
                        Layer::PropositionLayer(props) => {
                            for p in props.iter() {
                                effects.insert(p.clone());
                            }
                        }
                    }
                }

                Layer::PropositionLayer(effects)
            },
            Layer::PropositionLayer(props) => {
                let props_hash = HashSet::from_iter(props.iter().cloned());
                let mut al = ActionLayerData::new();
                {
                    for a in all_actions {
                        // if a has all props push it to layer
                        if a.reqs.is_subset(&props_hash) {
                            al.insert(a);
                        }
                    }
                }
                Layer::ActionLayer(al)
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
                let a1_req_vec: Vec<Proposition> = a1.clone().reqs
                    .into_iter()
                    .collect();
                let a2_req_vec: Vec<Proposition> = a2.clone().reqs
                    .into_iter()
                    .collect();
                let req_pairs = pairs_from_sets(a1_req_vec, a2_req_vec);
                let req_pair_set:HashSet<PairSet<Proposition>> = req_pairs
                    .into_iter()
                    .collect();
                let competing_needs: HashSet<PairSet<Proposition>> = req_pair_set
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

#[test]
fn proposition_hashing_works() {
    let set = hashset!{Proposition::from_str("caffeinated")};
    assert!(set.contains(&Proposition::from_str("caffeinated")));

    let set = hashset!{Proposition::from_str("caffeinated").negate()};
    assert!(set.contains(&Proposition::from_str("caffeinated").negate()));

    let set = hashset!{Proposition::from_str("caffeinated").negate()};
    assert!(!set.contains(&Proposition::from_str("caffeinated")));
}

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
                "drink coffee",
                hashset!{Proposition::from_str("coffee")},
                hashset!{Proposition::from_str("caffeinated"),
                         Proposition::from_str("coffee").negate()},
            ),
            Action::new(
                "make coffee",
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
        "drink coffee",
        hashset!{},
        hashset!{Proposition::from_str("coffee").negate()}
    );

    let a2 = Action::new(
        "make coffee",
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
        "eat sandwich",
        hashset!{Proposition::from_str("hungry")},
        hashset!{Proposition::from_str("hungry").negate()}
    );

    let a2 = Action::new(
        "eat soup",
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
        "eat sandwich",
        hashset!{Proposition::from_str("hungry")},
        hashset!{Proposition::from_str("hungry").negate()}
    );

    let a2 = Action::new(
        "eat soup",
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

pub struct PlanGraph {
    layers: Vec<Layer>
}

impl PlanGraph {
    pub fn new() -> Self {
        PlanGraph {
            layers: Vec::new()
        }
    }

    pub fn push(&mut self, layer: Layer) {
        self.layers.push(layer.clone())
    }
}

#[test]
fn integration() {
    let mut pg = PlanGraph::new();
    let p1 = Proposition::from_str("tired");
    let p2 = Proposition::from_str("dog needs to pee");
    let p3 = Proposition::from_str("caffeinated");

    let a1 = Action::new(
        "coffee",
        hashset!{p1.clone()},
        hashset!{p3.clone()}
    );

    let a2 = Action::new(
        "walk dog",
        hashset!{p2.clone(), p3.clone()},
        hashset!{p2.clone().negate()},
    );

    let all_actions = hashset!{a1.clone(), a2.clone()};

    let props = hashset!{p1.clone(), p2.clone()};
    let prop_l1 = Layer::PropositionLayer(props.clone());
    // let mutex_props_l1 = Layer::proposition_mutexes(
    //     vec![],
    //     props.clone()
    // );

    // Generate the action layer
    let action_l1 = Layer::from_layer(
        all_actions.clone(),
        Option::None,
        prop_l1.clone()
    );
    let expected_action_l1 = Layer::ActionLayer(hashset!{a1.clone()});
    assert!(expected_action_l1 == action_l1,
            format!("{:?} != {:?}", expected_action_l1, action_l1));

    // Generate the next proposition layer
    let prop_l2 = Layer::from_layer(
        all_actions.clone(),
        Some(prop_l1.clone()),
        action_l1.clone()
    );
    let expected_prop_l2 = Layer::PropositionLayer(
        hashset![p1.clone(), p2.clone(), p3.clone()]
    );
    assert!(expected_prop_l2 == prop_l2,
            format!("{:?} != {:?}", expected_prop_l2, prop_l2));

    // Insert the layers
    pg.push(prop_l1);
    pg.push(action_l1);
    pg.push(prop_l2);

    assert!(pg.layers.len() == 3);
}
