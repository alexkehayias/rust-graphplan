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

#[macro_export]
/// Create a **BTreeSet** from a list of elements. Implementation
/// copied from the maplit library https://github.com/bluss/maplit
///
/// ## Example
///
/// ```
/// #[macro_use] extern crate graphplan;
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


#[derive(Eq, PartialEq, Clone, Ord, PartialOrd)]
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
/// A two element tuple that, when constructed via `new`, guarantees that
/// (a, b) == (b, a)
pub struct UniquePair<T: Ord + PartialEq + Eq>(T, T);

impl <T> UniquePair<T> where T: Ord + PartialEq + Eq{
    /// Constructs a UniquePair, insertion order is guaranteed so UniquePair p1, p2
    /// is equivalent UniquePair p2, p1
    fn new(item1: T, item2: T) -> UniquePair<T> {
        // Can't think of a more elegant way to do this so since n is 2 can
        // just stick this in a conditional
        if item1 < item2 {
            UniquePair(item1, item2)
        } else {
            UniquePair(item2, item1)
        }
    }
}

#[test]
fn uniq_pair_test() {
    assert_eq!(UniquePair::<&'static str>::new("test1", "test2"),
               UniquePair::<&'static str>::new("test2", "test1"));
}

/// Returns the pairs of a set of items
pub fn pairs<T: Eq + Hash + Clone + Ord>(items: Vec<T>) -> Vec<UniquePair<T>> {
    let mut accum = Vec::new();

    // TODO maybe try to borrow this instead?
    let mut sorted = items.clone();
    sorted.sort();

    for i in sorted.iter().cloned() {
        for j in sorted.iter().cloned() {
            if i != j && j > i {
                accum.push(UniquePair::new(i.clone(), j.clone()));
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
        vec![UniquePair::new(p1.clone(), p2.clone()),
             UniquePair::new(p1.clone(), p3.clone()),
             UniquePair::new(p2.clone(), p3.clone())],
        pairs(vec![p1.clone(), p2.clone(), p3.clone()])
    );
}

type MutexPairs<T> = HashSet<UniquePair<T>>;

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
        let mutexes = MutexPairs::new();
        let action_vec: Vec<Action> = actions.into_iter().collect();
        let action_pairs = pairs(action_vec);

        for UniquePair(a1, a2) in action_pairs {
            // TODO Inconsistent effects: The effect of one action is the
            // negation of another
            // - Negate the effects of one of the actions then check the overlap
            // - If there is any overlap the two actions are mutex

            // TODO Interference: One action deletes the precondition of
            // another action (they can't be done in parallel then)
        }





        // TODO Competing needs: Another action has precondition that
        // is mutex

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
                mutexes.insert(UniquePair::new(p.clone(), not_p.clone()));
            }
        }

        // Find mutexes where all ways of achieving p are mutex
        // - Get all uniq pairs of propositions
        // - For each pair, get all uniq action pairs that could output the prop pair
        // - Set difference with the mutex_actions
        // - If there is no difference then the props are mutex
        if let Some(mx_actions) = mutex_actions {
            let prop_vec: Vec<Proposition> = props.into_iter().collect();
            for UniquePair(p1, p2) in pairs(prop_vec) {
                let viable_acts: Vec<Action> = actions.clone()
                    .into_iter()
                    .filter(|a| a.effects.contains(&p1) || a.effects.contains(&p2))
                    .collect();

                let viable_act_pairs: HashSet<UniquePair<Action>> = pairs(viable_acts).into_iter().collect();

                let diff: HashSet<_> = viable_act_pairs
                    .difference(&mx_actions)
                    .collect();

                if diff.is_empty() && !mx_actions.is_empty() {
                    mutexes.insert(UniquePair::new(p1.clone(), p2.clone()));
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
        UniquePair::new(Proposition::from_str("caffeinated"),
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
        UniquePair::new(
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
        UniquePair::new(Proposition::from_str("caffeinated"),
                   Proposition::from_str("coffee"))
    };
    assert_eq!(
        expected,
        Layer::proposition_mutexes(props, actions, Some(action_mutexes))
    );
}

#[test]
fn action_mutexes() {
    let actions = hashset!{
        Action::new("drink coffee",
                    hashset!{},
                    hashset!{})
    };
    let props = MutexPairs::new();
    let expected = MutexPairs::new();
    assert_eq!(expected, Layer::action_mutexes(actions, Some(props)));
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
