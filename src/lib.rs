#![crate_name = "graphplan"]
use std::collections::HashSet;
use std::iter::FromIterator;


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
#[macro_export]
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


#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct Proposition {
    name: &'static str,
    negation: bool,
}

impl Proposition {
    pub fn from_str(name: &'static str) -> Proposition {
        Proposition {name: name, negation: false}
    }

    pub fn negate(&mut self) -> Proposition {
        self.negation = !self.negation;
        self.to_owned()
    }

    pub fn is_negation(&mut self, prop: &Proposition) -> bool {
        prop.name == self.name && prop.negation != self.negation
    }
}

#[test]
fn propositions_can_be_negated() {
    let mut p1 = Proposition::from_str("test");
    assert!(false == p1.negation);
    assert!(true == p1.negate().negation);
    let mut p2 = Proposition::from_str("test");

    {
        assert!(p1.is_negation(&p2));
    }

    p2.negate();

    {
        assert!(false == p1.is_negation(&p2))
    };
}

// TODO: Support HashSet<Action>, need to do the hashing based on the
// name of the action. That would simplify a lot of the operations

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Action {
    name: &'static str,
    reqs: HashSet<Proposition>,
    effects: HashSet<Proposition>,
}

impl Action {
    pub fn new(name: &'static str, reqs: HashSet<Proposition>, effects: HashSet<Proposition>) -> Action {
        Action {name: name, reqs: reqs, effects: effects}
    }
}

pub type ActionLayerData = Vec<Action>;
pub type PropositionLayerData = HashSet<Proposition>;

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Layer {
    ActionLayer(ActionLayerData),
    PropositionLayer(PropositionLayerData),
}

impl Layer {
    /// Create a new layer from another. ActionLayer returns a
    /// PropositionLayer and PropositionLayer returns an ActionLayer
    /// # Example
    /// ```
    /// #[macro_use] extern crate graphplan;
    /// use graphplan::Layer;
    /// let prop_layer = Layer::PropositionLayer(hashset!{});
    /// Layer::from_layer(vec![], Option::None, prop_layer);
    /// ```
    pub fn from_layer(all_actions: Vec<Action>,
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
                            al.push(a)
                        }
                    }
                }
                Layer::ActionLayer(al)
            },
        }
    }
    /// Returns a set of propositions that are mutually exclusive
    pub fn proposition_mutexes(actions: Vec<Action>,
                               props: HashSet<Proposition>)
                               -> HashSet<Proposition> {

        props
    }

    pub fn action_mutexes(actions: Vec<Action>,
                          props: HashSet<Proposition>)
                          -> Vec<Action> {
        actions
    }
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

    let all_actions = vec![a1.clone(), a2.clone()];

    let props = hashset!{p1.clone(), p2.clone()};
    let prop_l1 = Layer::PropositionLayer(props.clone());
    let mutex_props_l1 = Layer::proposition_mutexes(
        vec![],
        props.clone()
    );

    // Generate the action layer
    let action_l1 = Layer::from_layer(
        all_actions.clone(),
        Option::None,
        prop_l1.clone()
    );
    let expected_action_l1 = Layer::ActionLayer(vec![a1.clone()]);
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
