#![allow(dead_code)]
use std::collections::HashSet;
use std::iter::FromIterator;


#[derive(Hash, Eq, PartialEq, Clone, Debug)]
struct Proposition {
    name: &'static str,
}

impl Proposition {
    pub fn new(name: &'static str) -> Proposition {
        Proposition {name: name}
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
struct Action {
    name: &'static str,
    reqs: HashSet<Proposition>,
    outs: HashSet<Proposition>,
}

impl Action {
    pub fn new(name: &'static str) -> Action {
        Action {name: name, reqs: HashSet::new(), outs: HashSet::new()}
    }
}

type ActionLayerData = Vec<Action>;
type PropositionLayerData = Vec<Proposition>;

#[derive(Eq, PartialEq, Clone, Debug)]
enum Level {
    ActionLayer(ActionLayerData),
    PropositionLayer(PropositionLayerData),
}

impl Level {
    // Unimplemented
    // Convert a layer from one kind to another
    fn from_level(all_actions: Vec<Action>,
                  _all_propositions: Vec<Proposition>,
                  level: Level) -> Level {
        match &level {
            Level::ActionLayer(_actions) => {
                level.clone()
            },
            Level::PropositionLayer(props) => {
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
                Level::ActionLayer(al)
            },
        }
    }
}

struct PlanGraph {
    levels: Vec<Level>
}

impl PlanGraph {
    pub fn new() -> Self {
        PlanGraph {
            levels: Vec::new()
        }
    }

    pub fn push(&mut self, level: Level) {
        self.levels.push(level.clone())
    }
}

#[test]
fn it_works() {
    let mut pg = PlanGraph::new();
    let p1 = Proposition::new("tired");
    let p2 = Proposition::new("dog needs to pee");
    let p3 = Proposition::new("caffeinated");
    let p4 = Proposition::new("dog empty");

    let mut a1 = Action::new("coffee");
    a1.reqs.insert(p1.clone());
    a1.outs.insert(p3.clone());

    let mut a2 = Action::new("walk dog");
    a2.reqs.insert(p2.clone());
    a2.reqs.insert(p3.clone());
    a1.outs.insert(p4.clone());

    let props = vec![p1.clone(), p2.clone()];
    let prop_l1 = Level::PropositionLayer(props);

    // Generate the action layer
    let action_l1 = Level::from_level(
        vec![a1.clone(), a2.clone()],
        vec![p1.clone(), p2.clone(), p3.clone(), p4.clone()],
        prop_l1.clone()
    );
    let expected_action_l1 = Level::ActionLayer(vec![a1.clone()]);
    assert!(expected_action_l1 == action_l1,
            format!("{:?} != {:?}", expected_action_l1, action_l1));

    // Insert the layers
    pg.push(prop_l1);
    pg.push(action_l1);

    assert!(pg.levels.len() == 2);
}
