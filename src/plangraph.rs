use std::fmt::{Debug, Display};
use std::collections::{HashMap, HashSet, BTreeSet};
use std::hash::Hash;
use log::{debug};
use crate::proposition::Proposition;
use crate::action::{Action, ActionType};
use crate::pairset::{pairs, PairSet};
use crate::solver::GraphPlanSolver;
use crate::layer::{Layer, MutexPairs, ActionLayerData, PropositionLayerData};


type LayerNumber = usize;
pub type Solution<'a, ActionId, PropositionId> = Vec<HashSet<&'a Action<'a, ActionId, PropositionId>>>;

#[derive(Debug)]
pub struct PlanGraph<'a,
                     ActionId: Eq + Hash + Ord + PartialOrd + Clone + Debug,
                     PropositionId: Eq + Hash + Ord + PartialOrd + Clone + Debug + Display> {
    pub goals: HashSet<&'a Proposition<PropositionId>>,
    pub actions: HashSet<&'a Action<'a, ActionId, PropositionId>>,
    pub layers: Vec<Layer<'a, ActionId, PropositionId>>,
    pub mutex_props: HashMap<LayerNumber, MutexPairs<&'a Proposition<PropositionId>>>,
    pub mutex_actions: HashMap<LayerNumber, MutexPairs<&'a Action<'a, ActionId, PropositionId>>>,
}

impl<'a,
     ActionId: Eq + Hash + Ord + PartialOrd + Clone + Debug,
     PropositionId: Eq + Hash + Ord + PartialOrd + Clone + Debug + Display>
    PlanGraph<'a, ActionId, PropositionId> {
    pub fn new(initial_props: HashSet<&'a Proposition<PropositionId>>,
               goals: HashSet<&'a Proposition<PropositionId>>,
               actions: HashSet<&'a Action<ActionId, PropositionId>>) -> Self {
        let init_layer = Layer::PropositionLayer(initial_props);
        PlanGraph {
            goals,
            actions,
            layers: vec![init_layer],
            mutex_props: HashMap::new(),
            mutex_actions: HashMap::new()
        }
    }

    pub fn push(&mut self, layer: Layer<'a, ActionId, PropositionId>) {
        self.layers.push(layer)
    }

    /// Extends the plangraph to depth i+1
    /// Inserts another action layer and proposition layer
    pub fn extend(&mut self) {
        let layers = &self.layers;
        let actions = &self.actions;
        let length = layers.len();

        let layer = layers.last()
            .expect("Tried to extend a plangraph that is not initialized. Please use PlanGraph::new instead of instantiating it as a struct.");

        let mutex_props = self.mutex_props.get(&(length - 1));
        let actions_no_mutex_reqs = if let Some(mux) = mutex_props {
            // Filter out the actions that we know are mutex
            actions
                .iter()
                .filter(|a| {
                    pairs(&a.reqs)
                        .intersection(mux)
                        .next()
                        .is_none()})
                .map(|i| *i)
                .collect::<HashSet<&'a Action<'a, ActionId, PropositionId>>>()
        } else {
            actions.to_owned()
        };


        // TODO: For some reason from_layer messes up lifetime
        // inference when called this way vs inlined
        // let action_layer = Layer::from_layer(
        //     actions_no_mutex_reqs,
        //     &layer
        // );
        let props = match layer {
            Layer::PropositionLayer(props) => props,
            _ => unreachable!()
        };
        let action_layer = {
            let mut layer_data = ActionLayerData::new();

            for a in actions_no_mutex_reqs {
                // Include action if it satisfies one or more props
                if a.reqs.is_subset(props) {
                    layer_data.insert(a);
                }
            }

            // TODO: Move creation of maintenance actions out of
            // here otherwise it will conflict with the lifetime
            // 'a
            // Add in maintenance actions for all props
            // for p in props {
            //     layer_data.insert(Action::new_maintenance(p.to_owned()));
            // }

            Layer::ActionLayer(layer_data)
        };

        // let prop_layer = Layer::from_layer(
        //     self.actions,
        //     &action_layer
        // );

        let prop_layer = {
            let mut layer_data = PropositionLayerData::new();
            for a in actions {
                for e in a.effects.iter() {
                    layer_data.insert(e);
                }
            }

            Layer::PropositionLayer(layer_data)
        };

        let action_layer_actions = match &action_layer {
            Layer::ActionLayer(action_data) => action_data,
            _ => unreachable!("Tried to get actions from PropositionLayer")
        };
        let action_mutexes = Layer::action_mutexes(
            action_layer_actions,
            mutex_props
        );
        self.mutex_actions.insert(
            length,
            action_mutexes.clone().into_iter().collect()
        );

        let prop_layer_props = match &prop_layer {
            Layer::PropositionLayer(prop_data) => prop_data,
            _ => unreachable!("Tried to get propositions from ActionLayerr")
        };
        let prop_mutexes = Layer::proposition_mutexes(
            prop_layer_props,
            action_layer_actions,
            Some(action_mutexes)
        );
        self.mutex_props.insert(length, prop_mutexes);
        self.layers.push(action_layer);
        self.layers.push(prop_layer);
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
    pub fn actions_at_layer(&self, index: usize) -> Result<BTreeSet<&Action<ActionId, PropositionId>>, String> {
        self.layers.get(index).map_or(
            Err(format!("Layer {} does not exist", index)),
            |layer| {
                match layer {
                    Layer::ActionLayer(actions) => {
                        let acts = actions
                           .iter()
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

    /// The graph is considered to have "leveled off" when proposition
    /// layer P and an adjacent proposition layer Q are equal
    fn has_leveled_off(&self) -> bool {
        let len = self.layers.len();
        if len > 2 as usize {
            let prop_layer = self.layers.get(len - 1).expect("Failed to get layer");
            let adjacent_prop_layer = self.layers.get(len - 3).expect("Failed to get adjacent layer");
            prop_layer == adjacent_prop_layer
        } else {
            false
        }
    }
}
