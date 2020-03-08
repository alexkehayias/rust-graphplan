use std::fmt::{Debug, Display};
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use crate::proposition::Proposition;
use crate::action::Action;
use crate::pairset::pairs;
use crate::layer::{Layer, MutexPairs};


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
    pub fn extend(&mut self) -> &mut Self {
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
                .copied()
                .collect::<HashSet<&'a Action<'a, ActionId, PropositionId>>>()
        } else {
            actions.to_owned()
        };

        let action_layer = Layer::from_layer(
            &actions_no_mutex_reqs,
            &layer
        );

        let prop_layer = Layer::from_layer(
            &actions,
            &action_layer
        );

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

        self
    }

    /// Returns the depth of the planning graph
    pub fn depth(&self) -> usize {
        if self.layers.len() > 2 {
            (self.layers.len() - 1) / 2
        } else {
            0
        }
    }

    /// A solution is possible if all goals exist in the last
    /// proposition layer and are not mutex
    pub fn has_possible_solution(&self) -> bool {
        let goals = self.goals.clone();
        let last_layer_idx = self.layers.len() - 1;

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
    pub fn has_leveled_off(&self) -> bool {
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
