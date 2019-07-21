use std::fmt::Debug;
use std::collections::{HashMap, HashSet, BTreeSet};
use std::hash::Hash;
use log::{debug};
use crate::proposition::Proposition;
use crate::action::{Action, ActionType};
use crate::pairset::{pairs, PairSet};
use crate::solver::GraphPlanSolver;
use crate::layer::{Layer, MutexPairs};


type LayerNumber = usize;
pub type Solution<ActionId> = Vec<HashSet<Action<ActionId>>>;

#[derive(Debug)]
pub struct PlanGraph<ActionId: Eq + Hash + Ord + PartialOrd + Clone + Debug> {
    pub goals: HashSet<Proposition>,
    pub actions: HashSet<Action<ActionId>>,
    pub layers: Vec<Layer<ActionId>>,
    pub mutex_props: HashMap<LayerNumber, MutexPairs<Proposition>>,
    pub mutex_actions: HashMap<LayerNumber, MutexPairs<Action<ActionId>>>,
}

impl<ActionId: Eq + Hash + Ord + PartialOrd + Clone + Debug> PlanGraph<ActionId> {
    pub fn new(initial_props: HashSet<Proposition>,
               goals: HashSet<Proposition>,
               actions: HashSet<Action<ActionId>>) -> Self {
        let init_layer = Layer::PropositionLayer(initial_props);
        PlanGraph {
            goals: goals,
            actions: actions,
            layers: vec![init_layer],
            mutex_props: HashMap::new(),
            mutex_actions: HashMap::new()
        }
    }

    pub fn push(&mut self, layer: Layer<ActionId>) {
        self.layers.push(layer)
    }

    /// Extends the plangraph to depth i+1
    /// Inserts another action layer and proposition layer
    pub fn extend(&mut self) {
        let length = self.layers.len();
        if let Some(layer) = self.layers.last() {
            match layer {
                Layer::ActionLayer(_) => {
                    panic!("Tried to extend a plangraph from an ActionLayer which is not allowed")
                },
                Layer::PropositionLayer(props) => {
                    let mutex_props = self.mutex_props.get(&(length - 1));
                    let actions_no_mutex_reqs = mutex_props
                        .map(|mp| {
                            self.actions.iter()
                                .filter(|action| {
                                    pairs(&action.reqs).intersection(&mp)
                                        .collect::<Vec<_>>()
                                        .is_empty()
                                })
                                .collect::<HashSet<&Action<_>>>()
                        })
                        .unwrap_or(self.actions.iter().collect());

                    let p_layer = Layer::PropositionLayer(props.to_owned());
                    let action_layer = Layer::from_layer(
                        actions_no_mutex_reqs,
                        &p_layer
                    );
                    let action_layer_actions: HashSet<&Action<_>> = match &action_layer {
                        Layer::ActionLayer(action_data) => action_data.iter().collect(),
                        _ => unreachable!("Tried to get actions from PropositionLayer")
                    };
                    let action_mutexes = Layer::action_mutexes(
                        &action_layer_actions,
                        mutex_props
                    );
                    self.mutex_actions.insert(
                        self.layers.len(),
                        action_mutexes.clone()
                            .into_iter()
                            .map(|i| {
                                let PairSet(a, b) = i;
                                PairSet(a.to_owned(), b.to_owned())
                            })
                            .collect()
                    );

                    let prop_layer = Layer::from_layer(
                        self.actions.iter().collect(),
                        &action_layer
                    );
                    let prop_layer_props = match &prop_layer {
                        Layer::PropositionLayer(prop_data) => Some(prop_data),
                        _ => unreachable!("Tried to get propositions from ActionLayerr")
                    };
                    let prop_mutexes = Layer::proposition_mutexes(
                        // This shouldn't fail
                        &prop_layer_props.unwrap(),
                        &action_layer_actions,
                        Some(&action_mutexes)
                    );
                    self.layers.push(action_layer);
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
    pub fn actions_at_layer(&self, index: usize) -> Result<BTreeSet<Action<ActionId>>, String> {
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

    /// Searches the planning graph for a solution using the solver if
    /// there is no solution, extends the graph to depth i+1 and tries
    /// to solve again
    pub fn search_with<T>(&mut self, solver: &T) -> Option<Solution<ActionId>>
    where T: GraphPlanSolver<ActionId> {
        let mut tries = 0;
        let mut solution = None;
        let max_tries = self.actions.len() + 1;

        while tries < max_tries {
            // This doesn't provide early termination for _all_
            // cases that won't yield a solution.
            if self.has_leveled_off() {
                break;
            }
            if self.has_possible_solution() {
                if let Some(s) = solver.search(self) {
                    solution = Some(s);
                    break;
                } else {
                    debug!("No solution found at depth {}", self.depth());
                    self.extend();
                    tries += 1
                }
            } else {
                debug!("No solution exists at depth {}", self.depth());
                self.extend();
                tries += 1;
            }
        };
        solution
    }

    /// Takes a solution and filters out maintenance actions
    pub fn format_plan(solution: Solution<ActionId>) -> Solution<ActionId> {
        solution.iter()
            .map(|s| s.iter()
                 .filter(|i| match i.id {
                     ActionType::Action(_) => true,
                     ActionType::Maintenance(_) => false})
                 .cloned()
                 .collect())
            .collect()
    }
}
