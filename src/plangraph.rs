use std::collections::{HashMap, HashSet, BTreeSet};
use log::{debug};
use crate::proposition::Proposition;
use crate::action::Action;
use crate::pairset::{pairs};
use crate::solver::GraphPlanSolver;
use crate::layer::{Layer, MutexPairs};


type LayerNumber = usize;
type QueryActions = Result<BTreeSet<Action>, String>;
pub type Solution = Vec<HashSet<Action>>;

#[derive(Debug)]
pub struct PlanGraph {
    pub goals: HashSet<Proposition>,
    pub actions: HashSet<Action>,
    pub layers: Vec<Layer>,
    pub mutex_props: HashMap<LayerNumber, MutexPairs<Proposition>>,
    pub mutex_actions: HashMap<LayerNumber, MutexPairs<Action>>,
}

impl PlanGraph {
    pub fn new(initial_props: HashSet<Proposition>,
               goals: HashSet<Proposition>,
               actions: HashSet<Action>) -> Self {
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
                    let mutex_props = self.mutex_props.get(&(length - 1));
                    let borrowed_all_actions: HashSet<&Action> = self.actions
                        .iter()
                        .collect();
                    let actions_no_mutex_reqs = mutex_props
                        .map(|mp| {
                            self.actions.iter()
                                .filter(|action| {
                                    pairs(&action.reqs).intersection(&mp)
                                        .collect::<Vec<_>>()
                                        .is_empty()
                                })
                                .collect::<HashSet<_>>()
                        })
                        .unwrap_or(borrowed_all_actions.clone());

                    let action_layer = Layer::from_layer(
                        actions_no_mutex_reqs,
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
                        borrowed_all_actions,
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
    pub fn search_with<T>(&mut self, solver: &T) -> Option<Solution>
    where T: GraphPlanSolver {
        let mut tries = 0;
        let mut solution = None;
        let max_tries = self.actions.clone().len() + 1;

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
