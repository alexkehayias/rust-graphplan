use std::collections::{HashSet};
use std::hash::Hash;
use std::fmt::{Debug, Display};
use log::{debug};

#[cfg(any(feature = "toml", feature = "wasm"))]
#[macro_use] extern crate serde_derive;

#[cfg(any(feature = "toml", feature = "wasm"))]
use std::fs;

#[cfg(any(feature = "toml", feature = "wasm"))]
extern crate serde;

#[cfg(any(feature = "toml", feature = "wasm"))]
use toml_crate as toml;


#[macro_use] pub mod macros;
pub mod proposition;
pub mod action;
pub mod plangraph;
pub mod solver;
mod layer;
mod pairset;

pub use crate::proposition::Proposition;
pub use crate::action::{Action, ActionType};
pub use crate::plangraph::{PlanGraph, Solution};
pub use crate::solver::{GraphPlanSolver, SimpleSolver};

#[cfg(any(feature = "toml", feature = "wasm"))]
#[derive(Deserialize)]
struct Config {
    initial: Vec<String>,
    goals: Vec<String>,
    actions: Vec<ConfigAction>
}

#[cfg(any(feature = "toml", feature = "wasm"))]
#[derive(Deserialize)]
struct ConfigAction {
    name: String,
    reqs: Vec<String>,
    effects: Vec<String>,
}

pub struct GraphPlan<'a,
                     ActionId: Debug + Hash + Ord + Clone,
                     PropositionId: Debug + Display + Hash + Ord + Clone> {
    plangraph: PlanGraph<'a, ActionId, PropositionId>,
}

impl<'a,
     ActionId: Debug + Hash + Ord + Clone,
     PropositionId: Debug + Display + Hash + Ord + Clone>
    GraphPlan<'a, ActionId, PropositionId> {
    pub fn new(initial_props: HashSet<&'a Proposition<PropositionId>>,
               goals: HashSet<&'a Proposition<PropositionId>>,
               actions: HashSet<&'a Action<'a, ActionId, PropositionId>>)
               -> GraphPlan<'a, ActionId, PropositionId> {
        // TODO: add in maintenance actions for all props
        let plangraph = PlanGraph::new(
            initial_props,
            goals,
            actions,
        );
        GraphPlan { plangraph }
    }

    pub fn search<Solver>(&mut self) -> Option<Solution<'a, ActionId, PropositionId>>
        where Solver: GraphPlanSolver<'a, ActionId, PropositionId> {

        let mut tries = 0;
        let mut solution = None;
        let max_tries = self.plangraph.actions.len() + 1;

        while tries < max_tries {
            self.plangraph.extend();
            // This doesn't provide early termination for _all_
            // cases that won't yield a solution.
            if self.plangraph.has_leveled_off() {
                break;
            }

            if !self.plangraph.has_possible_solution() {
                debug!("No solution exists at depth {}", self.plangraph.depth());
                tries += 1;
                continue
            }

            if let Some(result) = Solver::search(&self.plangraph) {
                solution = Some(result);
                break;
            } else {
                debug!("No solution found at depth {}", self.plangraph.depth());
                tries += 1
            }
        };

        solution
    }

    /// Takes a solution and filters out maintenance actions
    pub fn format_plan(solution: Solution<ActionId, PropositionId>) -> Solution<ActionId, PropositionId> {
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

impl<'a> GraphPlan<'a, String, String> {
    #[cfg(any(feature = "toml", feature = "wasm"))]
    pub fn from_toml_string(string: String) -> GraphPlan<'a, String, String> {
        unimplemented!();
        // let config: Config = toml::from_str(&string).expect("Fail");
        // let initial_props: HashSet<Proposition<String>> = config.initial
        //     .iter()
        //     .map(|i| Proposition::new(i.replace("not_", ""),
        //                               i.starts_with("not_")))
        //     .collect();
        // let goals: HashSet<Proposition<String>> = config.goals
        //     .iter()
        //     .map(|i| Proposition::new(i.replace("not_", ""),
        //                               i.starts_with("not_")))
        //     .collect();
        // let actions: HashSet<Action<String, String>> = config.actions
        //     .iter()
        //     .map(|i| {
        //         let reqs: HashSet<Proposition<String>> = i.reqs.iter()
        //             .map(|r| Proposition::new(r.replace("not_", ""),
        //                                       r.starts_with("not_")))
        //             .collect();
        //         let effects: HashSet<Proposition<String>> = i.effects.iter()
        //             .map(|e| Proposition::new(e.replace("not_", ""),
        //                                       e.starts_with("not_")))
        //             .collect();
        //         Action::new(
        //             i.name.to_string(),
        //             reqs,
        //             effects
        //         )
        //     })
        //     .collect();
        // (initial_props, goals, actions)
        // GraphPlan::new(initial_props, goals, actions)
    }

    #[cfg(any(feature = "toml", feature = "wasm"))]
    pub fn from_toml(filepath: String) -> GraphPlan<'a, String, String> {
        let string = fs::read_to_string(filepath).expect("Failed to read file");
        GraphPlan::from_toml_string(string)
    }
}

#[cfg(test)]
mod integration_test {
    use crate::GraphPlan;
    use crate::proposition::Proposition;
    use crate::action::Action;
    use crate::solver::SimpleSolver;

    #[test]
    fn integration() {
        let p1 = Proposition::from("tired");
        let not_p1 = p1.negate();
        let p2 = Proposition::from("dog needs to pee");
        let not_p2 = p2.negate();

        let a1 = Action::new(
            "coffee",
            hashset!{&p1},
            hashset!{&not_p1}
        );

        let a2 = Action::new(
            "walk dog",
            hashset!{&p2, &not_p1},
            hashset!{&not_p2},
        );

        let a3 = Action::new_maintenance(&p1);
        let a4 = Action::new_maintenance(&not_p1);
        let a5 = Action::new_maintenance(&p2);
        let a6 = Action::new_maintenance(&not_p2);

        let initial_props = hashset!{&p1, &p2};
        let goals = hashset!{&not_p1, &not_p2};
        let actions = hashset!{&a1, &a2, &a3, &a4, &a5, &a6};

        let mut pg = GraphPlan::<&str, &str>::new(
            initial_props,
            goals,
            actions
        );
        assert!(pg.search::<SimpleSolver>() != None, "Solution should not be None");
    }

    #[cfg(any(feature = "toml", feature = "wasm"))]
    #[test]
    fn load_from_toml_config() {
        unimplemented!()
        // let path = String::from("resources/rocket_domain.toml");
        // let mut pg: GraphPlan<_, _, SimpleSolver> = GraphPlan::from_toml(path);
        // assert!(pg.search() != None, "Solution should not be None");
    }
}
