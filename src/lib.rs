use std::collections::{HashSet};
use std::fs;
use std::hash::Hash;
use std::fmt::Debug;

#[macro_use] extern crate serde_derive;
extern crate serde;
use toml;

#[macro_use] pub mod macros;
pub mod proposition;
pub mod action;
pub mod plangraph;
pub mod solver;
mod layer;
mod pairset;

pub use crate::proposition::Proposition;
pub use crate::action::Action;
pub use crate::plangraph::{PlanGraph, Solution};
pub use crate::solver::{GraphPlanSolver, SimpleSolver};


#[derive(Deserialize)]
struct Config {
    initial: Vec<String>,
    goals: Vec<String>,
    actions: Vec<ConfigAction>
}

#[derive(Deserialize)]
struct ConfigAction {
    name: String,
    reqs: Vec<String>,
    effects: Vec<String>,
}

pub struct GraphPlan<ActionId: Debug + Hash + Ord + Clone,
                     T: GraphPlanSolver<ActionId>> {
    pub solver: T,
    pub plangraph: PlanGraph<ActionId>,
}

impl<ActionId: Debug + Hash + Ord + Clone, T: GraphPlanSolver<ActionId>> GraphPlan<ActionId, T> {
    pub fn new(initial_props: HashSet<Proposition>,
               goals: HashSet<Proposition>,
               actions: HashSet<Action<ActionId>>,
               solver: T) -> GraphPlan<ActionId, T> {
        let pg = PlanGraph::new(initial_props, goals, actions);
        GraphPlan {
            solver: solver,
            plangraph: pg
        }
    }

    pub fn search(&mut self) -> Option<Solution<ActionId>>{
        self.plangraph.search_with(&self.solver)
    }
}

impl GraphPlan<String, SimpleSolver> {
    pub fn from_toml_string(string: String) -> GraphPlan<String, SimpleSolver> {
        let config: Config = toml::from_str(&string).expect("Fail");
        let initial_props: HashSet<Proposition> = config.initial
            .iter()
            .map(|i| Proposition::new(i.to_owned().replace("not_", ""),
                                      i.starts_with("not_")))
            .collect();
        let goals: HashSet<Proposition> = config.goals
            .iter()
            .map(|i| Proposition::new(i.to_owned().replace("not_", ""),
                                      i.starts_with("not_")))
            .collect();
        let actions: HashSet<Action<String>> = config.actions
            .iter()
            .map(|i| {
                let reqs: HashSet<Proposition> = i.reqs.iter()
                    .map(|r| Proposition::new(r.clone().replace("not_", ""),
                                              r.starts_with("not_")))
                    .collect();
                let effects: HashSet<Proposition> = i.effects.iter()
                    .map(|e| Proposition::new(e.clone().replace("not_", ""),
                                              e.starts_with("not_")))
                    .collect();
                Action::new(
                    i.name.to_string(),
                    reqs.iter().collect(),
                    effects.iter().collect()
                )
            })
            .collect();
        let solver = SimpleSolver::new();
        GraphPlan::new(
            initial_props,
            goals,
            actions,
            solver
        )
    }

    pub fn from_toml(filepath: String) -> GraphPlan<String, SimpleSolver> {
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
        let p1 = Proposition::from_str("tired");
        let not_p1 = p1.negate();
        let p2 = Proposition::from_str("dog needs to pee");
        let not_p2 = p2.negate();

        let a1 = Action::new(
            String::from("coffee"),
            hashset!{&p1},
            hashset!{&not_p1}
        );

        let a2 = Action::new(
            String::from("walk dog"),
            hashset!{&p2, &not_p1},
            hashset!{&not_p2},
        );

        let mut pg = GraphPlan::new(
            hashset!{p1, p2},
            hashset!{not_p1, not_p2},
            hashset!{a1, a2},
            SimpleSolver::new()
        );
        assert!(pg.search() != None, "Solution should not be None");
    }

    #[test]
    fn load_from_toml_config() {
        let path = String::from("resources/rocket_domain.toml");
        let mut pg: GraphPlan<_, SimpleSolver> = GraphPlan::from_toml(path);
        assert!(pg.search() != None, "Solution should not be None");
    }
}
