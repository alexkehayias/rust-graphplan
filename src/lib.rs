use std::collections::{HashSet};
use std::fs;

#[macro_use] extern crate serde_derive;
#[macro_use] extern crate serde;
#[macro_use] use toml;

#[macro_use] pub mod macros;
pub mod proposition;
pub mod action;
pub mod plangraph;
pub mod solver;
mod layer;
mod pairset;

use crate::proposition::Proposition;
use crate::action::Action;
use crate::plangraph::{PlanGraph, Solution};
use crate::solver::{GraphPlanSolver, SimpleSolver};

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


pub struct GraphPlan<T: GraphPlanSolver> {
    pub solver: T,
    pub plangraph: PlanGraph,
}

impl<T: GraphPlanSolver> GraphPlan<T> {
    pub fn new(initial_props: HashSet<Proposition>,
           goals: HashSet<Proposition>,
           actions: HashSet<Action>,
           solver: T) -> GraphPlan<T> {
        let pg = PlanGraph::new(initial_props, goals, actions);
        GraphPlan {
            solver: solver,
            plangraph: pg
        }
    }

    pub fn search(&mut self) -> Option<Solution>{
        self.plangraph.search_with(&self.solver)
    }
}

impl GraphPlan<SimpleSolver> {
    pub fn from_toml(filepath: String) -> GraphPlan<SimpleSolver> {
        let st = fs::read_to_string(filepath).expect("Failed to read file");
        let config: Config = toml::from_str(&st).expect("Fail");
        let initial_props: HashSet<Proposition> = config.initial
            .into_iter()
            // TODO check if there is a prefix for "not"
            .map(|i| Proposition::new(i, false))
            .collect();
        let goals: HashSet<Proposition> = config.goals
            .into_iter()
            .map(|i| Proposition::new(i, false))
            .collect();
        let actions: HashSet<Action> = config.actions
            .into_iter()
            .map(|i| Action::new(
                String::from(i.name),
                i.reqs.iter()
                    .map(|r| Proposition::new(r.to_string(), false))
                    .collect(),
                i.effects.iter()
                    .map(|e| Proposition::new(e.to_string(), false))
                    .collect()))
            .collect();
        let solver = SimpleSolver::new();
        GraphPlan::new(initial_props.to_owned(), goals.to_owned(), actions.to_owned(), solver)
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
        let p2 = Proposition::from_str("dog needs to pee");
        let p3 = Proposition::from_str("caffeinated");

        let a1 = Action::new(
            String::from("coffee"),
            hashset!{p1.clone()},
            hashset!{p3.clone(), p1.clone().negate()}
        );

        let a2 = Action::new(
            String::from("walk dog"),
            hashset!{p2.clone(), p3.clone()},
            hashset!{p2.clone().negate()},
        );

        let mut pg = GraphPlan::new(
            hashset!{p1.clone(), p2.clone()},
            hashset!{p1.clone().negate(),
                     p2.clone().negate(),
                     p3.clone()},
            hashset!{a1.clone(), a2.clone()},
            SimpleSolver::new()
        );
        assert!(pg.search() != None, "Solution should not be None");
    }

    #[test]
    fn load_from_toml_config() {
        let path = String::from("resources/rocket_domain.toml");
        let mut pg: GraphPlan<SimpleSolver> = GraphPlan::from_toml(path);
        assert!(pg.search() != None, "Solution should not be None");
    }
}
