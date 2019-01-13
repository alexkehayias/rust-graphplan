use std::collections::{HashSet};
use std::fs;

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
    pub fn new(initial_props: HashSet<&Proposition>,
           goals: HashSet<&Proposition>,
           actions: HashSet<&Action>,
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
            .iter()
            .map(|i| Proposition::new(i.to_owned(), i.starts_with("not_")))
            .collect();
        let goals: HashSet<Proposition> = config.goals
            .iter()
            .map(|i| Proposition::new(i.to_owned(), i.starts_with("not_")))
            .collect();
        let actions: HashSet<Action> = config.actions
            .iter()
            .map(|i| {
                let reqs: HashSet<Proposition> = i.reqs.iter()
                    .map(|r| Proposition::new(r.clone(), r.starts_with("not_")))
                    .collect();
                let effects: HashSet<Proposition> = i.effects.iter()
                    .map(|e| Proposition::new(e.clone(), e.starts_with("not_")))
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
            initial_props.iter().collect(),
            goals.iter().collect(),
            actions.iter().collect(),
            solver
        )
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
        let p3 = Proposition::from_str("caffeinated");

        let a1 = Action::new(
            String::from("coffee"),
            hashset!{&p1},
            hashset!{&p3, &not_p1}
        );

        let a2 = Action::new(
            String::from("walk dog"),
            hashset!{&p2, &p3},
            hashset!{&not_p2},
        );

        let mut pg = GraphPlan::new(
            hashset!{&p1, &p2},
            hashset!{&not_p1, &not_p2, &p3},
            hashset!{&a1, &a2},
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
