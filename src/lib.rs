use std::collections::{HashSet};

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
use crate::solver::{GraphPlanSolver};


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
}
