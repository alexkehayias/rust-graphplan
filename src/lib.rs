#![allow(dead_code)]

#[macro_use] mod macros;
mod pairset;
mod proposition;
mod action;
mod layer;
mod plangraph;
mod solver;

// TODO package this up
// struct GraphPlan<T: GraphPlanSolver> {
//     solver: T,
//     data: PlanGraph,
// }

// impl<T: GraphPlanSolver> GraphPlan<T> {
//     // fn new(solver: T) -> GraphPlan<T> {
//     //     GraphPlan {
//     //         solver: solver,
//     //         data: PlanGraph::new()
//     //     }
//     // }
// }

#[cfg(test)]
mod integration_test {
    use crate::proposition::Proposition;
    use crate::action::Action;
    use crate::plangraph::{PlanGraph};
    use crate::solver::SimpleSolver;

    #[test]
    fn integration() {
        let p1 = Proposition::from_str("tired");
        let p2 = Proposition::from_str("dog needs to pee");
        let p3 = Proposition::from_str("caffeinated");

        let a1 = Action::new(
            String::from("coffee"),
            hashset!{p1.clone()},
            hashset!{p3.clone()}
        );

        let a2 = Action::new(
            String::from("walk dog"),
            hashset!{p2.clone(), p3.clone()},
            hashset!{p2.clone().negate()},
        );

        let mut pg = PlanGraph::new(
            hashset!{p1.clone(), p2.clone(), p3.clone()},
            hashset!{p1.clone().negate(),
                     p2.negate().clone(),
                     p3.clone()},
            hashset!{a1.clone(), a2.clone()}
        );

        let solver = SimpleSolver::new();
        assert!(pg.search_with(solver) != None, "Solution should not be None");
    }
}
