use std::collections::{HashSet};
use std::hash::Hash;
use std::fmt::{Debug, Display};
use log::{debug};

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


/// Represents a planning domain that can generate a GraphPlan via
/// `from_domain`. Serves as a helper object to prevent lifetime
/// issues when auto generating things like maintenance actions
pub struct Domain<'a,
                  ActionId: Debug + Hash + Ord + Clone,
                  PropositionId: Debug + Display + Hash + Ord + Clone> {
    initial_props: HashSet<&'a Proposition<PropositionId>>,
    goals: HashSet<&'a Proposition<PropositionId>>,
    actions: HashSet<Action<'a, ActionId, PropositionId>>
}

pub struct GraphPlan<'a,
                     ActionId: Debug + Hash + Ord + Clone,
                     PropositionId: Debug + Display + Hash + Ord + Clone> {
    plangraph: PlanGraph<'a, ActionId, PropositionId>,
}

impl<'a,
     ActionId: Debug + Hash + Ord + Clone,
     PropositionId: Debug + Display + Hash + Ord + Clone> GraphPlan<'a, ActionId, PropositionId> {

    /// Returns a new GraphPlan. Note: you probably want to use
    /// `from_domain` instead.
    pub fn new(initial_props: HashSet<&'a Proposition<PropositionId>>,
               goals: HashSet<&'a Proposition<PropositionId>>,
               actions: HashSet<&'a Action<'a, ActionId, PropositionId>>)
               -> GraphPlan<'a, ActionId, PropositionId> {
        let plangraph = PlanGraph::new(
            initial_props,
            goals,
            actions,
        );
        GraphPlan { plangraph }
    }

    pub fn from_domain(domain: &'a Domain<'a, ActionId, PropositionId>)
               -> GraphPlan<'a, ActionId, PropositionId> {
        let plangraph = PlanGraph::new(
            domain.initial_props.clone(),
            domain.goals.clone(),
            domain.actions.iter().collect(),
        );
        GraphPlan { plangraph }
    }

    /// Returns a domain with all maintenance actions.automatically
    /// created. This is needed to avoid lifetime issues with PlanGraph
    pub fn create_domain(initial_props: HashSet<&'a Proposition<PropositionId>>,
                         goals: HashSet<&'a Proposition<PropositionId>>,
                         actions: HashSet<&'a Action<'a, ActionId, PropositionId>>)
                         -> Domain<'a, ActionId, PropositionId> {
        let mut all_actions = HashSet::new();

        for p in &initial_props {
            all_actions.insert(Action::new_maintenance(*p));
        }

        for a in actions {
            all_actions.insert(a.to_owned());

            for p in &a.reqs {
                all_actions.insert(Action::new_maintenance(p));
            }

            for p in &a.effects {
                all_actions.insert(Action::new_maintenance(p));
            }
        }

        Domain {
            initial_props,
            goals,
            actions: all_actions,
        }
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

        let actions = hashset!{&a1, &a2};
        let initial_props = hashset!{&p1, &p2};
        let goals = hashset!{&not_p1, &not_p2};

        let domain = GraphPlan::create_domain(
            initial_props,
            goals,
            actions,
        );

        let mut pg = GraphPlan::<&str, &str>::from_domain(&domain);
        assert!(pg.search::<SimpleSolver>() != None, "Solution should not be None");
    }
}
