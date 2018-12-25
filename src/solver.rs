use std::collections::{HashMap, HashSet, BTreeSet, VecDeque};
use log::{debug};
use itertools::Itertools;
use crate::proposition::Proposition;
use crate::action::Action;
use crate::pairset::{PairSet, pairs_from_sets};
use crate::layer::{MutexPairs};
use crate::plangraph::{PlanGraph, Solution};


pub trait GraphPlanSolver {
    /// Searches a plangraph for a sequence of collection of actions
    /// that satisfy the goals
    fn search(&self, plangraph: &PlanGraph) -> Option<Solution>;
}

pub struct SimpleSolver;

impl SimpleSolver {
    pub fn new() -> SimpleSolver {
        SimpleSolver {}
    }
}

impl SimpleSolver {
    fn action_combinations(goals: Vec<Proposition>,
                           actions: BTreeSet<Action>,
                           mutexes: Option<MutexPairs<Action>>) -> Option<(HashSet<Action>, HashMap<usize, BTreeSet<Action>>)>{
        let mut stack: VecDeque<(
            usize,
        )> = VecDeque::new();
        let mut action_accum = HashSet::new();
        let mut attempts: HashMap<usize, BTreeSet<Action>> = HashMap::new();
        let goal_len = goals.len();
        let init_goal_idx = 0;
        let mut goals_met = false;

        // Add the end goals to the stack
        stack.push_front((init_goal_idx,));

        while let Some((goal_idx, )) = stack.pop_front() {
            let available_actions = if let Some(acts) = attempts.get(&goal_idx) {
                acts.to_owned()
            } else {
                let goal = &goals[goal_idx];
                debug!("Looking for action for goal {:?} in {:?}", goal, actions.clone());
                let mut available = BTreeSet::new();
                // Only actions that produce the goal and are not
                // mutex with any other actions and have not
                // already been attempted in combination with the
                // other attempted actions and are not mutex with
                // any other action
                for a in &actions {
                    // Early continue since the later checks are
                    // more expensive
                    if !a.effects.contains(goal) {
                        continue
                    };

                    if action_accum.contains(a) {
                        available.insert(a.clone());
                        break
                    };

                    // Check if this action is mutex with any of
                    // the other accumulated actions
                    let mut acts = action_accum.clone();
                    acts.insert(a.clone());
                    let pairs = pairs_from_sets(
                        hashset!{a.clone()},
                        acts
                    );
                    let action_mutexes: Vec<PairSet<Action>> = mutexes
                        .clone()
                        .unwrap_or(HashSet::new())
                        .intersection(&pairs)
                        .into_iter()
                        .cloned()
                        .collect();

                    if action_mutexes.is_empty() {
                        available.insert(a.clone());
                    }
                };
                available
            };

            if available_actions.is_empty() {
                if goal_idx == 0 {
                    // Complete fail
                    break;
                }
                // Go back to previous goal
                debug!("Unable to find actions for goal {:?}. Going back to previous goal...", goal_idx);
                stack.push_front((goal_idx - 1,));
            } else {
                // Add the action to the plan and continue
                let next_action = available_actions.iter()
                    .next()
                    .map(|i| {action_accum.insert(i.clone()); i})
                    .unwrap();
                debug!("Selected action {:?}", next_action.clone());

                // Add to previous attempts in case we need to backtrack
                let mut remaining_actions = available_actions.clone();
                remaining_actions.remove(&next_action);
                attempts.insert(goal_idx, remaining_actions);

                // TODO if this action staisfies more than one
                // goal then handle that...

                // Proceed to the next goal
                if goal_idx < goal_len - 1 {
                    stack.push_front((goal_idx + 1,));
                } else {
                    goals_met = true;
                }
            };
        };

        if goals_met {
            Some((action_accum, attempts))
        } else {
            None
        }
    }
}

impl GraphPlanSolver for SimpleSolver {
    fn search<>(&self, plangraph: &PlanGraph) -> Option<Solution> {
        // The implementation is basically a recursive function where
        // we are using a stack and a loop rather than a recursive
        // function. Not sure how recursion works with Rust and the
        // allowed call depth so sticking with this approach for now
        let mut success = false;
        let mut plan = Solution::new();

        // Initialize the loop
        let mut stack: VecDeque<(usize, Vec<Proposition>)> = VecDeque::new();
        let init_goals: Vec<Proposition> = plangraph.goals.clone()
            .into_iter()
            .collect();
        let init_layer_idx = plangraph.layers.clone().len() - 1;
        stack.push_front((init_layer_idx, init_goals));

        while let Some((idx, goals)) = stack.pop_front() {
            debug!("Working on layer {:?} with goals {:?}", idx, goals);
            // Note: This is a btreeset so ordering is guaranteed
            // TODO maybe remove the use of unwrap
            let actions = plangraph.actions_at_layer(idx - 1).unwrap();
            let mutexes = plangraph.mutex_actions.get(&(idx - 1)).cloned();

            if let Some((goal_actions, _attempts)) = SimpleSolver::action_combinations(goals.clone(), actions.clone(), mutexes) {
                debug!("Found actions {:?}", goal_actions);
                // If we are are on the second to last proposition
                // layer, we are done
                if (idx - 2) == 0 {
                    plan.push(goal_actions.clone());
                    debug!("Found plan! {:?}", plan);
                    success = true;
                } else {
                    plan.push(goal_actions.clone());
                    let next_goals = goal_actions.into_iter()
                        .flat_map(|action| action.reqs)
                        .unique()
                        .collect();
                    stack.push_front((idx - 2, next_goals));
                };
            } else {
                debug!("Unable to find actions for goals {:?} from actions {:?}", goals, actions);
            }
        };

        if success {
            // Since this solver goes from the last layer to the
            // first, we need to reverse the plan
            plan.reverse();
            Some(plan)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod simple_solver_test {
    use super::*;

    #[test]
    fn action_combinations_single_goal() {
        let p1 = Proposition::from_str("coffee");
        let p2 = Proposition::from_str("caffeinated");
        let goals = vec![p2.clone()];
        let mut actions = BTreeSet::new();
        let a1 = Action::new(
            String::from("drink coffee"),
            hashset!{p1.clone()},
            hashset!{p2.clone()}
        );
        actions.insert(a1.clone());
        let mutexes = Some(MutexPairs::new());
        let attempts: HashMap<usize, BTreeSet<Action>> = hashmap!{0 => btreeset!{}};
        let expected = Some((hashset!{a1.clone()}, attempts));
        let actual = SimpleSolver::action_combinations(goals, actions, mutexes);
        assert_eq!(expected, actual);
    }

    #[test]
    fn action_combinations_multiple_goals() {
        let p1 = Proposition::from_str("coffee");
        let p2 = Proposition::from_str("caffeinated");
        let p3 = Proposition::from_str("breakfast");
        let p4 = Proposition::from_str("full");
        let goals = vec![p2.clone()];
        let mut actions = BTreeSet::new();
        let a1 = Action::new(
            String::from("drink coffee"),
            hashset!{p1.clone()},
            hashset!{p2.clone()}
        );
        actions.insert(a1.clone());

        let a2 = Action::new(
            String::from("eat breakfast"),
            hashset!{p3.clone()},
            hashset!{p4.clone()}
        );
        actions.insert(a2.clone());

        let mutexes = Some(MutexPairs::new());
        let attempts: HashMap<usize, BTreeSet<Action>> = hashmap!{0 => btreeset!{}};
        let expected = Some((hashset!{a1.clone(), a1.clone()}, attempts));
        let actual = SimpleSolver::action_combinations(goals, actions, mutexes);
        assert_eq!(expected, actual);
    }

    #[test]
    fn solver_works() {
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

        let mut pg = PlanGraph::new(
            hashset!{p1.clone(), p2.clone()},
            hashset!{p1.clone().negate(),
                     p2.clone().negate(),
                     p3.clone()},
            hashset!{a1.clone(), a2.clone()}
        );
        pg.extend();
        pg.extend();
        debug!("Plangraph: {:?}", pg);

        let solver = SimpleSolver::new();
        let expected = Some(vec![hashset!{a1.clone()}, hashset!{a2.clone()}]);
        let actual = PlanGraph::format_plan(solver.search(&pg));
        assert_eq!(expected, actual);
    }
}

#[cfg(test)]
/// Prove that BTreeSet preserves ordering
mod btreeset_test {
    use std::collections::BTreeSet;

    #[test]
    fn is_sorted () {
        let mut b = BTreeSet::new();
        b.insert(2);
        b.insert(3);
        b.insert(1);
        assert_eq!(b.into_iter().collect::<Vec<_>>(), vec![1, 2, 3]);

        let mut b = BTreeSet::new();
        b.insert(1);
        b.insert(2);
        b.insert(3);
        assert_eq!(b.into_iter().collect::<Vec<_>>(), vec![1, 2, 3]);
    }
}
