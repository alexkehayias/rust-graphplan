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

type GoalIndex = usize;
type Attempts = HashMap<usize, BTreeSet<Action>>;

#[derive(Clone, Debug, PartialEq)]
struct ActionCombination(HashMap<GoalIndex, Action>);

impl ActionCombination {
    pub fn to_set(&self) -> HashSet<Action> {
        self.0.values()
            .cloned()
            .into_iter()
            .collect::<HashSet<Action>>()
    }
}

#[derive(Clone, Debug)]
struct ActionCombinations {
    goals: Vec<Proposition>,
    actions: BTreeSet<Action>,
    mutexes: Option<MutexPairs<Action>>,
}

impl ActionCombinations {
    pub fn new(goals: Vec<Proposition>,
               actions: BTreeSet<Action>,
               mutexes: Option<MutexPairs<Action>>,) -> ActionCombinations {
        ActionCombinations {goals, actions, mutexes}
    }
}

impl IntoIterator for ActionCombinations {
    type Item = ActionCombination;
    type IntoIter = ActionCombinationIterator;

    fn into_iter(self) -> Self::IntoIter {
        ActionCombinationIterator::new(self)
    }
}

#[derive(Clone, Debug)]
struct ActionCombinationIterator {
    meta: ActionCombinations, // defines goals we are trying achieve
    attempts: Attempts, // previous attempts to meet a goal
    goals_met: bool, // flag indicating all goals are met or restart
    accum: HashMap<GoalIndex, Action> // combination of actions
}

impl ActionCombinationIterator {
    pub fn new(action_combinations: ActionCombinations) -> ActionCombinationIterator {
        ActionCombinationIterator {
            meta: action_combinations,
            attempts: Attempts::new(),
            goals_met: false,
            accum: HashMap::new(),
        }
    }
}

impl Iterator for ActionCombinationIterator {
    type Item = ActionCombination;

    fn next(&mut self) -> Option<Self::Item> {
        let goals = &self.meta.goals;
        let actions = &self.meta.actions;
        let mutexes = &self.meta.mutexes;
        let goal_len = goals.len();

        let mut stack = VecDeque::new();

        // If the goals have already been met, we need to look for a
        // new combination that also meets the goals
        if self.goals_met {
            // Remove the previous action used to satisfy the last
            // goal and start the loop from the last goal. This will
            // yield a new combination or recursively back track.
            let goal_idx = goal_len - 1;
            self.accum.remove(&goal_idx);
            stack.push_front((goal_idx,));
        } else {
            stack.push_front((0,));
        }

        while let Some((goal_idx, )) = stack.pop_front() {
            let available_actions = if let Some(acts) = self.attempts.get(&goal_idx) {
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
                for a in actions {
                    // Early continue since the later checks are
                    // more expensive
                    if !a.effects.contains(goal) {
                        continue
                    };

                    if self.accum.get(&goal_idx).map(|i| i == a).unwrap_or(false) {
                        available.insert(a.clone());
                        break
                    };

                    // Check if this action is mutex with any of
                    // the other accumulated actions
                    let mut acts = self.accum.clone();
                    acts.insert(goal_idx, a.clone());
                    let pairs = pairs_from_sets(
                        hashset!{a.clone()},
                        ActionCombination(acts).to_set()
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
                debug!("Unable to find actions for goal {:?}. Going back to previous goal...", goal_idx);
                // Clear attempts for this goal so finding an action
                // can be retried with a new set of actions
                self.attempts.remove(&goal_idx);
                // Backtrack to the previous goal
                stack.push_front((goal_idx - 1,));
            } else {
                // Add the action to the plan and continue
                let next_action = available_actions.iter().next().unwrap();
                self.accum.insert(goal_idx, next_action.clone());
                debug!("Selected action {:?}", next_action.clone());

                // Add to previous attempts in case we need to backtrack
                let mut remaining_actions = available_actions.clone();
                remaining_actions.remove(&next_action);
                self.attempts.insert(goal_idx, remaining_actions);

                // TODO if this action staisfies more than one
                // goal then handle that...

                // Proceed to the next goal
                if goal_idx < goal_len - 1 {
                    stack.push_front((goal_idx + 1,));
                } else {
                    self.goals_met = true;
                }
            };
        };

        if self.goals_met {
            Some(ActionCombination(self.accum.clone()))
        } else {
            None
        }
    }
}

impl SimpleSolver {
    fn action_combinations(goals: Vec<Proposition>,
                           actions: BTreeSet<Action>,
                           mutexes: Option<MutexPairs<Action>>) -> Option<(ActionCombination, ActionCombinationIterator)> {
        let mut iterable = ActionCombinations::new(goals, actions, mutexes).into_iter();

        if let Some(i) = iterable.next() {
            Some((i, iterable))
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
        let mut failed_goals_memo: HashSet<(usize, Vec<Proposition>)> = HashSet::new();

        // Initialize the loop
        let mut stack: VecDeque<(usize, Vec<Proposition>)> = VecDeque::new();
        let init_goals: Vec<Proposition> = plangraph.goals.clone()
            .into_iter()
            .collect();
        let init_layer_idx = plangraph.layers.clone().len() - 1;
        stack.push_front((init_layer_idx, init_goals));

        while let Some((idx, goals)) = stack.pop_front() {
            debug!("Working on layer {:?} with goals {:?}", idx, goals);
            // Check if the goal set is unsolvable at level i
            if failed_goals_memo.contains(&(idx, goals.clone())) {
                // TODO go back to previous goalset
            }

            // Note: This is a btreeset so ordering is guaranteed
            let actions = plangraph.actions_at_layer(idx - 1)
                .expect("Failed to get actions");
            let mutexes = plangraph.mutex_actions.get(&(idx - 1)).cloned();

            if let Some((goal_actions, _iter)) = SimpleSolver::action_combinations(goals.clone(), actions.clone(), mutexes) {
                debug!("Found actions {:?}", goal_actions);
                // If we are are on the second to last proposition
                // layer, we are done
                if (idx - 2) == 0 {
                    plan.push(goal_actions.to_set());
                    debug!("Found plan! {:?}", plan);
                    success = true;
                } else {
                    plan.push(goal_actions.to_set());
                    let next_goals = goal_actions.to_set().into_iter()
                        .flat_map(|action| action.reqs)
                        .unique()
                        .collect();
                    stack.push_front((idx - 2, next_goals));
                };
            } else {
                debug!("Unable to find actions for goals {:?} from actions {:?}", goals, actions);
                // Record the failed goals at level i and retry with
                // another set of actions if possible
                failed_goals_memo.insert((idx, goals.clone()));
                // TODO go back to previous goalset
                let next_goals = goals.clone();
                stack.push_front((idx + 2, next_goals));
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
        let expected = ActionCombination(hashmap!{0 => a1});
        let (actual, _) = SimpleSolver::action_combinations(goals, actions, mutexes).unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn action_combinations_resume() {
        assert!(false, "Pleaase test condition where we want to resume action combinations")
    }

    #[test]
    fn action_combinations_multiple_goals() {
        let p1 = Proposition::from_str("coffee");
        let p2 = Proposition::from_str("caffeinated");
        let p3 = Proposition::from_str("breakfast");
        let p4 = Proposition::from_str("full");
        let goals = vec![p2.clone(), p4.clone()];
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
        let expected = ActionCombination(hashmap!{0 => a1.clone(), 1 => a2.clone()});
        let (actual, _) = SimpleSolver::action_combinations(goals, actions, mutexes).unwrap();
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
