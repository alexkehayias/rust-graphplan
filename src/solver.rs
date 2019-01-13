use std::collections::{HashMap, HashSet, BTreeSet, VecDeque};
use log::{debug};
use itertools::Itertools;
use std::iter::FromIterator;
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
    pub fn as_set(&self) -> HashSet<Action> {
        self.0.values()
            .cloned()
            .into_iter()
            .collect::<HashSet<Action>>()
    }

    pub fn as_vec(&self) -> Vec<Action> {
        self.0.values()
            .cloned()
            .into_iter()
            .collect::<Vec<Action>>()
    }
}

#[derive(Clone, Debug)]
struct GoalSetActionGenerator {
    goals: Vec<Proposition>,
    actions: BTreeSet<Action>,
    mutexes: Option<MutexPairs<Action>>,
}

impl GoalSetActionGenerator {
    pub fn new(goals: Vec<Proposition>,
               actions: BTreeSet<Action>,
               mutexes: Option<MutexPairs<Action>>,) -> GoalSetActionGenerator {
        GoalSetActionGenerator {goals, actions, mutexes}
    }
}

impl IntoIterator for GoalSetActionGenerator {
    type Item = ActionCombination;
    type IntoIter = ActionCombinationIterator;

    fn into_iter(self) -> Self::IntoIter {
        ActionCombinationIterator::new(self)
    }
}

#[derive(Clone, Debug)]
struct ActionCombinationIterator {
    meta: GoalSetActionGenerator, // defines goals we are trying achieve
    attempts: Attempts, // previous attempts to meet a goal
    goals_met: bool, // flag indicating all goals are met or restart
    accum: HashMap<GoalIndex, Action> // combination of actions
}

impl ActionCombinationIterator {
    pub fn new(action_combinations: GoalSetActionGenerator) -> ActionCombinationIterator {
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
            debug!("Resuming action combination generator: {:?}", self.clone());
            // Remove the previous action used to satisfy the last
            // goal and start the loop from the last goal. This will
            // yield a new combination or recursively back track.
            self.goals_met = false;
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
                        ActionCombination(acts).as_set()
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

                // TODO Implement minimal action sets i.e handle if
                // this action staisfies more than one
                // goal. Otherwise, the speed of finding a solution is
                // dependent on the ordering of goals

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


#[cfg(test)]
mod goal_set_action_generator_test {
    use super::*;

    #[test]
    fn single_goal() {
        let p1 = Proposition::from_str("coffee");
        let p2 = Proposition::from_str("caffeinated");
        let goals = vec![p2.clone()];
        let mut actions = BTreeSet::new();
        let a1 = Action::new(
            String::from("drink coffee"),
            hashset!{&p1},
            hashset!{&p2}
        );
        actions.insert(a1.clone());
        let mutexes = Some(MutexPairs::new());
        let expected = ActionCombination(hashmap!{0 => a1});
        let actual = GoalSetActionGenerator::new(goals, actions, mutexes)
            .into_iter()
            .next()
            .unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn multiple_goals() {
        let p1 = Proposition::from_str("coffee");
        let p2 = Proposition::from_str("caffeinated");
        let p3 = Proposition::from_str("breakfast");
        let p4 = Proposition::from_str("full");
        let goals = vec![p2.clone(), p4.clone()];
        let mut actions = BTreeSet::new();
        let a1 = Action::new(
            String::from("drink coffee"),
            hashset!{&p1},
            hashset!{&p2}
        );
        actions.insert(a1.clone());

        let a2 = Action::new(
            String::from("eat breakfast"),
            hashset!{&p3},
            hashset!{&p4}
        );
        actions.insert(a2.clone());

        let mutexes = Some(MutexPairs::new());
        let expected = ActionCombination(hashmap!{0 => a1.clone(), 1 => a2.clone()});
        let actual = GoalSetActionGenerator::new(goals, actions, mutexes)
            .into_iter()
            .next()
            .unwrap();
        assert_eq!(expected, actual);
    }

    #[test]
    fn yields_all() {
        let p1 = Proposition::from_str("tea");
        let p2 = Proposition::from_str("coffee");
        let p3 = Proposition::from_str("caffeinated");
        let p4 = Proposition::from_str("scone");
        let p5 = Proposition::from_str("muffin");
        let p6 = Proposition::from_str("full");
        let goals = vec![p3.clone(), p6.clone()];
        let mut actions = BTreeSet::new();

        let a1 = Action::new(
            String::from("drink coffee"),
            hashset!{&p2},
            hashset!{&p3}
        );
        actions.insert(a1.clone());

        let a2 = Action::new(
            String::from("drink tea"),
            hashset!{&p1},
            hashset!{&p3}
        );
        actions.insert(a2.clone());

        let a3 = Action::new(
            String::from("eat scone"),
            hashset!{&p4},
            hashset!{&p6}
        );
        actions.insert(a3.clone());

        let a4 = Action::new(
            String::from("eat muffin"),
            hashset!{&p5},
            hashset!{&p6}
        );
        actions.insert(a4.clone());

        let mutexes = Some(MutexPairs::new());
        let expected = vec![
            vec![a1.clone(), a4.clone()],
            vec![a1.clone(), a3.clone()],
            vec![a2.clone(), a4.clone()],
            vec![a2.clone(), a3.clone()],
        ];
        let generator = GoalSetActionGenerator::new(goals, actions, mutexes);
        let actual: Vec<Vec<Action>> = generator.into_iter()
            .map(|combo| {let mut i = combo.as_vec(); i.sort(); i})
            .collect();

        assert_eq!(expected, actual);
    }
}

impl GraphPlanSolver for SimpleSolver {
    fn search(&self, plangraph: &PlanGraph) -> Option<Solution> {
        let mut success = false;
        let mut plan = Solution::new();
        let mut failed_goals_memo: HashSet<(usize, Vec<Proposition>)> = HashSet::new();

        // Initialize the loop
        let mut stack: VecDeque<(usize, Vec<Proposition>, Option<ActionCombinationIterator>)> = VecDeque::new();
        let init_goals = Vec::from_iter(plangraph.goals.clone());
        let init_layer_idx = plangraph.layers.len() - 1;
        let init_action_gen = None;
        stack.push_front((init_layer_idx, init_goals, init_action_gen));

        while let Some((idx, goals, action_gen)) = stack.pop_front() {
            debug!("Working on layer {:?} with goals {:?}", idx, goals);
            // Check if the goal set is unsolvable at level idx
            if failed_goals_memo.contains(&(idx, goals.clone())) {
                // Continue to previous layer (the next element in the queue)
                continue;
            }

            // Note: This is a btreeset so ordering is guaranteed
            let actions = plangraph.actions_at_layer(idx - 1)
                .expect("Failed to get actions");
            let mutexes = plangraph.mutex_actions.get(&(idx - 1)).cloned();
            let mut gen = action_gen
                .or(Some(GoalSetActionGenerator::new(goals.clone(),
                                                     actions.clone(),
                                                     mutexes).into_iter()))
                .unwrap();

            if let Some(goal_actions) = gen.next() {
                debug!("Found actions {:?}", goal_actions);
                // If we are are on the second to last proposition
                // layer, we are done
                if (idx - 2) == 0 {
                    plan.push(goal_actions.as_set());
                    debug!("Found plan! {:?}", plan);
                    success = true;
                    break;
                } else {
                    plan.push(goal_actions.as_set());
                    let next_goals = goal_actions.as_set().into_iter()
                        .flat_map(|action| action.reqs)
                        .unique()
                        .collect();
                    // Add this layer back into the queue incase we need to backtrack
                    stack.push_front((idx, goals, Some(gen)));
                    stack.push_front((idx - 2, next_goals, None));
                };
            } else {
                debug!("Unable to find actions for goals {:?} from actions {:?}", goals, actions);
                // Record the failed goals at level idx
                failed_goals_memo.insert((idx, goals.clone()));
                // Backtrack to previous layer and goalset or nothing
                // (the next element in the queue)
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
    fn solver_works() {
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

        let goals = hashset!{&not_p1, &not_p2, &p3};

        let mut pg = PlanGraph::new(
            hashset!{&p1, &p2},
            goals,
            hashset!{&a1, &a2}
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
