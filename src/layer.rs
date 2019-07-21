use std::fmt::Debug;
use std::hash::Hash;
use std::collections::{HashSet};
use crate::proposition::Proposition;
use crate::action::Action;
use crate::pairset::{PairSet, pairs, pairs_from_sets};


pub type ActionLayerData<ActionId> = HashSet<Action<ActionId>>;
pub type PropositionLayerData = HashSet<Proposition>;

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Layer<ActionId: Eq + Hash + Ord + PartialOrd + Clone + Debug> {
    ActionLayer(ActionLayerData<ActionId>),
    PropositionLayer(PropositionLayerData),
}

pub type MutexPairs<T> = HashSet<PairSet<T>>;

impl<ActionId: Eq + Hash + Ord + PartialOrd + Clone + Debug> Layer<ActionId> {
    /// Create a new layer from another. ActionLayer returns a
    /// PropositionLayer and PropositionLayer returns an ActionLayer
    pub fn from_layer(all_actions: HashSet<&Action<ActionId>>, layer: &Layer<ActionId>) -> Layer<ActionId> {
        match layer {
            Layer::ActionLayer(actions) => {
                let mut layer_data = PropositionLayerData::new();
                for a in actions {
                    for e in a.effects.iter() {
                        layer_data.insert(e.clone());
                    }
                }

                Layer::PropositionLayer(layer_data)
            },
            Layer::PropositionLayer(props) => {
                let mut layer_data = ActionLayerData::new();

                for a in all_actions {
                    // Include action if it satisfies one or more props
                    if a.reqs.is_subset(&props) {
                        layer_data.insert(a.to_owned());
                    }
                }

                // Add in maintenance actions for all props
                for p in props {
                    layer_data.insert(Action::new_maintenance(p.to_owned()));
                }

                Layer::ActionLayer(layer_data)
            },
        }
    }

    pub fn action_mutexes<'a>(actions: &HashSet<&'a Action<ActionId>>,
                              mutex_props: Option<&MutexPairs<Proposition>>)
                              -> MutexPairs<&'a Action<ActionId>> {
        let mut mutexes = MutexPairs::new();
        let action_pairs = pairs(&actions);

        for PairSet(a1, a2) in action_pairs {
            // Inconsistent effects: The effect of one action is the
            // negation of another
            // - Negate the effects of one of the actions then check
            //   the overlap
            // - If there is any overlap the two actions
            //   are mutex
            let negated_fx: HashSet<Proposition> = a1.effects
                .iter()
                .map(|e| e.negate())
                .collect();
            let inconsistent_fx: HashSet<Proposition> = a2.effects
                .intersection(&negated_fx)
                .map(|i| i.to_owned())
                .collect();
            if !inconsistent_fx.is_empty() {
                mutexes.insert(PairSet(a1, a2));
                continue
            }

            // Interference: One action deletes the precondition of
            // another action (they can't be done in parallel then)
            let left_interference: HashSet<Proposition> = a2.reqs
                .intersection(&negated_fx)
                .map(|i| i.to_owned())
                .collect();

            if !left_interference.is_empty() {
                mutexes.insert(PairSet(a1, a2));
                continue
            }

            // Since actions are not symetrical (they may have different
            // reqs) we need to check if the right hand side action
            // interferes with left hand side too
            let right_negated_fx: HashSet<Proposition> = a2.clone().effects
                .iter()
                .map(|e| e.negate())
                .collect();
            let right_interference: HashSet<Proposition> = a1.clone().reqs
                .intersection(&right_negated_fx)
                .map(|i| i.to_owned())
                .collect();

            if !right_interference.is_empty() {
                mutexes.insert(PairSet(a1, a2));
                continue
            }

            // Competing needs: Action has precondition that is
            // mutex
            // - Generate pairs of Action.reqs and compare them
            //   to proposition mutees
            // - Check for intersection with mutex props
            if let Some(mx_props) = mutex_props.clone() {
                let req_pairs = pairs_from_sets(a1.clone().reqs, a2.clone().reqs);
                let competing_needs: HashSet<PairSet<Proposition>> = req_pairs
                    .intersection(&mx_props)
                    .map(|i| i.to_owned())
                    .collect();

                if !competing_needs.is_empty() {
                    mutexes.insert(PairSet(a1, a2));
                }
            }
        }
        mutexes
    }

    /// Returns a set of propositions that are mutually exclusive
    ///
    /// Propositions are mutex if
    /// - They are negations of one another
    /// - All ways of achieving the propositions at are pairwise mutex
    pub fn proposition_mutexes(
        props: &HashSet<Proposition>,
        actions: &HashSet<&Action<ActionId>>,
        mutex_actions: Option<&MutexPairs<&Action<ActionId>>>,
    ) -> MutexPairs<Proposition> {
        let mut mutexes = MutexPairs::new();

        // Find mutexes due to negation
        for p in props.iter() {
            let not_p = p.negate();
            if props.contains(&not_p) {
                mutexes.insert(PairSet(p.clone(), not_p.clone()));
            }
        }

        // Find mutexes where all ways of achieving p are mutex
        // - Get all uniq pairs of propositions
        // - For each pair, get all uniq action pairs that could output the prop pair
        // - Set difference with the mutex_actions
        // - If there is no difference then the props are mutex
        if let Some(mx_actions) = mutex_actions {
            for PairSet(p1, p2) in pairs(&props) {
                let viable_acts: HashSet<&Action<_>> = actions.iter()
                    .filter(|a| a.effects.contains(&p1) || a.effects.contains(&p2))
                    .map(|a| a.to_owned())
                    .collect();

                let viable_act_pairs: HashSet<PairSet<&Action<_>>> = pairs(&viable_acts)
                    .into_iter()
                    .collect();

                let diff: HashSet<_> = viable_act_pairs
                    .difference(mx_actions)
                    .collect();

                if diff.is_empty() && !mx_actions.is_empty() {
                    mutexes.insert(PairSet(p1.clone(), p2.clone()));
                }
            }
        }
        mutexes
    }
}

#[cfg(test)]
mod from_layer_test {
    use super::*;

    #[test]
    fn action_layer_from_proposition_layer() {
        let prop = Proposition::from_str("test");
        let layer = Layer::<String>::PropositionLayer(hashset!{prop.clone()});
        let actual = Layer::from_layer(hashset!{}, &layer);
        let expected = Layer::ActionLayer(hashset!{Action::new_maintenance(prop)});
        assert_eq!(expected, actual);
    }
}

#[cfg(test)]
mod mutex_test {
    use super::*;

    #[test]
    fn proposition_mutexes_due_to_negation() {
        let props = hashset!{
            Proposition::from_str("caffeinated"),
            Proposition::from_str("caffeinated").negate(),
            Proposition::from_str("tired"),
        };
        let actions = hashset!{};
        let action_mutexes = MutexPairs::new();
        let expected = hashset!{
            PairSet(Proposition::from_str("caffeinated"),
                    Proposition::from_str("caffeinated").negate())
        };

        assert_eq!(
            expected,
            Layer::<String>::proposition_mutexes(
                &props,
                &actions,
                Some(&action_mutexes)
            )
        );
    }

    #[test]
    fn proposition_mutexes_due_to_mutex_actions() {
        let p1 = Proposition::from_str("caffeinated");
        let p2 = Proposition::from_str("coffee");
        let not_p2 = p2.negate();
        let actions = hashset!{};
        let a1 = Action::new(
            String::from("drink coffee"),
            hashset!{&p2},
            hashset!{&p1, &not_p2},
        );
        let a2 = Action::new(
            String::from("make coffee"),
            hashset!{},
            hashset!{&p2},
        );
        let action_mutexes = hashset!{PairSet(&a1, &a2)};
        let expected = hashset!{PairSet(p1.clone(), p2.clone())};
        let props = hashset!{p1, p2};
        assert_eq!(
            expected,
            Layer::proposition_mutexes(&props, &actions, Some(&action_mutexes))
        );
    }

    #[test]
    fn action_mutexes_due_to_inconsistent_fx() {
        let prop = Proposition::from_str("coffee");
        let not_prop = prop.negate();
        let a1 = Action::new(
            String::from("drink coffee"),
            hashset!{},
            hashset!{&not_prop}
        );

        let a2 = Action::new(
            String::from("make coffee"),
            hashset!{},
            hashset!{&prop}
        );
        let actions = hashset!{&a1, &a2};
        let props = MutexPairs::new();
        let actual = Layer::action_mutexes(&actions, Some(&props));

        let mut expected = MutexPairs::new();
        expected.insert(PairSet(&a1, &a2));

        assert_eq!(expected, actual);
    }

    #[test]
    fn action_mutexes_due_to_interference() {
        let prop = Proposition::from_str("hungry");
        let not_prop = prop.negate();
        let a1 = Action::new(
            String::from("eat sandwich"),
            hashset!{&prop},
            hashset!{&not_prop}
        );

        let a2 = Action::new(
            String::from("eat soup"),
            hashset!{&prop},
            hashset!{&not_prop}
        );
        let actions = hashset!{&a1, &a2};
        let props = MutexPairs::new();
        let actual = Layer::action_mutexes(&actions, Some(&props));

        let mut expected = MutexPairs::new();
        expected.insert(PairSet(&a1, &a2));

        assert_eq!(expected, actual);
    }

    #[test]
    fn action_mutexes_due_to_competing_needs() {
        let prop = Proposition::from_str("hungry");
        let not_prop = prop.negate();
        let a1 = Action::new(
            String::from("eat sandwich"),
            hashset!{&prop},
            hashset!{&not_prop}
        );

        let a2 = Action::new(
            String::from("eat soup"),
            hashset!{&prop},
            hashset!{&not_prop}
        );
        let actions = hashset!{&a1, &a2};
        let mut mutex_props = MutexPairs::new();
        mutex_props.insert(PairSet(prop.clone(), prop.negate()));
        let actual = Layer::action_mutexes(&actions, Some(&mutex_props));

        let mut expected = MutexPairs::new();
        expected.insert(PairSet(&a1, &a2));

        assert_eq!(expected, actual);
    }
}
