#[macro_use]
extern crate stdweb;

#[macro_use]
extern crate graphplan;

use graphplan::GraphPlan;
use graphplan::plangraph::PlanGraph;
use graphplan::proposition::Proposition;
use graphplan::action::Action;
use graphplan::solver::SimpleSolver;

fn main() {
    stdweb::initialize();
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

    js!(
        console.log(@{format!("{:?}", PlanGraph::format_plan(pg.search()))});
    );
}
