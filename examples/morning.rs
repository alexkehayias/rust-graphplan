#[macro_use] extern crate graphplan;
use graphplan::{Proposition, Action, GraphPlan, SimpleSolver};


fn main() {
    let p1 = Proposition::from("tired");
    let not_p1 = p1.negate();

    let p2 = Proposition::from("dog needs to pee");
    let not_p2 = p2.negate();

    let p3 = Proposition::from("at work");
    let p4 = p3.negate();

    let a1 = Action::new(
        "drink coffee",
        hashset!{&p1},
        hashset!{&not_p1}
    );

    let a2 = Action::new(
        "walk dog",
        hashset!{&p2, &not_p1},
        hashset!{&not_p2},
    );

    let a3 = Action::new(
        "go to work",
        hashset!{&not_p1, &not_p2},
        hashset!{&p3},
    );

    let domain = GraphPlan::create_domain(
        hashset!{&p1, &p2, &p4},
        hashset!{&not_p1, &not_p2, &p3},
        hashset!{&a1, &a2, &a3}
    );
    let mut pg = GraphPlan::from_domain(&domain);

    println!("Plan:");

    for step in pg.search::<SimpleSolver>().unwrap() {
        for action in step {
            println!("- {:?}", action.id);
        }
    }
}
