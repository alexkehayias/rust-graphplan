#[macro_use] extern crate graphplan;
use graphplan::{Proposition, Action, GraphPlan, SimpleSolver};


fn main() {
    let p1 = Proposition::from("rocket1_location1");
    let p2 = Proposition::from("rocket1_location2");
    let p3 = Proposition::from("rocket1_location3");
    let p4 = Proposition::from("rocket2_location2");
    let p5 = Proposition::from("rocket2_location3");

    let a1 = Action::new(
       "move_rocket1_location2",
       hashset!{&p1},
       hashset!{&p2},
    );
    let a2 = Action::new(
       "move_rocket1_location3",
       hashset!{&p2},
       hashset!{&p3},
    );
    let a3 = Action::new(
       "move_rocket2_location3",
       hashset!{&p4},
       hashset!{&p5},
    );

    let domain = GraphPlan::create_domain(
        hashset!{&p1, &p4},
        hashset!{&p3, &p5},
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
