#[macro_use] extern crate criterion;
use criterion::Criterion;

#[macro_use] extern crate graphplan;
use graphplan::{GraphPlan, Proposition, Action};
use graphplan::solver::SimpleSolver;

fn solver_benchmark(c: &mut Criterion) {
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

    c.bench_function("solve 100", |b| b.iter(||{
        let mut pg = GraphPlan::from_domain(&domain);
        pg.search::<SimpleSolver>();
    }));
}

criterion_group!(benches, solver_benchmark);
criterion_main!(benches);
