#[macro_use] extern crate criterion;
use criterion::Criterion;

#[macro_use] extern crate graphplan;
use graphplan::proposition::Proposition;
use graphplan::action::Action;
use graphplan::plangraph::PlanGraph;

fn plangraph_benchmark(c: &mut Criterion) {
    let p1 = Proposition::from("tired");
    let not_p1 = p1.negate();

    let p2 = Proposition::from("dog needs to pee");
    let not_p2 = p2.negate();

    let p3 = Proposition::from("caffeinated");

    let a1 = Action::new(
        String::from("coffee"),
        hashset!{&p1},
        hashset!{&p3}
    );
    let a2 = Action::new(
        String::from("walk dog"),
        hashset!{&p2, &p3},
        hashset!{&not_p2},
    );

    c.bench_function("plangraph 100", |b| b.iter(||{
        let mut pg = PlanGraph::new(
            hashset!{&p1, &p2, &p3},
            hashset!{&not_p1, &not_p2, &p3},
            hashset!{&a1, &a2}
        );
        for i in 0..100 {
            pg.extend();
            assert!(pg.depth() == i + 1);
        }
    }));
}

criterion_group!(benches, plangraph_benchmark);
criterion_main!(benches);
