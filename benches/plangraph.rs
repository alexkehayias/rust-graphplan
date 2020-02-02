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

    c.bench_function("plangraph 100", move |b| b.iter(||{
        let mut pg = PlanGraph::new(
            hashset!{p1.clone(), p2.clone(), p3.clone()},
            hashset!{not_p1.clone(), not_p2.clone(), p3.clone()},
            hashset!{a1.clone(), a2.clone()}
        );
        for _ in 0..100 {
            pg.extend();
        }
    }));
}

criterion_group!(benches, plangraph_benchmark);
criterion_main!(benches);
