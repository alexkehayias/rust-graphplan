#[macro_use]
extern crate criterion;


use graphplan::*;

use criterion::Criterion;

fn benchmark(c: &mut Criterion) {
    let p1 = Proposition::from_str("tired");
    let p2 = Proposition::from_str("dog needs to pee");
    let p3 = Proposition::from_str("caffeinated");

    let a1 = Action::new(
        String::from("coffee"),
        hashset!{p1.clone()},
        hashset!{p3.clone()}
    );
    let a2 = Action::new(
        String::from("walk dog"),
        hashset!{p2.clone(), p3.clone()},
        hashset!{p2.clone().negate()},
    );

    c.bench_function("plangraph 100", move |b| b.iter(||{
        let mut pg = PlanGraph::new(
            hashset!{p1.clone(), p2.clone(), p3.clone()},
            hashset!{p1.clone().negate(),
                     p2.negate().clone(),
                     p3.clone()},
            hashset!{a1.clone(), a2.clone()}
        );
        for _ in 0..100 {
            pg.extend();
        }
    }));
}

criterion_group!(benches, benchmark);
criterion_main!(benches);
