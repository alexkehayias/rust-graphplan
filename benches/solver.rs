#[macro_use] extern crate criterion;
use criterion::Criterion;

use graphplan::GraphPlan;
use graphplan::solver::SimpleSolver;

fn solver_benchmark(c: &mut Criterion) {
    c.bench_function("solve 100", move |b| b.iter(||{
        let path = String::from("resources/rocket_domain.toml");
        let mut pg: GraphPlan<_, _, SimpleSolver> = GraphPlan::from_toml(path);
        pg.search();
    }));
}

criterion_group!(benches, solver_benchmark);
criterion_main!(benches);
