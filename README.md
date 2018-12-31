# Graphplan

Implementation of the Graphplan planning algorithm and Plangraph data structure written in Rust.

Original paper: [Fast Planning Through Planning Graph Analysis](https://www.cs.cmu.edu/~avrim/Papers/graphplan.pdf) by Avrim L. Blum and Merrick L. Furst

## Usage

You can load a domain from a toml file (see the `resources/example.toml` directory for expected format) and instantiate a `GraphPlan`.

Example:

```rust
let path = String::from("resources/rocket_domain.toml");
let mut pg: GraphPlan<SimpleSolver> = GraphPlan::from_toml(path);
pg.search();
```

The lower level API allows you to constructing your own problem domain programmatically. You can also implement your own solver by implementing the `GraphPlanSolver` trait.

Example:

```rust
#[macro_use] extern crate graphplan;
use graphplan::GraphPlan;
use graphplan::proposition::Proposition;
use graphplan::action::Action;
use graphplan::solver::SimpleSolver;

#[test]
fn integration() {
    let p1 = Proposition::from_str("tired");
    let p2 = Proposition::from_str("dog needs to pee");
    let p3 = Proposition::from_str("caffeinated");

    let a1 = Action::new(
        String::from("coffee"),
        hashset!{p1.clone()},
        hashset!{p3.clone(), p1.clone().negate()}
    );

    let a2 = Action::new(
        String::from("walk dog"),
        hashset!{p2.clone(), p3.clone()},
        hashset!{p2.clone().negate()},
    );

    let mut pg = GraphPlan::new(
        hashset!{p1.clone(), p2.clone()},
        hashset!{p1.clone().negate(),
                 p2.clone().negate(),
                 p3.clone()},
        hashset!{a1.clone(), a2.clone()},
        SimpleSolver::new()
    );
    println!("Result: {:?}", PlanGraph::format_plan(pg.search());
}
```

## Running benchmarks

Benchmarks using `criterion` can be found in the `benches` directory. To run them:

```
cargo bench
open target/criterion/report/index.html
```

## License

Copyright (c) Alex Kehayias. All rights reserved. The use and
distribution terms for this software are covered by the Eclipse Public
License 1.0 (http://opensource.org/licenses/eclipse-1.0.php). By using
this software in any fashion, you are agreeing to be bound by the
terms of this license. You must not remove this notice, or any other,
from this software.
