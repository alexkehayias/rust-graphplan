# Graphplan

Implementation of the Graphplan planning algorithm and Plangraph data structure from Avrim L. Blum and Merrick L. Furst written in Rust.

## Usage

```rust
use graphplan::macros;
use graphplan::proposition::Proposition;
use graphplan::action::Action;
use graphplan::plangraph::PlanGraph;

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

let mut pg = PlanGraph::new(
    hashset!{p1.clone(), p2.clone()},
    hashset!{p1.clone().negate(),
             p2.clone().negate(),
             p3.clone()},
    hashset!{a1.clone(), a2.clone()}
);
pg.extend();
pg.extend();

let solver = SimpleSolver::new();
println!("Result: {:?}", PlanGraph::format_plan(solver.search(&pg));
```

## License

Copyright (c) Alex Kehayias. All rights reserved. The use and
distribution terms for this software are covered by the Eclipse Public
License 1.0 (http://opensource.org/licenses/eclipse-1.0.php). By using
this software in any fashion, you are agreeing to be bound by the
terms of this license. You must not remove this notice, or any other,
from this software.
