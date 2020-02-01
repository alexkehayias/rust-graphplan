# Graphplan

Implementation of the Graphplan planning algorithm and Plangraph data structure written in Rust.

Original paper: [Fast Planning Through Planning Graph Analysis](https://www.cs.cmu.edu/~avrim/Papers/graphplan.pdf) by Avrim L. Blum and Merrick L. Furst

## Usage

You can load a domain from a toml file (see the `resources/example.toml` directory for expected format) and instantiate a `GraphPlan`.

Example:

```rust
#[macro_use] extern crate graphplan;
use graphplan::GraphPlan;
use graphplan::solver::SimpleSolver;
use graphplan::plangraph::PlanGraph;

fn main() -> () {
    let path = String::from("resources/rocket_domain.toml");
    let mut pg: GraphPlan<_, SimpleSolver> = GraphPlan::from_toml(path);
    println!("Result: {:?}", PlanGraph::format_plan(pg.search()));
}
```

The lower level API allows you to constructing your own problem domain programmatically. You can also implement your own solver by implementing the `GraphPlanSolver` trait.

Example:

```rust
#[macro_use] extern crate graphplan;
use graphplan::GraphPlan;
use graphplan::proposition::Proposition;
use graphplan::action::Action;
use graphplan::solver::SimpleSolver;

fn main() -> () {
    let p1 = Proposition::from("tired");
    let not_p1 = p1.negate();
    let p2 = Proposition::from("dog needs to pee");
    let not_p2 = p2.negate();

    let a1 = Action::new(
        String::from("coffee"),
        hashset!{&p1},
        hashset!{&not_p1}
    );

    let a2 = Action::new(
        String::from("walk dog"),
        hashset!{&p2, &not_p1},
        hashset!{&not_p2},
    );

    let mut pg = GraphPlan::new(
        hashset!{p1, p2},
        hashset!{not_p1, not_p2},
        hashset!{a1, a2},
        SimpleSolver::new()
    );

    println!("Result: {:?}", PlanGraph::format_plan(pg.search());
}
```

### Finite actions

Sometimes you want to have a set actions that can be statically checked. To do that you can use your own `ActionId` which will be used to uniquely identify an `Action`. For example:

```rust
#[derive(Debug, Clone, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub enum MyAction {
    Drink,
    Eat,
}

let p1 = graphplan::Proposition::from("hungry");
let p2 = p1.negate();
let p3 = graphplan::Proposition::from("thirsty");
let p4 = p3.negate();

let a1 = graphplan::Action::new(
  MyAction::Eat,
  hashset!{&p1},
  hashset!{&p2},
);

let a2 = graphplan::Action::new(
  MyAction::Drink,
  hashset!{&p3},
  hashset!{&p4},
);
```

Now our planner knows there will only ever by `MyAction` set of actions. This is particularly useful if you will be iterating over a plan and want to get the benefit of exhaustiveness checking that way when you add a new action, you'll get a compile time error that you forgot to handle your new action.

### Embedding data in actions

Sometimes we also want to attach additional data that's not relevant to GraphPlan, but convenient to pass through. To do that, you can use an enum variant's associated data.

```rust
#[derive(Debug, Clone, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub struct LocationId(String);

#[derive(Debug, Clone, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub enum MyAction {
    Move(LocationId),
}

let p1 = graphplan::Proposition::from("location1");

let a1 = graphplan::Action::new(
  MyAction::Move(LocationId(String::from("1"))),
  hashset!{},
  hashset!{&p1},
);

```

Now the caller can access the associated data of an action (which is specific to your program) when doing something with a returned plan. This avoids a bunch of boilerplate by preventing the need for a wrapper around GraphPlan to map actions to your program.

### Finite propositions

The same thing applies to `Proposition`s, you can use your own type to use as an identifier.

For example
```rust
#[derive(PartialEq, Clone, Hash, Eq)]
enum MyPropositions {
    A,
    B,
}

impl From<MyPropositions> for graphplan::Proposition<MyPropositions> {
    fn from(prop: MyPropositions) -> Self {
        Self::new(prop, false)
    }
}

let p1 = graphplan::Proposition::from(Props::A);
let p2 = graphplan::Proposition::from(Props::B);

// We can now use it with an action
let action = graphplan::Action::new(
  "my action ID",
  hashset!{&p1},
  hashset!{&p2},
);

```

## Running benchmarks

Benchmarks using `criterion` can be found in the `benches` directory. To run them:

```
cargo bench
open target/criterion/report/index.html
```

## Running the wasm demo

This crate ships with a demo of running graphplan in the browser via wasm. To run it you will need to install the `cargo-web` plugin then run the following.

```
cargo web --features wasm
open localhost:8000
```

Open the js console and you can use the method `graphplan.run` to take a toml formatted string (see `resources/example.toml`) and return a plan if there is one.

## License

Copyright (c) Alex Kehayias. All rights reserved. The use and
distribution terms for this software are covered by the Eclipse Public
License 1.0 (http://opensource.org/licenses/eclipse-1.0.php). By using
this software in any fashion, you are agreeing to be bound by the
terms of this license. You must not remove this notice, or any other,
from this software.
