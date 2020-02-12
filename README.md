# Graphplan

![crates.io](https://img.shields.io/crates/v/graphplan.svg)

Implementation of the Graphplan planning algorithm and Plangraph data structure written in Rust.

Original paper: [Fast Planning Through Planning Graph Analysis](https://www.cs.cmu.edu/~avrim/Papers/graphplan.pdf) by Avrim L. Blum and Merrick L. Furst

## Example

Below is an example plan you might put together using Graphplan. We describe the 'morning' domain as an initial state (called 'propositions'), actions (which have preconditions and effects which alter the state), and a goal state.

```rust
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
```

## Advanced Usage

### Finite actions

Sometimes you want to have a set actions that can be statically checked. To do that you can use your own `ActionId` which will be used to uniquely identify an `Action`. For example:

```rust
#[derive(Debug, Clone, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub enum MyAction {
    Drink,
    Eat,
}

let p1 = Proposition::from("hungry");
let p2 = p1.negate();
let p3 = Proposition::from("thirsty");
let p4 = p3.negate();

let a1 = Action::new(
  MyAction::Eat,
  hashset!{&p1},
  hashset!{&p2},
);

let a2 = Action::new(
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

let p1 = Proposition::from("location1");

let a1 = Action::new(
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

impl From<MyPropositions> for Proposition<MyPropositions> {
    fn from(prop: MyPropositions) -> Self {
        Self::new(prop, false)
    }
}

let p1 = Proposition::from(Props::A);
let p2 = Proposition::from(Props::B);

// We can now use it with an action
let action = Action::new(
  "my action ID",
  hashset!{&p1},
  hashset!{&p2},
);

```

## Running benchmarks

Benchmarks using `criterion` can be found in the `benches` directory. To run them:

```
cargo bench --features toml
open target/criterion/report/index.html
```

## Running examples

```
cargo run --example morning
```

-------------------

## License

Copyright (c) Alex Kehayias. All rights reserved. The use and
distribution terms for this software are covered by the Eclipse Public
License 1.0 (http://opensource.org/licenses/eclipse-1.0.php). By using
this software in any fashion, you are agreeing to be bound by the
terms of this license. You must not remove this notice, or any other,
from this software.
