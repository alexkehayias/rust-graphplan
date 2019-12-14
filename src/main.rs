#[cfg(feature = "wasm")]
#[macro_use] extern crate stdweb;

extern crate graphplan;

use graphplan::GraphPlan;
use graphplan::plangraph::PlanGraph;
use graphplan::solver::SimpleSolver;


#[cfg(feature = "wasm")]
fn graphplan(toml_string: String) -> String {
    let mut pg: GraphPlan<String, SimpleSolver> = GraphPlan::from_toml_string(toml_string);
    let solution = PlanGraph::format_plan(pg.search().expect("No solution found"));
    format!(
        "{:?}",
        solution.iter()
            .map(|i| format!("[{}]",
                             i.iter().map(|j| format!("{:?}", j.id))
                             .collect::<Vec<_>>()
                             .join(", ")))
            .collect::<Vec<_>>()
            .join(", "))
}


/// Attach the graphplan fn to the dom and it will now be callable via
/// js i.e `graphplan.run("some toml string")`
#[cfg(feature = "wasm")]
fn run() {
    stdweb::initialize();
    js!{
        window.graphplan = {};
        window.graphplan.run = @{graphplan};
    };
}

#[cfg(not(feature = "wasm"))]
fn run() {
    println!("This bin requires the \"wasm\" feature. Try running `cargo web --features wasm")
}

fn main() {
    run()
}
