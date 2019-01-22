#[macro_use]
extern crate stdweb;
extern crate graphplan;

use graphplan::GraphPlan;
use graphplan::plangraph::PlanGraph;
use graphplan::solver::SimpleSolver;


fn graphplan(toml_string: String) -> String {
    let mut pg: GraphPlan<SimpleSolver> = GraphPlan::from_toml_string(toml_string);
    let solution = PlanGraph::format_plan(pg.search()).expect("No solution found");
    format!(
        "{:?}",
        solution.iter()
            .map(|i| format!("[{}]",
                             i.iter().map(|j| j.name.clone())
                             .collect::<Vec<_>>()
                             .join(", ")))
            .collect::<Vec<_>>()
            .join(", "))
}

// Attach the graphplan fn to the dom and it will now be callable via
// js i.e `graphplan.run("some toml string")`
fn main() {
    stdweb::initialize();
    js!{
        window.graphplan = {};
        window.graphplan.run = @{graphplan};
    };
}
