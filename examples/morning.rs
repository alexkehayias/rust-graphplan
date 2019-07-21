use graphplan::GraphPlan;
use graphplan::solver::SimpleSolver;
use graphplan::plangraph::PlanGraph;

fn main() -> () {
    let path = String::from("resources/morning_domain.toml");
    let mut pg: GraphPlan<String, SimpleSolver> = GraphPlan::from_toml(path);
    println!("Result: {:?}", PlanGraph::format_plan(pg.search().unwrap()));
}
