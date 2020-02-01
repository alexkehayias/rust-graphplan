#[cfg(feature = "toml")]
fn run() {
    use graphplan::GraphPlan;
    use graphplan::solver::SimpleSolver;
    use graphplan::plangraph::PlanGraph;

    let path = String::from("resources/morning_domain.toml");
    let mut pg: GraphPlan<_, _, SimpleSolver> = GraphPlan::from_toml(path);
    println!("Result: {:?}", PlanGraph::format_plan(pg.search().unwrap()));
}

#[cfg(not(feature = "toml"))]
fn run() {
    println!("This example requires the \"toml\" feature. Try running `cargo run --example morning --features toml")
}

fn main() -> () {
    run()
}
