// SPDX-License-Identifier: PMPL-1.0-or-later
//! Service dependency graph and topological sorting

use anyhow::Result;
use petgraph::graph::{DiGraph, NodeIndex};
use petgraph::algo::toposort;
use std::collections::HashMap;

use crate::compose::ComposeFile;

/// Build service dependency graph
pub fn build_dependency_graph(compose: &ComposeFile) -> Result<Vec<String>> {
    let mut graph = DiGraph::<String, ()>::new();
    let mut nodes: HashMap<String, NodeIndex> = HashMap::new();

    // Create nodes for all services
    for (name, _) in &compose.services {
        let node = graph.add_node(name.clone());
        nodes.insert(name.clone(), node);
    }

    // Add edges for dependencies (A depends_on B means B -> A)
    for (name, service) in &compose.services {
        let node = nodes[name];
        for dep in &service.depends_on {
            let dep_node = nodes[dep];
            graph.add_edge(dep_node, node, ());
        }
    }

    // Topological sort to get deployment order
    let sorted = toposort(&graph, None)
        .map_err(|_| anyhow::anyhow!("Circular dependency detected"))?;

    // Convert node indices back to service names
    let order: Vec<String> = sorted
        .into_iter()
        .map(|idx| graph[idx].clone())
        .collect();

    Ok(order)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compose::{Service, ComposeFile};
    use std::collections::HashMap;

    #[test]
    fn test_simple_dependency_order() {
        let mut services = HashMap::new();

        services.insert(
            "web".to_string(),
            Service {
                image: "web.ctp".to_string(),
                depends_on: vec!["api".to_string()],
                command: None,
                environment: HashMap::new(),
                ports: vec![],
                volumes: vec![],
                networks: vec![],
                restart: "no".to_string(),
                healthcheck: None,
                deploy: None,
            },
        );

        services.insert(
            "api".to_string(),
            Service {
                image: "api.ctp".to_string(),
                depends_on: vec![],
                command: None,
                environment: HashMap::new(),
                ports: vec![],
                volumes: vec![],
                networks: vec![],
                restart: "no".to_string(),
                healthcheck: None,
                deploy: None,
            },
        );

        let compose = ComposeFile {
            version: "1.0".to_string(),
            services,
            volumes: HashMap::new(),
            networks: HashMap::new(),
            secrets: HashMap::new(),
        };

        let order = build_dependency_graph(&compose).unwrap();

        // api should come before web
        assert_eq!(order, vec!["api", "web"]);
    }
}
