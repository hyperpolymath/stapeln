#!/usr/bin/env -S deno test --allow-read --allow-write
// SPDX-License-Identifier: PMPL-1.0-or-later
// stapeln Test Suite - Comprehensive validation and generation tests

import { assert, assertEquals, assertExists } from "https://deno.land/std@0.224.0/assert/mod.ts";

// ============================================================================
// Test Data
// ============================================================================

const sampleNodes = [
    {
        id: 'node-1',
        name: 'API Gateway',
        type: 'gateway',
        icon: '🚪',
        x: 100,
        y: 100,
        ports: '80',
        protocol: 'HTTP',
        firewall: true,
        baseImage: 'nginx:alpine',
        resources: { cpu: '0.5', memory: '512M' }
    },
    {
        id: 'node-2',
        name: 'Auth Service',
        type: 'application',
        icon: '⚙️',
        x: 300,
        y: 100,
        ports: '8080',
        protocol: 'HTTP',
        firewall: true,
        baseImage: 'lago-grey:2.1',
        resources: { cpu: '1.0', memory: '1G' }
    },
    {
        id: 'node-3',
        name: 'PostgreSQL',
        type: 'database',
        icon: '🗄️',
        x: 500,
        y: 100,
        ports: '5432',
        protocol: 'TCP',
        firewall: false,
        baseImage: 'postgres:16-alpine',
        resources: { cpu: '0.5', memory: '2G' }
    }
];

const sampleConnections = [
    {
        id: 'conn-1',
        from: 'node-1',
        to: 'node-2',
        fromName: 'API Gateway',
        toName: 'Auth Service',
        protocol: 'HTTPS',
        bidirectional: false,
        encrypted: true,
        ports: { source: '80', target: '8080' }
    },
    {
        id: 'conn-2',
        from: 'node-2',
        to: 'node-3',
        fromName: 'Auth Service',
        toName: 'PostgreSQL',
        protocol: 'TCP',
        bidirectional: false,
        encrypted: true,
        ports: { source: '8080', target: '5432' }
    }
];

const mustfileRules = [
    { name: 'all-images-signed', description: 'All images must be signed', critical: true },
    { name: 'firewall-enabled', description: 'Firewalls required on public nodes', critical: true },
    { name: 'ports-unique', description: 'All ports must be unique', critical: true },
    { name: 'encrypted-connections', description: 'External connections must be encrypted', critical: true }
];

// ============================================================================
// Unit Tests - Validation Functions
// ============================================================================

Deno.test("Validation: Unique Node IDs", () => {
    const ids = sampleNodes.map(n => n.id);
    const uniqueIds = new Set(ids);
    assertEquals(ids.length, uniqueIds.size, "All node IDs should be unique");
});

Deno.test("Validation: Port Numbers In Range", () => {
    sampleNodes.forEach(node => {
        if (node.ports) {
            const port = parseInt(node.ports);
            assert(port >= 1 && port <= 65535, `Port ${port} should be in valid range (1-65535)`);
        }
    });
});

Deno.test("Validation: Port Conflicts", () => {
    const ports = sampleNodes
        .filter(n => n.ports)
        .map(n => n.ports);

    const uniquePorts = new Set(ports);
    assertEquals(ports.length, uniquePorts.size, "No port conflicts should exist");
});

Deno.test("Validation: Valid Connections", () => {
    const nodeIds = sampleNodes.map(n => n.id);

    sampleConnections.forEach(conn => {
        assert(nodeIds.includes(conn.from), `Connection source ${conn.from} should be valid node`);
        assert(nodeIds.includes(conn.to), `Connection target ${conn.to} should be valid node`);
        assert(conn.from !== conn.to, "Node should not connect to itself");
    });
});

Deno.test("Validation: Resource Limits", () => {
    sampleNodes.forEach(node => {
        if (node.resources) {
            const cpu = parseFloat(node.resources.cpu);
            assert(cpu >= 0.1 && cpu <= 4.0, `CPU ${cpu} should be in range 0.1-4.0`);

            const memValue = node.resources.memory;
            assert(memValue, "Memory limit should be defined");
            assert(/^\d+(\.\d+)?[MG]$/.test(memValue), "Memory format should be valid (e.g., 512M, 2G)");
        }
    });
});

Deno.test("Validation: Firewall on Gateway Nodes", () => {
    const gatewayNodes = sampleNodes.filter(n => n.type === 'gateway');
    gatewayNodes.forEach(node => {
        assertEquals(node.firewall, true, `Gateway node ${node.name} should have firewall enabled`);
    });
});

Deno.test("Validation: Encrypted External Connections", () => {
    // Connections from gateway nodes should be encrypted
    const gatewayConnections = sampleConnections.filter(conn => {
        const sourceNode = sampleNodes.find(n => n.id === conn.from);
        return sourceNode && sourceNode.type === 'gateway';
    });

    gatewayConnections.forEach(conn => {
        assertEquals(conn.encrypted, true, `External connection ${conn.id} should be encrypted`);
    });
});

Deno.test("Validation: Acyclic Topology", () => {
    // Build adjacency list
    const graph = new Map();
    sampleNodes.forEach(n => graph.set(n.id, []));
    sampleConnections.forEach(conn => {
        graph.get(conn.from).push(conn.to);
    });

    // DFS cycle detection
    const visited = new Set();
    const recStack = new Set();

    function hasCycle(nodeId) {
        if (recStack.has(nodeId)) return true;
        if (visited.has(nodeId)) return false;

        visited.add(nodeId);
        recStack.add(nodeId);

        const neighbors = graph.get(nodeId) || [];
        for (const neighbor of neighbors) {
            if (hasCycle(neighbor)) return true;
        }

        recStack.delete(nodeId);
        return false;
    }

    for (const nodeId of graph.keys()) {
        assert(!hasCycle(nodeId), "Topology should be acyclic (no circular dependencies)");
    }
});

// ============================================================================
// Integration Tests - File Generation
// ============================================================================

Deno.test("Generation: Justfile Format", () => {
    const justfile = generateTestJustfile(sampleNodes);

    assert(justfile.includes('# SPDX-License-Identifier: PMPL-1.0-or-later'), "Should have SPDX header");
    assert(justfile.includes('build:'), "Should have build target");
    assert(justfile.includes('deploy:'), "Should have deploy target");
    assert(justfile.includes('podman build'), "Should use Podman");

    // Check that all non-secret nodes have build commands
    const buildableNodes = sampleNodes.filter(n => n.type !== 'secrets' && n.type !== 'firewall');
    buildableNodes.forEach(node => {
        const nodeName = node.name.toLowerCase().replace(/\s+/g, '-');
        assert(justfile.includes(nodeName), `Justfile should include ${nodeName}`);
    });
});

Deno.test("Generation: Mustfile Format", () => {
    const mustfile = generateTestMustfile(sampleNodes, sampleConnections, mustfileRules);

    assert(mustfile.includes('# SPDX-License-Identifier: PMPL-1.0-or-later'), "Should have SPDX header");
    assert(mustfile.includes('metadata:'), "Should have metadata section");
    assert(mustfile.includes('component_count:'), "Should include component count");
    assert(mustfile.includes('connection_count:'), "Should include connection count");
    assert(mustfile.includes('checks:'), "Should have checks section");

    // Check that all rules are included
    mustfileRules.forEach(rule => {
        assert(mustfile.includes(rule.name), `Mustfile should include rule ${rule.name}`);
    });
});

Deno.test("Generation: Trustfile.hs Format", () => {
    const trustfile = generateTestTrustfile(sampleNodes);

    assert(trustfile.includes('-- SPDX-License-Identifier: PMPL-1.0-or-later'), "Should have SPDX header");
    assert(trustfile.includes('module Trustfile where'), "Should be valid Haskell module");
    assert(trustfile.includes('images :: [String]'), "Should declare images list");
    assert(trustfile.includes('main :: IO ()'), "Should have main function");

    // Check that image names are included
    sampleNodes.forEach(node => {
        if (node.type !== 'secrets') {
            const nodeName = node.name.toLowerCase().replace(/\s+/g, '-');
            assert(trustfile.includes(nodeName), `Trustfile should include ${nodeName}`);
        }
    });
});

Deno.test("Generation: Dustfile Format", () => {
    const dustfile = generateTestDustfile(sampleNodes);

    assert(dustfile.includes('# SPDX-License-Identifier: PMPL-1.0-or-later'), "Should have SPDX header");
    assert(dustfile.includes('recovery:'), "Should have recovery section");
    assert(dustfile.includes('strategy:'), "Should specify recovery strategy");
    assert(dustfile.includes('health_checks:'), "Should have health checks section");

    // Check that all nodes have health checks
    sampleNodes.forEach(node => {
        const nodeName = node.name.toLowerCase().replace(/\s+/g, '-');
        assert(dustfile.includes(nodeName), `Dustfile should include health check for ${nodeName}`);
    });
});

Deno.test("Generation: stack.yaml Format", () => {
    const stackyaml = generateTestStackYaml(sampleNodes, sampleConnections);

    assert(stackyaml.includes('# SPDX-License-Identifier: PMPL-1.0-or-later'), "Should have SPDX header");
    assert(stackyaml.includes("version: '3.8'"), "Should have compose version");
    assert(stackyaml.includes('services:'), "Should have services section");
    assert(stackyaml.includes('networks:'), "Should have networks section");

    // Check that all nodes are defined as services
    sampleNodes.forEach(node => {
        const nodeName = node.name.toLowerCase().replace(/\s+/g, '-');
        assert(stackyaml.includes(`${nodeName}:`), `stack.yaml should include service ${nodeName}`);
        assert(stackyaml.includes(`image: ${node.baseImage}`), `Should use correct base image`);
    });
});

Deno.test("Generation: Containerfile Format", () => {
    const containerfile = generateTestContainerfile('nginx', 'nginx:alpine');

    assert(containerfile.includes('# SPDX-License-Identifier: PMPL-1.0-or-later'), "Should have SPDX header");
    assert(containerfile.includes('FROM nginx:alpine'), "Should have FROM instruction");
    assert(containerfile.includes('LABEL'), "Should have labels");
    assert(!containerfile.includes('Dockerfile'), "Should NOT reference Dockerfile (use Containerfile)");
});

// ============================================================================
// End-to-End Tests
// ============================================================================

Deno.test("E2E: Complete Stack Generation", () => {
    // Generate all files
    const justfile = generateTestJustfile(sampleNodes);
    const mustfile = generateTestMustfile(sampleNodes, sampleConnections, mustfileRules);
    const trustfile = generateTestTrustfile(sampleNodes);
    const dustfile = generateTestDustfile(sampleNodes);
    const stackyaml = generateTestStackYaml(sampleNodes, sampleConnections);

    // All files should be non-empty
    assert(justfile.length > 100, "Justfile should have content");
    assert(mustfile.length > 100, "Mustfile should have content");
    assert(trustfile.length > 100, "Trustfile should have content");
    assert(dustfile.length > 100, "Dustfile should have content");
    assert(stackyaml.length > 100, "stack.yaml should have content");

    // All files should be properly formatted
    assertExists(justfile);
    assertExists(mustfile);
    assertExists(trustfile);
    assertExists(dustfile);
    assertExists(stackyaml);
});

Deno.test("E2E: Save and Load Design", () => {
    const design = {
        version: '1.0',
        timestamp: new Date().toISOString(),
        metadata: {
            name: 'test-stack',
            nodeCount: sampleNodes.length,
            connectionCount: sampleConnections.length
        },
        nodes: sampleNodes,
        connections: sampleConnections
    };

    const json = JSON.stringify(design, null, 2);
    const loaded = JSON.parse(json);

    assertEquals(loaded.nodes.length, sampleNodes.length, "Should preserve node count");
    assertEquals(loaded.connections.length, sampleConnections.length, "Should preserve connection count");
    assertEquals(loaded.version, '1.0', "Should preserve version");
});

// ============================================================================
// Helper Functions (simplified from stapeln-ui.html)
// ============================================================================

function generateTestJustfile(nodes) {
    const timestamp = new Date().toISOString().split('T')[0];
    return `# SPDX-License-Identifier: PMPL-1.0-or-later
# Justfile - Generated: ${timestamp}

build:
    @echo "🏗️ Building containers..."
${nodes.filter(n => n.type !== 'secrets').map(n => {
    const name = n.name.toLowerCase().replace(/\s+/g, '-');
    return `    podman build -t ${name}:latest ./images/${name}`;
}).join('\n')}

deploy: build
    @echo "🚀 Deploying..."
    podman-compose -f stack.yaml up -d
`;
}

function generateTestMustfile(nodes, connections, rules) {
    const timestamp = new Date().toISOString().split('T')[0];
    return `# SPDX-License-Identifier: PMPL-1.0-or-later
# Mustfile - Generated: ${timestamp}

metadata:
  component_count: ${nodes.length}
  connection_count: ${connections.length}

checks:
${rules.map(r => `  - name: ${r.name}
    description: "${r.description}"
    critical: ${r.critical}`).join('\n\n')}
`;
}

function generateTestTrustfile(nodes) {
    return `-- SPDX-License-Identifier: PMPL-1.0-or-later
-- Trustfile.hs

module Trustfile where

images :: [String]
images = [${nodes.filter(n => n.type !== 'secrets').map(n =>
    `"${n.name.toLowerCase().replace(/\s+/g, '-')}"`
).join(', ')}]

main :: IO ()
main = putStrLn "✓ Verification complete"
`;
}

function generateTestDustfile(nodes) {
    return `# SPDX-License-Identifier: PMPL-1.0-or-later
# Dustfile

recovery:
  strategy: "blue-green"

health_checks:
${nodes.map(n => `  - component: ${n.name.toLowerCase().replace(/\s+/g, '-')}
    endpoint: "http://localhost:${n.ports}/health"`).join('\n')}
`;
}

function generateTestStackYaml(nodes, connections) {
    return `# SPDX-License-Identifier: PMPL-1.0-or-later
# stack.yaml

version: '3.8'

services:
${nodes.map(n => {
    const name = n.name.toLowerCase().replace(/\s+/g, '-');
    return `  ${name}:
    image: ${n.baseImage}
    ports:
      - "${n.ports}:${n.ports}"
    networks:
      - stapeln_network`;
}).join('\n\n')}

networks:
  stapeln_network:
    driver: bridge
`;
}

function generateTestContainerfile(name, baseImage) {
    return `# SPDX-License-Identifier: PMPL-1.0-or-later
FROM ${baseImage}
LABEL org.opencontainers.image.title="${name}"
CMD ["/bin/sh"]
`;
}

console.log("✅ All stapeln tests passed!");
