// SPDX-License-Identifier: PMPL-1.0-or-later
// PipelineModel.res - Types and helpers for the visual assembly pipeline designer
//
// A node-graph editor where users construct container build workflows visually.
// Each node represents a stage in the container assembly pipeline, and directed
// edges define the flow of artifacts between stages.

// Status of an individual pipeline node during execution
type nodeStatus = Idle | Running | Success | Failed | Skipped

// Pipeline node types — each variant represents a distinct stage in the assembly
type nodeKind =
  // Pull a base image from a registry
  | Source({imageRef: string, tag: string})
  // Lago Grey formation (Chainguard-style minimal base)
  | LagoGreyFormation({formation: string, size: int})
  // Build step: RUN, COPY, ADD, or other Containerfile instruction
  | BuildStep({command: string, layer: int})
  // Svalinn security verification gate
  | SecurityGate({policy: string, mode: string})
  // Cerro Torre cryptographic signing
  | SignStep({keyId: string, transparency: bool})
  // Software bill of materials generation
  | SbomGenerate({format: string})
  // SLSA provenance attestation
  | ProvenanceAttach({builder: string})
  // Push built image to a registry
  | Push({registry: string, repository: string})
  // Multi-service composition (selur-compose style)
  | Compose({services: array<string>})
  // User-defined script step
  | CustomScript({script: string, shell: string})

// A single node in the pipeline graph
type pipelineNode = {
  id: string,
  kind: nodeKind,
  x: float,
  y: float,
  width: float,
  height: float,
  label: string,
  status: nodeStatus,
  validationErrors: array<string>,
}

// Directed edge between two nodes
type connection = {
  id: string,
  fromNode: string,
  fromPort: string, // "output" or a named port like "artifact", "metadata"
  toNode: string,
  toPort: string, // "input" or a named port like "image", "config"
}

// The full pipeline graph
type pipeline = {
  id: string,
  name: string,
  description: string,
  nodes: array<pipelineNode>,
  connections: array<connection>,
  selectedNode: option<string>,
  selectedConnection: option<string>,
  zoom: float,
  panX: float,
  panY: float,
}

// Output format targets for code generation
type outputFormat =
  | Containerfile
  | SelurCompose
  | PodmanCompose
  | K8sManifest
  | HelmChart
  | OciBundle

// Per-node validation issue
type nodeIssue = {
  nodeId: string,
  message: string,
}

// Validation result for the entire pipeline
type pipelineValidation = {
  isValid: bool,
  errors: array<nodeIssue>,
  warnings: array<nodeIssue>,
  securityScore: float, // 0.0–100.0
  estimatedImageSize: int, // bytes
}

// Left sidebar tabs
type leftTab = Components | Templates | History

// Right sidebar tabs
type rightTab = Preview | Validation | Output | Security

// Three-panel layout state (PanLL-inspired)
type panelState = {
  leftPanelWidth: float,
  rightPanelWidth: float,
  leftCollapsed: bool,
  rightCollapsed: bool,
  activeLeftTab: leftTab,
  activeRightTab: rightTab,
}

// Drag-in-progress state for moving nodes
type dragState = {
  nodeId: string,
  offsetX: float,
  offsetY: float,
}

// Connection-in-progress state for drawing edges
type connectState = {
  fromNode: string,
  fromPort: string,
  mouseX: float,
  mouseY: float,
}

// Summary entry for the recent pipelines list
type pipelineSummary = {
  id: string,
  name: string,
  updatedAt: string,
}

// Reusable pipeline template
type pipelineTemplate = {
  id: string,
  name: string,
  description: string,
  category: string,
  pipeline: pipeline,
}

// Top-level state for the pipeline designer view
type pipelineDesignerState = {
  pipeline: pipeline,
  panels: panelState,
  validation: option<pipelineValidation>,
  outputFormat: outputFormat,
  generatedOutput: option<string>,
  isDragging: option<dragState>,
  isConnecting: option<connectState>,
  templates: array<pipelineTemplate>,
  recentPipelines: array<pipelineSummary>,
  isDirty: bool,
}

// Messages dispatched by the pipeline designer UI
type pipelineMsg =
  // Node operations
  | AddNode(nodeKind, Model.position)
  | RemoveNode(string)
  | MoveNode(string, Model.position)
  | SelectNode(option<string>)
  | UpdateNodeConfig(string, string, string) // nodeId, key, value
  | UpdateNodeLabel(string, string)
  // Connection operations
  | AddEdge(string, string, string, string) // fromNode, fromPort, toNode, toPort
  | RemoveEdge(string)
  // Pipeline metadata
  | SetPipelineName(string)
  // Left panel (palette)
  | SetPaletteTab(leftTab)
  | SetPaletteSearch(string)
  | TogglePaletteCollapsed
  | SetPaletteWidth(float)
  // Right panel (output)
  | SetOutputTab(rightTab)
  | ToggleOutputCollapsed
  | SetOutputWidth(float)
  // Export / code generation
  | SetOutputFormat(outputFormat)
  | RegeneratePreview
  | CopyToClipboard
  | DownloadFile(string)
  | ExportAllZip
  | Deploy
  // Templates and history
  | LoadTemplate(pipelineTemplate)
  | LoadRecentPipeline(string)
  // Validation
  | RunValidation
  | ValidationComplete(pipelineValidation)
  // Security analysis
  | RunSecurityAnalysis
  | SecurityAnalysisComplete // results applied via validation
  // Canvas navigation
  | ZoomIn
  | ZoomOut
  | ResetZoom
  | SetZoom(float)
  | PanCanvas(Model.position)
  | SetPan(float, float)
  // Canvas node dragging
  | StartDrag(string, float, float) // nodeId, offsetX, offsetY
  | UpdateDrag(float, float) // worldX, worldY (snapped)
  | EndDrag
  // Canvas connection drawing
  | StartConnect(string, string, float, float) // fromNode, fromPort, mouseX, mouseY
  | UpdateConnect(float, float) // worldX, worldY
  | EndConnect(string, string) // toNode, toPort
  | CancelConnect
  // Canvas selection
  | SelectConnection(option<string>)
  | SelectAll
  | DeleteSelected
  | DuplicateNode(string)
  | Undo
  // Context menu
  | OpenContextMenu(float, float)
  | CloseContextMenu
  // Persistence
  | SavePipeline
  | LoadPipeline(string)
  | PipelineSaved
  | PipelineLoaded(pipelineDesignerState)

// ---------------------------------------------------------------------------
// Helper functions
// ---------------------------------------------------------------------------

// Create an empty pipeline with sensible defaults
let emptyPipeline = (): pipeline => {
  id: Model.generateId(),
  name: "Untitled Pipeline",
  description: "",
  nodes: [],
  connections: [],
  selectedNode: None,
  selectedConnection: None,
  zoom: 1.0,
  panX: 0.0,
  panY: 0.0,
}

// Default panel layout dimensions
let defaultPanelState = (): panelState => {
  leftPanelWidth: 260.0,
  rightPanelWidth: 320.0,
  leftCollapsed: false,
  rightCollapsed: false,
  activeLeftTab: Components,
  activeRightTab: Preview,
}

// CSS colour for each node kind (used in the graph canvas)
let nodeColor = (kind: nodeKind): string => {
  switch kind {
  | Source(_) => "#4fc3f7" // light blue — image origin
  | LagoGreyFormation(_) => "#81c784" // green — minimal base
  | BuildStep(_) => "#ffb74d" // orange — build work
  | SecurityGate(_) => "#ef5350" // red — security checkpoint
  | SignStep(_) => "#ab47bc" // purple — cryptographic signing
  | SbomGenerate(_) => "#26c6da" // cyan — metadata generation
  | ProvenanceAttach(_) => "#7e57c2" // deep purple — attestation
  | Push(_) => "#66bb6a" // green — publish
  | Compose(_) => "#42a5f5" // blue — composition
  | CustomScript(_) => "#bdbdbd" // grey — user-defined
  }
}

// Icon character for each node kind (rendered in node headers)
let nodeIcon = (kind: nodeKind): string => {
  switch kind {
  | Source(_) => "\u{1F4E6}" // package
  | LagoGreyFormation(_) => "\u{1F9CA}" // ice — minimal, frozen base
  | BuildStep(_) => "\u{1F527}" // wrench
  | SecurityGate(_) => "\u{1F6E1}" // shield
  | SignStep(_) => "\u{1F58A}" // pen — signing
  | SbomGenerate(_) => "\u{1F4CB}" // clipboard
  | ProvenanceAttach(_) => "\u{1F3F7}" // label — attestation tag
  | Push(_) => "\u{1F680}" // rocket — publish
  | Compose(_) => "\u{1F517}" // link — composition
  | CustomScript(_) => "\u{1F4DD}" // memo — script
  }
}

// Human-readable label for a node kind
let nodeKindToString = (kind: nodeKind): string => {
  switch kind {
  | Source(_) => "Source Image"
  | LagoGreyFormation(_) => "Lago Grey Formation"
  | BuildStep(_) => "Build Step"
  | SecurityGate(_) => "Security Gate"
  | SignStep(_) => "Sign Step"
  | SbomGenerate(_) => "SBOM Generate"
  | ProvenanceAttach(_) => "Provenance Attach"
  | Push(_) => "Push"
  | Compose(_) => "Compose"
  | CustomScript(_) => "Custom Script"
  }
}

// Human-readable label for an output format
let outputFormatToString = (fmt: outputFormat): string => {
  switch fmt {
  | Containerfile => "Containerfile"
  | SelurCompose => "selur-compose"
  | PodmanCompose => "Podman Compose"
  | K8sManifest => "Kubernetes Manifest"
  | HelmChart => "Helm Chart"
  | OciBundle => "OCI Bundle"
  }
}

// ---------------------------------------------------------------------------
// Default templates
// ---------------------------------------------------------------------------

// Internal helper to build a template entry
let makeTemplate = (
  ~id: string,
  ~name: string,
  ~description: string,
  ~category: string,
  ~nodes: array<pipelineNode>,
  ~connections: array<connection>,
): pipelineTemplate => {
  id,
  name,
  description,
  category,
  pipeline: {
    id: Model.generateId(),
    name,
    description,
    nodes,
    connections,
    selectedNode: None,
    selectedConnection: None,
    zoom: 1.0,
    panX: 0.0,
    panY: 0.0,
  },
}

// Pre-built pipeline templates for common use cases
let defaultTemplates = (): array<pipelineTemplate> => {
  [
    makeTemplate(
      ~id="tpl-web-app",
      ~name="Web Application",
      ~description="Node/Deno web app with security scanning, signing, and registry push",
      ~category="Application",
      ~nodes=[
        {
          id: "src-1",
          kind: Source({imageRef: "cgr.dev/chainguard/wolfi-base", tag: "latest"}),
          x: 50.0, y: 200.0, width: 180.0, height: 80.0,
          label: "Base Image", status: Idle, validationErrors: [],
        },
        {
          id: "build-1",
          kind: BuildStep({command: "COPY . /app && RUN deno task build", layer: 1}),
          x: 300.0, y: 200.0, width: 180.0, height: 80.0,
          label: "Build App", status: Idle, validationErrors: [],
        },
        {
          id: "sec-1",
          kind: SecurityGate({policy: "strict", mode: "enforce"}),
          x: 550.0, y: 200.0, width: 180.0, height: 80.0,
          label: "Security Scan", status: Idle, validationErrors: [],
        },
        {
          id: "sign-1",
          kind: SignStep({keyId: "default", transparency: true}),
          x: 800.0, y: 200.0, width: 180.0, height: 80.0,
          label: "Sign Image", status: Idle, validationErrors: [],
        },
        {
          id: "push-1",
          kind: Push({registry: "ghcr.io", repository: "org/web-app"}),
          x: 1050.0, y: 200.0, width: 180.0, height: 80.0,
          label: "Push to Registry", status: Idle, validationErrors: [],
        },
      ],
      ~connections=[
        {id: "c1", fromNode: "src-1", fromPort: "output", toNode: "build-1", toPort: "input"},
        {id: "c2", fromNode: "build-1", fromPort: "output", toNode: "sec-1", toPort: "input"},
        {id: "c3", fromNode: "sec-1", fromPort: "output", toNode: "sign-1", toPort: "input"},
        {id: "c4", fromNode: "sign-1", fromPort: "output", toNode: "push-1", toPort: "input"},
      ],
    ),
    makeTemplate(
      ~id="tpl-microservice",
      ~name="Microservice",
      ~description="Minimal Rust microservice with SBOM and provenance attestation",
      ~category="Application",
      ~nodes=[
        {
          id: "src-1",
          kind: Source({imageRef: "cgr.dev/chainguard/rust", tag: "latest"}),
          x: 50.0, y: 200.0, width: 180.0, height: 80.0,
          label: "Rust Base", status: Idle, validationErrors: [],
        },
        {
          id: "build-1",
          kind: BuildStep({command: "cargo build --release", layer: 1}),
          x: 300.0, y: 200.0, width: 180.0, height: 80.0,
          label: "Compile", status: Idle, validationErrors: [],
        },
        {
          id: "sbom-1",
          kind: SbomGenerate({format: "spdx-json"}),
          x: 550.0, y: 120.0, width: 180.0, height: 80.0,
          label: "Generate SBOM", status: Idle, validationErrors: [],
        },
        {
          id: "prov-1",
          kind: ProvenanceAttach({builder: "slsa-github-generator"}),
          x: 550.0, y: 280.0, width: 180.0, height: 80.0,
          label: "Attach Provenance", status: Idle, validationErrors: [],
        },
        {
          id: "push-1",
          kind: Push({registry: "ghcr.io", repository: "org/microservice"}),
          x: 800.0, y: 200.0, width: 180.0, height: 80.0,
          label: "Push to Registry", status: Idle, validationErrors: [],
        },
      ],
      ~connections=[
        {id: "c1", fromNode: "src-1", fromPort: "output", toNode: "build-1", toPort: "input"},
        {id: "c2", fromNode: "build-1", fromPort: "output", toNode: "sbom-1", toPort: "input"},
        {id: "c3", fromNode: "build-1", fromPort: "output", toNode: "prov-1", toPort: "input"},
        {id: "c4", fromNode: "sbom-1", fromPort: "output", toNode: "push-1", toPort: "metadata"},
        {id: "c5", fromNode: "prov-1", fromPort: "output", toNode: "push-1", toPort: "attestation"},
      ],
    ),
    makeTemplate(
      ~id="tpl-static-site",
      ~name="Static Site",
      ~description="Static site build with Lago Grey minimal base and signing",
      ~category="Web",
      ~nodes=[
        {
          id: "lago-1",
          kind: LagoGreyFormation({formation: "static-serve", size: 5}),
          x: 50.0, y: 200.0, width: 180.0, height: 80.0,
          label: "Lago Grey Base", status: Idle, validationErrors: [],
        },
        {
          id: "build-1",
          kind: BuildStep({command: "COPY dist/ /srv/static", layer: 1}),
          x: 300.0, y: 200.0, width: 180.0, height: 80.0,
          label: "Copy Assets", status: Idle, validationErrors: [],
        },
        {
          id: "sign-1",
          kind: SignStep({keyId: "default", transparency: true}),
          x: 550.0, y: 200.0, width: 180.0, height: 80.0,
          label: "Sign Image", status: Idle, validationErrors: [],
        },
        {
          id: "push-1",
          kind: Push({registry: "ghcr.io", repository: "org/static-site"}),
          x: 800.0, y: 200.0, width: 180.0, height: 80.0,
          label: "Push", status: Idle, validationErrors: [],
        },
      ],
      ~connections=[
        {id: "c1", fromNode: "lago-1", fromPort: "output", toNode: "build-1", toPort: "input"},
        {id: "c2", fromNode: "build-1", fromPort: "output", toNode: "sign-1", toPort: "input"},
        {id: "c3", fromNode: "sign-1", fromPort: "output", toNode: "push-1", toPort: "input"},
      ],
    ),
    makeTemplate(
      ~id="tpl-ml-model",
      ~name="ML Model",
      ~description="Machine learning model container with large layer optimisation and security gate",
      ~category="Data Science",
      ~nodes=[
        {
          id: "src-1",
          kind: Source({imageRef: "cgr.dev/chainguard/python", tag: "latest-dev"}),
          x: 50.0, y: 200.0, width: 180.0, height: 80.0,
          label: "Python Base", status: Idle, validationErrors: [],
        },
        {
          id: "build-1",
          kind: BuildStep({command: "pip install -r requirements.txt", layer: 1}),
          x: 300.0, y: 140.0, width: 180.0, height: 80.0,
          label: "Install Deps", status: Idle, validationErrors: [],
        },
        {
          id: "build-2",
          kind: BuildStep({command: "COPY model/ /app/model", layer: 2}),
          x: 300.0, y: 260.0, width: 180.0, height: 80.0,
          label: "Copy Model Weights", status: Idle, validationErrors: [],
        },
        {
          id: "sec-1",
          kind: SecurityGate({policy: "ml-supply-chain", mode: "audit"}),
          x: 550.0, y: 200.0, width: 180.0, height: 80.0,
          label: "Supply Chain Gate", status: Idle, validationErrors: [],
        },
        {
          id: "push-1",
          kind: Push({registry: "ghcr.io", repository: "org/ml-model"}),
          x: 800.0, y: 200.0, width: 180.0, height: 80.0,
          label: "Push", status: Idle, validationErrors: [],
        },
      ],
      ~connections=[
        {id: "c1", fromNode: "src-1", fromPort: "output", toNode: "build-1", toPort: "input"},
        {id: "c2", fromNode: "src-1", fromPort: "output", toNode: "build-2", toPort: "input"},
        {id: "c3", fromNode: "build-1", fromPort: "output", toNode: "sec-1", toPort: "input"},
        {id: "c4", fromNode: "build-2", fromPort: "output", toNode: "sec-1", toPort: "input"},
        {id: "c5", fromNode: "sec-1", fromPort: "output", toNode: "push-1", toPort: "input"},
      ],
    ),
    makeTemplate(
      ~id="tpl-database",
      ~name="Database",
      ~description="Database container with persistent volume and custom init scripts",
      ~category="Infrastructure",
      ~nodes=[
        {
          id: "src-1",
          kind: Source({imageRef: "cgr.dev/chainguard/postgres", tag: "latest"}),
          x: 50.0, y: 200.0, width: 180.0, height: 80.0,
          label: "PostgreSQL Base", status: Idle, validationErrors: [],
        },
        {
          id: "build-1",
          kind: BuildStep({command: "COPY init.sql /docker-entrypoint-initdb.d/", layer: 1}),
          x: 300.0, y: 200.0, width: 180.0, height: 80.0,
          label: "Init Scripts", status: Idle, validationErrors: [],
        },
        {
          id: "sec-1",
          kind: SecurityGate({policy: "database-hardening", mode: "enforce"}),
          x: 550.0, y: 200.0, width: 180.0, height: 80.0,
          label: "Hardening Gate", status: Idle, validationErrors: [],
        },
        {
          id: "sign-1",
          kind: SignStep({keyId: "infra-key", transparency: false}),
          x: 800.0, y: 200.0, width: 180.0, height: 80.0,
          label: "Sign", status: Idle, validationErrors: [],
        },
      ],
      ~connections=[
        {id: "c1", fromNode: "src-1", fromPort: "output", toNode: "build-1", toPort: "input"},
        {id: "c2", fromNode: "build-1", fromPort: "output", toNode: "sec-1", toPort: "input"},
        {id: "c3", fromNode: "sec-1", fromPort: "output", toNode: "sign-1", toPort: "input"},
      ],
    ),
    makeTemplate(
      ~id="tpl-multi-service",
      ~name="Multi-Service",
      ~description="Compose-based multi-service stack with shared security policy",
      ~category="Infrastructure",
      ~nodes=[
        {
          id: "src-api",
          kind: Source({imageRef: "cgr.dev/chainguard/wolfi-base", tag: "latest"}),
          x: 50.0, y: 100.0, width: 180.0, height: 80.0,
          label: "API Base", status: Idle, validationErrors: [],
        },
        {
          id: "src-worker",
          kind: Source({imageRef: "cgr.dev/chainguard/wolfi-base", tag: "latest"}),
          x: 50.0, y: 300.0, width: 180.0, height: 80.0,
          label: "Worker Base", status: Idle, validationErrors: [],
        },
        {
          id: "build-api",
          kind: BuildStep({command: "COPY api/ /app && RUN deno cache main.ts", layer: 1}),
          x: 300.0, y: 100.0, width: 180.0, height: 80.0,
          label: "Build API", status: Idle, validationErrors: [],
        },
        {
          id: "build-worker",
          kind: BuildStep({command: "COPY worker/ /app && RUN deno cache worker.ts", layer: 1}),
          x: 300.0, y: 300.0, width: 180.0, height: 80.0,
          label: "Build Worker", status: Idle, validationErrors: [],
        },
        {
          id: "compose-1",
          kind: Compose({services: ["api", "worker", "redis"]}),
          x: 550.0, y: 200.0, width: 180.0, height: 80.0,
          label: "Compose Stack", status: Idle, validationErrors: [],
        },
        {
          id: "sec-1",
          kind: SecurityGate({policy: "strict", mode: "enforce"}),
          x: 800.0, y: 200.0, width: 180.0, height: 80.0,
          label: "Security Gate", status: Idle, validationErrors: [],
        },
      ],
      ~connections=[
        {id: "c1", fromNode: "src-api", fromPort: "output", toNode: "build-api", toPort: "input"},
        {id: "c2", fromNode: "src-worker", fromPort: "output", toNode: "build-worker", toPort: "input"},
        {id: "c3", fromNode: "build-api", fromPort: "output", toNode: "compose-1", toPort: "api"},
        {id: "c4", fromNode: "build-worker", fromPort: "output", toNode: "compose-1", toPort: "worker"},
        {id: "c5", fromNode: "compose-1", fromPort: "output", toNode: "sec-1", toPort: "input"},
      ],
    ),
  ]
}

// Initial state for the pipeline designer
let initialState = (): pipelineDesignerState => {
  pipeline: emptyPipeline(),
  panels: defaultPanelState(),
  validation: None,
  outputFormat: Containerfile,
  generatedOutput: None,
  isDragging: None,
  isConnecting: None,
  templates: defaultTemplates(),
  recentPipelines: [],
  isDirty: false,
}
