// SPDX-License-Identifier: PMPL-1.0-or-later
// PipelinePalette.res - Left panel for the Assembly Pipeline Designer
//
// Provides three tabs: Components (draggable node types by category),
// Templates (pre-built pipelines), and History (recent pipelines).
// All user interactions dispatch messages through the provided callback.

open PipelineModel

// ---------------------------------------------------------------------------
// Category colour mapping (matches nodeColor palette in PipelineModel)
// ---------------------------------------------------------------------------

let categoryColor = (kind: nodeKind): string => nodeColor(kind)

let categoryBadgeColor = (cat: string): string => {
  switch cat {
  | "Application" => "#4fc3f7"
  | "Web" => "#81c784"
  | "Data Science" => "#ffb74d"
  | "Infrastructure" => "#42a5f5"
  | _ => "#bdbdbd"
  }
}

// ---------------------------------------------------------------------------
// Palette item descriptors (one per draggable node type)
// ---------------------------------------------------------------------------

type paletteItem = {
  kind: nodeKind,
  label: string,
  description: string,
  icon: string,
  category: string,
}

// All draggable node types grouped by category
let sourceItems: array<paletteItem> = [
  {
    kind: Source({imageRef: "", tag: "latest"}),
    label: "Base Image",
    description: "Pull a standard base image from a registry",
    icon: nodeIcon(Source({imageRef: "", tag: ""})),
    category: "Sources",
  },
  {
    kind: LagoGreyFormation({formation: "", size: 0}),
    label: "Lago Grey Formation",
    description: "Minimal Lago Grey ice formation base",
    icon: nodeIcon(LagoGreyFormation({formation: "", size: 0})),
    category: "Sources",
  },
  {
    kind: CustomScript({script: "", shell: "/bin/sh"}),
    label: "Custom Base",
    description: "User-specified base image URI",
    icon: nodeIcon(CustomScript({script: "", shell: ""})),
    category: "Sources",
  },
]

let buildStepItems: array<paletteItem> = [
  {
    kind: BuildStep({command: "RUN", layer: 0}),
    label: "RUN Command",
    description: "Execute a shell command in the build",
    icon: nodeIcon(BuildStep({command: "", layer: 0})),
    category: "Build Steps",
  },
  {
    kind: BuildStep({command: "COPY", layer: 0}),
    label: "COPY Files",
    description: "Copy files from the build context",
    icon: nodeIcon(BuildStep({command: "", layer: 0})),
    category: "Build Steps",
  },
  {
    kind: BuildStep({command: "ADD", layer: 0}),
    label: "ADD Archive",
    description: "Add and extract an archive into the image",
    icon: nodeIcon(BuildStep({command: "", layer: 0})),
    category: "Build Steps",
  },
  {
    kind: BuildStep({command: "ENV", layer: 0}),
    label: "ENV Variable",
    description: "Set an environment variable",
    icon: nodeIcon(BuildStep({command: "", layer: 0})),
    category: "Build Steps",
  },
  {
    kind: BuildStep({command: "WORKDIR", layer: 0}),
    label: "WORKDIR",
    description: "Set the working directory",
    icon: nodeIcon(BuildStep({command: "", layer: 0})),
    category: "Build Steps",
  },
  {
    kind: BuildStep({command: "USER", layer: 0}),
    label: "USER",
    description: "Set the runtime user",
    icon: nodeIcon(BuildStep({command: "", layer: 0})),
    category: "Build Steps",
  },
  {
    kind: BuildStep({command: "EXPOSE", layer: 0}),
    label: "EXPOSE",
    description: "Declare a network port",
    icon: nodeIcon(BuildStep({command: "", layer: 0})),
    category: "Build Steps",
  },
  {
    kind: BuildStep({command: "LABEL", layer: 0}),
    label: "LABEL",
    description: "Add metadata label",
    icon: nodeIcon(BuildStep({command: "", layer: 0})),
    category: "Build Steps",
  },
]

let securityItems: array<paletteItem> = [
  {
    kind: SecurityGate({policy: "", mode: "enforce"}),
    label: "Svalinn Gate",
    description: "Security gate checkpoint (Svalinn)",
    icon: nodeIcon(SecurityGate({policy: "", mode: ""})),
    category: "Security",
  },
  {
    kind: SecurityGate({policy: "cve-scan", mode: "audit"}),
    label: "Vulnerability Scan",
    description: "Scan for known CVEs",
    icon: nodeIcon(SecurityGate({policy: "", mode: ""})),
    category: "Security",
  },
  {
    kind: SecurityGate({policy: "policy-check", mode: "enforce"}),
    label: "Policy Check",
    description: "Validate against security policy",
    icon: nodeIcon(SecurityGate({policy: "", mode: ""})),
    category: "Security",
  },
]

let supplyChainItems: array<paletteItem> = [
  {
    kind: SignStep({keyId: "", transparency: true}),
    label: "Sign (Cerro Torre)",
    description: "Cryptographically sign with Cerro Torre",
    icon: nodeIcon(SignStep({keyId: "", transparency: true})),
    category: "Supply Chain",
  },
  {
    kind: SbomGenerate({format: "cyclonedx"}),
    label: "SBOM Generate",
    description: "Generate Software Bill of Materials",
    icon: nodeIcon(SbomGenerate({format: ""})),
    category: "Supply Chain",
  },
  {
    kind: ProvenanceAttach({builder: ""}),
    label: "Provenance Attach",
    description: "Attach SLSA provenance attestation",
    icon: nodeIcon(ProvenanceAttach({builder: ""})),
    category: "Supply Chain",
  },
]

let registryItems: array<paletteItem> = [
  {
    kind: Push({registry: "", repository: ""}),
    label: "Push to Registry",
    description: "Push image to a container registry",
    icon: nodeIcon(Push({registry: "", repository: ""})),
    category: "Registry",
  },
  {
    kind: Source({imageRef: "", tag: "latest"}),
    label: "Pull from Registry",
    description: "Pull image from a container registry",
    icon: nodeIcon(Source({imageRef: "", tag: ""})),
    category: "Registry",
  },
]

let compositionItems: array<paletteItem> = [
  {
    kind: Compose({services: []}),
    label: "Multi-service Compose",
    description: "Compose multiple services together",
    icon: nodeIcon(Compose({services: []})),
    category: "Composition",
  },
  {
    kind: Compose({services: []}),
    label: "Dependency Link",
    description: "Declare a service dependency",
    icon: nodeIcon(Compose({services: []})),
    category: "Composition",
  },
]

// All items flattened for search filtering
let allPaletteItems: array<paletteItem> =
  Array.concatMany([
    sourceItems,
    buildStepItems,
    securityItems,
    supplyChainItems,
    registryItems,
    compositionItems,
  ])

// ---------------------------------------------------------------------------
// Search filter helper
// ---------------------------------------------------------------------------

let filterItems = (items: array<paletteItem>, query: string): array<paletteItem> => {
  if query === "" {
    items
  } else {
    let lowerQuery = String.toLowerCase(query)
    Belt.Array.keep(items, item => {
      String.includes(String.toLowerCase(item.label), lowerQuery) ||
      String.includes(String.toLowerCase(item.description), lowerQuery) ||
      String.includes(String.toLowerCase(item.category), lowerQuery)
    })
  }
}

// ---------------------------------------------------------------------------
// Sub-components
// ---------------------------------------------------------------------------

// Render a single draggable palette item
module PaletteItemView = {
  @react.component
  let make = (
    ~item: paletteItem,
    ~onAdd: paletteItem => unit,
  ) => {
    <button
      className="flex items-start gap-2 w-full p-2 rounded-md text-left
                 hover:bg-gray-700/50 transition-colors cursor-grab
                 focus:outline-none focus:ring-2 focus:ring-blue-500"
      onClick={_ => onAdd(item)}
      draggable=true
      onDragStart={e => {
        // Encode the node kind label so the canvas can decode on drop
        let _label = item.label
        ignore(%raw(`e.dataTransfer.setData("text/plain", _label)`))
      }}
      ariaLabel={"Add " ++ item.label ++ " node to pipeline"}
    >
      <span
        className="flex-shrink-0 w-8 h-8 flex items-center justify-center rounded text-sm font-bold"
        style={{backgroundColor: categoryColor(item.kind), color: "#fff"}}
      >
        {item.icon->React.string}
      </span>
      <div className="min-w-0">
        <div className="text-sm font-semibold text-gray-100 truncate">
          {item.label->React.string}
        </div>
        <div className="text-xs text-gray-400 leading-tight">
          {item.description->React.string}
        </div>
      </div>
    </button>
  }
}

// Render a collapsible category group
module CategoryGroup = {
  @react.component
  let make = (
    ~title: string,
    ~items: array<paletteItem>,
    ~onAdd: paletteItem => unit,
  ) => {
    let (collapsed, setCollapsed) = React.useState(() => false)

    if Array.length(items) === 0 {
      React.null
    } else {
      <div className="mb-3" role="group" ariaLabel={title ++ " category"}>
        <button
          className="flex items-center gap-1 w-full text-xs font-bold uppercase tracking-wider
                     text-gray-400 mb-1 px-1 hover:text-gray-200 transition-colors
                     focus:outline-none focus:ring-1 focus:ring-blue-500 rounded"
          onClick={_ => setCollapsed(prev => !prev)}
          ariaExpanded={!collapsed}
        >
          <span className="text-[10px]">
            {(collapsed ? "+" : "-")->React.string}
          </span>
          {title->React.string}
          <span className="ml-auto text-gray-500">
            {Int.toString(Array.length(items))->React.string}
          </span>
        </button>
        {collapsed
          ? React.null
          : <div className="flex flex-col gap-0.5">
              {Array.mapWithIndex(items, (idx, item) => {
                <PaletteItemView
                  key={item.label ++ Int.toString(idx)} item onAdd
                />
              })->React.array}
            </div>}
      </div>
    }
  }
}

// Render a template card
module TemplateCard = {
  @react.component
  let make = (
    ~template: pipelineTemplate,
    ~onLoad: pipelineTemplate => unit,
  ) => {
    let nodeCount = Array.length(template.pipeline.nodes)

    <button
      className="w-full text-left p-3 rounded-lg bg-gray-800/60 hover:bg-gray-700/60
                 border border-gray-700 hover:border-gray-500 transition-all
                 focus:outline-none focus:ring-2 focus:ring-blue-500"
      onClick={_ => onLoad(template)}
      ariaLabel={"Load " ++ template.name ++ " template"}
    >
      <div className="flex items-center justify-between mb-1">
        <span className="text-sm font-semibold text-gray-100">
          {template.name->React.string}
        </span>
        <span
          className="text-[10px] px-1.5 py-0.5 rounded-full font-medium"
          style={{
            backgroundColor: categoryBadgeColor(template.category),
            color: "#000",
          }}
        >
          {template.category->React.string}
        </span>
      </div>
      <p className="text-xs text-gray-400 mb-2 leading-tight">
        {template.description->React.string}
      </p>
      <div className="flex gap-3 text-xs text-gray-500">
        <span> {(Int.toString(nodeCount) ++ " nodes")->React.string} </span>
        <span> {(Int.toString(Array.length(template.pipeline.connections)) ++ " edges")->React.string} </span>
      </div>
    </button>
  }
}

// Render a recent pipeline row
module RecentPipelineRow = {
  @react.component
  let make = (
    ~entry: pipelineSummary,
    ~onLoad: string => unit,
  ) => {
    <button
      className="flex items-center gap-2 w-full p-2 rounded-md text-left
                 hover:bg-gray-700/50 transition-colors
                 focus:outline-none focus:ring-2 focus:ring-blue-500"
      onClick={_ => onLoad(entry.id)}
      ariaLabel={"Load recent pipeline: " ++ entry.name}
    >
      <div className="min-w-0 flex-1">
        <div className="text-sm font-medium text-gray-100 truncate">
          {entry.name->React.string}
        </div>
        <div className="text-xs text-gray-500">
          {entry.updatedAt->React.string}
        </div>
      </div>
    </button>
  }
}

// ---------------------------------------------------------------------------
// Main component
// ---------------------------------------------------------------------------

@react.component
let make = (
  ~state: pipelineDesignerState,
  ~dispatch: pipelineMsg => unit,
) => {
  let panels = state.panels
  let activeTab = panels.activeLeftTab
  let searchQuery = switch activeTab {
  | Components => "" // we manage local search state here
  | _ => ""
  }
  // Local search state for the components tab
  let (localSearch, setLocalSearch) = React.useState(() => "")

  let effectiveSearch = switch activeTab {
  | Components => localSearch
  | _ => searchQuery
  }

  // Handler: add a palette item as a new node at center of canvas
  let handleAddItem = (item: paletteItem) => {
    let centerX = state.pipeline.panX +. 400.0
    let centerY = state.pipeline.panY +. 300.0
    dispatch(
      AddNode(
        item.kind,
        {x: centerX, y: centerY},
      ),
    )
  }

  let handleLoadTemplate = (template: pipelineTemplate) => {
    dispatch(LoadTemplate(template))
  }

  let handleLoadRecent = (id: string) => {
    dispatch(LoadRecentPipeline(id))
  }

  if panels.leftCollapsed {
    // Collapsed state: narrow strip with expand button
    <aside
      className="flex flex-col items-center py-4 bg-gray-900 border-r border-gray-700"
      style={{width: "40px", minWidth: "40px"}}
      role="complementary"
      ariaLabel="Pipeline palette (collapsed)"
    >
      <button
        className="p-1 rounded hover:bg-gray-700 text-gray-400 hover:text-gray-100
                   focus:outline-none focus:ring-2 focus:ring-blue-500"
        onClick={_ => dispatch(TogglePaletteCollapsed)}
        ariaLabel="Expand palette panel"
        title="Expand palette"
      >
        {">"->React.string}
      </button>
    </aside>
  } else {
    <aside
      className="flex flex-col bg-gray-900 border-r border-gray-700 overflow-hidden"
      style={{
        width: Float.toString(panels.leftPanelWidth) ++ "px",
        minWidth: "200px",
        maxWidth: "400px",
      }}
      role="complementary"
      ariaLabel="Pipeline palette"
    >
      // Panel header with collapse button
      <div className="flex items-center justify-between px-3 py-2 border-b border-gray-700">
        <h2 className="text-sm font-bold text-gray-200 tracking-wide uppercase">
          {"Palette"->React.string}
        </h2>
        <button
          className="p-1 rounded hover:bg-gray-700 text-gray-400 hover:text-gray-100 text-xs
                     focus:outline-none focus:ring-2 focus:ring-blue-500"
          onClick={_ => dispatch(TogglePaletteCollapsed)}
          ariaLabel="Collapse palette panel"
          title="Collapse"
        >
          {"<"->React.string}
        </button>
      </div>

      // Tab bar
      <div
        className="flex border-b border-gray-700"
        role="tablist"
        ariaLabel="Palette tabs"
      >
        {[
          (Components, "Components"),
          (Templates, "Templates"),
          (History, "History"),
        ]
        ->Array.map(((tab, label)) => {
          let isActive = activeTab == tab
          <button
            key={label}
            role="tab"
            ariaSelected={isActive}
            className={
              "flex-1 px-2 py-2 text-xs font-medium transition-colors "
              ++ "focus:outline-none focus:ring-2 focus:ring-inset focus:ring-blue-500 "
              ++ (isActive
                ? "text-blue-400 border-b-2 border-blue-400"
                : "text-gray-400 hover:text-gray-200")
            }
            onClick={_ => dispatch(SetPaletteTab(tab))}
          >
            {label->React.string}
          </button>
        })
        ->React.array}
      </div>

      // Tab content (scrollable)
      <div className="flex-1 overflow-y-auto px-3 py-2" role="tabpanel">
        {switch activeTab {
        // ----- Components Tab -----
        | Components =>
          <>
            // Search / filter
            <div className="mb-3">
              <input
                type_="search"
                placeholder="Filter components..."
                value={localSearch}
                onChange={e => {
                  let v: string = ReactEvent.Form.target(e)["value"]
                  setLocalSearch(_ => v)
                }}
                className="w-full px-2 py-1.5 text-sm rounded-md bg-gray-800 border border-gray-600
                           text-gray-100 placeholder-gray-500
                           focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                ariaLabel="Filter pipeline components"
              />
            </div>

            // Categorised lists
            <CategoryGroup
              title="Sources"
              items={filterItems(sourceItems, effectiveSearch)}
              onAdd={handleAddItem}
            />
            <CategoryGroup
              title="Build Steps"
              items={filterItems(buildStepItems, effectiveSearch)}
              onAdd={handleAddItem}
            />
            <CategoryGroup
              title="Security"
              items={filterItems(securityItems, effectiveSearch)}
              onAdd={handleAddItem}
            />
            <CategoryGroup
              title="Supply Chain"
              items={filterItems(supplyChainItems, effectiveSearch)}
              onAdd={handleAddItem}
            />
            <CategoryGroup
              title="Registry"
              items={filterItems(registryItems, effectiveSearch)}
              onAdd={handleAddItem}
            />
            <CategoryGroup
              title="Composition"
              items={filterItems(compositionItems, effectiveSearch)}
              onAdd={handleAddItem}
            />
          </>

        // ----- Templates Tab -----
        | Templates =>
          <div className="flex flex-col gap-2">
            {Array.length(state.templates) === 0
              ? <p className="text-sm text-gray-500 py-4 text-center">
                  {"No templates available."->React.string}
                </p>
              : state.templates
                ->Array.mapWithIndex((idx, tpl) => {
                  <TemplateCard
                    key={tpl.id ++ Int.toString(idx)}
                    template={tpl}
                    onLoad={handleLoadTemplate}
                  />
                })
                ->React.array}
          </div>

        // ----- History Tab -----
        | History =>
          <div className="flex flex-col gap-0.5">
            {Array.length(state.recentPipelines) === 0
              ? <div className="text-center py-6">
                  <p className="text-sm text-gray-500 mb-1">
                    {"No recent pipelines."->React.string}
                  </p>
                  <p className="text-xs text-gray-600">
                    {"Saved pipelines will appear here."->React.string}
                  </p>
                </div>
              : state.recentPipelines
                ->Array.mapWithIndex((idx, entry) => {
                  <RecentPipelineRow
                    key={entry.id ++ Int.toString(idx)}
                    entry
                    onLoad={handleLoadRecent}
                  />
                })
                ->React.array}
          </div>
        }}
      </div>
    </aside>
  }
}
