// SPDX-License-Identifier: PMPL-1.0-or-later
// PipelineDesigner.res - Main container for the Assembly Pipeline Designer
//
// Composes the three-panel layout: left palette, center canvas, right output.
// Includes a top toolbar (pipeline name, save, load, run, export) and a
// bottom status bar (node count, connection count, validation, zoom).

open PipelineModel

// ---------------------------------------------------------------------------
// Toolbar component (top bar)
// ---------------------------------------------------------------------------

module Toolbar = {
  @react.component
  let make = (
    ~pipelineName: string,
    ~isDirty: bool,
    ~dispatch: pipelineMsg => unit,
  ) => {
    let (isEditing, setIsEditing) = React.useState(() => false)
    let (editValue, setEditValue) = React.useState(() => pipelineName)

    let commitName = () => {
      if editValue !== "" {
        dispatch(SetPipelineName(editValue))
      } else {
        setEditValue(_ => pipelineName)
      }
      setIsEditing(_ => false)
    }

    <header
      className="flex items-center gap-3 px-4 py-2 bg-gray-900 border-b border-gray-700"
      role="toolbar"
      ariaLabel="Pipeline designer toolbar"
    >
      // Pipeline name (inline editable)
      <div className="flex items-center gap-2 min-w-0">
        {isEditing
          ? <input
              type_="text"
              value={editValue}
              onChange={e => {
                let v: string = ReactEvent.Form.target(e)["value"]
                setEditValue(_ => v)
              }}
              onBlur={_ => commitName()}
              onKeyDown={e => {
                if ReactEvent.Keyboard.key(e) === "Enter" {
                  commitName()
                } else if ReactEvent.Keyboard.key(e) === "Escape" {
                  setEditValue(_ => pipelineName)
                  setIsEditing(_ => false)
                }
              }}
              className="text-lg font-semibold bg-gray-800 text-gray-100 border border-blue-500
                         rounded px-2 py-0.5 focus:outline-none focus:ring-2 focus:ring-blue-400"
              ariaLabel="Edit pipeline name"
              autoFocus=true
            />
          : <button
              className="text-lg font-semibold text-gray-100 hover:text-blue-300
                         truncate max-w-xs cursor-text
                         focus:outline-none focus:ring-2 focus:ring-blue-500 rounded px-1"
              onClick={_ => {
                setEditValue(_ => pipelineName)
                setIsEditing(_ => true)
              }}
              ariaLabel={"Pipeline name: " ++ pipelineName ++ ". Click to edit."}
              title="Click to rename"
            >
              {pipelineName->React.string}
            </button>}
        {isDirty
          ? <span
              className="text-xs text-yellow-400 flex-shrink-0"
              ariaLabel="Unsaved changes"
              title="Unsaved changes"
            >
              {"*"->React.string}
            </span>
          : React.null}
      </div>

      // Spacer
      <div className="flex-1" />

      // Action buttons
      <nav className="flex items-center gap-2" ariaLabel="Pipeline actions">
        <button
          className="px-3 py-1.5 text-sm rounded-md bg-gray-700 text-gray-200
                     hover:bg-gray-600 transition-colors
                     focus:outline-none focus:ring-2 focus:ring-blue-500"
          onClick={_ => dispatch(SavePipeline)}
          ariaLabel="Save pipeline"
          title="Save (Ctrl+S)"
        >
          {"Save"->React.string}
        </button>
        <button
          className="px-3 py-1.5 text-sm rounded-md bg-gray-700 text-gray-200
                     hover:bg-gray-600 transition-colors
                     focus:outline-none focus:ring-2 focus:ring-blue-500"
          onClick={_ => dispatch(LoadPipeline(""))}
          ariaLabel="Load pipeline"
          title="Load"
        >
          {"Load"->React.string}
        </button>
        <button
          className="px-3 py-1.5 text-sm rounded-md bg-blue-600 text-white
                     hover:bg-blue-500 transition-colors
                     focus:outline-none focus:ring-2 focus:ring-blue-400"
          onClick={_ => dispatch(RunValidation)}
          ariaLabel="Validate and run pipeline"
          title="Run (Ctrl+Enter)"
        >
          {"Run"->React.string}
        </button>
        <button
          className="px-3 py-1.5 text-sm rounded-md bg-green-700 text-white
                     hover:bg-green-600 transition-colors
                     focus:outline-none focus:ring-2 focus:ring-green-400"
          onClick={_ => dispatch(RegeneratePreview)}
          ariaLabel="Export pipeline output"
          title="Export"
        >
          {"Export"->React.string}
        </button>
      </nav>
    </header>
  }
}

// ---------------------------------------------------------------------------
// Status bar component (bottom bar)
// ---------------------------------------------------------------------------

module StatusBar = {
  @react.component
  let make = (
    ~state: pipelineDesignerState,
    ~dispatch: pipelineMsg => unit,
  ) => {
    let nodeCount = Array.length(state.pipeline.nodes)
    let connCount = Array.length(state.pipeline.connections)
    let zoomPct = Float.toFixed(state.pipeline.zoom *. 100.0, ~digits=0)

    let validationStatus = switch state.validation {
    | None => ("--", "text-gray-500")
    | Some(v) =>
      if v.isValid {
        ("Valid", "text-green-400")
      } else {
        let errCount = Array.length(v.errors)
        (Int.toString(errCount) ++ " error" ++ (errCount === 1 ? "" : "s"), "text-red-400")
      }
    }

    <footer
      className="flex items-center gap-4 px-4 py-1.5 bg-gray-900 border-t border-gray-700
                 text-xs text-gray-400"
      role="status"
      ariaLabel="Pipeline status"
    >
      <span ariaLabel={"Nodes: " ++ Int.toString(nodeCount)}>
        {(Int.toString(nodeCount) ++ " node" ++ (nodeCount === 1 ? "" : "s"))->React.string}
      </span>
      <span className="text-gray-600"> {"|"->React.string} </span>
      <span ariaLabel={"Connections: " ++ Int.toString(connCount)}>
        {(Int.toString(connCount) ++ " connection" ++ (connCount === 1 ? "" : "s"))->React.string}
      </span>
      <span className="text-gray-600"> {"|"->React.string} </span>
      <span className={snd(validationStatus)} ariaLabel={"Validation: " ++ fst(validationStatus)}>
        {fst(validationStatus)->React.string}
      </span>

      // Spacer
      <div className="flex-1" />

      // Zoom controls
      <div className="flex items-center gap-1" role="group" ariaLabel="Zoom controls">
        <button
          className="px-1.5 py-0.5 rounded hover:bg-gray-700 transition-colors
                     focus:outline-none focus:ring-1 focus:ring-blue-500"
          onClick={_ => dispatch(ZoomOut)}
          ariaLabel="Zoom out"
          title="Zoom out"
        >
          {"-"->React.string}
        </button>
        <span className="min-w-[3rem] text-center" ariaLabel={"Zoom: " ++ zoomPct ++ "%"}>
          {(zoomPct ++ "%")->React.string}
        </span>
        <button
          className="px-1.5 py-0.5 rounded hover:bg-gray-700 transition-colors
                     focus:outline-none focus:ring-1 focus:ring-blue-500"
          onClick={_ => dispatch(ZoomIn)}
          ariaLabel="Zoom in"
          title="Zoom in"
        >
          {"+"->React.string}
        </button>
        <button
          className="px-1.5 py-0.5 rounded hover:bg-gray-700 text-gray-500
                     hover:text-gray-300 transition-colors ml-1
                     focus:outline-none focus:ring-1 focus:ring-blue-500"
          onClick={_ => dispatch(ResetZoom)}
          ariaLabel="Reset zoom to 100%"
          title="Reset zoom"
        >
          {"1:1"->React.string}
        </button>
      </div>

      // Keyboard shortcut hints
      <span className="text-gray-600 hidden sm:inline">
        {"Ctrl+S Save  |  Ctrl+Enter Run  |  Ctrl+Z Undo"->React.string}
      </span>
    </footer>
  }
}

// ---------------------------------------------------------------------------
// Center canvas placeholder
// ---------------------------------------------------------------------------

// The canvas renders pipeline nodes and edges. This placeholder provides
// the flex container; a full graph renderer (SVG or Canvas) would be
// mounted here once the node-graph engine is implemented.
module CanvasPlaceholder = {
  @react.component
  let make = (
    ~state: pipelineDesignerState,
    ~dispatch: pipelineMsg => unit,
  ) => {
    let nodeCount = Array.length(state.pipeline.nodes)

    <div
      className="flex-1 relative bg-gray-950 overflow-hidden"
      role="application"
      ariaLabel="Pipeline canvas"
      ariaRoledescription="Node graph editor"
      tabIndex={0}
      onDragOver={e => ReactEvent.Mouse.preventDefault(e)}
      onDrop={e => {
        ReactEvent.Mouse.preventDefault(e)
        // Read the node type label from the drag transfer
        let _label: string = %raw(`e.dataTransfer.getData("text/plain")`)
        // Calculate drop position relative to the canvas
        let rect: {..} = %raw(`e.currentTarget.getBoundingClientRect()`)
        let x = Float.fromInt(ReactEvent.Mouse.clientX(e)) -. rect["left"]
        let y = Float.fromInt(ReactEvent.Mouse.clientY(e)) -. rect["top"]
        // Default to a Source node; a real implementation would decode _label
        dispatch(AddNode(Source({imageRef: "", tag: "latest"}), {x, y}))
      }}
    >
      // Grid background pattern
      <div
        className="absolute inset-0 opacity-10"
        style={{
          backgroundImage: "radial-gradient(circle, #555 1px, transparent 1px)",
          backgroundSize: "24px 24px",
        }}
      />

      // Render nodes
      {nodeCount === 0
        ? // Empty state
          <div className="absolute inset-0 flex flex-col items-center justify-center text-gray-500">
            <div className="text-4xl mb-4"> {"+"->React.string} </div>
            <p className="text-sm mb-1">
              {"Drag components from the palette"->React.string}
            </p>
            <p className="text-xs text-gray-600">
              {"or select a template to get started"->React.string}
            </p>
          </div>
        : // Render each node as a positioned card
          state.pipeline.nodes
          ->Array.mapWithIndex((idx, node) => {
            let isSelected = state.pipeline.selectedNode === Some(node.id)
            <div
              key={node.id ++ Int.toString(idx)}
              className={
                "absolute rounded-lg shadow-lg border-2 cursor-move select-none "
                ++ "transition-shadow "
                ++ (isSelected
                  ? "border-blue-400 shadow-blue-500/30 ring-2 ring-blue-400/50"
                  : "border-gray-600 hover:border-gray-400")
              }
              style={{
                left: Float.toString(node.x) ++ "px",
                top: Float.toString(node.y) ++ "px",
                width: Float.toString(node.width) ++ "px",
                minHeight: Float.toString(node.height) ++ "px",
                transform: "scale(" ++ Float.toString(state.pipeline.zoom) ++ ")",
                transformOrigin: "top left",
              }}
              onClick={_ => dispatch(SelectNode(Some(node.id)))}
              role="button"
              ariaLabel={node.label ++ " (" ++ nodeKindToString(node.kind) ++ ")"}
              tabIndex={0}
              onKeyDown={e => {
                if ReactEvent.Keyboard.key(e) === "Delete" || ReactEvent.Keyboard.key(e) === "Backspace" {
                  dispatch(RemoveNode(node.id))
                }
              }}
            >
              // Node header
              <div
                className="flex items-center gap-2 px-3 py-2 rounded-t-lg text-white text-sm font-medium"
                style={{backgroundColor: nodeColor(node.kind)}}
              >
                <span> {nodeIcon(node.kind)->React.string} </span>
                <span className="truncate"> {node.label->React.string} </span>
              </div>
              // Node body
              <div className="px-3 py-2 bg-gray-800 rounded-b-lg">
                <div className="text-xs text-gray-400">
                  {nodeKindToString(node.kind)->React.string}
                </div>
                // Show validation errors if any
                {Array.length(node.validationErrors) > 0
                  ? <div className="mt-1 text-xs text-red-400">
                      {Array.getUnsafe(node.validationErrors, 0)->React.string}
                    </div>
                  : React.null}
              </div>
            </div>
          })
          ->React.array}

      // Render connections as SVG lines
      {Array.length(state.pipeline.connections) > 0
        ? <svg
            className="absolute inset-0 w-full h-full pointer-events-none"
            style={{zIndex: "0"}}
          >
            {state.pipeline.connections
            ->Array.mapWithIndex((idx, conn) => {
              // Look up source and target node positions
              let fromNode = Belt.Array.getBy(state.pipeline.nodes, n => n.id === conn.fromNode)
              let toNode = Belt.Array.getBy(state.pipeline.nodes, n => n.id === conn.toNode)
              switch (fromNode, toNode) {
              | (Some(from), Some(to_)) =>
                let x1 = Float.toString(from.x +. from.width /. 2.0)
                let y1 = Float.toString(from.y +. from.height)
                let x2 = Float.toString(to_.x +. to_.width /. 2.0)
                let y2 = Float.toString(to_.y)
                <line
                  key={conn.id ++ Int.toString(idx)}
                  x1
                  y1
                  x2
                  y2
                  stroke="#555"
                  strokeWidth="2"
                  strokeDasharray="4"
                />
              | _ => React.null
              }
            })
            ->React.array}
          </svg>
        : React.null}
    </div>
  }
}

// ---------------------------------------------------------------------------
// Main PipelineDesigner component
// ---------------------------------------------------------------------------

@react.component
let make = (
  ~state: pipelineDesignerState,
  ~dispatch: pipelineMsg => unit,
): React.element => {
  // Keyboard shortcut handler
  React.useEffect0(() => {
    let handler = (e: {..}) => {
      let key: string = e["key"]
      let ctrlKey: bool = e["ctrlKey"]
      let metaKey: bool = e["metaKey"]
      let mod_ = ctrlKey || metaKey

      if mod_ && key === "s" {
        e["preventDefault"](.)
        dispatch(SavePipeline)
      } else if mod_ && key === "Enter" {
        e["preventDefault"](.)
        dispatch(RunValidation)
      }
    }
    let _: unit = %raw(`document.addEventListener("keydown", handler)`)
    Some(
      () => {
        let _: unit = %raw(`document.removeEventListener("keydown", handler)`)
      },
    )
  })

  <div
    className="flex flex-col h-full w-full bg-gray-950 text-gray-100"
    role="region"
    ariaLabel="Assembly Pipeline Designer"
  >
    // Top toolbar
    <Toolbar
      pipelineName={state.pipeline.name}
      isDirty={state.isDirty}
      dispatch
    />

    // Three-panel layout
    <div className="flex flex-1 overflow-hidden">
      // Left panel - Palette
      <PipelinePalette.make state dispatch />

      // Center - Canvas
      <CanvasPlaceholder state dispatch />

      // Right panel - Output
      <PipelineOutput.make state dispatch />
    </div>

    // Bottom status bar
    <StatusBar state dispatch />
  </div>
}
