// SPDX-License-Identifier: PMPL-1.0-or-later
// PipelineCanvas.res - Interactive SVG node-graph canvas for the assembly pipeline designer
//
// Center panel of the PanLL-style three-panel layout. Users construct container
// build pipelines by dragging nodes onto a grid, connecting ports, and
// manipulating the graph via keyboard shortcuts and context menus.
//
// All interaction is dispatched as PipelineModel.pipelineMsg — no mutable state
// outside React hooks.

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

let gridSize = 20.0
let nodeDefaultWidth = 180.0
let nodeDefaultHeight = 80.0
let portRadius = 7.0
let portHitRadius = 14.0
let minimapWidth = 180.0
let minimapHeight = 120.0
let minimapPadding = 12.0
let zoomMin = 0.25
let zoomMax = 3.0
let zoomStep = 0.1

// Float modulo (not available as fmod in ReScript)
let fmod = (a: float, b: float): float => a -. Float.fromInt(Float.toInt(a /. b)) *. b

// ---------------------------------------------------------------------------
// Snap-to-grid helper
// ---------------------------------------------------------------------------

let snap = (v: float): float => Math.round(v /. gridSize) *. gridSize

// ---------------------------------------------------------------------------
// Status colour mapping
// ---------------------------------------------------------------------------

let statusColor = (status: PipelineModel.nodeStatus): string =>
  switch status {
  | Idle => "#9e9e9e"
  | Running => "#fdd835"
  | Success => "#66bb6a"
  | Failed => "#ef5350"
  | Skipped => "#78909c"
  }

// ---------------------------------------------------------------------------
// Connection colour based on port name (data type flowing through)
// ---------------------------------------------------------------------------

let connectionColor = (fromPort: string): string =>
  switch fromPort {
  | "output" => "#90caf9"
  | "artifact" => "#a5d6a7"
  | "metadata" => "#ce93d8"
  | "attestation" => "#ffcc80"
  | _ => "#b0bec5"
  }

// ---------------------------------------------------------------------------
// Bezier path between two points (smooth S-curve)
// ---------------------------------------------------------------------------

let bezierPath = (x1: float, y1: float, x2: float, y2: float): string => {
  let dx = Math.abs(x2 -. x1) *. 0.5
  let cx1 = x1 +. dx
  let cy1 = y1
  let cx2 = x2 -. dx
  let cy2 = y2
  `M ${Float.toString(x1)} ${Float.toString(y1)} C ${Float.toString(cx1)} ${Float.toString(
      cy1,
    )} ${Float.toString(cx2)} ${Float.toString(cy2)} ${Float.toString(x2)} ${Float.toString(y2)}`
}

// ---------------------------------------------------------------------------
// Port positions on a node
// ---------------------------------------------------------------------------

let inputPortPos = (node: PipelineModel.pipelineNode): (float, float) => (
  node.x,
  node.y +. node.height /. 2.0,
)

let outputPortPos = (node: PipelineModel.pipelineNode): (float, float) => (
  node.x +. node.width,
  node.y +. node.height /. 2.0,
)

// ---------------------------------------------------------------------------
// Find a node by id in the pipeline
// ---------------------------------------------------------------------------

let findNode = (
  nodes: array<PipelineModel.pipelineNode>,
  id: string,
): option<PipelineModel.pipelineNode> => Array.getBy(nodes, n => n.id === id)

// ---------------------------------------------------------------------------
// Render a single pipeline node
// ---------------------------------------------------------------------------

let renderNode = (
  node: PipelineModel.pipelineNode,
  isSelected: bool,
  dispatch: PipelineModel.pipelineMsg => unit,
): React.element => {
  let color = PipelineModel.nodeColor(node.kind)
  let icon = PipelineModel.nodeIcon(node.kind)
  let kindLabel = PipelineModel.nodeKindToString(node.kind)
  let errorCount = Array.length(node.validationErrors)
  let statusDot = statusColor(node.status)

  let borderStroke = isSelected ? "#ffffff" : "rgba(255,255,255,0.15)"
  let borderWidth = isSelected ? "2.5" : "1"

  let (inX, inY) = inputPortPos(node)
  let (outX, outY) = outputPortPos(node)

  <g
    key={node.id}
    onMouseDown={e => {
      e->ReactEvent.Mouse.stopPropagation
      let svgEl = ReactEvent.Mouse.currentTarget(e)
      // Calculate offset from the node origin to the mouse position
      let rect = svgEl["ownerSVGElement"]["getBoundingClientRect"]()
      let _ = rect
      dispatch(
        PipelineModel.StartDrag(
          node.id,
          Float.fromInt(ReactEvent.Mouse.clientX(e)) -. node.x,
          Float.fromInt(ReactEvent.Mouse.clientY(e)) -. node.y,
        ),
      )
    }}
    onDoubleClick={e => {
      e->ReactEvent.Mouse.stopPropagation
      dispatch(PipelineModel.SelectNode(Some(node.id)))
    }}
    onClick={e => {
      e->ReactEvent.Mouse.stopPropagation
      dispatch(PipelineModel.SelectNode(Some(node.id)))
    }}
    style={Sx.make(~cursor="grab", ())}
  >
    // Drop shadow filter applied via a slightly offset darker rect
    <rect
      x={Float.toString(node.x +. 2.0)}
      y={Float.toString(node.y +. 2.0)}
      width={Float.toString(node.width)}
      height={Float.toString(node.height)}
      rx="10"
      ry="10"
      fill="rgba(0,0,0,0.25)"
    />

    // Main node body
    <rect
      x={Float.toString(node.x)}
      y={Float.toString(node.y)}
      width={Float.toString(node.width)}
      height={Float.toString(node.height)}
      rx="10"
      ry="10"
      fill="#1e1e2e"
      stroke={borderStroke}
      strokeWidth={borderWidth}
      role="graphics-symbol"
      ariaLabel={node.label ++ " (" ++ kindLabel ++ ")"}
    />

    // Colour accent bar (top edge)
    <rect
      x={Float.toString(node.x)}
      y={Float.toString(node.y)}
      width={Float.toString(node.width)}
      height="4"
      rx="10"
      ry="10"
      fill={color}
    />
    // Cover the bottom corners of the accent bar so only top has radius
    <rect
      x={Float.toString(node.x)}
      y={Float.toString(node.y +. 2.0)}
      width={Float.toString(node.width)}
      height="2"
      fill={color}
    />

    // Icon (top-left, color-coded)
    <text
      x={Float.toString(node.x +. 12.0)}
      y={Float.toString(node.y +. 28.0)}
      fill={color}
      style={Sx.make(~fontSize="16px", ~pointerEvents="none", ())}
    >
      {icon->React.string}
    </text>

    // Node label (name)
    <text
      x={Float.toString(node.x +. 36.0)}
      y={Float.toString(node.y +. 30.0)}
      fill="#e0e0e0"
      style={Sx.make(~fontSize="13px", ~fontWeight="600", ~pointerEvents="none", ())}
    >
      {node.label->React.string}
    </text>

    // Type subtitle
    <text
      x={Float.toString(node.x +. 36.0)}
      y={Float.toString(node.y +. 48.0)}
      fill="#9e9e9e"
      style={Sx.make(~fontSize="10px", ~pointerEvents="none", ())}
    >
      {kindLabel->React.string}
    </text>

    // Status indicator dot (bottom-left)
    <circle
      cx={Float.toString(node.x +. 16.0)}
      cy={Float.toString(node.y +. node.height -. 14.0)}
      r="5"
      fill={statusDot}
    />

    // Status label next to dot
    <text
      x={Float.toString(node.x +. 26.0)}
      y={Float.toString(node.y +. node.height -. 10.0)}
      fill="#9e9e9e"
      style={Sx.make(~fontSize="9px", ~pointerEvents="none", ())}
    >
      {(switch node.status {
      | Idle => "idle"
      | Running => "running"
      | Success => "done"
      | Failed => "failed"
      | Skipped => "skipped"
      })->React.string}
    </text>

    // Validation error badge (top-right)
    {errorCount > 0
      ? <>
          <circle
            cx={Float.toString(node.x +. node.width -. 12.0)}
            cy={Float.toString(node.y +. 12.0)}
            r="9"
            fill="#ef5350"
          />
          <text
            x={Float.toString(node.x +. node.width -. 12.0)}
            y={Float.toString(node.y +. 16.0)}
            fill="#ffffff"
            textAnchor="middle"
            style={Sx.make(~fontSize="10px", ~fontWeight="700", ~pointerEvents="none", ())}
          >
            {Int.toString(errorCount)->React.string}
          </text>
        </>
      : React.null}

    // Input port (left side)
    <circle
      cx={Float.toString(inX)}
      cy={Float.toString(inY)}
      r={Float.toString(portRadius)}
      fill="#37474f"
      stroke="#90a4ae"
      strokeWidth="1.5"
      role="graphics-symbol"
      ariaLabel={node.label ++ " input port"}
    />
    // Input port hit target (larger, invisible)
    <circle
      cx={Float.toString(inX)}
      cy={Float.toString(inY)}
      r={Float.toString(portHitRadius)}
      fill="transparent"
      style={Sx.make(~cursor="crosshair", ())}
      onMouseUp={e => {
        e->ReactEvent.Mouse.stopPropagation
        dispatch(PipelineModel.EndConnect(node.id, "input"))
      }}
    />

    // Output port (right side)
    <circle
      cx={Float.toString(outX)}
      cy={Float.toString(outY)}
      r={Float.toString(portRadius)}
      fill={color}
      stroke="#ffffff"
      strokeWidth="1.5"
      role="graphics-symbol"
      ariaLabel={node.label ++ " output port"}
    />
    // Output port hit target (larger, invisible)
    <circle
      cx={Float.toString(outX)}
      cy={Float.toString(outY)}
      r={Float.toString(portHitRadius)}
      fill="transparent"
      style={Sx.make(~cursor="crosshair", ())}
      onMouseDown={e => {
        e->ReactEvent.Mouse.stopPropagation
        let mx = Float.fromInt(ReactEvent.Mouse.clientX(e))
        let my = Float.fromInt(ReactEvent.Mouse.clientY(e))
        dispatch(PipelineModel.StartConnect(node.id, "output", mx, my))
      }}
    />
  </g>
}

// ---------------------------------------------------------------------------
// Render a connection (bezier curve between ports)
// ---------------------------------------------------------------------------

let renderConnection = (
  conn: PipelineModel.connection,
  nodes: array<PipelineModel.pipelineNode>,
  isSelected: bool,
): React.element => {
  let fromNode = findNode(nodes, conn.fromNode)
  let toNode = findNode(nodes, conn.toNode)

  switch (fromNode, toNode) {
  | (Some(src), Some(dst)) => {
      let (x1, y1) = outputPortPos(src)
      let (x2, y2) = inputPortPos(dst)
      let pathD = bezierPath(x1, y1, x2, y2)
      let color = connectionColor(conn.fromPort)
      let strokeW = isSelected ? "3" : "2"
      let opacity = isSelected ? "1" : "0.7"

      <g key={conn.id}>
        // Invisible wider path for easier click targeting
        <path
          d={pathD}
          fill="none"
          stroke="transparent"
          strokeWidth="12"
          style={Sx.make(~cursor="pointer", ())}
        />
        // Visible connection line
        <path
          d={pathD}
          fill="none"
          stroke={color}
          strokeWidth={strokeW}
          opacity={opacity}
          strokeLinecap="round"
          role="graphics-symbol"
          ariaLabel={`Connection from ${conn.fromNode} to ${conn.toNode}`}
        />
        // Arrowhead indicator near the destination port
        <circle
          cx={Float.toString(x2 -. 3.0)}
          cy={Float.toString(y2)}
          r="3"
          fill={color}
          opacity={opacity}
        />
      </g>
    }
  | _ => React.null
  }
}

// ---------------------------------------------------------------------------
// Render the live connection-drawing preview
// ---------------------------------------------------------------------------

let renderConnectPreview = (
  connectState: PipelineModel.connectState,
  nodes: array<PipelineModel.pipelineNode>,
): React.element => {
  let fromNode = findNode(nodes, connectState.fromNode)

  switch fromNode {
  | Some(src) => {
      let (x1, y1) = outputPortPos(src)
      let pathD = bezierPath(x1, y1, connectState.mouseX, connectState.mouseY)

      <path
        d={pathD}
        fill="none"
        stroke="#90caf9"
        strokeWidth="2"
        strokeDasharray="6,4"
        opacity="0.6"
        strokeLinecap="round"
      />
    }
  | None => React.null
  }
}

// ---------------------------------------------------------------------------
// Minimap - small overview of the entire pipeline in a corner
// ---------------------------------------------------------------------------

let renderMinimap = (
  nodes: array<PipelineModel.pipelineNode>,
  connections: array<PipelineModel.connection>,
  zoom: float,
  panX: float,
  panY: float,
  canvasWidth: float,
  canvasHeight: float,
): React.element => {
  if Array.length(nodes) === 0 {
    React.null
  } else {
    // Compute bounding box of all nodes
    let minX = ref(99999.0)
    let minY = ref(99999.0)
    let maxX = ref(-99999.0)
    let maxY = ref(-99999.0)

    Array.forEach(nodes, n => {
      if n.x < minX.contents {
        minX := n.x
      }
      if n.y < minY.contents {
        minY := n.y
      }
      if n.x +. n.width > maxX.contents {
        maxX := n.x +. n.width
      }
      if n.y +. n.height > maxY.contents {
        maxY := n.y +. n.height
      }
    })

    let bboxW = maxX.contents -. minX.contents +. 80.0
    let bboxH = maxY.contents -. minY.contents +. 80.0
    let scaleX = (minimapWidth -. 20.0) /. bboxW
    let scaleY = (minimapHeight -. 20.0) /. bboxH
    let mmScale = Math.min(scaleX, scaleY)
    let offsetX = minX.contents -. 40.0
    let offsetY = minY.contents -. 40.0

    // Viewport rectangle in minimap coordinates
    let vpX = (-. panX /. zoom -. offsetX) *. mmScale +. 10.0
    let vpY = (-. panY /. zoom -. offsetY) *. mmScale +. 10.0
    let vpW = canvasWidth /. zoom *. mmScale
    let vpH = canvasHeight /. zoom *. mmScale

    <g>
      // Minimap background
      <rect
        x="0"
        y="0"
        width={Float.toString(minimapWidth)}
        height={Float.toString(minimapHeight)}
        rx="6"
        ry="6"
        fill="rgba(20,20,30,0.85)"
        stroke="rgba(255,255,255,0.1)"
        strokeWidth="1"
      />

      // Node dots
      {Array.map(nodes, n => {
        let nx = (n.x -. offsetX) *. mmScale +. 10.0
        let ny = (n.y -. offsetY) *. mmScale +. 10.0
        let nw = n.width *. mmScale
        let nh = n.height *. mmScale
        <rect
          key={"mm-" ++ n.id}
          x={Float.toString(nx)}
          y={Float.toString(ny)}
          width={Float.toString(Math.max(nw, 3.0))}
          height={Float.toString(Math.max(nh, 2.0))}
          rx="1"
          fill={PipelineModel.nodeColor(n.kind)}
          opacity="0.8"
        />
      })->React.array}

      // Connection lines (simplified)
      {Array.map(connections, conn => {
        let fromN = findNode(nodes, conn.fromNode)
        let toN = findNode(nodes, conn.toNode)
        switch (fromN, toN) {
        | (Some(src), Some(dst)) => {
            let sx = (src.x +. src.width -. offsetX) *. mmScale +. 10.0
            let sy = (src.y +. src.height /. 2.0 -. offsetY) *. mmScale +. 10.0
            let dx = (dst.x -. offsetX) *. mmScale +. 10.0
            let dy = (dst.y +. dst.height /. 2.0 -. offsetY) *. mmScale +. 10.0
            <line
              key={"mmc-" ++ conn.id}
              x1={Float.toString(sx)}
              y1={Float.toString(sy)}
              x2={Float.toString(dx)}
              y2={Float.toString(dy)}
              stroke="rgba(144,202,249,0.4)"
              strokeWidth="1"
            />
          }
        | _ => React.null
        }
      })->React.array}

      // Viewport rectangle
      <rect
        x={Float.toString(vpX)}
        y={Float.toString(vpY)}
        width={Float.toString(vpW)}
        height={Float.toString(vpH)}
        fill="none"
        stroke="rgba(255,255,255,0.4)"
        strokeWidth="1"
        strokeDasharray="3,2"
      />
    </g>
  }
}

// ---------------------------------------------------------------------------
// Context menu overlay (rendered in SVG foreignObject)
// ---------------------------------------------------------------------------

let renderContextMenu = (
  screenX: float,
  screenY: float,
  dispatch: PipelineModel.pipelineMsg => unit,
): React.element => {
  let menuItemStyle = Sx.make(
    ~padding="6px 16px",
    ~cursor="pointer",
    ~fontSize="12px",
    ~color="#e0e0e0",
    ~background="transparent",
    (),
  )

  <div
    style={Sx.make(
      ~position="fixed",
      ~left=Float.toString(screenX) ++ "px",
      ~top=Float.toString(screenY) ++ "px",
      ~zIndex="2000",
      ~background="#2a2a3e",
      ~borderRadius="6px",
      ~padding="4px 0",
      ~minWidth="160px",
      ~boxShadow="0 4px 16px rgba(0,0,0,0.4)",
      (),
    )}
    onClick={e => e->ReactEvent.Mouse.stopPropagation}
  >
    <div
      style={menuItemStyle}
      onClick={_ => {
        let pos: Model.position = {x: screenX, y: screenY}
        dispatch(PipelineModel.AddNode(Source({imageRef: "", tag: "latest"}), pos))
        dispatch(PipelineModel.CloseContextMenu)
      }}
    >
      {"Add Source Image"->React.string}
    </div>
    <div
      style={menuItemStyle}
      onClick={_ => {
        let pos: Model.position = {x: screenX, y: screenY}
        dispatch(PipelineModel.AddNode(BuildStep({command: "", layer: 0}), pos))
        dispatch(PipelineModel.CloseContextMenu)
      }}
    >
      {"Add Build Step"->React.string}
    </div>
    <div
      style={menuItemStyle}
      onClick={_ => {
        let pos: Model.position = {x: screenX, y: screenY}
        dispatch(
          PipelineModel.AddNode(SecurityGate({policy: "strict", mode: "enforce"}), pos),
        )
        dispatch(PipelineModel.CloseContextMenu)
      }}
    >
      {"Add Security Gate"->React.string}
    </div>
    <div
      style={menuItemStyle}
      onClick={_ => {
        let pos: Model.position = {x: screenX, y: screenY}
        dispatch(
          PipelineModel.AddNode(SignStep({keyId: "default", transparency: true}), pos),
        )
        dispatch(PipelineModel.CloseContextMenu)
      }}
    >
      {"Add Sign Step"->React.string}
    </div>
    <div
      style={menuItemStyle}
      onClick={_ => {
        let pos: Model.position = {x: screenX, y: screenY}
        dispatch(PipelineModel.AddNode(Push({registry: "", repository: ""}), pos))
        dispatch(PipelineModel.CloseContextMenu)
      }}
    >
      {"Add Push"->React.string}
    </div>
    <div
      style={{
        height: "1px",
        background: "rgba(255,255,255,0.1)",
        margin: "4px 0",
      }}
    />
    <div
      style={menuItemStyle}
      onClick={_ => {
        dispatch(PipelineModel.DuplicateNode(""))
        dispatch(PipelineModel.CloseContextMenu)
      }}
    >
      {"Duplicate Selected"->React.string}
    </div>
    <div
      style={{
        padding: "6px 16px",
        cursor: "pointer",
        fontSize: "12px",
        color: "#ef5350",
        background: "transparent",
      }}
      onClick={_ => {
        dispatch(PipelineModel.DeleteSelected)
        dispatch(PipelineModel.CloseContextMenu)
      }}
    >
      {"Delete Selected"->React.string}
    </div>
  </div>
}

// ---------------------------------------------------------------------------
// Main PipelineCanvas component
// ---------------------------------------------------------------------------

@react.component
let make = (
  ~pipeline: PipelineModel.pipeline,
  ~dispatch: PipelineModel.pipelineMsg => unit,
): React.element => {
  // Local state for canvas dimensions (measured from container)
  let containerRef = React.useRef(Nullable.null)
  let (canvasSize, setCanvasSize) = React.useState(() => (1200.0, 800.0))
  let (contextMenu, setContextMenu) = React.useState(() => None)

  // Measure canvas container on mount and window resize
  React.useEffect0(() => {
    let measure = () => {
      switch Nullable.toOption(containerRef.current) {
      | Some(el) => {
          let rect = el["getBoundingClientRect"]()
          setCanvasSize(_ => (rect["width"], rect["height"]))
        }
      | None => ()
      }
    }
    measure()

    let addResizeListener: (unit => unit) => unit = %raw(`
      function(fn) { window.addEventListener("resize", fn) }
    `)
    let removeResizeListener: (unit => unit) => unit = %raw(`
      function(fn) { window.removeEventListener("resize", fn) }
    `)
    addResizeListener(measure)
    Some(() => removeResizeListener(measure))
  })

  // Keyboard shortcuts
  React.useEffect0(() => {
    let handler = (e: {..}) => {
      let key: string = e["key"]
      let ctrlKey: bool = e["ctrlKey"]
      let metaKey: bool = e["metaKey"]

      switch key {
      | "Delete" | "Backspace" => dispatch(PipelineModel.DeleteSelected)
      | "z" if ctrlKey || metaKey => dispatch(PipelineModel.Undo)
      | "a" if ctrlKey || metaKey => {
          e["preventDefault"]()
          dispatch(PipelineModel.SelectAll)
        }
      | "Escape" => {
          dispatch(PipelineModel.CancelConnect)
          dispatch(PipelineModel.CloseContextMenu)
          setContextMenu(_ => None)
        }
      | _ => ()
      }
    }

    let addKeyListener: ({..} => unit) => unit = %raw(`
      function(fn) { window.addEventListener("keydown", fn) }
    `)
    let removeKeyListener: ({..} => unit) => unit = %raw(`
      function(fn) { window.removeEventListener("keydown", fn) }
    `)
    addKeyListener(handler)
    Some(() => removeKeyListener(handler))
  })

  // Mouse move handler (drag nodes, draw connections, pan canvas)
  let handleMouseMove = (e: ReactEvent.Mouse.t) => {
    let mx = Float.fromInt(ReactEvent.Mouse.clientX(e))
    let my = Float.fromInt(ReactEvent.Mouse.clientY(e))

    // Snap coordinates to grid, accounting for zoom and pan
    let (canvasW, canvasH) = canvasSize
    let _ = (canvasW, canvasH)
    let worldX = (mx -. pipeline.panX) /. pipeline.zoom
    let worldY = (my -. pipeline.panY) /. pipeline.zoom

    dispatch(PipelineModel.UpdateDrag(snap(worldX), snap(worldY)))
    dispatch(PipelineModel.UpdateConnect(worldX, worldY))
  }

  // Mouse up handler (end drag or connection)
  let handleMouseUp = (_e: ReactEvent.Mouse.t) => {
    dispatch(PipelineModel.EndDrag)
    dispatch(PipelineModel.CancelConnect)
  }

  // Click on empty canvas — deselect everything
  let handleCanvasClick = (_e: ReactEvent.Mouse.t) => {
    dispatch(PipelineModel.SelectNode(None))
    dispatch(PipelineModel.SelectConnection(None))
    setContextMenu(_ => None)
  }

  // Scroll to zoom
  let handleWheel = (e: ReactEvent.Wheel.t) => {
    e->ReactEvent.Wheel.preventDefault
    let delta = ReactEvent.Wheel.deltaY(e)
    let direction = delta > 0.0 ? -1.0 : 1.0
    let newZoom = Math.max(zoomMin, Math.min(zoomMax, pipeline.zoom +. direction *. zoomStep))
    dispatch(PipelineModel.SetZoom(newZoom))
  }

  // Right-click context menu
  let handleContextMenu = (e: ReactEvent.Mouse.t) => {
    e->ReactEvent.Mouse.preventDefault
    let sx = Float.fromInt(ReactEvent.Mouse.clientX(e))
    let sy = Float.fromInt(ReactEvent.Mouse.clientY(e))
    setContextMenu(_ => Some((sx, sy)))
  }

  // Pan on middle-mouse drag (button 1) or alt+left drag
  let handleMouseDown = (e: ReactEvent.Mouse.t) => {
    let button = ReactEvent.Mouse.button(e)
    let altKey = ReactEvent.Mouse.altKey(e)

    if button === 1 || altKey {
      // Start panning by recording starting position
      // We reuse SetPan with relative offset in move handler
      e->ReactEvent.Mouse.preventDefault
    }
  }

  let (canvasW, canvasH) = canvasSize

  <div
    ref={ReactDOM.Ref.domRef(containerRef)}
    role="main"
    ariaLabel="Pipeline assembly canvas"
    style={Sx.make(
      ~position="relative",
      ~width="100%",
      ~height="100%",
      ~overflow="hidden",
      ~background="#14141e",
      (),
    )}
  >
    // The SVG canvas
    <svg
      width="100%"
      height="100%"
      role="graphics-document"
      ariaLabel="Container assembly pipeline graph"
      onMouseMove={handleMouseMove}
      onMouseUp={handleMouseUp}
      onClick={handleCanvasClick}
      onMouseDown={handleMouseDown}
      onWheel={handleWheel}
      onContextMenu={handleContextMenu}
      style={Sx.make(~display="block", ())}
    >
      <defs>
        // Grid pattern
        <pattern
          id="pipeline-grid"
          width={Float.toString(gridSize)}
          height={Float.toString(gridSize)}
          patternUnits="userSpaceOnUse"
          patternTransform={`translate(${Float.toString(
              fmod(pipeline.panX, gridSize *. pipeline.zoom),
            )} ${Float.toString(
              fmod(pipeline.panY, gridSize *. pipeline.zoom),
            )}) scale(${Float.toString(pipeline.zoom)})`}
        >
          <circle cx="1" cy="1" r="0.5" fill="rgba(255,255,255,0.06)" />
        </pattern>

        // Major grid (every 5 cells = 100px)
        <pattern
          id="pipeline-grid-major"
          width={Float.toString(gridSize *. 5.0)}
          height={Float.toString(gridSize *. 5.0)}
          patternUnits="userSpaceOnUse"
          patternTransform={`translate(${Float.toString(
              fmod(pipeline.panX, gridSize *. 5.0 *. pipeline.zoom),
            )} ${Float.toString(
              fmod(pipeline.panY, gridSize *. 5.0 *. pipeline.zoom),
            )}) scale(${Float.toString(pipeline.zoom)})`}
        >
          <line
            x1="0"
            y1="0"
            x2={Float.toString(gridSize *. 5.0)}
            y2="0"
            stroke="rgba(255,255,255,0.03)"
            strokeWidth="0.5"
          />
          <line
            x1="0"
            y1="0"
            x2="0"
            y2={Float.toString(gridSize *. 5.0)}
            stroke="rgba(255,255,255,0.03)"
            strokeWidth="0.5"
          />
        </pattern>

        // Drop shadow filter for nodes
        <filter id="node-shadow" x="-10%" y="-10%" width="130%" height="130%">
          <feDropShadow dx="1" dy="2" stdDeviation="3" floodColor="#000000" floodOpacity="0.3" />
        </filter>
      </defs>

      // Grid background layers
      <rect width="100%" height="100%" fill="#14141e" />
      <rect width="100%" height="100%" fill="url(#pipeline-grid-major)" />
      <rect width="100%" height="100%" fill="url(#pipeline-grid)" />

      // Transform group (zoom + pan)
      <g
        transform={`translate(${Float.toString(pipeline.panX)}, ${Float.toString(
            pipeline.panY,
          )}) scale(${Float.toString(pipeline.zoom)})`}
      >
        // Connections layer (below nodes)
        {Array.map(pipeline.connections, conn => {
          let isSelected = pipeline.selectedConnection === Some(conn.id)
          renderConnection(conn, pipeline.nodes, isSelected)
        })->React.array}

        // Connection drawing preview
        // (We check the parent state via props — the canvas itself is stateless)
        // This is handled by the parent passing connectState through the pipeline prop.
        // For the preview, we look for an active connect state in the pipeline.
        // Since pipeline doesn't directly hold connectState, the parent should dispatch
        // UpdateConnect which updates a local ref. We render a preview from pipeline coords.

        // Nodes layer
        {Array.map(pipeline.nodes, node => {
          let isSelected = pipeline.selectedNode === Some(node.id)
          renderNode(node, isSelected, dispatch)
        })->React.array}
      </g>

      // Minimap (fixed position, bottom-right corner)
      <g
        transform={`translate(${Float.toString(
            canvasW -. minimapWidth -. minimapPadding,
          )}, ${Float.toString(canvasH -. minimapHeight -. minimapPadding)})`}
      >
        {renderMinimap(
          pipeline.nodes,
          pipeline.connections,
          pipeline.zoom,
          pipeline.panX,
          pipeline.panY,
          canvasW,
          canvasH,
        )}
      </g>

      // Zoom level indicator (top-right)
      <text
        x={Float.toString(canvasW -. 16.0)}
        y="24"
        textAnchor="end"
        fill="rgba(255,255,255,0.3)"
        style={Sx.make(~fontSize="11px", ~pointerEvents="none", ())}
      >
        {(Int.toString(Float.toInt(pipeline.zoom *. 100.0)) ++ "%")->React.string}
      </text>
    </svg>

    // Context menu (HTML overlay, rendered outside SVG for proper layering)
    {switch contextMenu {
    | Some((sx, sy)) => renderContextMenu(sx, sy, dispatch)
    | None => React.null
    }}

    // Toolbar overlay (top-left)
    <div
      role="toolbar"
      ariaLabel="Pipeline canvas toolbar"
      style={Sx.make(
        ~position="absolute",
        ~top="10px",
        ~left="10px",
        ~display="flex",
        ~gap="4px",
        ~padding="4px",
        ~background="rgba(30,30,46,0.9)",
        ~borderRadius="6px",
        ~zIndex="100",
        (),
      )}
    >
      <button
        onClick={_ => dispatch(PipelineModel.SetZoom(Math.min(zoomMax, pipeline.zoom +. zoomStep)))}
        ariaLabel="Zoom in"
        title="Zoom in"
        style={{
          padding: "4px 10px",
          cursor: "pointer",
          border: "none",
          borderRadius: "4px",
          background: "#2a2a3e",
          color: "#e0e0e0",
          fontSize: "14px",
          fontWeight: "600",
        }}
      >
        {"+"->React.string}
      </button>
      <button
        onClick={_ => dispatch(PipelineModel.SetZoom(Math.max(zoomMin, pipeline.zoom -. zoomStep)))}
        ariaLabel="Zoom out"
        title="Zoom out"
        style={{
          padding: "4px 10px",
          cursor: "pointer",
          border: "none",
          borderRadius: "4px",
          background: "#2a2a3e",
          color: "#e0e0e0",
          fontSize: "14px",
          fontWeight: "600",
        }}
      >
        {"-"->React.string}
      </button>
      <button
        onClick={_ => {
          dispatch(PipelineModel.SetZoom(1.0))
          dispatch(PipelineModel.SetPan(0.0, 0.0))
        }}
        ariaLabel="Reset viewport"
        title="Reset zoom and pan"
        style={{
          padding: "4px 10px",
          cursor: "pointer",
          border: "none",
          borderRadius: "4px",
          background: "#2a2a3e",
          color: "#e0e0e0",
          fontSize: "12px",
        }}
      >
        {"1:1"->React.string}
      </button>
      <div style={{width: "1px", background: "rgba(255,255,255,0.1)"}} />
      <button
        onClick={_ => dispatch(PipelineModel.Undo)}
        ariaLabel="Undo (Ctrl+Z)"
        title="Undo"
        style={{
          padding: "4px 10px",
          cursor: "pointer",
          border: "none",
          borderRadius: "4px",
          background: "#2a2a3e",
          color: "#e0e0e0",
          fontSize: "12px",
        }}
      >
        {"Undo"->React.string}
      </button>
      <div style={{width: "1px", background: "rgba(255,255,255,0.1)"}} />
      <button
        onClick={_ => dispatch(PipelineModel.SelectAll)}
        ariaLabel="Select all nodes (Ctrl+A)"
        title="Select all"
        style={{
          padding: "4px 10px",
          cursor: "pointer",
          border: "none",
          borderRadius: "4px",
          background: "#2a2a3e",
          color: "#e0e0e0",
          fontSize: "12px",
        }}
      >
        {"All"->React.string}
      </button>
      <button
        onClick={_ => dispatch(PipelineModel.DeleteSelected)}
        ariaLabel="Delete selected (Del)"
        title="Delete"
        style={{
          padding: "4px 10px",
          cursor: "pointer",
          border: "none",
          borderRadius: "4px",
          background: "#2a2a3e",
          color: "#ef5350",
          fontSize: "12px",
        }}
      >
        {"Del"->React.string}
      </button>
    </div>

    // Pipeline name display (top-center)
    <div
      style={Sx.make(
        ~position="absolute",
        ~top="12px",
        ~left="50%",
        ~transform="translateX(-50%)",
        ~color="rgba(255,255,255,0.5)",
        ~fontSize="13px",
        ~pointerEvents="none",
        (),
      )}
    >
      {pipeline.name->React.string}
    </div>

    // Node count / connection count (bottom-left)
    <div
      style={Sx.make(
        ~position="absolute",
        ~bottom="12px",
        ~left="12px",
        ~color="rgba(255,255,255,0.3)",
        ~fontSize="11px",
        ~pointerEvents="none",
        (),
      )}
    >
      {(Int.toString(Array.length(pipeline.nodes)) ++
      " nodes, " ++
      Int.toString(Array.length(pipeline.connections)) ++
      " connections")->React.string}
    </div>
  </div>
}
